#' Build Gov Query
#'
#' A function designed to make it simple to build query urls for the search API
#'
#' @param q query string
#' @param count (single integer) Maximum number of search results to return. If insufficient documents match, as many as possible are returned (subject to the supplied start offset). This may be set to 0 to return no results (which may be useful if only, say, facet values are wanted). Setting this to 0 will reduce processing time.
#' @param start (single integer) Position in search result list to start returning results (0-based) If the start offset is greater than the number of matching results, no results will be returned (but also no error will be returned).
#' @param order (single string) The sort order. A field name, with an optional preceding "-" to sort in descending order. If not specified, sort order is relevance. Only some fields can be sorted on - an HTTP 422 error will be returned if the requested field is not a valid sort field.
#' @param org_any list of organisation names to filter to. returns results if _any_ of the organisations match
#' @param org_all list of organisation names to filter to. returns results if _all_ of the organisations match
#' @param fields a vector of field names to return. See https://docs.publishing.service.gov.uk/apis/search/fields.html
#'
#' @return
#' @export
#'
#' @examples
#' build_gov_query()
build_gov_query <- function(q = "",
                            count = 10,
                            start = 0,
                            order = "-popularity",
                            org_any = NULL,
                            org_all = NULL,
                            fields = NULL) {
  query_vec <- vector(mode = "character")
  
  
  # Base Query --------------------------------------------------------------
  base_query <-
    # Base URL
    urltools::url_decode("https://www.gov.uk/api/search.json") %>%
    
    # Empty Query
    urltools::param_set(key = "q", value = q) %>%
    urltools::param_set(key = "count", value = count) %>%
    urltools::param_set(key = "start", value = start) %>%
    urltools::param_set(key = "order", value = order)
  
  query_vec <- c(query_vec, base_query)
  
  
  # Organisation Filter -----------------------------------------------------
  if (!is.null(org_any) && !is.null(org_all)) {
    stop("use either the org_any filter, or the org_all filter, not both!")
  }
  
  if (!is.null(org_any)) {
    org_string <-
      paste0("filter_any_organisations=", org_any, collapse = "&")
    
    query_vec <- c(query_vec, org_string)
  }
  
  if (!is.null(org_all)) {
    org_string <-
      paste0("filter_all_organisations=", org_all, collapse = "&")
    
    query_vec <- c(query_vec, org_string)
  }
  
  # Fields Filter -----------------------------------------------------------
  if (!is.null(fields)) {
    field_string <-
      paste0("fields=", fields, collapse = "&")
    
    query_vec <- c(query_vec, field_string)
  }
  
  # Assemble URL ------------------------------------------------------------
  query_url <- paste0(query_vec, collapse = "&")
  
  return(query_url)
}

#' Execute Gov Query
#'
#' A function which takes urls made using `build_gov_query`, executes the
#' resulting query, then formats the result as a dataframe
#' 
#' Note - this will result in some columns which have 0 length vectors in some of the rows, where the page does not have that attribute
#' These can be hard to work with - you can filter them out using something like:
#' dplyr::filter(purrr::map(<your-col-name>, is.list) == TRUE)
#'
#' @param query_url A query url built by build_gov_query
#'
#' @return a dataframe of query results
#' @export
#'
#' @examples
#' 
#' execute_gov_query(build_gov_query())
execute_gov_query <- function(query_url) {
  
  # Execute query and fetch result json
  result_list <-
    jsonlite::read_json(query_url)
  
  # Store the results part of the results list as a list column in a tibble
  result_tibble <-
    tibble::tibble(
      result_col = result_list$results
    )
  
  # unnest that tibble
  wide_tibble <-
    tidyr::unnest_wider(data = result_tibble, col = result_col)
  
  return(wide_tibble)
}

#' URL With Page function to modify a url to set count & start parameters -
#' useful when mapping over pages of query
#'
#' @param url url to modify - as prepared by build_gov_query
#' @param count (single integer) Maximum number of search results to return. If
#'   insufficient documents match, as many as possible are returned (subject to
#'   the supplied start offset). This may be set to 0 to return no results
#'   (which may be useful if only, say, facet values are wanted). Setting this
#'   to 0 will reduce processing time.
#' @param start (single integer) Position in search result list to start
#'   returning results (0-based) If the start offset is greater than the number
#'   of matching results, no results will be returned (but also no error will be
#'   returned).
#'
#' @return url with count and start modified
#' @export
#'
#' @examples
#' # get second 10 pages
#' url_with_page(build_gov_query(), count = 10, start = 10)
url_with_page <- function(url, count, start) {
  
  url <- urltools::param_set(url, key = "count", value = count)
  url <- urltools::param_set(url, key = "start", value = start)
  
  return(url)
}

#' Paginate Query
#'
#' @param chunksize number of results to get with each query
#' @param query_url A query url built by build_gov_query
#' @param limit an upper limit on the total number of results to collect (rounded to chunksize)
#'
#' @return
#' @export
#'
#' @examples
#' query_url <- build_gov_query()
#' paginate_query(query_url = query_url, chunksize = 3, limit = 10)
#'  
paginate_query <- function(query_url, chunksize = 10, limit = 45) {
  
  # Get Number of Available Results -----------------------------------------
  # build query, but with count set to 0, to get the number of results
  count_0_url <-
    urltools::param_set(urls = query_url, key = "count", value = "0")
  
  query_list <- jsonlite::read_json(path = count_0_url)
  
  #browser()
  
  nresults <- purrr::pluck(.x = query_list, "total")
  
  message(nresults, " available for query.")
  message(limit, " set as limit.")
  
  # Prepare Iteration Vars --------------------------------------------------
  # Build a vector of start numbers, based on the chunksize and total number of results
  starts <-
    seq.int(
      from = 0,
      to = nresults,
      by = chunksize
    )
  
  # if a limit is specified, chop items off starts less than the limit
  if (!is.null(limit)) {
    starts <- starts[starts < limit]
  } else {
    message( "no limit set, collecting ", nresults, " pages")
  }
  
  message("Query to be made in ", length(starts), " chunks of ", chunksize, ".")
  
  pb <- dplyr::progress_estimated(length(starts))
  # Prepare Mapping Functions ------------------------------------------------

  # make a safe version of query so we catch errors without stopping
  safe_execute_gov_query <- purrr::safely(.f = execute_gov_query)
  
  # note query_url & chunksize are inherited from defining env (this function)
  paged_query <- function(start) {
    
    pb$tick()$print()
    
    safe_execute_gov_query(
      url_with_page(
        url = query_url,
        count = chunksize, 
        start = start
      )
    )
  }
  
  # Map Query ---------------------------------------------------------------------
  # map execute_gov_query over starts to get all the results
  results <-
    purrr::map(
      .x = starts,
      .f = paged_query
    )
  
  # Tidy up results ---------------------------------------------------------
  transposed_results <- purrr::transpose(.l = results)
  
  result_list <- purrr::compact(.x = transposed_results$result)
  
  error_list <- purrr::compact(.x = transposed_results$error)
  
  # Notify user of queries & errors -----------------------------------------
  if (length(error_list) > 0) {
    
    warning("\n", length(error_list), " queries resulted in error")
    
  } else if (length(result_list) > 0 ) {
    
    message("\n", length(result_list), " queries made successfully")
    
  } else {
    
    warning("\n no results")
    
  }
  
  result_df <- dplyr::bind_rows(result_list)
  
  return(result_df)
}

#' Get Government Organisations
#'
#' A function to get government organisations as a nice tibble
#'
#' @param limit limit on number of organisations to fetch - default 10k will get all orgs, which as of Jan 2020 number 1099.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' get_gov_orgs()
get_gov_orgs <- function(limit = 10000) {
  
  # TODO - figure out if there is a better way to do this than using a large
  # number current number 10k, seems safe given api doesnt complain and there are
  # ~1k orgs
  org_query <-
    paste0(
      "https://www.gov.uk/api/search.json?count=0&aggregate_organisations=",
      limit
    )
  
  # Execute query and fetch result json
  result_list <-
    jsonlite::read_json(org_query)
  
  # Store the results part of the results list as a list column in a tibble
  result_tibble <-
    tibble::tibble(
      result_col = result_list$aggregates$organisations$options
    )
  
  # unnest that tibble
  wide_tibble <-
    tidyr::unnest_wider(data = result_tibble, col = result_col)
  
  wide_tibble <-
    tidyr::unnest_wider(data = wide_tibble, col = value)
}
