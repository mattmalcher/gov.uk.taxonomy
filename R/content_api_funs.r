#' Get Taxon Dataframe
#'
#' Function to take a page and get the taxons it relates to
#'
#' @param path content api path to query - i.e. www.gov.uk/api/content/<page>
#'
#' @return a dataframe of information about the taxon
#' @export
#'
#' @examples
#' get_taxon_df("https://www.gov.uk/api/content/")
get_taxon_df <- function(path = "https://www.gov.uk/api/content/") {
  content <- jsonlite::read_json(path = path)

  # format of root taxon (i.e. gov.uk/ ) is different from others (inc world)
  if (path %in% c(
    "https://www.gov.uk/api/content/",
    "https://www.gov.uk/api/content",
    "govuk_root.json"
  ) # for tests
  ) {

    # root_content$links$level_one_taxons
    child_taxons <- purrr::pluck(content, "links", "level_one_taxons")
  } else {
    # root_content$links$child_taxons
    child_taxons <- purrr::pluck(content, "links", "child_taxons")
  }

  # there might not be any child taxons, so as a fallback, return an empty tibble
  if (is.null(child_taxons)) {
    taxon_tibble <- tibble::tibble()
    # message("no child taxons for: ", path)
  } else {
    list_col_tibble <- tibble::tibble(taxon = child_taxons)

    taxon_tibble <-
      tidyr::unnest_wider(
        data = list_col_tibble,
        col = taxon
      )

    taxon_tibble <-
      tidyr::hoist(
        .data = taxon_tibble,
        .col = links,
        parent_taxons = list("parent_taxons", 1, "base_path")
      )
  }

  return(taxon_tibble)
}


#' Safe Mapper
#'
#' a wrapper for map, that uses safely to bypass any errors and raises a warning
#'
#' @param .x an item to map over
#' @param .f a function to map which produces a dataframe / tibble
#'
#' @return a dataframe containing the results of mapping the function
#' @export
#'
#' @examples
#' 
#' safe_map(c("https://www.gov.uk/api/content/"), get_taxon_df)
safe_map <- function(.x, .f) {
  safe_f <- purrr::safely(.f = .f)

  map_result <- purrr::map(
    .x = .x,
    .f = safe_f
  )

  results <- purrr::transpose(.l = map_result)
  errors <- purrr::compact(results[["error"]])

  err_count <- length(errors)

  if (err_count > 0) {
    warning(err_count, " errors encountered. Example: ")
    print(errors[[1]])
  }

  dplyr::bind_rows(results[["result"]])
}

#' Build Taxon Dataframe
#'
#' A wrapper function around `get_taxon_df` which works from the starting taxons
#' (gov.uk & gov.uk/world/all) and then recursively on child taxons to build a
#' complete taxonomy tree dataframe.
#'
#' @param include_world include the world taxonomy tree? i.e. taxons below
#'   world/all
#' @param filter_world_help_services Filter the world taxon to remove "help & services in
#' @param max_levels maximum number of levels to go down the taxon
#'
#' @return
#' @export
#'
#' @examples
#' 
#' build_taxon_df(max_levels = 2)
build_taxon_df <- function(include_world = TRUE,
                           filter_world_help_services = TRUE,
                           max_levels = Inf) {

  # Level 1 Taxons ----------------------------------------------------------

  start_taxons <- c("https://www.gov.uk/api/content")

  # add root of the world taxon tree
  if (include_world) {
    start_taxons <- c(start_taxons, "https://www.gov.uk/api/content/world/all")
  }

  message("Getting taxon level: 1")

  taxon_list <- list()

  taxon_list[[1]] <-
    safe_map(
      .x = start_taxons,
      .f = get_taxon_df
    )

  # Optional Filter on World Pages ------------------------------------------

  # filter out world help pages
  if (filter_world_help_services) {
    taxon_list[[1]] <-
      dplyr::filter(
        taxon_list[[1]],
        !stringr::str_detect(title, pattern = "help and services")
      )
  }

  # While Loop --------------------------------------------------------------

  # set up while conditions
  taxons_remain <- TRUE
  level <- 1

  while (taxons_remain && (level < max_levels)) {

    # increment iteration var based on length of list
    level <- length(taxon_list) + 1
    
    n_children <- length(taxon_list[[level - 1]]$api_url)

    message(
      "\nGetting taxon level: ", 
      level, 
      ". (Made up of ", 
      n_children, 
      " children of level ", 
      level - 1, 
      ").")

    # make progress bar for level
    pb <- dplyr::progress_estimated(n_children)

    # add item to list
    taxon_list[[level]] <-
      safe_map(
        .x = taxon_list[[level - 1]]$api_url,
        .f = function(x) {
          pb$tick()$print() # increment progress bar
          get_taxon_df(x)
        }
      )

    # if looking for child taxons yields nothing, set the while condition false
    if (nrow(taxon_list[[level]]) == 0) {
      taxons_remain <- FALSE
    }
  }

  # Tidy Results ------------------------------------------------------------

  # remove empty list items
  taxon_list <- purrr::compact(taxon_list)

  # bind contents of list back together
  taxon_df <- dplyr::bind_rows(taxon_list, .id = "level")
  
  # Add World Root Taxon Row ------------------------------------
  # This requires some manual patching
  
  # Make row for world/all and add it to taxon df
  if (include_world) {
    
    world_all_list <-
      taxon_df %>%
      dplyr::filter(parent_taxons == "/world/all") %>%
      dplyr::slice(1) %>%
      dplyr::pull("links") %>% 
      purrr::pluck(1,"parent_taxons",1) 
    
    # set links of world/all taxon to match money taxon, giving it same parent
    # without doing this the world pages have no parent and you end up with 2 trees
    world_all_list$links <- 
      taxon_df %>%
      dplyr::filter(base_path == "/money") %>% # identify a taxon in the regular root.
      dplyr::pull("links")
    
    world_all_list$details <- list(NA)
    
    world_all_list$base_path <- "/world/all"
    
    world_taxon_row <- tibble::as_tibble(world_all_list) 
    
    # Add the row on
    taxon_df <- dplyr::bind_rows(world_taxon_row, taxon_df)
    
  }

  # Add Homepage Root Taxon Row ------------------------------------
  root_taxon_list <- 
    taxon_df %>%
    dplyr::filter(base_path == "/money") %>% # identify a taxon in the regular root.
    dplyr::pull("links") %>% 
    purrr::pluck(1,"root_taxon",1) 
  
  # Give the links list an entry so we dont end up with nothing coercing to tibble
  root_taxon_list$links <- list(NA)
  
  # make this a tibble row
  root_taxon_row <- tibble::as_tibble(root_taxon_list)
  
  # bind
  taxon_df <- dplyr::bind_rows(root_taxon_row, taxon_df)
  

  
  # add level for taxons rows bound on after main bind_rows
  taxon_df <- tidyr::replace_na(
    data = taxon_df, 
    replace = list("level" = "0")
    )

return(taxon_df)
  
}
