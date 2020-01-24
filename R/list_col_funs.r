#' Filter List Column
#'
#' This filter works for a list column by creating an intermediary logical vector.
#  If _any_ of the items in the list col match the picklist item, it is kept
#'
#' @param df dataframe with a list column
#' @param items a list of items to look for in the list column
#' @param column the list column
#'
#' @return
#' @export
#'
#' @examples
#' 
#' related_pages <- 
#'   filter_list_col(
#'     .data = pages_df, 
#'     column = taxons, 
#'     items = taxon_group
#'   )
#' 
filter_list_col <- function(.data, column, items ) {
  
  #quote the column
  col_quo <- enquo(column)
  
  # filter the page data
  dplyr::filter(
    .data = .data,
    TRUE == purrr::map(
      .x = !!col_quo,
      .f = function(x) {
        any(x %in% items)
      }
    )
  )
}

#' Make Taxon Named List
#'
#' little wrapper around setNames to make a named list suitable for
#' `summary_by_taxon()`
#'
#' @param taxon_df a taxon dataframe, including a name_agg column and a to
#'   column. These are created by using `data.tree::ToDataFrameNetwork` on a
#'   data.tree which has had the name (taxon) field aggregated using the
#'   `list_up_tree` function. See `generate_trees.R` in data-raw folder for example.
#'
#' @return
#' @export
#'
#' @examples
#' make_named_taxon_list(taxonomy_df_lower_threshold)
make_named_taxon_list <- function(taxon_df){
  
  setNames(object = taxon_df$name_agg,
           nm = taxon_df$to)
}

#' Summary by Taxon Groups
#' 
#' A function, which given some page data, tagged by taxon (where each page may relate to multiple taxons) can calculate summaries for groups of taxons
#' 
#'
#' @param page_data a dataframe of page data, which includes 
#' @param taxon_col 
#' @param named_taxon_list 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' taxon_group_page_counts <-
#'summary_by_taxon_groups(
#'  page_data = pages_df, 
#'  taxon_col = taxons, 
#'  named_taxon_list = taxon_groups,
#'  n = n()
#'  ) 
#' 
summary_by_taxon_groups <- 
  function(page_data, taxon_col, named_taxon_list, ...) {
    
    #quote the taxon column
    col_quo <- enquo(taxon_col)
    
    summary_for_taxon_group <- function(taxon_group) {
      # note, defined in the context of summary_for_picklist
      # (so no need to pass page data & column explicitly)
      
      # increment progress bar
      pb$tick()$print()
      
      filter_list_col(
        .data = page_data, 
        column = !!col_quo, 
        items = taxon_group) %>%
        # passed the dots {...} which means that it can calculate an arbitrary summary
        dplyr::summarise(...)
    }
    
    # start progress bar
    pb <- dplyr::progress_estimated(length(named_taxon_list))
    
    # iterate over the picklist, for each item,
    # calculating the summary as passed in by the dots.
    output <-
      purrr::map_dfr(
        .x = named_taxon_list,
        .f = summary_for_taxon_group,
        .id = "taxon" # id taken from named list
      )
    
    return(output)
  }