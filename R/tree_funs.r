#' Prepare Taxonomy Dataframe
#'
#' @param taxon_df taxons dataframe as generated by `build_taxon_df()`
#'
#' @return
#' @export
#'
#' @examples
#' prepare_taxonomy_df(taxon_df)
prepare_taxonomy_df <- function(taxon_df) {
  
  # drop bits we wont use
  relation_cols <-
    dplyr::select(
      .data = taxon_df,
      -c("api_path", # we have api url
         "locale",
         "public_updated_at",
         "schema_name",
         "withdrawn",
         "phase",
         "details",
         "document_type" # always taxon...
      )
    ) 
  
  # extract parent / root taxon from links
  relation_cols <-
    tidyr::hoist(
      .data = relation_cols,
      .col = links, 
      parent_id = list("parent_taxons", 1, "content_id"),
      root_id = list("root_taxon", 1, "content_id")
    ) 
  
  #merge these into one column for convenience
  relation_cols <-
    tidyr::unite(
      data = relation_cols, 
      col = "parent_id",
      parent_id, 
      root_id, 
      na.rm = TRUE)
  
  return(relation_cols)
}

#' Sum Property Up Tree
#'
#' Recursive function to be applied to a tree node,
#' will *sum* a specified quantity from all child nodes, and grandchildren etc
#'
#' Does this by calling itself on the $children of a node.
#' Once it reaches a leaf, there are no children.
#'
#' Works as a side effect, output does not need to be assigned
#'
#' @param node data.tree node, for which a specified quantity is aggregated
#' @param property name of a property to be aggregated
#'
#' @return a data.tree with <property>_agg as a property
#' @export
#'
#' @examples
sum_up_tree <- function(node, property) {
  
  # use [[ ]] - as $ wont work here
  node[[paste0(property, "_agg")]] <-
    
    sum(c(
      node[[property]],
      purrr::map_dbl(
        .x = node$children,
        .f = sum_up_tree, # calls itself on child nodes - recursive!
        property = property
      )
    ),
    na.rm = TRUE
    )
}

#' List Up Tree
#'
#' Recursive function to be applied to a tree node,
#' will aggregate a specified *list* from all child nodes, and grandchildren etc
#'
#' Works as a side effect, output does not need to be assigned
#'
#' @param node data.tree node, for which a specified quantity is aggregated
#' @param property name of a property to be aggregated
#'
#' @return a data.tree with <property>_agg as a property
#' @export
#'
#' @examples
#' 
#' list_up_tree(node = taxonomy_tree, property = "name")
list_up_tree <- function(node, property) {
  
  # use [[ ]] - as $ wont work here
  node[[paste0(property, "_agg")]] <-
    
    # remove any duplicates
    unique(
      # combine list from node itself, with list from children
      c(
        # unlist the existing property and map results to keep the list flat!
        unlist(node[[property]]),
        
        unlist(
          purrr::map(
            .x = node$children,
            .f = list_up_tree, # calls itself on child nodes - recursive!
            property = property
          )
        )
      )
    )
}

#'Remove aggregated quantity if it exists in a child node
#'
#'Idea is that you can aggregate a property up a tree (taxon id), then prune the
#'tree, then apply this function to end up with taxons that were originally on
#'pruned nodes, on their closest unpruned parent
#'
#'@param node data.tree node, where a quantity has been aggregated using
#'  `list_up_tree`()
#'@param property name of the property which was originally aggregated (i.e.
#'  same char vector you passed to `list_up_tree`())
#'
#'@return a tree where the aggregated field no longer contains ids if they are
#'  elsewhere in the tree
#'@export
#'
#' @examples
#'
#' ### Aggregate some property
#' list_up_tree(node = taxonomy_tree, property = "name")
#'
#' ### Prune tree
#' data.tree::Prune(taxonomy_tree, function(x) x$n_pages_agg >= threshold)
#'
#' ### remove properties from the aggregate if they are still in the tree
#' remove_agg_if_in_child(node = taxonomy_tree, property = "name")
#' 
remove_agg_if_in_child <- function(node, property) {
  
  # Build Function to apply to nodes
  remove_if_in_child <- function(node){
    
    node[[paste0(property, "_agg")]] <- {
      
      existing <- unlist(node[[paste0(property, "_agg")]])
      
      in_children <- unlist(
        purrr::map(
          .x = node$children,
          .f = getElement, 
          name = paste0(property, "_agg")
        )
      )
      
      setdiff(existing,in_children)
      
    }
      
  }
  
  # apply remove_if_in_child to nodes, starting at the root and working to leaves
  node$Do(fun = remove_if_in_child,
          traversal = "pre-order")
}