#' Generate visnetwork
#'
#' @param network_df dataframe to turn into a network
#' @param threshold_field field to be used to prune the network
#' @param threshold_value value to be used in the `threshold_field` to prune the network
#' @param size_field field to use as the network size
#' @param tooltip_field field to use as hover text on the nodes
#' @param ... additional arguments passed to the call to visNetwork
#' @param hierarchical should the nodes be laid out in a heirachical way
#' @param igraph_init use igraph package to initialise positions - faster but uglier
#' @param seed set the random seed for layout consistency
#' @param smooth_edges should edges be rendered as bezier curves - slower but results in nicer layout.
#'
#' @return
#' @export
#'
#' @examples
#' gen_visnetwork(
#'    network_df = taxonomy_df_lower_threshold,
#'    hierarchical = TRUE 
#' )
gen_visnetwork <- function(
  network_df, 
  threshold_field = n_pages_agg, 
  threshold_value = 0,
  size_field = NULL,
  tooltip_field = "title",
  width = "1000",
  height = "600",
  hierarchical = FALSE,
  igraph_init = FALSE,
  seed = 123,
  smooth_edges = TRUE,
  ...
){
  
  if (threshold_value > 0 ){
    
    field_quo <- rlang::enquo(threshold_field)
    
    network_df <- 
      dplyr::filter(.data = network_df,
                    !!field_quo >= threshold_value
      )
  }
  
  if (!is.null(size_field)) {
    nodes <-  
      network_df %>%
      dplyr::select(id = to,
                    label = base_path,
                    group = level,
                    size = dplyr::one_of(size_field),
                    title = dplyr::one_of(tooltip_field)
      ) %>%
      dplyr::mutate(size = 5 * log(size))
    
  } else {
    
    nodes <-  
      network_df %>%
      dplyr::select(id = to,
                    label = base_path,
                    group = level,
                    title = dplyr::one_of(tooltip_field)
      )
    
  }
  
  edges <- 
    network_df %>%
    dplyr::select(
      from,
      to
    ) %>% 
    dplyr::filter(from != "")
  
  base_network <-
    visNetwork::visNetwork(
      nodes = nodes, 
      edges = edges,
      ...
    ) 
  
  network_w_opts <-
    visNetwork::visPhysics(
      graph = base_network,
      stabilization = TRUE,
      solver = "barnesHut",#"repulsion",
      enabled = TRUE
    )
  
  network_w_opts <-
    visNetwork::visEdges(
      graph = network_w_opts,
      smooth = list("enabled" = smooth_edges)
    )
  
  network_w_opts <-
    visNetwork::visLayout(
      graph = network_w_opts,
      randomSeed = seed,
      improvedLayout = TRUE
    )
  
  if (hierarchical) {
    network_w_opts <-
      visNetwork::visHierarchicalLayout(
        graph = network_w_opts,
        enabled = hierarchical, 
        levelSeparation = NULL,
        nodeSpacing = NULL, 
        treeSpacing = NULL, 
        blockShifting = FALSE,
        edgeMinimization = TRUE, 
        parentCentralization = NULL,
        direction = "LR", 
        sortMethod = "directed"
      )
  }
  
  if (igraph_init) {
    
    network_w_opts <- 
      visNetwork::visIgraphLayout(
        graph = network_w_opts,
        physics = !igraph_init,
        randomSeed = seed,
        layout = "layout_with_fr",
        niter = 1000
        
      )
  }
  
  return(network_w_opts)
}
