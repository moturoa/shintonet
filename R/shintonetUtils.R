#' Utility functions for the package
#' @export
#' @param config A structured list form a config file which specifies the script/layout of how the network must be built
#' @param datasets A list of datasets which contain the data for the network
#' @rdname shintonetUtils
create_network_nodes <- function(config, datasets){
  network_nodes <-  data.frame(
    id = c(),
    label = c(),
    group =  c(),
    title =  c(),
    level =  c())

  node_kinds <- names(config$nodes)
  sapply(node_kinds, function(nk){
    network_nodes <<- network_nodes %>%
      shintonet::add_net_nodes(datasets[[config$nodes[[nk]]$dataset]], config$nodes[[nk]])
  })

  return(network_nodes)
}

#' @export
#' @param nodes a dataframe with the nodes that already have been added
#' @param config A structured list form a config file which specifies the script/layout of the nodes of the network that must be built
#' @param datasets The dataset from which to obtain the node-data
#' @rdname shintonetUtils
add_net_nodes <- function(nodes, dataset, config_file){
  if(!is.null(dataset) && nrow(dataset) > 0){
    new_nodes <- data.frame(id = dataset[[config_file$id]],
                            label = dataset[[config_file$label]],
                            group = config_file$group,
                            title = dataset[[config_file$title]],
                            level = config_file$level)

    nodes <- rbind(nodes, new_nodes)
  }
  return(nodes)
}

#' @export
#' @param config A structured list form a config file which specifies the script/layout of how the network must be built
#' @param datasets A list of datasets which contain the data for the network
#' @rdname shintonetUtils
create_network_edges <- function(config, datasets){
  network_edges <-  data.frame(
    from = c(),
    to =  c(),
    title =  c(),
    label =  c())

  edge_kinds <- names(config$edges)
  sapply(edge_kinds, function(ek){
    network_edges <<- network_edges %>%
      shintonet::add_net_edges(datasets[[config$edges[[ek]]$dataset]], config$edges[[ek]])
  })

  return(network_edges)
}

#' @export
#' @param nodes a dataframe with the edges that already have been added
#' @param config A structured list form a config file which specifies the script/layout of the edges of the network that must be built
#' @param datasets The dataset from which to obtain the edge-data
#' @rdname shintonetUtils
add_net_edges <- function(edges, dataset, config_file){
  if(!is.null(dataset) && nrow(dataset) > 0){
    new_edges <- data.frame(from = dataset[[config_file$fromCol]],
                            to = dataset[[config_file$toCol]],
                            title = config_file$title,
                            label = config_file$label)

    edges <- rbind(edges, new_edges)
  }
  return(edges)
}
