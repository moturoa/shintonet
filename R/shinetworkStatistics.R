#' Utility functions for the package
#' @export
#' @param nodes A dataframe of the nodes in the network
#' @param groupname Optional groupname to be filtered on
#' @rdname shinetworkStatistics
count_nodes <- function(nodes, groupname = NULL){

  if(is.null(groupname)){
    return(nrow(nodes))

  } else {

    filtered_nodes <- nodes %>%
      dplyr::select(id, group) %>%
      dplyr::filter(group == !!groupname) %>%
      unique()

    return (nrow(filtered_nodes))
  }

}

#' Utility functions for the package
#' @export
#' @param edges A dataframe of the edges in the network
#' @param labelname Optional labelname to be filtered on
#' @rdname shinetworkStatistics
count_edges <- function(edges, labelname = NULL){

  if(is.null(labelname)){
    return(nrow(edges))

  } else {
    filtered_edges <- edges %>%
      dplyr::select(from, to, label) %>%
      dplyr::filter(label == !!labelname) %>%
      unique()

    return (nrow(filtered_edges))
  }

}
