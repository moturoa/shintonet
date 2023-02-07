#' Utility functions for the package
#' @export
#' @param nodes A dataframe of the nodes in the network
#' @param groupname Optional groupname to be filtered on
#' @rdname shintonetStatistics
count_nodes <- function(nodes, groupname = NULL){

  if(is.null(groupname)){
    return(nrow(nodes))

  } else {

    if(nrow(nodes) == 0){
      return(0)
    } else {
      filtered_nodes <- nodes %>%
        dplyr::select(id, group) %>%
        dplyr::filter(group == !!groupname) %>%
        unique()

      return (nrow(filtered_nodes))
    }
  }

}

#' @export
#' @param edges A dataframe of the edges in the network
#' @param labelname Optional labelname to be filtered on
#' @rdname shintonetStatistics
count_edges <- function(edges, labelname = NULL){

  if(is.null(labelname)){
    return(nrow(edges))

  } else {
    if(nrow(edges) == 0){
      return(0)
    } else {
      filtered_edges <- edges %>%
        dplyr::select(from, to, label) %>%
        dplyr::filter(label == !!labelname) %>%
        unique()

      return (nrow(filtered_edges))
    }
  }

}


# Nog niet @ param export
#' @param edges A dataframe of the edges in the network
#' @param start_node The node which we want to get connections to
#' @param directed Boolean specifying if direction should be taken into account
#' @rdname shintonetStatistics
# get_connected_nodes(edges, start_node, directed = TRUE){
#
#   browser()
#
# }
