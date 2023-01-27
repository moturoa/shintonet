#' Shiny module for creation and visualisation of networks
#' #' @export
#' @param id Shiny input id
#' @rdname shintoNetworkModule


shintoNetworkUI <- function(id){
  ns <- shiny::NS(id)

  softui::fluid_page(
    softui::fluid_row(
      shiny::column(12,
                    visNetwork::visNetworkOutput(ns("shinto_network"))
      )
    )
  )
}

#' @export
#' @param config A structured list form a config file which specifies the script/layout of how the network must be built
#' @param datasets A list of datasets which contain the data for the network
#' @param hierarchical A boolean specifying 'visHierarchicalLayout'
#' @rdname shintoNetworkModule

shintoNetworkModule <- function(input, output, session, config, datasets, hierarchical = reactive(NULL), show_labels = reactive(NULL)){

  ns <- session$ns


  output$shinto_network <- visNetwork::renderVisNetwork({
    nodes <- shinetwork::create_network_nodes(config, datasets)
    edges <- shinetwork::create_network_edges(config, datasets)

    if(is.null(show_labels())){
      show_labels <- TRUE
    } else {
      show_labels <- show_labels()
    }

    if(is.null(hierarchical())){
      hierarchical <- FALSE
    } else {
      hierarchical <- hierarchical()
    }


    if(!show_labels){
      edges <- edges %>% dplyr::select(!label)
    }

    res_network <- visNetwork::visNetwork(nodes, edges, width = "100%")  %>%
      visNetwork::visNodes(mass=config$nodeSettings$mass) %>%
      visNetwork::visEdges(arrows = list(to = config$edgeSettings$arrowTo),
                           color = list(color = config$edgeSettings$edgeColor,
                                        highlight = config$edgeSettings$edgeHighlight),
                           smooth = config$edgeSettings$smooth) %>%
      visNetwork::visInteraction(tooltipStyle = 'position: fixed;visibility:hidden; background-color: #efefef;  font-size: 14px; text-align: center; border-radius: 6px; padding: 8px; margin: 3px;') %>%
      visNetwork::visHierarchicalLayout(enabled=hierarchical)

    node_kinds <- names(config$nodes)

    sapply(node_kinds, function(nk){
      res_network <<- res_network %>%
        visNetwork::visGroups(groupname = config$nodes[[nk]]$group,
                              shape = config$nodes[[nk]]$shape,
                              image = config$nodes[[nk]]$image,
                              borderWidth = config$nodes[[nk]]$borderWidth,
                              color = list(background = config$nodes[[nk]]$colorBackground,
                                           border = config$nodes[[nk]]$colorBorder,
                                           highlight = list(background = config$nodes[[nk]]$highlightBackground,
                                                            border = config$nodes[[nk]]$highlightBorder)),
                              font = list(color = config$nodes[[nk]]$fontColor))
    })

    res_network

  })


}



