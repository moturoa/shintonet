#' Shiny module for creation and visualisation of networks
#' #' @export
#' @param id Shiny input id
#' @rdname shintoNetworkModule


shintoNetworkUI <- function(id){
  ns <- shiny::NS(id)

  softui::fluid_page(
    softui::fluid_row(
      shiny::column(12,
                    visNetwork::visNetworkOutput(ns("shinto_network")),
                    uiOutput(ns("expand_button_ui")),
                    uiOutput(ns("download_button_ui"))
      )
    )
  )
}

#' @export
#' @param config A structured list form a config file which specifies the script/layout of how the network must be built
#' @param nodes_data Optional dataset of precalculated nodes. Must be in the same format as if they would be created by shinetwork::create_network_nodes
#' @param edges_data Optional dataset of precalculated edges. Must be in the same format as if they would be created by shinetwork::create_network_edges
#' @param datasets A list of datasets which contain the data for the network, and can be submitted if nodes and edges have not been calculated yet.
#' @param hierarchical A boolean specifying 'visHierarchicalLayout'
#' @param show_labels A boolean specifying whether labels on the edges must be shown
#' @param hover_function A function specifying how the title of the nodes must be altered so it is shown in the in the hoover
#' @param hover_groups Vector specifying which groups to apply the hover upon. If the hover_function is set and hover_groups is NULL, the function is applied on all nodes.
#' @param expanded_data List with datasets for nodes and edges from expansion action
#' @rdname shintoNetworkModule

shintoNetworkModule <- function(input, output, session, config, nodes_data = reactive(NULL), edges_data = reactive(NULL),
                                datasets = reactive(NULL), hierarchical = reactive(NULL), show_labels = reactive(NULL),
                                hover_function = NULL, hover_groups = NULL, expanded_data = reactive(NULL)){

  ns <- session$ns

  nodes <- reactive({

    if(is.null(nodes_data())){
      if(is.null(datasets())){
        shinytoastr::toastr_error("No nodes have been supplied, but also no datasets have been supplied.
                                  Either nodes or a list of datasets must be supplied")
      } else {
        nodes <- shinetwork::create_network_nodes(config, datasets())
      }
    } else {
      nodes <- nodes_data()
    }
    if(!is.null(expanded_data())){
      if(nrow(expanded_data()$expanded_nodes) > 0){
        nodes <- rbind(nodes, expanded_data()$expanded_nodes) %>%
          unique()
      }
    }


    nodes
  })

  edges <- reactive({
    if(is.null(edges_data())){
      if(is.null(datasets())){
        shinytoastr::toastr_error("No edges have been supplied, but also no datasets have been supplied.
                                  Either edges or a list of datasets must be supplied")
      } else {
        edges <- shinetwork::create_network_edges(config, datasets())
      }
    } else {
      edges <- edges_data()
    }

    if(!is.null(expanded_data())){
      if(nrow(expanded_data()$expanded_edges) > 0){
        edges <- rbind(edges, expanded_data()$expanded_edges) %>%
          unique()
      }
    }

    edges
  })

  res_network <- reactive({

    nodes <- nodes()
    edges <- edges()

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

    if(!is.null(hover_function)){
      if(is.null(hover_groups)){
        nodes <- nodes %>%
          dplyr::rowwise() %>%
          dplyr::mutate(title = do.call(hover_function, list(title)))
      } else {
        nodes_altered <- nodes %>%
          dplyr::filter(group %in% hover_groups) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(title = do.call(hover_function, list(title)))

        nodes_not_altered <- nodes %>%
          dplyr::filter(!(id %in% nodes_altered$id))

        nodes <- rbind(nodes_not_altered, nodes_altered)
      }


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
      visNetwork::visInteraction(tooltipStyle = 'position: fixed;visibility:hidden; background-color: #efefef;  font-size: 14px; text-align: center; border-radius: 6px; padding: 8px; margin: 3px;',
                                 navigationButtons = config$networkSettings$navigation) %>%
      visNetwork::visHierarchicalLayout(enabled=hierarchical) %>%
      visNetwork::visOptions(manipulation = config$networkSettings$manipulation)

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

    if(config$networkSettings$expandable){

      clickid <- ns("current_node_id")
      res_network <- res_network %>%
        visNetwork::visEvents(click = paste0("function(nodes){Shiny.onInputChange('", clickid, "', nodes.nodes[0]);;}")
        )

    }

    res_network


  })


  output$shinto_network <- visNetwork::renderVisNetwork({

    res_network()

  })

  output$download_button_ui <- renderUI({
    if(config$networkSettings$savable){
      softui::fluid_row(
        shiny::column(12,
                     downloadButton(ns("btn_export"), "Exporteer", icon = icon("escape"),
                                    class = "btn-lg btn-light")
        )
      )
    }
  })

  output$btn_export <- downloadHandler(
    filename = function() {
      paste('network-', Sys.Date(), '.html', sep='')
    },
    content = function(network) {
      res_network() %>%
        visNetwork::visSave(network)
    }
  )

  clicked_node <- reactiveVal(NULL)

  output$expand_button_ui <- shiny::renderUI({
    req(input$current_node_id)
    softui::fluid_row(
      shiny::column(12,
                    softui::action_button(ns("btn_expand_node"), "Expand",
                                          icon = softui::bsicon("arrows-angle-expand"), status = "success")
      )
    )
  })


  observeEvent(input$btn_expand_node, {
    req(input$current_node_id)
    group <- nodes() %>%
      dplyr::filter(id == !!input$current_node_id) %>%
      dplyr::select(group) %>%
      unique() %>%
      dplyr::pull(group)
    clicked_node(list("node_id" = input$current_node_id, "group" = group))
  })

  return_list <- reactive({
    if(is.null(nodes_data()) && is.null(edges_data())){
      list(
        "clicked_node" = clicked_node(),
        "network_nodes" = nodes(),
        "network_edges" = edges()
      )

    } else {
      list(
        "clicked_node" = clicked_node()
      )
    }
  })

  return(reactive(return_list()))


}



