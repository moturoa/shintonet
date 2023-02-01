#' Shiny module for creation and visualisation of networks
#' #' @export
#' @param id Shiny input id
#' @rdname networkTableModule


networkTableUI <- function(id){
  ns <- shiny::NS(id)

  softui::fluid_page(
    softui::fluid_row(
      shiny::column(12,
                 reactable::reactableOutput(ns("dt_network"))
      )
    )
  )
}

#' @export
#' @param config A structured list form a config file which specifies the script/layout of how the network must be built
#' @param nodes_data Dataset of precalculated nodes. Must be in the same format as if they would be created by shinetwork::create_network_nodes
#' @param make_link Boolean specifying if the label in the table is a link
#' @param show_icons Boolean specifying if the group must be shown as an icon (specified in the config file (table_icon))
#' @rdname networkTableModule

networkTableModule <- function(input, output, session, config, nodes_data = reactive(NULL), make_link = FALSE, show_icons = FALSE){

  ns <- session$ns

  table_data <- reactive({

    data <- nodes_data()

    if(make_link){
      data <- data %>%
        dplyr::mutate(label = softui::page_link(id = id, label = label, "node_id"))
    }

    data %>%
      dplyr::select(group, label)


  })

  output$dt_network <- reactable::renderReactable({

    table_data() %>%
      reactable::reactable(defaultColDef = reactable::colDef(header = ""),
                           defaultPageSize = 10,
                           columns = c(list(
                             label = reactable::colDef(name = "Label", html = make_link),
                             group = reactable::colDef(cell = function(value){
                               if(show_icons){
                                 shiny::tags$span(
                                   softui::bsicon(config$nodes[[value]]$table_icon)
                                 )
                               } else {
                                 stringr::str_to_title(value)
                               }
                             })
                           )))
  })


  return(reactive(input$node_id$id))




}



