server <- function(input, output, session) {

  ordered <- reactive({
    input$switch_order
    #NULL
  })

  labels <- reactive({
    input$switch_labels
    #NULL
  })


  # OPTION 1

  nodes <- reactive({
    shinetwork::create_network_nodes(.cc$get("network"), list("address_data" = adressen,
                                                              "resident_data" = bewoners,
                                                              "person_data" = personen))
  })
  edges <- reactive({
    shinetwork::create_network_edges(.cc$get("network"), list("address_data" = adressen,
                                                              "resident_data" = bewoners,
                                                              "person_data" = personen))
  })

  callModule(shinetwork::shintoNetworkModule, "shintoNetwork",
             config = .cc$get("network"), nodes_data = nodes(), edges_data = edges(),
             hierarchical = ordered, show_labels = labels, hover_function = "make_link_from_text")

  # OPTION 2
  # callModule(shinetwork::shintoNetworkModule, "shintoNetwork", config = .cc$get("network"),
  #            datasets = list("address_data" = adressen,
  #                            "resident_data" = bewoners,
  #                            "person_data" = personen),
  #            hierarchical = ordered, show_labels = labels)

  output$amount_of_nodes_ui <- renderUI({
    n_nodes <- shinetwork::count_nodes(nodes())
    softui::fluid_row(
      shiny::column(12,
                    softui::value_box(value = n_nodes, title = "Aantal nodes", icon = bsicon("diagram-3-fill"), width = 12)
      )
    )
  })

  output$amount_of_edges_ui <- renderUI({
    n_edges <- shinetwork::count_edges(edges())
    softui::fluid_row(
      shiny::column(12,
                    softui::value_box(value = n_edges, title = "Aantal edges", icon = bsicon("link"), width = 12)
      )
    )
  })

  output$nodekind_selector <- renderUI({
    softui::fluid_row(
      shiny::column(12,
                    selectInput("sel_nodekind", label = "Soort node", choices = unique(nodes()$group))
      )
    )
  })

  output$amount_of_kindnodes_ui <- renderUI({
    n_nodes <- shinetwork::count_nodes(nodes(), input$sel_nodekind)
    softui::fluid_row(
      shiny::column(12,
                    softui::value_box(value = n_nodes, title = "Aantal nodes", icon = bsicon("diagram-3-fill"), width = 12)
      )
    )
  })

  output$edgekind_selector <- renderUI({
    softui::fluid_row(
      shiny::column(12,
                    selectInput("sel_edgekind", label = "Soort edge", choices = unique(edges()$label))
      )
    )
  })

  output$amount_of_kindedges_ui <- renderUI({
    n_edges <- shinetwork::count_edges(edges(), input$sel_edgekind)
    softui::fluid_row(
      shiny::column(12,
                    softui::value_box(value = n_edges, title = "Aantal edges", icon = bsicon("link"), width = 12)
      )
    )
  })


  clicked_value <- callModule(networkTableModule, "network_table", config = .cc$get("network"),
                              nodes_data = nodes(), make_link = TRUE, show_icons = TRUE)

  observeEvent(clicked_value(), {
    shiny::showModal(
      softui::modal(
        title = "Clicked value",

        softui::fluid_row(
          shiny::column(12,
                        shiny::tags$h1(clicked_value())
          )
        )
      )
    )

  })

  observeEvent(input$hover_click, {
    shiny::showModal(
      softui::modal(
        title = "Clicked value",

        softui::fluid_row(
          shiny::column(12,
                        shiny::tags$h1(input$hover_click$id)
          )
        )
      )
    )
  })

}
