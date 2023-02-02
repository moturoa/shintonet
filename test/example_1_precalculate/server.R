server <- function(input, output, session) {

  ordered <- reactive({
    input$switch_order
    #NULL
  })

  labels <- reactive({
    input$switch_labels
    #NULL
  })

  chosen_address <- reactive({
    chosen_address <- input$sel_address
    adressen %>%
      dplyr::filter(address_name == chosen_address)
  })

  residents <- reactive({
    req(chosen_address())
    personen %>%
      dplyr::filter(address_id == chosen_address()$address_id)
  })

  business_at_address <- reactive({
    req(chosen_address())
    businesses %>%
      dplyr::filter(address_id == chosen_address()$address_id) %>%
      dplyr::select(business_id, address_id) %>%
      unique()
  })

  ownership_at_address <- reactive({
    req(chosen_address())
    businesses %>%
      dplyr::filter(address_id == chosen_address()$address_id)
  })

  nodes <- reactive({
    expansion()
    original_nodes <- shinetwork::create_network_nodes(.cc$get("network"), list("address_data" = chosen_address(),
                                                                                "person_data" = residents(),
                                                                                "business_data" = business_at_address()))

    if(nrow(expansion()$expanded_nodes) > 0){
      nodes <- rbind(original_nodes, expansion()$expanded_nodes) %>%
        unique()
    } else {
      nodes <- original_nodes
    }

    nodes

  })
  edges <- reactive({
    expansion()
    original_edges <- shinetwork::create_network_edges(.cc$get("network"), list("address_data" = chosen_address(),
                                                                                "person_data" = residents(),
                                                                                "business_data" = business_at_address(),
                                                                                "ownership" = ownership_at_address()))

    if(nrow(expansion()$expanded_edges) > 0){
      edges <- rbind(original_edges, expansion()$expanded_edges) %>%
        unique()
    } else {
      edges <- original_edges
    }
  })

  network <- callModule(shinetwork::shintoNetworkModule, "shintoNetwork",
                             config = .cc$get("network"), nodes_data = nodes, edges_data = edges,
                             hierarchical = ordered, show_labels = labels, hover_function = "make_link_from_text",
                             hover_groups = c("address", "person"), expanded_data = expansion)



  expansion <- reactiveVal(list("expanded_nodes" = data.frame(), "expanded_edges" = data.frame()))

  clicked_node <- reactive({
    network()$clicked_node
  })

  observeEvent(network()$clicked_node, {

    clicked <- network()$clicked_node

    expanded_nodes <- expansion()$expanded_nodes
    expanded_edges <- expansion()$expanded_edges

    if(clicked$group == "person"){
      business_owned_by_person <- businesses %>%
        dplyr::filter(owner_id == !!clicked$node_id)


      add_nodes <- shinetwork::create_network_nodes(.cc$get("network"),
                                                    list("business_data" = business_owned_by_person))



      new_nodes <- rbind(expanded_nodes, add_nodes)
      new_nodes <- new_nodes %>%
        unique()

      add_edges <- shinetwork::create_network_edges(.cc$get("network"),
                                                    list("ownership" = business_owned_by_person))

      new_edges <- rbind(expanded_edges, add_edges)
      new_edges <- new_edges %>%
        unique()


    }

    if(nrow(new_nodes) != nrow(expanded_nodes) && nrow(new_edges) != nrow(expanded_edges)){
      expansion(list("expanded_nodes" = new_nodes, "expanded_edges" = new_edges))
    }


  })

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
                              nodes_data = nodes, make_link = TRUE, show_icons = TRUE)

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
