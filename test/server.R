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

  nodes <- shinetwork::create_network_nodes(.cc$get("network"), list("address_data" = adressen,
                                                                     "resident_data" = bewoners,
                                                                     "person_data" = personen))
  edges <- shinetwork::create_network_edges(.cc$get("network"), list("address_data" = adressen,
                                                                     "resident_data" = bewoners,
                                                                     "person_data" = personen))

  callModule(shinetwork::shintoNetworkModule, "shintoNetwork",
             config = .cc$get("network"), nodes_data = nodes, edges_data = edges,
             hierarchical = ordered, show_labels = labels)

  # OPTION 2
  # callModule(shinetwork::shintoNetworkModule, "shintoNetwork", config = .cc$get("network"),
  #            datasets = list("address_data" = adressen,
  #                            "resident_data" = bewoners,
  #                            "person_data" = personen),
  #            hierarchical = ordered, show_labels = labels)

}
