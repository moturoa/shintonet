server <- function(input, output, session) {

  ordered <- reactive({
    input$switch_order
  })

  labels <- reactive({
    input$switch_labels
  })

  callModule(shinetwork::shintoNetworkModule, "shintoNetwork", .cc$get("network"), list("address_data" = adressen,
                                                                                           "resident_data" = bewoners,
                                                                                           "person_data" = personen),
             hierarchical = ordered, show_labels = labels)

}
