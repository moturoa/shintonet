ui <- softui::simple_page(
  softui::fluid_page(
    softui::fluid_row(
      shiny::column(12,
                    shiny::selectInput("sel_address", label = "Select an address", choices = adressen$address_name)
      )
    ),
    softui::fluid_row(
      shiny::column(8,
                    softui::box(
                      shinyWidgets::materialSwitch("switch_labels",
                                                   "Toon Labels",
                                                   value = TRUE, status = "primary",
                                                   inline = TRUE, right = TRUE),
                      shinyWidgets::materialSwitch("switch_order",
                                                   "Geordend",
                                                   value = TRUE, status = "primary",
                                                   inline = TRUE, right = TRUE),
                      shintonet::shintoNetworkUI("shintoNetwork")
                    )
      ),
      shiny::column(4,
                    softui::box(
                      shintonet::networkTableUI("network_table")
                    )
      )
    ),
    softui::fluid_row(
      shiny::column(6,
                    uiOutput("amount_of_nodes_ui")
      ),
      shiny::column(6,
                    shiny::uiOutput("amount_of_edges_ui")
      )
    ),
    softui::fluid_row(
      shiny::column(6,
                    shiny::uiOutput("nodekind_selector"),
                    shiny::uiOutput("amount_of_kindnodes_ui")
      ),
      shiny::column(6,
                    shiny::uiOutput("edgekind_selector"),
                    shiny::uiOutput("amount_of_kindedges_ui")
      )
    ),
    softui::fluid_row(
      shiny::column(12,
                    # shiny::uiOutput("function_test_ui")
      )
    )
  )

)

