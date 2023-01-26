ui <- softui::simple_page(
  softui::box(
    shinyWidgets::materialSwitch("switch_labels",
                                 "Toon Labels",
                                 value = TRUE, status = "primary",
                                 inline = TRUE, right = TRUE),
    shinyWidgets::materialSwitch("switch_order",
                                 "Geordend",
                                 value = TRUE, status = "primary",
                                 inline = TRUE, right = TRUE),
    shinetwork::shintoNetworkUI("shintoNetwork")
  )
)
