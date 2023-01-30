source("preload/load_packages.R")
source("R/configurationR6.R")

personen <- shinetwork::person
adressen <- shinetwork::address
bewoners <- shinetwork::resident


.cc <- configurationObject$new(filename = "config_site/config_site.yml")

make_link_from_text <- function(id){
  softui::page_link(id = id, label = glue::glue("Details voor {id}"), "hover_click", session = NULL)
}
