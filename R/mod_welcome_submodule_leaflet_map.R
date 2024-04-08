#' welcome_page_submodule_leaflet_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_submodule_leaflet_map_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinydashboard::box(
      width = NULL,
      solidHeader = FALSE,
      status = "primary",
      title = "Map of study system: Pacific Northwest, USA ",
      collapsible = TRUE,
      collapsed = TRUE,
      leaflet::leafletOutput(ns("map"),
                             height = "350px"),
      br(),
      "Figure 1: Map of the Columbia and Snake River, Pacific Northwest, USA, with major hydroelectric dams denoted (dark circles) along Spring/Summer Chinook salmon and Steelhead migratory routes. HydroSurvDOYTEMP app underlying model predicts the probability of smolt-to-adult survival from outmigrating juveniles at Bonneville Dam (BON) with an adult return detection at Lower Granite Dam (LGR)."
    )



  )
}

#' welcome_page_submodule_leaflet_map Server Functions
#'
#' @noRd
mod_welcome_submodule_leaflet_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Mock data for Columbia and Snake rivers//add as needed
    rivers_data <- data.frame(
      Name = c("Columbia River", "Snake River"),
      Lon = c(-119.8, -117.4),
      Lat = c(45.6, 46.2)
    )

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::setView(lng = -120, lat = 45, zoom = 6) %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                          attribution = 'Tiles &copy; <a href="https://www.carto.com/">Carto</a>') %>%
        leaflet::addAwesomeMarkers(
          data = rivers_data,
          lng = ~Lon,
          lat = ~Lat,
          label = ~Name,
          icon = leaflet::awesomeIcons(
            # icon = 'cloud', #change to special icon
            iconColor = 'black',
            library = 'fa',
            markerColor = 'lightgray'
          )) %>%
        #add crosshairs to reset to setView position
        leaflet::addEasyButton(leaflet::easyButton(
          icon="fa-crosshairs", title="Locate",  onClick = leaflet::JS("function(btn, map) { map.setView([45, -120], 5); }")))
    })
  })
}

## To be copied in the UI
# mod_welcome_submodule_leaflet_map_ui("welcome_page_submodule_leaflet_map_1")

## To be copied in the server
# mod_welcome_submodule_leaflet_map_server("welcome_page_submodule_leaflet_map_1")
