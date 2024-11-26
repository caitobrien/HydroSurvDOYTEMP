#' Dashboard UI Function
#'
#' @description module for about page of app
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_page_ui <- function(id) {
  ns <- NS(id)
  tagList(

    #header image
    fluidRow(
      div(
      tags$img(src="www/welcomebanner_DOYTEMP_2.svg",  style = "width: 100%; height: auto"),#; max-height: 500px; object-fit: contain;
      br(),
      br()
      )
    ),
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Welcome to HydroSurvDOYTEMP,",
    #     width = 12,
    #     solidHeader = TRUE,
    #     status = "primary",
    #     em("a Shiny App based on the models adapted from Scheuerell et al. (2009) and Gosselin et al. (2018) predicting seasonal survival of Chinook salmon and Steelhead through the hydrosystem")
    #   )
    # ),

    # Info boxes
    fluidRow(

      shinydashboard::box(
        title = "What does this application do?",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        shiny::includeHTML(system.file("app/www/mod_about_Q1_text.html", package = "HydroSurvDOYTEMP")),
       # br() #add to make Q1/Q2 boxes even on expansion--remove as needed

      ),

      shinydashboard::box(
        title = "How to use this application?",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        shiny::includeHTML(system.file("app/www/mod_about_Q2_text.html", package = "HydroSurvDOYTEMP"))
      )
      ),

      # leaflet map
      fluidRow(
        column(width = 2),  # Empty column to center map
        column(
          width = 8,
          mod_about_submodule_leaflet_map_ui("leaflet_map_1")
        ),
        column(width = 2) # Empty column to center map
      ),

      #overview
      fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Overview",
        status = "primary",
        shiny::includeHTML(system.file("app/www/mod_about_overview_text.html", package = "HydroSurvDOYTEMP"))
      )
      )
    )
}

#' about Server Functions
#'
#' @noRd
mod_about_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_about_submodule_leaflet_map_server("leaflet_map_1")

  })
}

