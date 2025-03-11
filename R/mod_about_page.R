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


  })
}

