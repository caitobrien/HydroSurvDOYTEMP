#' Dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Welcome",
        width = 12,
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "What does this application do?",
        width = 6,
        "Basic objectives"
      ),
      shinydashboard::box(
        title = "Basic questions to answer",
        width = 6,
        "something something "
      )
    ),
    fluidRow(
      shinydashboard::box(
      title = "How to use this application",
      "-got for it!--> to hydro survival",
      br(),
      "see background section for more detailed information",
      width = 12
      )
    )
  )
}

#' Dashboard Server Functions
#'
#' @noRd
mod_Dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_Dashboard_ui("Dashboard_1")

## To be copied in the server
# mod_Dashboard_server("Dashboard_1")
