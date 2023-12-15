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
        title = "Welcome to the HydroSurvDOYTEMP shinyAPP,",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        em("an interactive visualization tool based on the models adapted from Scheuerell et al. (2009) and Gosselin et al. (2018)")
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinydashboard::box(
          title = "What does this application do?",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          div("•	This application showcases predicted survival for Chinook salmon,",em("Oncorhynchus tshawytscha,"),"and Steelhead, ", em("Oncorhynchus mykiss,"),"influenced by seasonal changes experienced during downstream migration through the Federal Columbia River Power System Hydrosystem (FCRPS)."),
          br(),
          "•	This application allows users to specify whether seasonal changes are represented by day-of-year or temperature and set variables of interest such as passage type, rearing type, and year(s) of interest.",
        ),
        shinydashboard::box(
          title = "How to use this application?",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          "•	By selecting the hydrosystem survival tab in the left navigation panel, the user will be prompted to select species, rearing type, seasonal covariate, and year(s) of interest to adjust the predicted survival estimates based on user interest.",
          br(),
          "•	Once variables of interest are selected, the figures will update to provide predicted smolt-to-adult return survival (SAR) and transported to bypassed fish survival ratio (T:B), two metrics of interest in determining the effectiveness of FCRPS transportation program.",
          br(),
          "•	To learn more about how this application can fill knowledge-action gaps and the reliability of the research methodologies featured, see the background tab in the left navigation panel."
          )
        ),
    column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        "MAP/IMAGE?"
      )
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
