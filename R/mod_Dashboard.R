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
          div(
            HTML("<p>This application showcases predicted survival for Chinook salmon, <em>Oncorhynchus tshawytscha</em>, and Steelhead, <em>Oncorhynchus mykiss</em>, influenced by seasonal changes experienced during downstream migration through the Federal Columbia River Power System Hydrosystem (FCRPS).</p>
                 <p>This application allows users to specify whether seasonal changes are represented by day-of-year or temperature and set variables of interest such as passage type, rearing type, and year(s) of interest.
                 </p>")
          )
        ),
        shinydashboard::box(
          title = "How to use this application?",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          div(
            HTML("<p>By selecting the hydrosystem survival tab in the left navigation panel, the user will be prompted to select species, rearing type, seasonal covariate, and year(s) of interest to adjust the predicted survival estimates based on user interest.</p>
                 <p>Once variables of interest are selected, the figures will update to provide predicted smolt-to-adult return survival (SAR) and transported to bypassed fish survival ratio (T:B), two metrics of interest in determining the effectiveness of FCRPS transportation program.</p>
                 <p>To learn more about how this application can fill knowledge-action gaps and the reliability of the research methodologies featured, see the background tab in the left navigation panel.
                 </p>")
          )
        )
      ),
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          shiny::img(src = "www/map.png", style = "max-width:100%; height:auto;"),
          br(),
          "Figure 1: Map of the Columbia and Snake River, Pacific Northwest, USA, with major hydroelectric dams denoted (dark circles) along Spring/Summer Chinook salmon and Steelhead migratory routes. HydroSurvDOYTEMP app underlying model predicts the probability of smolt-to-adult survival from outmigrating juveniles at Bonneville Dam (BON) with an adult return detection at Lower Granite Dam (LGR)."
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
