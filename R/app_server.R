#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Dashboard_ui("Dashboard_1")
  mod_Figures_ui("Figures_1")
  mod_Background_ui("Background_1")

}
