#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Dashboard_server("Dashboard_1")
  mod_Figures_server("Figures_UI_1") #needs to match UI naming to render output
  mod_Background_server("Background_1")

}
