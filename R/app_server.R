#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Dashboard_server("Dashboard_1")

  x <- mod_dataselect_server("dataselect_1")
  finalDf    <- x$finalDf
  mod_plot_server("plot_1", finalDf)

  mod_Background_server("Background_1")

}
