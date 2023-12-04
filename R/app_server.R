#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Dashboard_server("Dashboard_1")

  Pdata <- reactive(mod_dataselect_server("dataselect_1"))

  mod_SAR_plot_server("SAR_plot_1", Pdata())


  mod_TI_plot_server("TI_plot_1", Pdata())

  mod_Background_server("Background_1")

}
