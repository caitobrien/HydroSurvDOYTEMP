#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Dashboard_server("Dashboard_ui_1")

  filtered_data <- reactive(mod_dataselect_server("dataselect_1"))

  mod_SAR_plot_server("SAR_plot_1", filtered_data())
  mod_SAR_table_server("SAR_table_1", filtered_data())

  mod_TI_plot_server("TI_plot_1", filtered_data())

  mod_HydroSurv_server("HydroSurv_ui_1")

  mod_Background_server("Background_1")

}
