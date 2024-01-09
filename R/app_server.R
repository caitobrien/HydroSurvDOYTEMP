#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Dashboard_server("Dashboard_ui_1")

  dataselect_reactives <- mod_dataselect_server("dataselect_1")
      filtered_data <- dataselect_reactives$filtered_data
       year_display <- dataselect_reactives$year_display

  mod_SAR_plot_server("SAR_plot_1", data = filtered_data(), year_display = year_display())
 # mod_SAR_table_server("SAR_table_1", filtered_data())

  mod_TI_plot_server("TI_plot_1", filtered_data())

  mod_HydroSurv_server("HydroSurv_ui_1")

  mod_Background_server("Background_1")

}
