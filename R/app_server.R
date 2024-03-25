#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_welcome_page_server("welcome_page_ui_1")

  mod_welcome_page_submodule_leaflet_map_server("welcome_page_submodule_leaflet_map_1")

  #retrieve reactive values to use in plots and tables as needed
  dataselect_reactives <- mod_main_dataselect_server("main_dataselect_1")

  observe({
      filtered_data <- dataselect_reactives$filtered_data
       year_display <- dataselect_reactives$year_display
       plot_height <- dataselect_reactives$plot_height
       years_selected <- dataselect_reactives$years_selected


  mod_main_select_SAR_plot_server("SAR_plot_1", data = filtered_data(), year_display = year_display(), plot_height = plot_height(), years_selected = years_selected())
  mod_main_select_TI_plot_server("TI_plot_1", data = filtered_data(), year_display = year_display(), plot_height = plot_height(), years_selected = years_selected())
  mod_compare_SAR_TI_plot_server("compare_single_plot", data = filtered_data(), year_display = year_display())

  mod_main_page_server("main_page_ui_1")
  })





  mod_background_page_server("background_page_1")

}
