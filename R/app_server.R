#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  load(system.file("data/model_output.rda", package = "HydroSurvDOYTEMP"))
  get("model_output")

  mod_about_page_server("about_page_ui_1")


  #retrieve reactive values to use in plots and tables as needed
  dataselect_reactives <- mod_main_submodule_dataselect_server("main_dataselect_2", model_output = model_output)

  observe({
      filtered_data_pred <- dataselect_reactives$filtered_data_pred
      filtered_data_ti <- dataselect_reactives$filtered_data_ti
      filtered_observed_data <- dataselect_reactives$filtered_observed_data
       year_display <- dataselect_reactives$year_display
       plot_height <- dataselect_reactives$plot_height
       years_selected <- dataselect_reactives$years_selected
       selected_covariate <- dataselect_reactives$selected_covariate
       get_years <- dataselect_reactives$get_years
       update_button <- dataselect_reactives$update_button


  mod_main_submodule_select_SAR_plot_server("SAR_plot_1",
                                            data = filtered_data_pred(),
                                            observed_data = filtered_observed_data(),
                                            year_display = year_display(),
                                            plot_height = plot_height(),
                                            years_selected = years_selected(),
                                            selected_covariate = selected_covariate(),
                                            get_years = get_years(),
                                            update_button = update_button()
                                            )
  mod_main_submodule_select_TI_plot_server("TI_plot_1",
                                           data = filtered_data_ti(),
                                           year_display = year_display(),
                                           plot_height = plot_height(),
                                           years_selected = years_selected(),
                                           selected_covariate = selected_covariate(),
                                           get_years = get_years(),
                                           update_button = update_button()
                                           )
  mod_main_submodule_compare_SAR_TI_plot_server("compare_single_plot",
                                                data_pred = filtered_data_pred(),
                                                data_ti = filtered_data_ti(),
                                                observed_data = filtered_observed_data(),
                                                year_display = year_display(),
                                                years_selected = years_selected(),
                                                selected_covariate = selected_covariate()
                                                )

  mod_main_page_server("main_page_ui_1")
  })

  mod_main_submodule_walkthrough_server("walkthrough_example_1", model_output = model_output)

  mod_background_page_server("background_page_1")

}
