#' compare_single_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_submodule_compare_SAR_TI_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        align = "center",  # Center the content
        htmlOutput(outputId = ns("compare_single_plot_message"))  # Message to prompt viewer to select View by: Year
      )
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        plotOutput(outputId = ns("compare_single_plot"), width = "95%", height = 700)
      )
    )
  )
}

#' compare_single_plot Server Functions
#'
#' @noRd
mod_main_submodule_compare_SAR_TI_plot_server <- function(id, data_pred, data_ti, observed_data, year_display, years_selected,  selected_covariate, update_button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #set reactive for plot heights
    plot_height<- reactive({
      req(years_selected())
      nyears <- length(years_selected())

      if (nyears > 1) {
        plot_height<- 700 + (nyears-1)*500
      }else plot_height <-700
    })

    output$compare_single_plot <- renderPlot({
      update_button

      if (year_display() == "Year") {

        # Create an empty list to store plots
        plots_list <- list()

        # Loop through selected years and create plots
        for (i in seq_along(years_selected())) {
          year_selected <- years_selected()[i]

          # Filter data for the current year
          data_filtered_year_pred <- data_pred() %>% dplyr::filter(year == year_selected)
          data_filtered_year_ti <- data_ti() %>% dplyr::filter(year == year_selected)
          data_filtered_observed_data <- observed_data() %>% dplyr::filter(year == year_selected)


          # Skip the year if any dataset is empty
          if (nrow(data_filtered_year_pred) == 0 || nrow(data_filtered_year_ti) == 0 || nrow(data_filtered_observed_data) == 0) {
            print(paste("Skipping year", year_selected, "due to missing data"))
            next
          }

          plots_list[[as.character(year_selected)]] <- fct_compare_SAR_TI_plot(
            data_pred = data_filtered_year_pred,
            data_ti = data_filtered_year_ti,
            observed_data = data_filtered_observed_data,
            selected_year = year_selected,
            selected_covariate = selected_covariate()

          )
        }

        # Arrange plots using patchwork
        if (length(plots_list) > 0) {
          combined_plots <- patchwork::wrap_plots(plots_list, ncol = 1 )
          print(combined_plots)
        } else {
          # Handle case when no years are selected
          plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No data selected")
        }

      return(combined_plots)

      } else {

        # If "View by Year" is not selected, return NULL to avoid attempting to render the plot
        return(NULL)
      }
    },
     height = function() {
      plot_height()
      }
    )

    output$compare_single_plot_message <- renderUI({
      # Check if "View by Year" is not selected
      if (year_display() == "All Years") {
        # Show the message
        HTML("<br>
             <br>
             <p>Select 'View by Year' to view comparison plots.</p>")
      }
    })
  })
}
## To be copied in the UI
# mod_main_submodule_compare_single_plot_ui("compare_single_plot_1")

## To be copied in the server
# mod_main_submodule_compare_single_plot_server("compare_single_plot_1")
