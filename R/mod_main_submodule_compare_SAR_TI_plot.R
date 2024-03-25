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
        plotOutput(outputId = ns("compare_single_plot"), width = "100%", height = 1000)
      )
    )
  )
}

#' compare_single_plot Server Functions
#'
#' @noRd
mod_main_submodule_compare_SAR_TI_plot_server <- function(id, data, year_display){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$compare_single_plot <- renderPlot({

      if (year_display() == "Year") {
      # Call the function to generate the SAR plot
      sar_plot<- fct_SAR_all_years_single_plot(data())


      # Call the function to generate the TI plot
      ti_plot<-fct_TI_all_years_single_plot(data())

      #add margins between plots
      sar_plot + ggplot2::theme(plot.margin = ggplot2::margin(10, 0, 0, 100, "pt"))

      ti_plot + ggplot2::theme(plot.margin = ggplot2::margin(10, 0, 0, 100, "pt"))

      combined_plot<- cowplot::plot_grid(sar_plot, ti_plot, ncol=1)

      return(combined_plot)
      } else {

        # If "View by Year" is not selected, return NULL to avoid attempting to render the plot
        return(NULL)
      }
    })

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
