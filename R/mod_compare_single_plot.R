#' compare_single_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_compare_single_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("compare_single_plot"),
               width = "100%",
               height = 1000)
  )
}

#' compare_single_plot Server Functions
#'
#' @noRd
mod_compare_single_plot_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$compare_single_plot <- renderPlot({
      # Call the function to generate the SAR plot
      sar_plot<- fct_SAR_all_years_single_plot(data())


      # Call the function to generate the TI plot
      ti_plot<-fct_TI_all_years_single_plot(data())

      #add margins between plots
      sar_plot + theme(plot.margin = margin(10, 0, 0, 100, "pt"))

      ti_plot + theme(plot.margin = margin(10, 0, 0, 100, "pt"))

      combined_plot<- cowplot::plot_grid(sar_plot, ti_plot, ncol=1)

      return(combined_plot)
      })
  })
}

## To be copied in the UI
# mod_compare_single_plot_ui("compare_single_plot_1")

## To be copied in the server
# mod_compare_single_plot_server("compare_single_plot_1")
