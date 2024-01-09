#' SAR_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SAR_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(outputId = ns("SAR_plot"))

    )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_SAR_plot_server <- function(id, data, year_display){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$SAR_plot <- plotly::renderPlotly({

      # Filter data based on user selection
       if (year_display() == "All Years") {

         fct_SAR_all_years_plot(data())

      } else if (year_display() == "Year") {

        fct_SAR_by_year_plot(data())

       }
    })
  })
}



## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
