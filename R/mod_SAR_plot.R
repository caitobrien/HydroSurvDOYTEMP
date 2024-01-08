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

        ggplotly(ggplot(data(), aes(x=doy, y=SAR))+
          geom_point() + facet_grid(rear_type ~ species))

      } else if (year_display() == "Year") {

        ggplotly(ggplot(data(), aes(x=doy, y=SAR))+
                   geom_point() + facet_grid(rear_type ~ year + species))
       }
    })
  })
}



## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
