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
mod_SAR_plot_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Retrieve reactive values from mod_dataselect_server
    # dataselect_reactives <- mod_dataselect_server("dataselect_1")
    # filtered_data <- dataselect_reactives$filtered_data
    # year_display <- dataselect_reactives$year_display

    output$SAR_plot <- plotly::renderPlotly({

      # Filter data based on user selection
      # if (year_display() == "All Years") {

        ggplotly(ggplot(data(), aes(x=x_var, y=SAR))+
          geom_point())

      # } else if (year_display() == "Year") {
      #
      #   ggplot(filtered_data(), aes(x=x_var, y=SAR))+
      #     geom_point()
      #  }
    })
  })
}



## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
