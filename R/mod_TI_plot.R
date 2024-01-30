#' TI_plot UI Function
#'
#' @description used for layout of TI plots dependent on  user selection of all or by year
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TI_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("TI_plot"),
               width = "100%"
               )
  )
}

#' TI_plot Server Functions
#'
#' @noRd
mod_TI_plot_server <- function(id, data, year_display, plot_height){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$TI_plot <- renderPlot({

        # Filter data based on user selection
        if (year_display() == "All Years") {

          fct_TI_all_years_plot(data())

        } else if (year_display() == "Year") {

          fct_TI_by_years_plot(data())

        }
      }, height = plot_height() #this works--might try putting reactive lenth here as if-else instead?
      )
  })
}

## To be copied in the UI
# mod_TI_plot_ui("TI_plot_1")

## To be copied in the server
# mod_TI_plot_server("TI_plot_1")
