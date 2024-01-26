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
    plotOutput(outputId = ns("SAR_plot"),
               width = "100%")
              # height = 3000) #controls height of plot, but auto doesn't work. Need to look into getting a reactive based on the length of the years selected to use an ifelse statement to adjust height based on how many years selected.
    )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_SAR_plot_server <- function(id, data, year_display, plot_height){
  moduleServer(id, function(input, output, session){
    ns <- session$ns




    output$SAR_plot <- renderPlot({

      # Filter data based on user selection
       if (year_display() == "All Years") {

         fct_SAR_all_years_plot(data())

      } else if (year_display() == "Year") {

        fct_SAR_by_year_plot(data())

       }
      }, height = plot_height() #this works--might try putting reactive lenth here as if-else instead?
    )
  })
}






## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
