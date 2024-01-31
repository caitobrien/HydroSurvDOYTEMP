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
    fluidRow(
        column(
          width = 12,
          h4("Predicted Smolt-to-Adult Ratio (SAR) versus observed SAR from PIT tag recoveries"),
          # Adding an HTML output for the title
          htmlOutput(outputId = ns("title_html")),

          plotOutput(outputId = ns("SAR_plot"),
                 width = "100%")
               # height = 3000) #controls height of plot, but auto doesn't work. Need to look into getting a reactive based on the length of the years selected to use an ifelse statement to adjust height based on how many years selected.
          )
    )
  )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_SAR_plot_server <- function(id, data, year_display, plot_height, years_selected){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Dynamic title based on user selection
    output$title_html <- renderUI({
      title_text <- if (year_display() == "All Years") {
        "Viewing all years: 1993 to 2018"
      } else if (year_display() == "Year") {

        # Check if any years are selected
        if (length(years_selected()) > 0) {
          # Create the title based on selected years
          title_text <- paste("Viewing by year:", paste(sort(years_selected()), collapse = ", "))
        } else {
          title_text <- "No year selected"
        }
        h5(title_text)
      }
      })


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
