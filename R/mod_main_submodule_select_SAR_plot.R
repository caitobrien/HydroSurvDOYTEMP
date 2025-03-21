#' SAR_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_submodule_select_SAR_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
        column(
          width = 12,
          br(),
          h4("Predicted versus observed SAR from PIT tag recoveries"),
          # Adding an HTML output for the title
          htmlOutput(outputId = ns("title_html")),
          br(),
          plotOutput(outputId = ns("SAR_plot"),
                 width = "100%")
          )
    )
  )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_main_submodule_select_SAR_plot_server <- function(id, data, observed_data, year_display, plot_height, years_selected, selected_covariate, get_years, update_button){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Dynamic title based on user selection
    output$title_html <- renderUI({

      update_button
      title_text <- if (year_display() == "All Years") {

        #return data years if excluding incomplete adult returns
        adjusted_years<-fct_adjust_get_years(get_years())

        paste("Viewing years:", condense_years(adjusted_years))
      } else if (year_display() == "Year") {

        # Check if any years are selected
        if (length(years_selected()) > 0) {
          # Create the title based on selected years
          # title_text <- paste("Viewing by year:", paste(sort(years_selected()), collapse = ", "))
          title_text <- paste("Viewing by year:", condense_years(years_selected()))

        } else {
          title_text <- "No year selected"
        }
        h5(title_text)
      }
      })


    output$SAR_plot <- renderPlot({

      # Filter data based on user selection
       if (year_display() == "All Years") {

         fct_SAR_all_years_plot(data_pred = data(),observed_data = observed_data(),selected_covariate = selected_covariate(), observed = "yes")


      } else if (year_display() == "Year") {

        fct_SAR_by_year_plot(data_pred =data(), observed_data = observed_data(), observed = "yes", selected_covariate = selected_covariate(), legend_location = "top")

       }
      }, height = plot_height()
    )
  })
}






## To be copied in the UI
# mod_main_submodule_select_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_main_submodule_select_SAR_plot_server("SAR_plot_1")
