#' TI_plot UI Function
#'
#' @description used for layout of TI plots dependent on  user selection of all or by year
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_submodule_select_TI_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        br(),
        h4("Predicted Transport to Bypass Ratio (T:B)"),
        # Adding an HTML output for the title
        htmlOutput(outputId = ns("title_html")),
        br(),
        div(
          style = "text-align: right;",
        shinyWidgets::prettySwitch(
          inputId = ns("CI_display"),
          label = "View without 95% CI",
          inline = TRUE,
          status = "default",
          fill = TRUE
          )
        ),
        plotOutput(outputId = ns("TI_plot"),
               width = "100%")
      )
    )
  )
}

#' TI_plot Server Functions
#'
#' @noRd
mod_main_submodule_select_TI_plot_server <- function(id, data, year_display, plot_height, years_selected, selected_covariate, get_years, update_button){
  moduleServer( id, function(input, output, session){
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
          title_text <- paste("Viewing by year:", condense_years(years_selected()))
        } else {
          title_text <- "No year selected"
        }
        h5(title_text)
      }
    })


    output$TI_plot <- renderPlot({

        # Filter data based on user selection
        if (year_display() == "All Years") {

          fct_TI_all_years_plot(data(), selected_covariate(), credible_interval = !input$CI_display)

        } else if (year_display() == "Year") {

          fct_TI_by_years_plot(data(), selected_covariate(), credible_interval = !input$CI_display, legend_location = "top")

        }
      }, height = plot_height() #this works--might try putting reactive lenth here as if-else instead?
      )
  })
}

## To be copied in the UI
# mod_main_submodule_TI_plot_ui("TI_plot_1")

## To be copied in the server
# mod_main_submodule_TI_plot_server("TI_plot_1")
