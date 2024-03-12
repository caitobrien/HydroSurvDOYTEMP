#' dataselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_dataselect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    "To plot survival predictions:",

    # select covariate
    selectInput(
      inputId = ns("select_cov"),
      label = "Select covariate",
      choices = c("Day-of-year (DOY)", "Temperature (°C)"),
      selected = "Day of Year (DOY)",
      width = "200px",
      multiple = F
    ),

    # select species
    selectInput(
      inputId = ns("select_spp"),
      label = "Select species",
      choices = c("Chinook", "Steelhead"), # data.pred$species
      selected = unique(data.pred$species),
      width = "200px",
      multiple = T
    ),

    # select rear type
    selectInput(
      inputId = ns("select_rear"),
      label = "Select rearing type",
      choices = c("Natural-origin", "Hatchery-origin"), # data.pred$rear_type,
      selected = unique(data.pred$rear_type),
      width = "200px",
      multiple = T
    ),

    # prompt to select all years or by year
    selectInput(
      inputId = ns("year_display"),
      label = "View by",
      choices = c("All Years", "Year"),
      selected = "All Years"
    ),
    uiOutput(ns("year_picker"))
  )
}

#' dataselect Server Functions
#'
#' @noRd
mod_dataselect_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the UI for the year picker
    output$year_picker <- renderUI({


      if (input$year_display == "Year") {
        shinyWidgets::pickerInput(
          inputId = ns("select_years"),
          label = "Select Year(s)",
          choices = unique(data.pred$year),
          selected = 2000,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      } else {
        NULL
      }
    })

    # Reactive for year_display
    year_display <- reactive({
      input$year_display
    })

    # Reactive for selected_years
    years_selected <- reactive({
      input$select_years
    })


    # Reactive for plot height
    plot_height<- reactive({
      if (input$year_display == "All Years") {
        plot_height <-500
      } else

      req(input$select_years)
      nyears <- length(input$select_years)

        if (nyears > 3) {
          plot_height<- 500 + (nyears-3)*120
          }else plot_height <-500
    })

    filtered_data <- reactive({
      if (input$year_display == "All Years") {
        data.pred %>%
          dplyr::filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear),
            covariate %in% c(input$select_cov)
          )
      } else if (input$year_display == "Year" && !is.null(input$select_years)) {
        data.pred %>%
          dplyr::filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear),
            covariate %in% c(input$select_cov),
            year %in% c(input$select_years)
          )
      } else {
        NULL
      }
    })

    # Return the filtered data reactive expression
    return(list(
      filtered_data = reactive(filtered_data),
      year_display = reactive(year_display),
      plot_height = reactive(plot_height),
      years_selected = reactive(years_selected)
    ))
  })
}

## To be copied in the UI
# mod_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_dataselect_server("dataselect_1")
