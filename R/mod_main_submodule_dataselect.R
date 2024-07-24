#' submodule_dataselect UI Function
#'
#' @description sub module within main page module for the data selection only
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_main_submodule_dataselect_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # div("To plot survival predictions:", class = "instructions"),

    fluidRow(
      column(
        width = 3,

    # select covariate
    selectInput(
      inputId = ns("select_cov"),
      label = "Select covariate",
      choices = c("Day-of-year (DOY)", "Temperature (°C)"),
      selected = "Day of Year (DOY)",
      width = "200px",
      multiple = F
    )
    ),

    column(
      width = 3,

    # select species
    shinyWidgets::prettyCheckboxGroup(
      inputId = ns("select_spp"),
      label = "Select species",
      shape = "curve",
      outline = TRUE,
      choices = c("Chinook", "Steelhead"),
      selected = "Chinook",
      width = "200px"
      )
    ),

    column(
      width = 3,

    # select rear type
    shinyWidgets::prettyCheckboxGroup(
      inputId = ns("select_rear"),
      label = "Select rearing type",
      shape = "curve",
      outline = TRUE,
      choices = c("Natural-origin", "Hatchery-origin"),
      selected = "Natural-origin",
      width = "200px"
      )
    ),

    column(
      width = 3,

    # prompt to select all years or by year
    selectInput(
      inputId = ns("year_display"),
      label = "View by",
      choices = c("All Years", "Year"),
      selected = "All Years"
    ),
    uiOutput(ns("year_picker"))
  )
      )
  )
}

#' dataselect Server Functions
#'
#' @noRd
mod_main_submodule_dataselect_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the UI for the year picker
    output$year_picker <- renderUI({

      if (input$year_display == "Year") {
        shinyWidgets::pickerInput(
          inputId = ns("select_years"),
          label = "Select Year(s)",
          choices = unique(df_mod_predict$year),
          selected = 2018,
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
    plot_height <- reactive({
      if (input$year_display == "All Years") {
        return(375)  # Reset to default height when viewing all years
      } else {
        req(input$select_years)
        nyears <- length(input$select_years)
        if (nyears > 2) {
          return(375 + (nyears-2)*150)
        } else {
          return(375)
        }
      }
    })

    filtered_data <- reactive({
      if (input$year_display == "All Years") {
        df_mod_predict %>%
          dplyr::filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear),
            covariate %in% c(input$select_cov)
          )
      } else if (input$year_display == "Year" && !is.null(input$select_years)) {
        df_mod_predict %>%
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
# mod_main_submodule_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_main_submodule_dataselect_server("dataselect_1")
