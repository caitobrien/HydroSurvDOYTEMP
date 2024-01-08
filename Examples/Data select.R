#' dataselect UI Function
#'
#' @description module represents the select inputs within the conditional panel that appears when Hydrosytem Suvival tab is selected. This code includes the reactive that generates data used in plots
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
      choices = c("Day-of-year (DOY)", "Temperature (°C)"), # c("Day of Year (DOY)", "Temperature"),
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

    # # Reactive for year_display
    # year_display <- reactive({
    #   input$year_display
    # })


    # Create a reactive expression to store the filtered data
    #had this as data and would not return app--changed back to filtered_data and then it recognized it as a function. Sooooo thinking that way, since it's not recognizing year_display as a function i think i need to call it in the id space and then run it through call module?
    filtered_data <- reactive({
      if (input$year_display == "All Years") {
        data.pred %>%
          filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear),
            covariate %in% c(input$select_cov)
          )
      } else if (input$year_display == "Year" && !is.null(input$select_years)) {
        data.pred %>%
          filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear),
            covariate %in% c(input$select_cov),
            year %in% c(input$select_years)
          )
      } else {
        NULL
      }
    })

    # # Update the reactive value when inputs change
    # observe({
    #   filtered_data()
    #   year_display()
    # })

    # Render the UI for the year picker
    output$year_picker <- renderUI({

      ns <- session$ns

      if (input$year_display == "Year") {
        pickerInput(
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



    # Return the filtered data reactive expression
    return(list(filtered_data = reactive(filtered_data),
                year_display = reactive(year_display)))

  })
