#' dataselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_dataselect_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    "To plot survival predictions:",

    #select covariate
    selectInput(inputId = ns("select_cov"),
                label = "Select covariate",
                choices = c("Day-of-year (DOY)","Temperature (°C)"), #c("Day of Year (DOY)", "Temperature"),
                selected ="Day of Year (DOY)",
                width = "200px",
                multiple = F),

    #select species
    selectInput(inputId = ns("select_spp"),
                label = "Select species",
                choices =  c("Chinook", "Steelhead"),#data.pred$species
                selected = unique(data.pred$species),
                width = "200px",
                multiple = T),

    #select rear type
    selectInput(inputId = ns("select_rear"),
                label = "Select rearing type",
                choices = c("Natural-origin", "Hatchery-origin"), #data.pred$rear_type,
                selected = unique(data.pred$rear_type),
                width = "200px",
                multiple = T),

    # prompt to select all years or by year
    selectInput(
      inputId = ns("year_display"),
      label = "View by",
      choices = c("All Years", "Year"),
      selected = "All Years"
    ),
    uiOutput(ns("year_picker"))

    # # select years of interest
    # shinyWidgets::pickerInput(inputId = ns("select_year"),
    #                           label = "Select years",
    #                           choices = unique(data.pred$year),#1993:2018,
    #                           selected = 2000, #1993:2018,
    #                           options = list(`actions-box` = TRUE),
    #                           width = "200px",
    #                           multiple = T)

    # # add button to run after options are selected
    # actionButton(inputId = 'btn_run_selected',
    #              label = paste0('Run'),
    #              icon = icon("wrench")),
    # ## reset side bar selectoin
    # actionButton(inputId = 'btn_reset_selected',
    #              label = 'Reset',
    #              icon = icon('refresh') )
  )
}

#' dataselect Server Functions
#'
#' @noRd
mod_dataselect_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

  reactive({
      data.pred %>%
          filter(species %in% c(input$select_spp),
                 rear_type %in% c(input$select_rear),
                 covariate %in% c(input$select_cov),
                 year %in% c(input$select_year)
                 )

    })
  })
}

## To be copied in the UI
# mod_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_dataselect_server("dataselect_1")


