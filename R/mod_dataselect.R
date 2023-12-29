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

    # select years of interest
    shinyWidgets::pickerInput(inputId = ns("select_year"),
                              label = "Select years",
                              choices = unique(data.pred$year),#1993:2018,
                              selected = 2000, #1993:2018,
                              options = list(`actions-box` = TRUE),
                              width = "200px",
                              multiple = T)

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

  reactive({
      data.pred %>%
          filter(species %in% c(input$select_spp),
                 rear_type %in% c(input$select_rear),
                 covariate %in% c(input$select_cov),
                 year %in% c(input$select_year)
                 )
    # x_var <- switch(input$select_cov,
    #                 "Day-of-year (DOY)" == "doy",
    #                 "Temperature (°)" == "temp",
    #
    #                 )
    #
    #
    # if (x_var %in% colnames(filtered_data)) {
    #   filtered_data <- filtered_data %>% select(x_var, species)
    # }
    # return(filtered_data)
    })
  })
}

## To be copied in the UI
# mod_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_dataselect_server("dataselect_1")


