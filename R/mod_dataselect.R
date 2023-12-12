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
    #select species
    selectInput(inputId = ns("select_spp"),
                label = "Select species",
                choices =  data.pred$species,#c("Chinook (CH)", "Steelhead (STL)"),
                selected = unique(data.pred$species),
                width = "200px",
                multiple = T),

    #select rear type
    selectInput(inputId = ns("select_rear"),
                label = "Select rearing type",
                choices = data.pred$rear_type, #c("Wild (W)", "Hatchery (H)"),
                selected = unique(data.pred$rear_type),
                width = "200px",
                multiple = T),
    #select covariate
    selectInput(inputId = ns("select_cov"),
                label = "Select covariate",
                choices = data.pred$covariate, #c("Day of Year (DOY)", "Temperature"),
                selected ="Day of Year (DOY)",
                width = "200px",
                multiple = F),
    # select years of interest--currently able to select one year based on function writtten-UPDATE
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
        dplyr::filter(data.pred,
                      species %in% c(input$select_spp),
                      rear_type %in% c(input$select_rear),
                      covariate %in% c(input$select_cov),
                      year %in% c(input$select_year))
    })

  })
}

## To be copied in the UI
# mod_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_dataselect_server("dataselect_1")


