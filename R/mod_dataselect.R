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
    #select species
    selectInput(inputId = ns("select_spp"),
                label = "Select species",
                choices =  data$species,#c("Chinook (CH)", "Steelhead (STL)"),
                selected = "Ch",
                width = "200px",
                multiple = T),

    #select rear type
    selectInput(inputId = ns("select_rear"),
                label = "Select rearing type",
                choices = data$rear_type, #c("Wild (W)", "Hatchery (H)"),
                selected = "W",
                width = "200px",
                multiple = T),
    #select covariate
    selectInput(inputId = ns("select_cov"),
                label = "Select covariate",
                choices = data$covariate, #c("Day of Year (DOY)", "Temperature"),
                selected = "DOY",
                width = "200px",
                multiple = T),
    # select years of interest--currently able to select one year based on function writtten-UPDATE
    shinyWidgets::pickerInput(inputId = ns("select_year"),
                              label = "Select years",
                              choices = unique(data$year),#1993:2018,
                              selected = 2000, #1993:2018,
                              options = list(`actions-box` = TRUE),
                              width = "200px",
                              multiple = T) #,
    #
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

    # finalDf() is the data used to plot the table and plot
    finalDf <- reactive({
      filter(data,
             species == input$select_spp &
             rear_type == input$select_rear &
             covariate == input$select_cov &
             year == input$select_year)
    })

    return(
      list("finalDf" = finalDf)
    )

  })
}

## To be copied in the UI
# mod_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_dataselect_server("dataselect_1")


