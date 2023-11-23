#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Figures_ui <- function(id){
  ns <- NS(id)
  tagList(


  )
}

#' Home Server Functions
#'
#' @noRd
mod_Figures_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

