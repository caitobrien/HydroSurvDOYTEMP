#' Dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Dashboard Server Functions
#'
#' @noRd 
mod_Dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Dashboard_ui("Dashboard_1")
    
## To be copied in the server
# mod_Dashboard_server("Dashboard_1")
