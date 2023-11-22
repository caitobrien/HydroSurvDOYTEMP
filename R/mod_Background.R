#' Background UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Background_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Background Server Functions
#'
#' @noRd 
mod_Background_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Background_ui("Background_1")
    
## To be copied in the server
# mod_Background_server("Background_1")
