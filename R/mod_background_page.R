#' Background UI Function
#'
#' @description module for background subpage
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_background_page_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      shinydashboard::box(
        title = HTML("Supplementary infomation for HydroSurvDOYTEMP <small style ='font-size:0.8em; color: white;'>BetaVersion.Mar25</small>"),
        width = 12,
        solidHeader = TRUE,
        status = "primary"
      )
      ),

      fluidRow(
      shinydashboard::box(
        title = "Methods",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        width = 12,
        shiny::includeHTML(system.file("app/www/mod_background_methods_text.html", package = "HydroSurvDOYTEMP"))
          )
        ),

    fluidRow(
      shinydashboard::box(
        title = "References",
        status = "info",
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        shiny::includeHTML(system.file("app/www/mod_background_references_text.html", package = "HydroSurvDOYTEMP"))
      )
    ),

    fluidRow(
      shinydashboard::box(
        title = "Additional Information",
        status = "info",
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        # update when app is transfered to CBR org, Columbia-Basin-Research-CBR
        HTML("All code featured in this Shiny application is made publically available through our organizations GitHub repository: <a href='https://github.com/caitobrien'><i class='fab fa-github'></i> Columbia-Basin-Research-CBR</a>")
      )
    ),


      fluidRow(
        shinydashboard::box(
          title = "Contact Information",
          status = "info",
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          HTML("<p>This ShinyApp is a product of Columbia Basin Reasearch, School of Aquatic and Fishery Sciences, College of the Environment, University of Washington.</p>
               <p>Please direct general questions to: <a href='mailto:web@cbr.washington.edu'>web@cbr.washington.edu</a></p>"
          )
        )
      )
      )
}


#' Background Server Functions
#'
#' @noRd
mod_background_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

})
}

## To be copied in the UI
# mod_Background_ui("Background_1")

## To be copied in the server
# mod_Background_server("Background_1")
