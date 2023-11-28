#' Figures UI Function
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

    fluidPage(

      fluidRow(
        h2("Header"),
        shinydashboard::box(
          title = "X",
          width = 6,
          plotOutput(outputId = ns("plot1"))),

        shinydashboard::box(
          title = "X2",
          width = 6,
          plotOutput(outputId = ns("plot2"))
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "X",
          width = 6,
          plotOutput(outputId = ns("plot3"))),

        shinydashboard::box(
          title = "X2",
          width = 6,
          plotOutput(outputId = ns("plot4"))
        )
      )
    )
  )
}

#' Figures Server Functions
#'
#' @noRd
mod_Figures_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

     plot1 <- reactive({plot_fx(spp = input$select_spp,
                     rtype = input$select_rear,
                     cov = input$select_cov,
                     SAR_year = input$select_year)})
     output$plot1 <- renderPlot({
       plot1()
    })

    output$plot2 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })

    output$plot3 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })

    output$plot4 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })

  })
}

