library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),

  dashboardSidebar(
    selectizeInput(
      inputId = "select",
      label = "Select country:",
      choices = c("CH", "JP", "GER", "AT", "CA", "HK"),
      multiple = TRUE)
  ),

  dashboardBody(
    fluidRow(column(2, uiOutput("ui1")),
             column(10, plotOutput("some_plot"))))#,

  # column(4, uiOutput("ui2")),
  # column(4, uiOutput("ui3")))
)

server <- function(input, output) {
  plotht <- reactiveVal(360)
  observe({
    req(input$select)
    nvbox <- length(input$select)
    if (nvbox > 3) {
      plotheight <- 360 + (nvbox-3)*120
    }else plotheight <- 360
    plotht(plotheight)
  })

  output$ui1 <- renderUI({
    req(input$select)

    lapply(seq_along(input$select), function(i) {
      fluidRow(
        valueBox(value = input$select[i],
                 subtitle = "Box 1",
                 width = 12)
      )
    })
  })

  observe({
    output$some_plot <- renderPlot({
      plot(iris)
    }, height=plotht())
  })


}

shinyApp(ui = ui, server = server)
