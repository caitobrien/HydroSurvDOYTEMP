library(shiny)
library(shinydashboard)
library(ggplot2)

# Function to generate ggplot with facet grid
generate_ggplot <- function() {
  ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point() +
    facet_wrap(~cyl)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Scrollable ggplot Box"),
  dashboardSidebar(),
  dashboardBody(
    box(
      title = "Scrollable ggplot Box",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      height = "auto",  # Set a fixed height for the box
      div(style = "overflow-y: auto;",
          uiOutput("plots")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  output$plots <- renderUI({
    num_plots <- 6  # Change the number of plots as needed
    plot_list <- lapply(1:num_plots, function(i) {
      plotOutput(paste0("plot", i))
    })

    fluidRow(
      column(
        width = 12,
        do.call(tagList, plot_list)
      )
    )
  })

  for (i in 1:6) {
    output[[paste0("plot", i)]] <- renderPlot({
      generate_ggplot()
    })
  }
}

# Run the application
shinyApp(ui, server)
