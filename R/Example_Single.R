library(shiny)
library(ggplot2)
library(dplyr)




ui <- fluidPage(
  selectInput(inputId = "select_spp",
              label = "Select species",
              choices =  iris$Species,#c("Chinook (CH)", "Steelhead (STL)"),
              selected = NULL,#"Chinook (CH)",
              width = "200px",
              multiple = T),
  plotOutput("plot1")
)

server <- function(input, output, session) {


    output$plot1 <- renderPlot({
      df<-iris %>%
        filter(Species == input$select_spp)

        ggplot(df, aes(df$Sepal.Length, df$Sepal.Width, color = df$Species)) +
        geom_point()
    })

}

shinyApp(ui, server)

