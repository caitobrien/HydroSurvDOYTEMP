library(shiny)
library(ggplot2)
library(dplyr)



myModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
      selectInput(inputId = ns("select_spp"),
                  label = "Select species",
                  choices =  data$species,#c("Chinook (CH)", "Steelhead (STL)"),
                  selected = NULL,#"Chinook (CH)",
                  width = "200px",
                  multiple = T),

      #select rear type
      selectInput(inputId = ns("select_rear"),
                  label = "Select rearing type",
                  choices = data$rear_type, #c("Wild (W)", "Hatchery (H)"),
                  selected =  NULL, #"Wild (W)",
                  width = "200px",
                  multiple = T),
      selectInput(inputId = ns("select_cov"),
                  label = "Select covariate",
                  choices = data$covariate, #c("Day of Year (DOY)", "Temperature"),
                  selected = NULL, #"Day of Year (DOY)",
                  width = "200px",
                  multiple = T),
      plotOutput(ns("plot1")),
    )
}

myModuleServer<- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df<- reactive({
      filter(data,
             species == input$select_spp &
             rear_type == input$select_rear &
             covariate == input$select_cov
               )
    })

      output$plot1 <- renderPlot({
        ggplot(df(), aes(df()$doy, df()$SAR, color = df()$year)) +
          geom_point()
      })
    }
  )
}



ui <- fluidPage(
  myModuleUI("myModule_UI_1")
)

server <- function(input, output, session) {
  myModuleServer("myModule_UI_1")
}

shinyApp(ui, server)

