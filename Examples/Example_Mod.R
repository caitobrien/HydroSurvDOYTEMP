library(shiny)
library(ggplot2)
library(dplyr)
library(here)

data<-read.csv(here::here("data", "ChSSWRT_mod_predict.csv"))


data$transport <- factor(data$transport, levels = c(0,1), labels = c("ROR", "T"))
data$year <- factor(data$year)


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
      # select years of interest--currently able to select one year based on function writtten-UPDATE
      shinyWidgets::pickerInput(inputId = ns("select_year"),
                                label = "Select years",
                                choices = unique(data$year),#1993:2018,
                                selected = NULL, #1993:2018,
                                options = list(`actions-box` = TRUE),
                                width = "200px",
                                multiple = T), #windowPadding = 1 doesn't work
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
             covariate == input$select_cov &
             year == input$select_year
               )
    })

      output$plot1 <- renderPlot({
        ggplot(df(), aes(doy, SAR, color = year, group = year)) +
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

