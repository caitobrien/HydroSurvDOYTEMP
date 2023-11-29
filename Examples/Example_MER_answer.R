library(shiny)
library(plotly)
library(reshape2)
library(DT)
library(tidybayes)
library(tidyverse)

data<-read.csv(here::here("data", "ChSSWRT_mod_predict.csv"))


data$transport<-as.factor(data$transport)
data$year <- factor(data$year)


# data %>%
#   mutate(
#     transport = as.factor(transport),
#     year = as.factor(year),
#     species = recode(species, "Ch" = "Chinook"))

#----------------------------------------------------------------------------------------
# Dataselect module ####
dataselect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #select species
    selectInput(inputId = ns("select_spp"),
                label = "Select species",
                choices =  data$species,#c("Chinook (CH)", "Steelhead (STL)"),
                selected = "Ch",
                width = "200px",
                multiple = T),

    #select rear type
    selectInput(inputId = ns("select_rear"),
                label = "Select rearing type",
                choices = data$rear_type, #c("Wild (W)", "Hatchery (H)"),
                selected = "W",
                width = "200px",
                multiple = T),
    #select covariate
    selectInput(inputId = ns("select_cov"),
                label = "Select covariate",
                choices = data$covariate, #c("Day of Year (DOY)", "Temperature"),
                selected = "DOY",
                width = "200px",
                multiple = T),
    # select years of interest--currently able to select one year based on function writtten-UPDATE
    shinyWidgets::pickerInput(inputId = ns("select_year"),
                              label = "Select years",
                              choices = unique(data$year),#1993:2018,
                              selected = 2000, #1993:2018,
                              options = list(`actions-box` = TRUE),
                              width = "200px",
                              multiple = T)
  )
}

dataselect_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # finalDf() is the data used to plot the table and plot
    finalDf <- reactive({
      filter(data,
             species == input$select_spp &
             rear_type == input$select_rear &
             covariate == input$select_cov &
             year == input$select_year)
    })

    return(
      list("finalDf" = finalDf)
    )
  })
}

#-------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------
# Plot module ####
plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidPage(

      fluidRow(
#      h2("Figures"),

      shinydashboard::box(
        title = "SAR",
        width = 12,
        plotOutput(outputId = ns("plot"))
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "T:B",
          width = 12,
          plotOutput(outputId = "plot3"))
        )
      )
  )
  }

plot_server <- function(id, finalDf) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlot({
        ggplot(finalDf(), aes( x= doy, color = transport, group = year)) +
        geom_point(aes(y =SAR, fill =  transport))+
        geom_jitter(aes(y =sar.pit, shape =  transport), alpha = .7)+
        tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport), alpha = .25) +
        labs( x = "Day-of-year\n(DOY)", y = "SAR", color = NULL,
              fill = NULL, shape = NULL,
              title = "Predicted SAR versus observed SAR from PIT tag recoveries"
        ) +
        scale_color_manual(breaks = c("0", "1"),
                           values = c("steelblue4", "#b47747"),
                           labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
        scale_fill_manual(breaks = c("0", "1"),
                          values = c("steelblue4", "#b47747"),
                          labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
        scale_shape_manual(values = c(21,21),
                           breaks = c("0", "1"),
                           labels = c("In-river, observed", "Transported, observed")) +
        guides(shape = "legend") +
        theme_minimal()

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

#--------------------------------------------------------------------------------------
# application ####
ui <-
#   fluidPage(
#   titlePanel(title = "Hello"),
#   sidebarLayout(
#     sidebarPanel(
#   dataselect_ui("dataselect")
#     ),
#   mainPanel(
#     plot_ui("plot1")
#     )
#   )
# )

fluidPage(
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = "Seasonal Predictions of Smolt-to-Adult Survival and the Transported to Bypassed fish survival ratio (T:B)"
    ),

    ## Sidebar content - used as a navigation menu to each tab
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        shinydashboard::menuItem("Welcome", tabName = "dashboard", icon = icon("house")),
        shinydashboard::menuItem("Hydrosystem Survival", tabName = "figs", icon = icon("chart-line")),
        div(id = "tabs_filter",
            conditionalPanel(condition = "input.tabs == 'figs'",  dataselect_ui("dataselect"))
        ),
        shinydashboard::menuItem("Background Information", tabName = "bkg", icon = icon("book"))
      )
    ),
    body = shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "dashboard"),
        shinydashboard::tabItem(tabName = "figs", plot_ui("plot1")),
        shinydashboard::tabItem(tabName = "bkg"))
        )
      )
    )

server <- function(session, input, output) {
  x <- dataselect_server("dataselect")
  finalDf    <- x$finalDf
  plot_server("plot1", finalDf)
}

shinyApp(ui = ui, server = server)
