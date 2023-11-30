library(shiny)
library(plotly)
library(reshape2)
library(DT)
library(tidybayes)
library(tidyverse)
library(ggdist)

#  data<-read.csv(here::here("data", "ChSSWRT_mod_predict.csv"))
# #
# #
#  data$transport<-as.factor(data$transport)
#  data$year <- as.factor(data$year)


data<-data %>%
  mutate(
    transport = as.factor(transport),
    year = as.factor(year))
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
                              selected = 2000, #unique(data$year),
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
SAR_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
        plotOutput(outputId = ns("SAR_plot"))
  )
}

SAR_plot_server <- function(id, finalDf) {
  moduleServer(id, function(input, output, session) {

    output$SAR_plot <- renderPlot({
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
  })
}


TI_plot_ui <-function(id) {
  ns <-NS(id)
  tagList(
    plotOutput(outputId = ns("TI_plot"))
  )
}

TI_plot_server <- function(id, finalDf) {
  moduleServer(id, function(input, output, session) {

    output$TI_plot <- renderPlot({
      finalDf() %>%
        ggplot(aes(x= doy, y= TI, group = year)) +
        geom_point()+
        geom_line()+
        labs( x = "Day-of-year\n(DOY)", y = "TI", color = NULL,
              title = "Predicted TI"
        ) +
        geom_hline(yintercept = 1, color = "black" ) +
        scale_color_manual(values =  "black",
                            labels = "Transported:In-river ratio")+
        theme_minimal()
      })
  })
  }


#--------------------------------------------------------------------------------------
# application ####
ui <- fluidPage(
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
        shinydashboard::tabItem(tabName = "figs",
                                  fluidRow(
                                    shinydashboard::box(title = "SAR",
                                                        width = 12,SAR_plot_ui("SAR_plot_1")),
                                    shinydashboard::box(title = "TI",
                                                        width = 12,TI_plot_ui("TI_plot_1"))
                                    )
                                ),
        shinydashboard::tabItem(tabName = "bkg"))
        )
      )
    )

server <- function(session, input, output) {
  x <- dataselect_server("dataselect")
  finalDf    <- x$finalDf
  SAR_plot_server("SAR_plot_1", finalDf)
  TI_plot_server("TI_plot_1", finalDf)
}

shinyApp(ui = ui, server = server)
