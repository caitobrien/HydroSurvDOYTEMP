
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

data<- expand.grid(
  year = 1993:2005,
  species = c("Chinook", "Steelhead"),
  rear_type = c("Hatchery-origin", "Natural-origin"),
  doy = 90:180)
data<-data %>%
  mutate(SAR = rnorm(n(), mean = .05, sd = .01))



mod_dataselect_ui <- function(id){
  ns <- NS(id)
  tagList(

    #select species
    selectInput(inputId = ns("select_spp"),
                label = "Select species",
                choices =  c("Chinook", "Steelhead"),#data.pred$species
                selected = unique(data$species),
                width = "200px",
                multiple = T),

    #select rear type
    selectInput(inputId = ns("select_rear"),
                label = "Select rearing type",
                choices = c("Natural-origin", "Hatchery-origin"),
                selected = unique(data$rear_type),
                width = "200px",
                multiple = T),

    # prompt to select all years or by year
    selectInput(
      inputId = ns("year_display"),
      label = "View by",
      choices = c("All Years", "Year"),
      selected = "All Years"
    ),
    uiOutput(ns("year_picker"))
  )
}

mod_dataselect_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Render the UI for the year picker
    output$year_picker <- renderUI({

      ns <- session$ns


      if (input$year_display == "Year") {
        pickerInput(
          inputId = ns("select_years"),
          label = "Select Year(s)",
          choices = unique(data$year),
          selected = 2000,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      } else {
        NULL
      }
    })

    # Reactive for year_display
    year_display <- reactive({
      input$year_display
    })

    reactive({
      if (input$year_display == "All Years") {
        data %>%
          filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear)
          )
      } else if (input$year_display == "Year" && !is.null(input$select_years)) {
        data %>%
          filter(
            species %in% c(input$select_spp),
            rear_type %in% c(input$select_rear),
            year %in% c(input$select_years)
          )
      } else {
        NULL
      }
    })

  })
}


mod_SAR_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(outputId = ns("SAR_plot"))

  )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_SAR_plot_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #    Retrieve reactive values from mod_dataselect_server
         dataselect_reactives <- mod_dataselect_server("dataselect_1")
         filtered_data <- dataselect_reactives$filtered_data
         year_display <- dataselect_reactives$year_display

    output$SAR_plot <- plotly::renderPlotly({

      # Filter data based on user selection
      if (dataselect_reactives$year_display == "All Years") {

        ggplotly(ggplot(data, aes(x=doy, y=SAR))+
                   geom_point() + facet_grid(rear_type ~ species))

      } else if (dataselect_reactives$year_display == "Year") {

        ggplotly(ggplot(data(), aes(x=doy, y=SAR))+
                   geom_point() + facet_grid(rear_type ~ year + species))
      }
    })
  })
}


app_ui <- function(request) {
  tagList(

    fluidPage(
      shinydashboard::dashboardPage(

        ## Sidebar content - used as a navigation menu to each tab
        sidebar = shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            shinydashboard::menuItem("Plots", tabName = "figs", icon = icon("chart-line")),
            div(id = "tabs_filter",
                conditionalPanel(condition = "input.tabs == 'figs'",  mod_dataselect_ui("dataselect_1"))
            )
          )
        ),
        body = shinydashboard::dashboardBody(
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "figs",mod_HydroSurv_ui("HydroSurv_ui_1"))
          )
        )
      )
    )
  )
}


app_server <- function(input, output, session) {

  filtered_data <- reactive(mod_dataselect_server("dataselect_1"))

  mod_SAR_plot_server("SAR_plot_1", filtered_data())
  mod_SAR_table_server("SAR_table_1", filtered_data())
}


