library(shiny)
library(ggplot2)
library(shinydashboard)



####module
mod_Figures_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(

      fluidRow(
        h2("Header"),
        shinydashboard::box(
          title = "X",
          width = 6,
          plotOutput(outputId = ns("plot1")))
      )
    )
  )
}

mod_Figures_server<- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df<- reactive({
      filter(iris,species == input$select_spp)
    })

    output$plot1 <- renderPlot({
      ggplot(df(), aes(Sepal.Length, Sepal.Width, color = Species)) +
        geom_point()
    })
  }
  )
}


##app
app_ui <- function(request) {
  tagList(
    # Your application UI logic
    shinydashboard::dashboardPage(header = shinydashboard::dashboardHeader(title = "Header"),

                                  ## Sidebar content -
                                  sidebar = shinydashboard::dashboardSidebar(
                                    shinydashboard::sidebarMenu(
                                      id = "tabs",
                                      shinydashboard::menuItem("Welcome", tabName = "dashboard", icon = icon("house")),
                                      shinydashboard::menuItem("Penguins", tabName = "figs", icon = icon("chart-line")),
                                      div(id = "tabs_filter",
                                          conditionalPanel(condition = "input.tabs == 'figs'",
                                                           # select species of interest
                                                           selectInput(inputId = "select_spp",
                                                                       label = "Select species",
                                                                       choices =  iris$Species,
                                                                       selected = NULL,
                                                                       width = "200px",
                                                                       multiple = T)
                                          ),
                                      ),
                                      shinydashboard::menuItem("Background Information", tabName = "bkg", icon = icon("book"))
                                    )
                                  ),

                                  ## Body content
                                  body = shinydashboard::dashboardBody(
                                    shinydashboard::tabItems(
                                      shinydashboard::tabItem(tabName = "dashboard"),
                                      shinydashboard::tabItem(tabName = "figs", mod_Figures_ui("Figures_ui_1")),
                                      shinydashboard::tabItem(tabName = "bkg")
                                    )
                                  )
    )
  )
}

app_server <- function(input, output, session) {
  mod_Figures_server("Figures_UI_1") #needs to match UI naming to render output

}

shinyApp(app_ui, app_server)
