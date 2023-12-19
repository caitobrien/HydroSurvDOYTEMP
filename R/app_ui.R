#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'


# data.pred<-read.csv(here::here("data", "df_mod_predict.csv"))
# library(tidyverse)
# library(reactable)
# library(plotly)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      shinydashboard::dashboardPage(
        header = shinydashboard::dashboardHeader(
          title = "Seasonal Predictions Hydrosystem Survival", #Seasonal Predictions of Smolt-to-Adult Survival and the Transported to Bypassed fish survival ratio (T:B)
          titleWidth = 450
        ),

        ## Sidebar content - used as a navigation menu to each tab
        sidebar = shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            shinydashboard::menuItem("Welcome", tabName = "dashboard", icon = icon("house")),
            shinydashboard::menuItem("Hydrosystem Survival", tabName = "figs", icon = icon("chart-line")),
            div(id = "tabs_filter",
                conditionalPanel(condition = "input.tabs == 'figs'",  mod_dataselect_ui("dataselect_1"))
            ),
            shinydashboard::menuItem("Background Information", tabName = "bkg", icon = icon("book"))
          )
        ),
        body = shinydashboard::dashboardBody(
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "dashboard",mod_Dashboard_ui("dashboard_text_1")),
            shinydashboard::tabItem(tabName = "figs",
                                    fluidRow(
                                      shinydashboard::box(title = "Predicted Smolt-to-Adult Ratio (SAR) versus observed SAR from PIT tag recoveries",
                                                          width = 12, mod_SAR_plot_ui("SAR_plot_1"),
                                                          br(),
                                                          "Table output:",
                                                          mod_SAR_table_ui("SAR_table_1") ),
                                      shinydashboard::box(title = "Predicted Transport-to-Bypass Ratio (T:B)",
                                                          width = 12, mod_TI_plot_ui("TI_plot_1"))
                                    )
            ),
            shinydashboard::tabItem(tabName = "bkg", mod_Background_ui("Background_ui_1"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HydroSurvDOYTEMP"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}





