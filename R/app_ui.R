#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'

#
data.pred<-read.csv(here::here("data", "df_mod_predict.csv"))
library(tidyverse)
library(reactable)
library(plotly)
library(modelr)


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
                                                          width = 12, mod_TI_plot_ui("TI_plot_1")),
                                      shinydashboard::box( title = "As you explore this Shiny app, you may notice how:",
                                                           width = 12,
                                                           solidHeader = FALSE,
                                                           div(
                                                             HTML("<ul>
                                                                  <li>In many years, SAR survival declines through the smolt outmigration season</li>
                                                                  <li>In many years, SAR survival declines through the smolt outmigration season.</li>
                                                                  <li>•	Often, SAR survival is higher in in-river migration smolts early in the migration season, whereas later in the season transported smolts have higher SAR survival.</li>
                                                                  <li>In some years, SAR survival was relatively flat through the season. You can zoom in with the plot.ly tool to see the patterns better.</li>
                                                                  <li>When comparing between species, you may notice that there are more years with bell-shaped curved patterns of SAR in Steelhead than in Chinook Salmon.</li>
                                                                  </ul>"
                                                                  )
                                                             )
                                                           ),
                                      shinydashboard::box( title = "In addition, some questions you may want to ask yourself as you explore the Shiny app:",
                                                           width = 12,
                                                           solidHeader = FALSE,
                                                           div(
                                                             HTML("<ul>
                                                                  <li>Why are SAR survival patterns different year to year?</li>
                                                                  <li>What other freshwater, estuarine and marine conditions may be affecting their survival?</li>
                                                                  <li>How much confidence do we have in differences in SAR survival between transported and in-river migrating fish?</li>
                                                                  <li>And, would you consider these improvements to survival sufficient?</li>
                                                                  <li>How does the T:B index differ between Chinook Salmon and Steelhead?</li>
                                                                  </ul>"
                                                                  )
                                                             )
                                                           )
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





