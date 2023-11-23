#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
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
          conditionalPanel(condition = "input.tabs == 'figs'",
                           # select species of interest
                           selectInput(inputId = "select_spp",
                                       label = "Select species",
                                       choices = c("Chinook (CH)", "Steelhead (STL)"),
                                       selected = NULL,
                                       width = "200px",
                                       multiple = T),
                           #select rear type
                           selectInput(inputId = "select_rear",
                                       label = "Select rearing type",
                                       choices = c("Wild (W)", "Hatchery (H)"),
                                       selected = NULL,
                                       width = "200px",
                                       multiple = T),
                           # select response metric
                           selectInput(inputId = "select_metric",
                                       label = "Select response metric",
                                       choices = c("Smolt-to-adult return ratio (SAR) ", " Transport to bypass ratio (T:B)"),
                                       selected = NULL,
                                       width = "200px",
                                       multiple = T),
                           # select covariate of interest
                           selectInput(inputId = "select_cov",
                                       label = "Select covariate",
                                       choices = c("Day of Year (DOY)", "Temperature"),
                                       selected = NULL,
                                       width = "200px",
                                       multiple = T),
                           # select years of interest
                           shinyWidgets::pickerInput(inputId = "select_year",
                                       label = "Select years",
                                       choices = 1993:2018,
                                       options = list(`actions-box` = TRUE),
                                       width = "200px",
                                       multiple = T), #windowPadding = 1 doesn't work
                           # add button to run after options are selected
                           actionButton(inputId = 'btn_run_selected',
                                        label = paste0('Run'),
                                        icon = icon("wrench")),
                           ## reset side bar selectoin
                           actionButton(inputId = 'btn_reset_selected',
                                        label = 'Reset',
                                        icon = icon('refresh') )

                           ),

        ),
          shinydashboard::menuItem("Background Information", tabName = "bkg", icon = icon("book"))
        )
      ),

      ## Body content
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "dashboard", mod_Dashboard_ui("Dashboard_ui_1")),
          shinydashboard::tabItem(tabName = "figs", mod_Figures_ui("Figures_ui_1")),
          shinydashboard::tabItem(tabName = "bkg", mod_Background_ui("Background_ui_1")
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
