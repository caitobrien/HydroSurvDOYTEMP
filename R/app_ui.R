#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(

      shinydashboard::dashboardPage(
        header = shinydashboard::dashboardHeader(
          title = "Columbia Basin Research" #Seasonal Predictions of Smolt-to-Adult Survival and the Transported to Bypassed fish survival ratio (T:B)
        ),

        ## Sidebar content
        sidebar = shinydashboard::dashboardSidebar(
          #override theme for sidepanel selectInput color
          shiny::includeCSS(system.file("app/www/theme.css", package = "HydroSurvDOYTEMP")),
          shinydashboard::sidebarMenu(
            id = "tabs",
            shinydashboard::menuItem("About", tabName = "about", icon = icon("house")),
            shinydashboard::menuItem("Hydrosystem Survival", tabName = "figs", icon = icon("chart-line"), selected = TRUE),
            shinydashboard::menuItem("Supplementary Information", tabName = "supp", icon = icon("book")),
            br(),
            br(),
            # add zoom to sidebar
            tags$div(class = "zoom-controls",
                     tags$li(tags$a(id = "zoom_in", class = "btn btn-default zoom-btn", "Zoom In", icon("search-plus"))),
                     tags$li(tags$a(id = "zoom_out", class = "btn btn-default zoom-btn", "Zoom Out", icon("search-minus")))
            )
          )
        ),
        body = shinydashboard::dashboardBody(
          #add CSS CBR global theme
          fresh::use_theme(CBRtheme),

          #add optional zoom
          tags$head(
            shiny::includeScript(system.file("app/www/zoom_functions.js", package = "HydroSurvDOYTEMP"))
          ),

          #add tabItems
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "about",mod_about_page_ui("about_page_ui_1")),
            shinydashboard::tabItem(tabName = "figs",mod_main_page_ui("main_page_ui_1")),
            shinydashboard::tabItem(tabName = "supp", mod_background_page_ui("background_page_ui_1"))
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





