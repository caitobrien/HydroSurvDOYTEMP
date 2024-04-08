#' main_submodule_walkthrough UI Function
#'
#' @description A working shiny submodule that allows the user to walk through the steps of a shiny app with results and supplementary material
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_submodule_walkthrough_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      column(
        width = 12,
        br(),
        h5("Below describes the model used to produce results and interpretation of select results."),
    )
    ),

    shinydashboard::box(
      title = "Methods",
      status = "info",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(
          width = 12,
          shiny::includeHTML(system.file("app/www/mod_main_submodule_walkthrough_methods_text.html", package = "HydroSurvDOYTEMP")),
          br(),
          "Tabs below show the estimated coefficients for each model output:",
        )
      ),
      tabsetPanel(
        id = "myTabs",
        tabPanel("Natural-origin Chinook",
                 shiny::includeHTML(system.file("app/www/table_mod_natural_chinook_doy.html", package = "HydroSurvDOYTEMP")),
                 br(),
                 shiny::includeHTML(system.file("app/www/table_mod_natural_chinook_temp.html", package = "HydroSurvDOYTEMP"))
        ),
        tabPanel("Hatchery-origin Chinook",
                 shiny::includeHTML(system.file("app/www/table_mod_hatchery_chinook_doy.html", package = "HydroSurvDOYTEMP")),
                 br(),
                 shiny::includeHTML(system.file("app/www/table_mod_hatchery_chinook_temp.html", package = "HydroSurvDOYTEMP"))
        ),
        tabPanel("Natural-origin Steelhead",
                 shiny::includeHTML(system.file("app/www/table_mod_natural_steelhead_doy.html", package = "HydroSurvDOYTEMP")),
                 br(),
                 shiny::includeHTML(system.file("app/www/table_mod_natural_steelhead_temp.html", package = "HydroSurvDOYTEMP"))
        ),
        tabPanel("Hatchery-origin Steelhead",
                 shiny::includeHTML(system.file("app/www/table_mod_hatchery_steelhead_doy.html", package = "HydroSurvDOYTEMP")),
                 br(),
                 shiny::includeHTML(system.file("app/www/table_mod_hatchery_steelhead_temp.html", package = "HydroSurvDOYTEMP"))
        )
      ),
      fluidRow(
        column(
          width = 12,
          h4("Additional text after tabs")
        )
      )
    )

  )
}

#' main_submodule_walkthrough Server Functions
#'
#' @noRd
mod_main_submodule_walkthrough_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_main_submodule_walkthrough_ui("main_submodule_walkthrough_1")

## To be copied in the server
# mod_main_submodule_walkthrough_server("main_submodule_walkthrough_1")
