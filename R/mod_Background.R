#' Background UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Background_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Background information",
        width = 12
      ),
      shinydashboard::box(
        title = "Seasonal Predictions of Smolt-to-Adult Survival and
                          the Transported to Bypassed fish survival ratio (T:B)",
        width = 12,
        "Chinook Salmon and Steelhead may have decreased survival and endure experiences
                          through the hydroelectric power system as they migrate downstream the Snake and
                          Columbia rivers (Pacific Northwest) towards the ocean.  The US Army Corps of
                          Engineers runs the Juvenile Fish Transportation Program to help mitigate against
                          decreases in smolt-to-adult-return (SAR) survival going downstream, in the
                          ocean, and back to the river.",
        br(),
        "One way to assess the effectiveness of the transportation program is through
                          the SAR survival ratio of fish that were transported to that of fish bypassed
                          (T:B) through the dams and therefore migrating in-river through the hydrosystem.",
        br(),
        "A T:B = 1 would indicate a neutral effect of transportation on SAR survival.
                          T:B > 1 would indicate a benefit of transportation, and T:B < 1 would represent
                          a negative effects of transportation on SAR survival."
      ),
      shinydashboard::box(
        title = "Tabs in Box",
        status = "info",
        width = 12,
        fluidRow(
          column(
            width = 12,
            h4("Introduction text before tabs")
          )
        ),
        tabsetPanel(
          id = "myTabs",
          tabPanel("Natural-origin Chinook",
                   includeHTML("inst/app/www/table_mod_natural_chinook_doy.html"),
                   br(),
                   includeHTML("inst/app/www/table_mod_natural_chinook_temp.html")),
          tabPanel("Hatchery-origin Chinook",
                   includeHTML("inst/app/www/table_mod_hatchery_chinook_doy.html"),
                   br(),
                   includeHTML("inst/app/www/table_mod_hatchery_chinook_temp.html")),
          tabPanel("Natural-origin Steelhead",
                   includeHTML("inst/app/www/table_mod_natural_steelhead_doy.html"),
                   br(),
                   includeHTML("inst/app/www/table_mod_natural_steelhead_temp.html")),
          tabPanel("Hatchery-origin Steelhead",
                   includeHTML("inst/app/www/table_mod_hatchery_steelhead_doy.html"),
                   br(),
                   includeHTML("inst/app/www/table_mod_hatchery_steelhead_temp.html"))
        ),
        fluidRow(
          column(
            width = 12,
            h4("Additional text after tabs")
          )
        )
      )
    )
  )
}

#' Background Server Functions
#'
#' @noRd
mod_Background_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

})
}

## To be copied in the UI
# mod_Background_ui("Background_1")

## To be copied in the server
# mod_Background_server("Background_1")
