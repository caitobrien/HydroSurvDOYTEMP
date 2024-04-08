#' Background UI Function
#'
#' @description module for background subpage
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_background_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "Background information for  HydroSurvDOYTEMP shinyAPP,",
        em("Seasonal Predictions of Smolt-to-Adult Survival and the Transported to Bypassed fish survival ratio (T:B)")
      ),


      shinydashboard::box(
        title = "Methods",
        status = "info",
        width = 12,
        fluidRow(
          column(
            width = 12,
            HTML("
                 <div style='text-align: left;'>
                 <p>The data used per species, rear type, and covariate model include fish that were tagged with passive-integrated transponders (PIT) tags, data publicly available via ptagis.org. Data includes juveniles detected from 1993 to 2018 at Bonneville Dam (BON), the last dam prior to ocean entry. All fish (Steelhead and spring/summer Chinook) originated upstream of Lower Granite Dam (LGR), with a portion transported via barges from LGR to a release site below BON. Transportation designation was sourced via the DART transportation filter as described by Columbia Basin Research,  https://www.cbr.washington.edu/dart/metadata/pit. Juvenile-to-adult survival in this model is characterized as successful adult detection at LGR to best capture any straying that may have occurred during upstream migration.</p>
                 <p>For the model covariates, the day-of-year (DOY) covariate was assigned based on outmigration detection at BON and represented the migration timing to the estuary and, subsequently, ocean entry. Similarly, river temperature WQM (℃) was assigned based on passage detection at BON using a 7-day right-aligned rolling mean. Each covariate modeled included the linear term (doy, temp) and a quadratic term (i.e., doy2 and temp2 ) to account for non-linear patterns.</p>
                 </div>"
                 ),
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
  )
}


#' Background Server Functions
#'
#' @noRd
mod_background_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

})
}

## To be copied in the UI
# mod_Background_ui("Background_1")

## To be copied in the server
# mod_Background_server("Background_1")
