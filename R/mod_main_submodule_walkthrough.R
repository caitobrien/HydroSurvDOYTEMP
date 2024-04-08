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

    #information to add
    fluidRow(
          shinydashboard::box(
            title = "As you explore this Shiny app, you may notice how:",
            width = 12,
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            div(
              HTML("<ul>
                      <li>In many years, SAR survival declines through the smolt outmigration season</li>
                      <li>In many years, SAR survival declines through the smolt outmigration season.</li>
                      <li>Often, SAR survival is higher in in-river migration smolts early in the migration season, whereas later in the season transported smolts have higher SAR survival.</li>
                      <li>In some years, SAR survival was relatively flat through the season. You can zoom in with the plot.ly tool to see the patterns better.</li>
                      <li>When comparing between species, you may notice that there are more years with bell-shaped curved patterns of SAR in Steelhead than in Chinook Salmon.</li>
                    </ul>")
            )
          )
          ),

    fluidRow(
          shinydashboard::box(
            title = "In addition, some questions you may want to ask yourself as you explore the Shiny app:",
            width = 12,
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            div(
              HTML("<ul>
                      <li>Why are SAR survival patterns different year to year?</li>
                      <li>What other freshwater, estuarine and marine conditions may be affecting their survival?</li>
                      <li>How much confidence do we have in differences in SAR survival between transported and in-river migrating fish?</li>
                      <li>And, would you consider these improvements to survival sufficient?</li>
                      <li>How does the T:B index differ between Chinook Salmon and Steelhead?</li>
                    </ul>")
            )
          )
        ),

    fluidRow(
        shinydashboard::box(
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          title = "Model parameter estimates",
          "Tabs below show the estimated coefficients for each model output:",
          br(),
          tabsetPanel(
            id = ns("modelTabs"),
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
