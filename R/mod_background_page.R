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
        width = 12,
        solidHeader = FALSE,
        title = "Overview",
        status = "info",
        br(),
        HTML("
                <div style='text-align: left;'>
                 <p>Chinook Salmon and Steelhead may have decreased survival and endure experiences through the hydroelectric power system as they migrate downstream the Snake and Columbia rivers (Pacific Northwest) towards the ocean. The US Army Corps of Engineers runs the Juvenile Fish Transportation Program to help mitigate decreases in smolt-to-adult-return (SAR) survival going downstream, in the ocean, and back to the river.</p>
                 <p>One way to assess the effectiveness of the transportation program is through the SAR survival ratio of fish that were transported to that of fish bypassed (T:B) through the dams and therefore migrating in-river through the hydrosystem.</p>
                  <div style='text-align: center; margin: auto;'>
                    <ul style='display: inline-block; text-align: left;'>
                     <li>T:B = 1 would indicate a neutral effect of transportation on SAR survival.</li>
                     <li>T:B > 1 would indicate a benefit of transportation, and</li>
                     <li>T:B < 1 would represent a negative effects of transportation on SAR survival.</li>
                    </ul>
                  </div>
                   <p>Understanding which underlying experiences may affect the T:B ratio and thereby improve survival of migratory fishes is useful for management decisions such as water temperature control, when to transport fish, and annual spill rates. Scheuerell et al. (2009) and Gosselin et al. (2018) both consider the effects of freshwater experiences on survival, with the former highlighting the importance of migration timing, and the latter further investigating the difference of transported versus fish migrating in-river.</p>
                   <p>Scheuerell et al. (2009) found that date of arrival in the estuary for natural-origin chinook and steelhead, while varying by year, was an indicator of juveniles surviving to maturity, with early to mid-May having higher survival than later migration in mid-June. Increased springtime river flows or water spilled to expedite transit through the hydrosystem were suggested management options to increase early arrival times to the estuary.</p>
                   <p>Gosselin et al. (2018) using a similar model approach, found that survival associated with carryover effects of juvenile freshwater and marine experiences differ between hatchery- and natural-origin Chinook, as well as, transported versus fish migrating in-river. Such that hatchery fish benefitted from transportation through the migration season, especially in warm PDO years, whereas transportation was detrimental to early season natural-origin chinook, unless in cooler PDO years.</p>
                   <p>While the differences pose a challenge to management decisions, such as when to transport and which rear type to preference in those decisions, Gosselin et al. (2018) did find higher survival with increased river flow, high predation in the estuary and plume areas, and faster migration and development- related increased survival with temperature.</p>
                   <p>This application has adapted both models in a Bayesian framework to provide access to the research findings and allow adjustments based on users’ individual needs to better understand and explore the underlying hypothesis and models. Through this application, the user is also able to compare model predictions to observations, providing a sense of model skill.</p>
                   <p>To learn more about the model see the methods section below</p>
                </div>"
             )
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
                  shiny::includeHTML(here::here("inst/app/www/table_mod_natural_chinook_doy.html")),
                   br(),
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_natural_chinook_temp.html"))
                   ),
          tabPanel("Hatchery-origin Chinook",
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_hatchery_chinook_doy.html")),
                   br(),
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_hatchery_chinook_temp.html"))
                   ),
          tabPanel("Natural-origin Steelhead",
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_natural_steelhead_doy.html")),
                   br(),
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_natural_steelhead_temp.html"))
                   ),
          tabPanel("Hatchery-origin Steelhead",
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_hatchery_steelhead_doy.html")),
                   br(),
                  # shiny::includeHTML(here::here("inst/app/www/table_mod_hatchery_steelhead_temp.html"))
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
