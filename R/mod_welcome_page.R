#' Dashboard UI Function
#'
#' @description module for welcome page of app
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Welcome to HydroSurvDOYTEMP,",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        em("a Shiny App based on the models adapted from Scheuerell et al. (2009) and Gosselin et al. (2018) predicting seasonal survival of Chinook salmon and Steelhead through the hydrosystem")
      )
    ),


    fluidRow(


      column(
        width = 5,
        shinydashboard::box(
          title = "What does this application do?",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          div(
            HTML("<p>This application showcases predicted survival for Chinook salmon, <em>Oncorhynchus tshawytscha</em>, and Steelhead, <em>Oncorhynchus mykiss</em>,
                 influenced by seasonal changes experienced during downstream migration through the Federal Columbia River Power System Hydrosystem (FCRPS) (Figure 1).</p>
                 <p>This application allows users to explore two metrics of interests used to determine the effectiveness of FCRPS transportation program:</p>
                 <ul>
                   <li>Smolt-to-Adult Return survival (SAR),</li>
                   <li>and Transported to Bypassed fish survival ratio (T:B).</li>
                   </ul>
                 ")
          )
        ),
        shinydashboard::box(
          title = "How to use this application?",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          div(
            HTML("<ol>
               <li>To start, select the <b>Hydrosystem Survival</b> tab in the left navigation panel. </li>
               <li>To view specific predicted survival estimates, select:
               <ul>
               <li>species,</li>
               <li>rearing type,</li>
               <li>seasonal covariate,</li>
               <li>and years of interest.</li>
               </ul>
               </li>
               <li>Once selected, figures will update to provide predicted SAR and T:B estimates based on variables of interest.</li>
               </ol>
                 <p>To learn more about how this application can fill knowledge-action gaps and the reliability of the research methodologies featured, see the <b>background tab</b> in the left navigation panel.
                 </p>")
          )
        )
    ),

    column(
      width = 7,
      fluidRow(
        column(width = 2),  # Empty column to center map
        column(
          width = 8,
          shinydashboard::box(
            width = NULL,
            solidHeader = FALSE,
            status = "primary",
            title = "Map of study system: Pacific Northwest, USA ",
            #add leaflet map
            mod_welcome_submodule_leaflet_map_ui("leaflet_map_1"),
            br(),
            "Figure 1: Map of the Columbia and Snake River, Pacific Northwest, USA, with major hydroelectric dams denoted (dark circles) along Spring/Summer Chinook salmon and Steelhead migratory routes. HydroSurvDOYTEMP app underlying model predicts the probability of smolt-to-adult survival from outmigrating juveniles at Bonneville Dam (BON) with an adult return detection at Lower Granite Dam (LGR)."
            )
          ),
          column(width = 2) # Empty column to center map
        )
      )
    )
  )
}

#' Welcome Server Functions
#'
#' @noRd
mod_welcome_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_welcome_submodule_leaflet_map_server("leaflet_map_1")

  })
}

