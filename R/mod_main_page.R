#' HydroSurv main page UI Function
#'
#' @description module outlining the UI elements of the hydro surv main page
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        9,
      shinydashboard::box(

        title = "Seasonal predictions of Smolt-to-Adult and Transport to Bypass ratios",
        width = 12,
        height = "auto",
        # fluidRow(
        #   column(
        #     "plots")
        #   ),
        fluidRow(
          column(
            width = 12,
            "Select tabs below show predicted results:"
          )
        ),
        tabsetPanel(
          id = "plotTabs",
          tabPanel("Smolt-to-Adult Ratio (SAR)",
                   tags$div(
                     style = "margin-bottom: 20px;",  # Add margin within this tab
                     mod_main_select_SAR_plot_ui("SAR_plot_1")
                   )
          ),
          tabPanel("Transport to Bypass Ratio (T:B)", mod_TI_plot_ui("TI_plot_1")),
          tabPanel("SAR & TB, compare select years", mod_compare_single_plot_ui("compare_single_plot"))
        ),
        # fluidRow(
        #   column(
        #     width = 12,
        #     h4("Additional text after tabs")
        #   )
        # ),
        div(style='height:600;overflow-y: scroll;')
      )

      ),

      # shinydashboard::box(
      #   title = "Predicted Transport-to-Bypass Ratio (T:B)",
      #   width = 12, mod_TI_plot_ui("TI_plot_1"),
      #   height = "auto",
      #   div(style='height:"auto;overflow-y: scroll;')
      # ),
column(
  3,
      shinydashboard::box(
        title = "As you explore this Shiny app, you may notice how:",
        width = 12,
        solidHeader = FALSE,
        collapsible = TRUE,
        closable = TRUE,
        height = "auto",
        div(style='overflow-y: scroll;'),
        div(
          HTML("<ul>
                  <li>In many years, SAR survival declines through the smolt outmigration season</li>
                  <li>In many years, SAR survival declines through the smolt outmigration season.</li>
                  <li>Often, SAR survival is higher in in-river migration smolts early in the migration season, whereas later in the season transported smolts have higher SAR survival.</li>
                  <li>In some years, SAR survival was relatively flat through the season. You can zoom in with the plot.ly tool to see the patterns better.</li>
                  <li>When comparing between species, you may notice that there are more years with bell-shaped curved patterns of SAR in Steelhead than in Chinook Salmon.</li>
                </ul>")
        )
      ),

      shinydashboard::box(
        title = "In addition, some questions you may want to ask yourself as you explore the Shiny app:",
        width = 12,
        solidHeader = FALSE,
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
    )
  )
)
}

#' HydroSurv main page Server Functions
#'
#' @noRd
mod_main_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_HydroSurv_ui("HydroSurv_1")

## To be copied in the server
# mod_HydroSurv_server("HydroSurv_1")
