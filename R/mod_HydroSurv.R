#' HydroSurv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_HydroSurv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        9,
      shinydashboard::box(

        title = "Predicted Smolt-to-Adult Ratio (SAR) versus observed SAR from PIT tag recoveries",
        width = 12,
        height = "auto",
        # fluidRow(
        #   column(
        #     "plots")
        #   ),
        fluidRow(
          column(
            width = 12,
            "Select tabs below to adjust temporal resolution:"
          )
        ),
        tabsetPanel(
          id = "plotTabs",
          tabPanel("SAR",mod_SAR_plot_ui("SAR_plot_1")),
          tabPanel("T:B", mod_TI_plot_ui("TI_plot_1")),
          tabPanel("compare", mod_compare_single_plot_ui("compare_single_plot"))
        ),
        # fluidRow(
        #   column(
        #     width = 12,
        #     h4("Additional text after tabs")
        #   )
        # ),
        div(style='height:500;overflow-y: scroll;')
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

#' HydroSurv Server Functions
#'
#' @noRd
mod_HydroSurv_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_HydroSurv_ui("HydroSurv_1")

## To be copied in the server
# mod_HydroSurv_server("HydroSurv_1")
