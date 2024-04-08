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
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Seasonal predictions of Smolt-to-Adult (SAR) and Transport to Bypass (T:B) ratios",
        "By selecting factors of interest, explore seasonal predictions of SAR and T:B for Chinook salmon and Steelhead.",

      )
    ),
    fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = FALSE,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "To plot survival predictions:",
        mod_main_submodule_dataselect_ui("main_dataselect_2")
      )
    ),

    fluidRow(
      shinydashboard::box(
        id = ns("box_plots"),
        title = "Select tabs below show predicted results:",
        width = 12,
        status = "info",
        height = "auto",
        collapsible = TRUE,
        collapsed = FALSE,

        tabsetPanel(
          id = ns("plotTabs"),
          tabPanel("Smolt-to-Adult Ratio (SAR)", mod_main_submodule_select_SAR_plot_ui("SAR_plot_1")),
          tabPanel("Transport to Bypass Ratio (T:B)", mod_main_submodule_select_TI_plot_ui("TI_plot_1")),
          tabPanel("SAR & T:B, compare select years", mod_main_submodule_compare_SAR_TI_plot_ui("compare_single_plot")),
          tabPanel("Walkthrough", mod_main_submodule_walkthrough_ui("walkthrough_example_1"))
      )
    )
    )
# column(
#   3,
#       shinydashboard::box(
#         title = "As you explore this Shiny app, you may notice how:",
#         width = 12,
#         solidHeader = FALSE,
#         collapsible = TRUE,
#         closable = TRUE,
#         height = "auto",
#         div(style='overflow-y: scroll;'),
#         div(
#           HTML("<ul>
#                   <li>In many years, SAR survival declines through the smolt outmigration season</li>
#                   <li>In many years, SAR survival declines through the smolt outmigration season.</li>
#                   <li>Often, SAR survival is higher in in-river migration smolts early in the migration season, whereas later in the season transported smolts have higher SAR survival.</li>
#                   <li>In some years, SAR survival was relatively flat through the season. You can zoom in with the plot.ly tool to see the patterns better.</li>
#                   <li>When comparing between species, you may notice that there are more years with bell-shaped curved patterns of SAR in Steelhead than in Chinook Salmon.</li>
#                 </ul>")
#         )
#       ),
#
#       shinydashboard::box(
#         title = "In addition, some questions you may want to ask yourself as you explore the Shiny app:",
#         width = 12,
#         solidHeader = FALSE,
#         div(
#           HTML("<ul>
#                   <li>Why are SAR survival patterns different year to year?</li>
#                   <li>What other freshwater, estuarine and marine conditions may be affecting their survival?</li>
#                   <li>How much confidence do we have in differences in SAR survival between transported and in-river migrating fish?</li>
#                   <li>And, would you consider these improvements to survival sufficient?</li>
#                   <li>How does the T:B index differ between Chinook Salmon and Steelhead?</li>
#                 </ul>")
#         )
#       )
#     )
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
