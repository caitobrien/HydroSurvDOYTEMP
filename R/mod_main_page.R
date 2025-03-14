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
        "By selecting factors of interest, explore seasonal predictions of SAR (LGR to LGR) and T:B for spring/summer Snake River Chinook salmon and Steelhead.",

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
          tabPanel(title = "Smolt-to-Adult Ratio (SAR)", value = "sar", mod_main_submodule_select_SAR_plot_ui("SAR_plot_1")),
          tabPanel(title = "Transport to Bypass Ratio (T:B)", value = "ti", mod_main_submodule_select_TI_plot_ui("TI_plot_1")),
          tabPanel(title = "SAR & T:B, compare select years", value = "sarxti", mod_main_submodule_compare_SAR_TI_plot_ui("compare_single_plot")),
          tabPanel(title = "As you explore...", value = "examples", mod_main_submodule_walkthrough_ui("walkthrough_example_1"))
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

    current_tab <- reactiveVal()

    observeEvent(input$plotTabs, {
      current_tab(input$plotTabs)
    })

    return(
      list(
      current_tab = reactive(current_tab)
      )
    )

  })
}

## To be copied in the UI
# mod_HydroSurv_ui("HydroSurv_1")

## To be copied in the server
# mod_HydroSurv_server("HydroSurv_1")
