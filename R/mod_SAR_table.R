#' SAR_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SAR_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(outputId = ns("SAR_table")
    )
  )
}

#' SAR_table Server Functions
#'
#' @noRd
mod_SAR_table_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$SAR_table <- reactable::renderReactable({
      df.table<- data() %>%
        select(species, rear_type, year, transport,doy, SAR, SAR.lo, SAR.hi, sar.pit)

      reactable::reactable(df.table, highlight = TRUE, filterable = TRUE,
                           groupBy = c("species","transport", "rear_type", "year"),
                           columns = list(
                             #year = colDef(aggregate = "unique"),
                             #doy = colDef(aggregate = "median", format = colFormat(digits = 0)),
                             SAR = colDef(aggregate = "median",format = colFormat(digits = 2)),
                             SAR.lo = colDef(aggregate = "median",format = colFormat(digits = 2)),
                             SAR.hi = colDef(aggregate = "median",format = colFormat(digits = 2)),
                             sar.pit = colDef(aggregate = "median",format = colFormat(digits = 2))
                           ))
  })
})
}

## To be copied in the UI
# mod_SAR_table_ui("SAR_table_1")

## To be copied in the server
# mod_SAR_table_server("SAR_table_1")


