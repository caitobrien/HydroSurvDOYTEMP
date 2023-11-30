#' TI_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TI_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("TI_plot"))
  )
}

#' TI_plot Server Functions
#'
#' @noRd
mod_TI_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$TI_plot <- renderPlot({
      finalDf() %>%
        ggplot(aes(x= doy, y= TI, group = year)) +
        geom_point()+
        geom_line()+
        labs( x = "Day-of-year\n(DOY)", y = "TI", color = NULL,
              title = "Predicted TI"
        ) +
        geom_hline(yintercept = 1, color = "black" ) +
        scale_color_manual(values =  "black",
                           labels = "Transported:In-river ratio")+
        theme_minimal()
    })
  })
}

## To be copied in the UI
# mod_TI_plot_ui("TI_plot_1")

## To be copied in the server
# mod_TI_plot_server("TI_plot_1")
