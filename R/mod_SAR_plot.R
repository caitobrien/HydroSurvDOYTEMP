#' SAR_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SAR_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("SAR_plot"))
  )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_SAR_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$SAR_plot <- renderPlot({
      ggplot2::ggplot(finalDf(), aes( x= doy, color = transport, group = year)) +
        geom_point(aes(y =SAR, fill =  transport))+
        geom_jitter(aes(y =sar.pit, shape =  transport), alpha = .7)+
        tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport), alpha = .25) +
        labs( x = "Day-of-year\n(DOY)", y = "SAR", color = NULL,
              fill = NULL, shape = NULL,
              title = "Predicted SAR versus observed SAR from PIT tag recoveries"
        ) +
        scale_color_manual(breaks = c("0", "1"),
                           values = c("steelblue4", "#b47747"),
                           labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
        scale_fill_manual(breaks = c("0", "1"),
                          values = c("steelblue4", "#b47747"),
                          labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
        scale_shape_manual(values = c(21,21),
                           breaks = c("0", "1"),
                           labels = c("In-river, observed", "Transported, observed")) +
        guides(shape = "legend") +
        theme_minimal()
    })
  })
}

## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
