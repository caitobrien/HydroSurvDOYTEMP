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
mod_TI_plot_server <- function(id, finalDf){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$TI_plot <- renderPlot({

      finalDf %>%
        ggplot( aes(x= doy, y= TI)) +
        geom_point(aes(group = year))+
        stat_summary(geom = "line", aes(group = year)) +
        stat_summary(data = data1, aes(y= TI, x = doy),
                     fun = median,
                     geom = "line",
                     color = "#545454",
                     linetype = "dashed")+
        labs( x = "Day-of-year\n(DOY)", y = "Transport to Bypass Ratio\n(T:B)", color = NULL,
              title = NULL
        ) +
        geom_hline(yintercept = 1, color = "black" ) +
        ggrepel::geom_text_repel(data = . %>% filter(doy == 160),aes( label = year),
                                 force        = 0.5,
                                 nudge_x      = 0.5,
                                 direction    = "y",
                                 hjust        = -1,
                                 segment.size = 0.2,
                                 min.segment.length = 0, #draw all line segments
                                 xlim = c(-Inf,Inf), #allow values to extend to edges
                                 max.overlaps = 30
        )+
        coord_cartesian(clip = "off") + #disable clipping labels
        scale_color_manual(values =  "black",
                           labels = "Transported:In-river ratio")+
        theme_light()
    })
  })
}

## To be copied in the UI
# mod_TI_plot_ui("TI_plot_1")

## To be copied in the server
# mod_TI_plot_server("TI_plot_1")