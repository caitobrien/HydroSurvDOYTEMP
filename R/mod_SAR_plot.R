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
mod_SAR_plot_server <- function(id, finalDf){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$SAR_plot <- renderPlot({
   finalDf() %>%
        # mutate(transport = as.factor(transport),
        #        year = as.factor(year)
        #        ) %>%
        ggplot( aes( x= doy, color = transport)) +
        geom_point(aes(y =SAR, fill =  transport))+
        geom_jitter(aes(y =sar.pit, shape =  transport), alpha = .7)+
        tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, group = year), alpha = .25) +
        # stat_summary( aes(y= SAR, group = transport,
        #              linetype = transport, color = transport),
        #              fun = median,
        #              geom = "line")+
        # stat_summary(data = data.pred, aes(y= SAR, group = transport, linetype = transport, color = transport),
        #              geom = "ribbon",
        #              alpha = .1,
        #              fun.max = max,
        #              fun.min = min)+
        labs( x = "Day-of-year\n(DOY)", y = "Smolt-to-Adult Ratio\n(SAR)", color = "Per year",
              fill = "Per year", shape = NULL, #linetype = "Combined years",
              title = NULL
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
        scale_linetype_manual(values = c("dashed","dashed"),
                              breaks = c("0", "1"),
                              labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability")) +
        guides(shape = "legend") +
        ggrepel::geom_text_repel(data = . %>% filter(doy == 160),aes(y = SAR, label = year, color = transport),
                                 force        = 0.5,
                                 nudge_x      = 0.5,
                                 direction    = "y",
                                 hjust        = -1,
                                 segment.size = 0.2,
                                 min.segment.length = 0, #draw all line segments
                                 xlim = c(-Inf,Inf), #allow values to extend to edges
                                 max.overlaps = 50
        )+
        coord_cartesian(clip = "off") + #disable clipping labels
        theme_light()+ facet_grid(.~transport)
    })
  })
}


## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
