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
    plotOutput(outputId = ns("SAR_plot")
               )
    )
}

#' SAR_plot Server Functions
#'
#' @noRd
mod_SAR_plot_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$SAR_plot <- renderPlot({

     data() %>%
        mutate(transport = as.factor(transport),
               year = as.factor(year),
               rear_type = as.factor(rear_type),
               covariate = as.factor(covariate),
               species = as.factor(species)
               ) %>%
       group_by( year) %>%
        ggplot( aes( x= doy, color = transport)) +
        geom_point(aes(y =SAR, fill =  transport))+
        tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, group = year), alpha = .25) +
        geom_point(aes(y =sar.pit, shape =  transport), alpha = .7)+
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
                              labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
        ggrepel::geom_text_repel(data = . %>% group_by(year, transport) %>% filter(doy == 160),aes(y = SAR, label = year, color = transport),
                                 force        = 0.1,
                                 nudge_x      = 0.1,
                                 direction    = "y",
                                 hjust        = -.7,
                                 segment.size = 0.1,
                                 min.segment.length = 0, #draw all line segments
                                 xlim = c(-Inf,Inf), #allow values to extend to edges
                                 max.overlaps = 30
        )+
        coord_cartesian(clip = "off") + #disable clipping labels
        xlim(NA, 170)+
        theme_light()+ facet_grid(rear_type ~ species, scales = "free_y") +
        theme(strip.background =element_rect(fill="lightgrey"))+
        theme(strip.text = element_text(colour = 'black')) +
        theme(plot.margin = margin(1, 0, 0, 1.5, "cm"))
    })
  })
}



## To be copied in the UI
# mod_SAR_plot_ui("SAR_plot_1")

## To be copied in the server
# mod_SAR_plot_server("SAR_plot_1")
