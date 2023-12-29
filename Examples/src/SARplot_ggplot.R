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
mod_SAR_plot_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$SAR_plot <- renderPlot({

      data() %>%
        mutate(x_var = case_when(
          covariate == "Day-of-year (DOY)" ~ doy,
          TRUE ~ temp
        )) %>%
        mutate(
          transport = as.factor(transport),
          year = as.factor(year),
          rear_type = as.factor(rear_type),
          covariate = as.factor(covariate),
          species = as.factor(species)
        ) %>%
        group_by( year) %>%
        ggplot( aes( x= x_var)) +
        geom_point(aes(y =SAR, fill =  transport, color = transport))+
        tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, group = year, color = transport), alpha = .25)+
        geom_point(aes(y =sar.pit, size = n.obs, shape =  transport, color = transport), alpha = .7)+
        # stat_summary(fun = "median",
        #              geom = "line",
        #              color = "black",
        #              size = 1,
        #              aes(linetype = transport, y = SAR)) +
        labs( x = data()$covariate,
              y = "Smolt-to-Adult Ratio\n(SAR)",
              shape = "Observed data",
              size = "Number of fish observed",
              color = "Predicted SAR",
              fill = "Predicted SAR",
              # linetype = "Combined years",
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
                           labels = c("In-river", "Transported")) +
        # scale_linetype_manual(values = c("solid","dashed"),
        #                       breaks = c("0", "1"),
        #                       labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
        scale_size_continuous(range = c(1, 5),
                              breaks = c(1, pretty(c(1, max(na.omit(data()$n.obs)), n = 3)))) +
        guides(shape = guide_legend(override.aes = list(color = c("steelblue4", "#b47747") ),
                                    order = 1),
               size = guide_legend(override.aes = list(
                 label = list(size = 8)),
                 order = 2),
               color = guide_legend(order = 3),
               fill = guide_legend(order = 3)) +
        # linetype = guide_legend(order = 4)) +
        ggrepel::geom_text_repel(data = . %>% group_by(covariate, species, rear_type, year, transport) %>% filter(x_var == max(x_var)) %>% arrange(year, desc(SAR)),aes(y = SAR, label = year, color = transport),
                                 force        = 0.1,
                                 nudge_x      = 0.1,
                                 direction    = "y",
                                 hjust        = -.7,
                                 segment.size = 0.05,
                                 min.segment.length = 0, #draw all line segments
                                 xlim = c(-Inf,Inf), #allow values to extend to edges
                                 max.overlaps = 100
        )+
        coord_cartesian(clip = "off") + #disable clipping labels
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
