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
mod_TI_plot_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$TI_plot <- renderPlot({

      # plotly::ggplotly(
        data() %>%
        mutate(x_var = case_when(
            covariate == "Day-of-year (DOY)" ~ doy,
            TRUE ~ temp
          )) %>%
        mutate(transport = as.factor(transport),
               rear_type = as.factor(rear_type),
               covariate = as.factor(covariate),
               species = as.factor(species)
        ) %>%
        ggplot( aes(x= x_var, y= TI)) +
        geom_point(aes(group = year))+
        stat_summary(geom = "line", aes(group = year), alpha =.25) +
        # stat_summary(data = data.pred, aes(y= TI, x = doy),
        #              fun = median,
        #              geom = "line",
        #              color = "#545454",
        #              linetype = "dashed")+
        labs(x = data()$covariate,
             y = "Transport to Bypass Ratio\n(T:B)",
             color = NULL,
             title = NULL) +
        geom_hline(yintercept = 1, color = "black" ) +
        ggrepel::geom_text_repel(data = . %>% group_by(covariate, species, rear_type, year) %>% filter(x_var == max(x_var)),aes( label = year),
                                 force        = 0.1,
                                 nudge_x      = 0.1,
                                 direction    = "y",
                                 hjust        = -.7,
                                 segment.size = 0.05,
                                 min.segment.length = 0, #draw all line segments
                                 xlim = c(-Inf,Inf), #allow values to extend to edges
                                 max.overlaps = 30
        )+
        coord_cartesian(clip = "off") + #disable clipping labels
        scale_color_manual(values =  "black",
                           labels = "Transported:In-river ratio")+
        theme_light()+ facet_grid(rear_type ~ species, scales = "free_y") +
        theme(strip.background =element_rect(fill="lightgrey"))+
        theme(strip.text = element_text(colour = 'black'))
      #     theme(plot.margin = margin(1, 0, 0, 1.5, "cm")),
      #   tooltip =  "text"
      # ) %>%
      #   layout(
      #     hovermode = "x"
      #   ) %>%
      #   plotly::config(displayModeBar = FALSE) %>%
      #   plotly::config(showLink = FALSE)
    })
  })
}

## To be copied in the UI
# mod_TI_plot_ui("TI_plot_1")

## To be copied in the server
# mod_TI_plot_server("TI_plot_1")
