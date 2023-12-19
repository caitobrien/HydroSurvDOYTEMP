library(ggrepel)
library(ggdist)


#plotly best optioon still needs work on multiple year CI and grouping levels
plotly::ggplotly(
  data() %>%
    mutate(transport = as.factor(transport),
           year = as.factor(year),
           rear_type = as.factor(rear_type),
           covariate = as.factor(covariate),
           species = as.factor(species)
    ) %>%
    group_by( year) %>%
    ggplot( aes( x= doy, color = transport)) +
    geom_point(aes(y =SAR, fill =  transport, text = paste ("Year", year, "\nDay-of-year:",doy, "\nPassage type:", transport, "\nPredicted SAR with 95% CI:", round(SAR,2))))+
    geom_line(aes(y = SAR, fill= transport)) +
    geom_ribbon(aes(ymin = SAR.lo, ymax = SAR.hi, fill= transport), alpha =.25, color = NA) +
    geom_point(aes(y =sar.pit, shape =  transport, text = paste ("Year", year, "\nDay-of-year:",doy, "\nPassage type:", transport,"\nObserved SAR:", round(sar.pit,2))), alpha = .7)+
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
    theme_light()+ facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background =element_rect(fill="lightgrey"))+
    theme(strip.text = element_text(colour = 'black')) +
    theme(plot.margin = margin(1, 0, 0, 1.5, "cm")),
  tooltip =  "text"
) %>%
  layout(
    hovermode = "x",
    showlegend = FALSE
  ) %>%
  plotly::config(displayModeBar = FALSE) %>%
  plotly::config(showLink = FALSE)
