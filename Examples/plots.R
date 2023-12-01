library(ggrepel)

data %>%
  filter(
         year == c(1993:2018)) %>%
  ggplot(aes(x= doy, y= TI, group = year)) +
  geom_point()+
  stat_summary(geom = "line")+
  stat_summary(fun = median, geom = "line")
  labs( x = "Day-of-year\n(DOY)", y = "TI", color = NULL,
        fill = NULL, shape = NULL,
        title = "Predicted TI"
  ) +
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
  geom_hline(yintercept = 1, color = "black" ) +
  theme_minimal()


data %>%
  filter(year == 1993:2018) %>%
  mutate(transport = as.factor(transport),
         year = as.factor(year)) %>%
  ggplot( aes( x= doy, color = transport)) +
  geom_point(aes(y =SAR, fill =  transport))+
  geom_jitter(aes(y =sar.pit, shape =  transport), alpha = .7)+
  #tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, group = year), alpha = .25) +
  stat_summary(data = data, aes(y= SAR, group = transport, linetype = transport),  fun = median, geom = "line", color = "black")+
  stat_summary(data = data, aes(y= SAR, group = transport), geom = "ribbon", alpha = .1, fun.max = max, fun.min = min, fill = "black", color = "transparent" )+
  labs( x = "Day-of-year\n(DOY)", y = "SAR", color = "per year",
        fill = "per year", shape = NULL, linetype = "all years",
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
  scale_linetype_manual(values = c("solid","dashed"),
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
  theme_minimal() + facet_grid(rear_type ~ species)

