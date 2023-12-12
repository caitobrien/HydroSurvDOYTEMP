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




data.pred %>%
  mutate(transport = as.factor(transport),
         year = as.factor(year),
         rear_type = as.factor(rear_type),
         covariate = as.factor(covariate),
         species = as.factor(species)
  ) %>%
  filter(year %in% c(2000),
         covariate == "Day-of-year (DOY)") %>%
  ggplot( aes( x= doy, color = transport)) +
  geom_point(aes(y =SAR, fill =  transport))+
  geom_point(aes(y =sar.pit, shape =  transport), alpha = .7)+
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
  # ggrepel::geom_text_repel(data = . %>% group_by(year, transport) %>% filter(SAR == max(SAR)),aes(y = SAR, label = year, color = transport),
  #                          # force        = 0.1,
  #                          # nudge_x      = 0.1,
  #                          # direction    = "y",
  #                          # hjust        = -.7,
  #                          # segment.size = 0.1,
  #                            min.segment.length = 0, #draw all line segments
  #                          # xlim = c(-Inf,Inf), #allow values to extend to edges
  #                          # max.overlaps = 100
  # )+
  coord_cartesian(clip = "off") + #disable clipping labels
  theme_light()+ facet_grid(rear_type ~ year*species)

ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(vs+gear ~ cyl+am,labeller = labeller(.rows = label_both, .cols = label_both))

