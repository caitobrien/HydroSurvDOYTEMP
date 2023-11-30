library(ggrepel)
data_ends<- data %>% filter(doy == 160, year == 2000)
data %>%
  filter(year == 2000) %>%
  ggplot(aes(x= doy, color = transport, group = year)) +
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
  geom_text_repel(data = data_ends, aes(label = year, y= SAR, color = transport, group = year), max.overlaps = 100, vjust = 1) +
  guides(shape = "legend") +
  theme_minimal()

p + geom_text_repel(data = data_ends, aes(label = year, y= SAR, color = transport, group = year), max.overlaps = 100, vjust = 1 )



data %>%
  filter(year == 2000,
         transport == 1) %>%
  ggplot(aes(x= doy, color = transport, group = year)) +
  geom_point(aes(y =TI, fill =  transport))+
  tidybayes::geom_lineribbon( aes(y = TI, ymin =TI.lo, ymax = TI.hi, fill =  transport), alpha = .25) +
  labs( x = "Day-of-year\n(DOY)", y = "TI", color = NULL,
        fill = NULL, shape = NULL,
        title = "Predicted TI"
  ) +
  geom_hline(yintercept = 1, color = "black" ) +
  scale_color_manual(values =  "black",
                     labels = "Transported:In-river ratio \npredicted with 95% CI")+
  scale_fill_manual(values =  "black",
                     labels = "Transported:In-river ratio \npredicted with 95% CI")+
  #geom_text_repel(data = data_ends, aes(label = year, y= SAR, color = transport, group = year), max.overlaps = 100, vjust = 1) +
  guides(shape = "legend") +
  theme_minimal()

data %>%
filter(transport == 1,
       year == c(2007)) %>%
  ggplot(aes(x= doy, y= TI, color = transport, group = year)) +
  # stat_summary(fun = median, geom = "line")+
  # stat_summary(fun = median, geom = "point") +
  geom_point()+
  geom_line()+
 # geom_ribbon(aes(ymin = TI.lo, ymax = TI.hi), alpha = .25, color = NA)+
  ggdist::geom_pointinterval(aes(ymin = TI.lo, ymax = TI.hi))+
  labs( x = "Day-of-year\n(DOY)", y = "TI", color = NULL,
      fill = NULL, shape = NULL,
      title = "Predicted TI"
  ) +
  geom_hline(yintercept = 1, color = "black" ) +
  scale_color_manual(values =  "black",
                     labels = "Transported:In-river ratio \npredicted with 95% CI")+
  theme_minimal()

data %>%
  filter(
         year == c(2007)) %>%
  ggplot(aes(x= doy, y= TI, group = year)) +
  # stat_summary(fun = median, geom = "line")+
  stat_summary(fun = median, geom = "pointrange", fun.min = TI.lo, fun.max = max) +
  geom_point()+
  geom_line()+
  # geom_ribbon(aes(ymin = TI.lo, ymax = TI.hi), alpha = .25, color = NA)+
  ggdist::geom_pointinterval(aes(y = TI, ymin = TI.lo, ymax = TI.hi))+
  labs( x = "Day-of-year\n(DOY)", y = "TI", color = NULL,
        fill = NULL, shape = NULL,
        title = "Predicted TI"
  ) +
  geom_hline(yintercept = 1, color = "black" ) +
  theme_minimal()


