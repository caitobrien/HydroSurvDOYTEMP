#' SAR_all_years_single_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


fct_SAR_all_years_single_plot<-function(data){



  # ggplot(data, aes(x = doy, y= SAR))+
  #   geom_point() +
  #   facet_grid(species~rear_type)
  #wrangle data to plot median per year per grouping
  data_summarized<- data %>%
    #filter(covariate == "Day-of-year (DOY)") %>%
    mutate(x_var = case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species)) %>%
    group_by(year)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <-  unique(data_summarized$covariate)

  p<- ggplot(data_summarized, aes( x= x_var, group = year)) +
    geom_point(aes(y =SAR, fill =  transport, color = transport))+
    tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, color = transport), alpha = .25)+
    geom_point(aes(y =sar.pit, size = n.obs, shape =  transport, color = transport), alpha = .7)+
    labs( x = covar_label,
          y = "Smolt-to-Adult Ratio\n(SAR)",
          shape = "Observed data",
          size = "Number of fish,\nobserved data",
          color = "Predicted SAR",
          fill = "Predicted SAR",
          # linetype = "Combined years",
          title = "Predicted Smolt-to-Adult Ratio (SAR) versus observed SAR from PIT tag recoveries",
          subtitle = "Comparison of selected years"
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
    scale_linetype_manual(values = c("solid","dashed"),
                          breaks = c("0", "1"),
                          labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
    scale_size_continuous(range = c(1, 5),
                          breaks = c(1, pretty(c(1, max(na.omit(data_summarized$n.obs)), n = 3)))) +
    guides(shape = guide_legend(override.aes = list(color = c("steelblue4", "#b47747") ),
                                order = 1),
           size = guide_legend(override.aes = list(
             label = list(size = 8)),
             order = 2),
           color = guide_legend(order = 3),
           fill = guide_legend(order = 3)) +
    ggrepel::geom_text_repel(data = . %>% group_by(covariate, species, rear_type,transport, year) %>% filter(SAR == max(SAR)),
                             aes( y= SAR,
                                  label = year,
                                  color = transport),
                             nudge_y = 0.1) +
    theme_light()+
    facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background =element_rect(fill="lightgrey"))+
    theme(strip.text = element_text(colour = 'black'))
    theme(panel.spacing = unit(2, "lines"))
    theme(aspect.ratio = .8)
  p
}
