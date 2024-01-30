#' TI_by_years_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return the return value is a plot with median TI and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd
fct_TI_by_years_plot <- function(data) {

  # wrangle data to plot median per year per grouping
  data_summarized <- data %>%
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
      species = as.factor(species),
      species_rear = interaction(species, rear_type)
    ) %>%
    group_by(year)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <- ggplot(data_summarized, aes(x = x_var, y = TI)) +
    geom_point()+
    geom_line(aes(group = year))+
   # geom_ribbon(aes(y = TI, ymin = .lower, ymax = .upper), alpha = .25) +
    #stat_summary(geom = "line", alpha =.25) +
    labs(
      x = covar_label,
      y = "Transport to Bypass Ratio\n(T:B)",
      title = NULL
    ) +
    geom_hline(yintercept = 1, color = "black" ) +
    scale_color_manual(values =  "black",
                       labels = "Transported:In-river ratio")+
    theme_light()+
    facet_wrap(~year + species_rear, scales = "free_y", ncol = 4) +
    theme(strip.background =element_rect(fill="lightgrey"),
          strip.text = element_text(colour = 'black'),
          panel.spacing = unit(2, "lines"),
          aspect.ratio = .8
          )

  p
}
