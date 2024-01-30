#' TI_all_years_plot
#'
#' @description A function to plot SAR with all years combined.
#'
#' @return The return value is a plot with median TI (and 95 CI ?) for all years of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd
#'
fct_TI_all_years_plot <- function(data) {

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
      species = as.factor(species)
    ) %>%
    group_by(covariate, x_var, species, rear_type,doy) %>%
    tidybayes::median_qi(TI, na.rm = TRUE)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <- ggplot(data_summarized, aes(x = x_var, y = TI)) +
    geom_point()+
    #geom_line()+
    geom_ribbon(aes(y = TI, ymin = .lower, ymax = .upper), alpha = .25) +
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
    facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background =element_rect(fill="lightgrey"))+
    theme(strip.text = element_text(colour = 'black'))

  p
}
