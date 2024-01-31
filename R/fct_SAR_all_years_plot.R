#' SAR_all_years_plot
#'
#' @description A function to plot SAR with all years combined.
#'
#' @return The return value is a plot with median SAR and 95 CI for all years of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd



fct_SAR_all_years_plot <- function(data) {

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
    group_by(covariate, x_var, species, rear_type, transport, doy) %>%
    summarise(SAR.median = median(SAR, na.rm = TRUE),
              sd.SAR = sd(SAR, na.rm = TRUE),
              n.SAR = n(),
              sar.pit.median = median(sar.pit, na.rm = TRUE),
              sd.sar.pit = sd(SAR, na.rm = TRUE),
              n.sar.pit = n()) %>%
    mutate(se.SAR = sd.SAR / sqrt(n.SAR),
           SAR.lower = SAR.median - qnorm(0.975) * se.SAR,
           SAR.upper = SAR.median + qnorm(0.975) * se.SAR,
           se.sar.pit = sd.sar.pit / sqrt(n.sar.pit),
           sar.pit.lower = sar.pit.median - qnorm(0.975) * se.sar.pit,
           sar.pit.upper = sar.pit.median + qnorm(0.975) * se.sar.pit)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <- ggplot(data_summarized, aes(x = x_var, color = transport)) +
    labs(
      x = covar_label,
      y = "Smolt-to-Adult Ratio\n(SAR)",
      shape = "Observed data",
      color = "Predicted SAR",
      fill = "Predicted SAR",
      title = NULL
    ) +
    geom_point(aes(y = SAR.median, fill = transport)) +
    geom_line(aes(y = SAR.median)) +
    geom_ribbon(aes(y = SAR.median, ymin = SAR.lower, ymax = SAR.upper, fill = transport), alpha = .25) +
    ggdist::geom_pointinterval(aes(
      y = ifelse(n.sar.pit > 7, sar.pit.median, NA),
      ymin = sar.pit.lower,
      ymax = sar.pit.upper,
      shape = transport,
      color = transport),
      alpha = .25
    ) +
    scale_color_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \nmedian with 95% CI", "Transported, \nmedian with 95% CI")
    ) +
    scale_fill_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \nmedian with 95% CI", "Transported, \nmedian with 95% CI")
    ) +
    scale_shape_manual(
      values = c(21, 21),
      breaks = c("0", "1"),
      labels = c("In-river,\nmedian per year", "Transported,\nmedian per year")
    ) +
    theme_light() +
    facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(strip.text = element_text(colour = "black"))

  p
}
