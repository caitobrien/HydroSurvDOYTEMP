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
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    dplyr::mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species)
    ) %>%
    dplyr::group_by(covariate, x_var, species, rear_type, transport, doy) %>%
    dplyr::summarise(SAR.median = stats::median(SAR, na.rm = TRUE),
              sd.SAR = stats::sd(SAR, na.rm = TRUE),
              n.SAR = dplyr::n(),
              sar.pit.median = stats::median(sar.pit, na.rm = TRUE),
              sd.sar.pit = stats::sd(SAR, na.rm = TRUE),
              n.sar.pit = dplyr::n()) %>%
    dplyr::mutate(se.SAR = sd.SAR / sqrt(n.SAR),
           SAR.lower = SAR.median - stats::qnorm(0.975) * se.SAR,
           SAR.upper = SAR.median + stats::qnorm(0.975) * se.SAR,
           se.sar.pit = sd.sar.pit / sqrt(n.sar.pit),
           sar.pit.lower = sar.pit.median - stats::qnorm(0.975) * se.sar.pit,
           sar.pit.upper = sar.pit.median + stats::qnorm(0.975) * se.sar.pit)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <- ggplot2::ggplot(data_summarized, ggplot2::aes(x = x_var, color = transport)) +
    ggplot2::labs(
      x = covar_label,
      y = "Smolt-to-Adult Ratio\n(SAR)",
      shape = "Observed data",
      color = "Predicted SAR",
      fill = "Predicted SAR",
      title = NULL
    ) +
    ggplot2::geom_point(ggplot2::aes(y = SAR.median, fill = transport)) +
    ggplot2::geom_line(ggplot2::aes(y = SAR.median)) +
    ggplot2::geom_ribbon(ggplot2::aes(y = SAR.median, ymin = SAR.lower, ymax = SAR.upper, fill = transport), alpha = .25) +
    ggdist::geom_pointinterval(ggplot2::aes(
      y = ifelse(n.sar.pit > 7, sar.pit.median, NA),
      ymin = sar.pit.lower,
      ymax = sar.pit.upper,
      shape = transport,
      color = transport),
      alpha = .25
    ) +
    ggplot2::scale_color_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \nmedian with 95% CI", "Transported, \nmedian with 95% CI")
    ) +
    ggplot2::scale_fill_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \nmedian with 95% CI", "Transported, \nmedian with 95% CI")
    ) +
    ggplot2::scale_shape_manual(
      values = c(21, 21),
      breaks = c("0", "1"),
      labels = c("In-river,\nmedian per year", "Transported,\nmedian per year")
    ) +
    ggplot2::theme_light() +
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgrey")) +
    ggplot2::theme(strip.text = ggplot2::element_text(colour = "black"))

  p
}
