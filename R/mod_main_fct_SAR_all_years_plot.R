#' SAR_all_years_plot
#'
#' @description A function to plot SAR with all years combined.-- plot called within submodule mod_main_submodule_select_SAR_plot.R
#'
#' @return The return value is a plot with median SAR and 95 CI for all years of data per species, rear type and covariate (DOY, TEMP)--included on first tab within mod_main_page.R,
#'
#' @noRd



fct_SAR_all_years_plot <- function(data) {

  # wrangle data to plot median per year per grouping
  data_n.obs<- data %>%
    # dplyr::mutate(x_var = dplyr::case_when(
    #   covariate == "Day-of-year (DOY)" ~ doy,
    #   TRUE ~ temp
    # )) %>%
    dplyr::mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species)
    ) %>%
    dplyr::group_by(covariate, species, rear_type, transport, doy) %>%
    dplyr::summarise(n.sar.pit = sum(n.obs, na.rm = TRUE))

  data_median<- data %>%
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
    ggdist::median_qi(SAR, sar.pit, na.rm=TRUE)


  data_summarized <- data_median %>%
    left_join(data_n.obs, by = c("covariate", "species", "rear_type", "transport", "doy"))

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
    ggplot2::geom_point(ggplot2::aes(y = SAR, fill = transport)) +
    ggplot2::geom_line(ggplot2::aes(y = SAR)) +
    ggplot2::geom_ribbon(ggplot2::aes(y = SAR, ymin = SAR.lower, ymax = SAR.upper, fill = transport), alpha = .25) +
    ggdist::geom_pointinterval(ggplot2::aes(
      y = sar.pit, #ifelse(n.sar.pit > 7, sar.pit, NA),
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
    ggplot2::facet_grid(rear_type ~ species, scales = "free") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgrey"),
                   strip.text = ggplot2::element_text(colour = "black"),
                   panel.grid.minor = ggplot2::element_blank())

  p
}
