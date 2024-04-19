#' SAR_all_years_single_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


fct_compare_SAR_TI_plot <- function(data) {
  # wrangle data to plot median per year per year
  data_summarized_sar <- data %>%
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
    dplyr::group_by(year)

  # Convert data_summarized to data frame
  data_summarized_sar <- as.data.frame(data_summarized_sar)

  # Extract unique covariate name
  covar_label_sar <- unique(data_summarized_sar$covariate)


  ## data for TI
  data_summarized_TI <- data %>%
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    dplyr::filter(transport == 1) %>% # remove in-river fish for transport:barge ratio plot
    dplyr::mutate(
      transport = as.factor(transport),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species)
    )

  # Convert data_summarized to data frame
  data_summarized_TI <- as.data.frame(data_summarized_TI)

  # Extract unique covariate name
  covar_label_TI <- unique(data_summarized_TI$covariate)



  # sar plot
  SAR_plot <- ggplot2::ggplot(data_summarized_sar, ggplot2::aes(x = x_var, group = year)) +
    ggplot2::geom_point(ggplot2::aes(y = SAR, fill = transport, color = transport)) +
    tidybayes::geom_lineribbon(ggplot2::aes(y = SAR, ymin = SAR.lo, ymax = SAR.hi, fill = transport, color = transport),
      alpha = .25
    ) +
    ggplot2::geom_point(ggplot2::aes(y = sar.pit, size = n.obs, shape = transport, color = transport),
      alpha = .7
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Smolt-to-Adult Ratio\n(SAR)",
      shape = "Observed data",
      size = "Number of fish,\nobserved data",
      color = "Predicted SAR",
      fill = "Predicted SAR",
      title = NULL
    ) +
    ggplot2::scale_color_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI")
    ) +
    ggplot2::scale_fill_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI")
    ) +
    ggplot2::scale_shape_manual(
      values = c(21, 21),
      breaks = c("0", "1"),
      labels = c("In-river", "Transported")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("solid", "dashed"),
      breaks = c("0", "1"),
      labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability")
    ) +
    ggplot2::scale_size_continuous(
      range = c(1, 5),
      breaks = c(1, pretty(c(1, max(stats::na.omit(data_summarized_sar$n.obs)), n = 3)))
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        override.aes = list(color = c("steelblue4", "#b47747")),
        order = 1
      ),
      size = ggplot2::guide_legend(
        override.aes = list(
          label = list(size = 8),
          shape = 21
        ),
        order = 2
      ),
      color = ggplot2::guide_legend(order = 3),
      fill = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::theme_light() +
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(colour = "black"),
      panel.spacing = ggplot2::unit(2, "lines"),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 15)
    )

  # set title for plot being showed
  SAR_plot <- SAR_plot + ggplot2::ggtitle(paste0("SAR & T:B, year of interest: ", data_summarized_sar$year)) +
                         ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))



  # TI plot
  TI_plot <- ggplot2::ggplot(data_summarized_TI, ggplot2::aes(x = x_var, y = TI)) +
    ggplot2::geom_point(ggplot2::aes(color = transport)) +
    ggplot2::geom_line(ggplot2::aes(group = year, color = transport)) +
    ggplot2::stat_summary(geom = "line", ggplot2::aes(group = year), alpha = .25) +
    tidybayes::geom_lineribbon(ggplot2::aes(y = TI, ymin = TI.lo, ymax = TI.hi),
                               alpha = .25,
                               fill = "darkgrey"
    ) +
    ggplot2::labs(
      x = covar_label_TI,
      y = "Transport to Bypass Ratio\n(T:B)",
      color = NULL,
      title = NULL
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "black") +
    ggplot2::scale_color_manual(
      values = c("black"),
      labels = "Predicted median\nper year"
    ) +
    ggplot2::theme_light() +
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "lightgrey"),
      strip.text = ggplot2::element_text(colour = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 15)
    )


  # arrange plots with patchwork
  combine_plot <- (
    (SAR_plot / TI_plot) +
      patchwork::plot_layout(guides = "keep")
  )

  # display the combined plot
  combine_plot
}
