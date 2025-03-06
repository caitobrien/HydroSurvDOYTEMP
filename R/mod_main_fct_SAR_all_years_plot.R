#' SAR_all_years_plot
#'
#' @description A function to plot SAR with all years combined.-- plot called within submodule mod_main_submodule_select_SAR_plot.R
#'
#' @return The return value is a plot with median SAR and 95 CI for all years of data per species, rear type and covariate (DOY, TEMP)--included on first tab within mod_main_page.R,
#'
#' @noRd


fct_SAR_all_years_plot <- function(data, observed_data, selected_covariate, observed = "no") {

  if (observed == "no") {

  if (selected_covariate == "Day-of-year (DOY)") {
    data_median<- data %>%
      dplyr::group_by(covariate, species, rear_type, transport, doy) %>%
      ggdist::median_qi(SAR, na.rm=TRUE)

    x_var <- data_median$doy
    covar_label <- "Day-of-year (DOY)"

  } else if (selected_covariate == "Temperature (°C)") {
    data_median<- data %>%
      dplyr::group_by(covariate, species, rear_type, transport, mean.temp) %>%
      ggdist::median_qi(SAR, na.rm=TRUE)

    x_var <- data_median$mean.temp
    covar_label <- "Temperature (°C)"
  }


  # plot
  p <- ggplot2::ggplot(data_median, ggplot2::aes(x = x_var, color = transport)) +
    ggplot2::geom_point(ggplot2::aes(y = SAR, fill = transport)) +
    tidybayes::geom_lineribbon(ggplot2::aes(y = SAR, ymin = .lower, ymax = .upper, fill = transport),
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
    ggplot2::labs(
      x = covar_label,
      y = "Smolt-to-Adult Ratio\n(SAR)",
      color = "Predicted SAR",
      fill = "Predicted SAR",
      title = NULL
    ) +
    ggplot2::theme_light() +
    ggplot2::facet_grid(rear_type ~ species, scales = "free") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgrey"),
                   strip.text = ggplot2::element_text(colour = "black"),
                   panel.grid.minor = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 15))

  } else if (observed == "yes") {

    if (selected_covariate == "Day-of-year (DOY)") {
      #predicted data
      data_median<- data %>%
        dplyr::group_by(covariate, species, rear_type, transport, doy) %>%
        ggdist::median_qi(SAR, na.rm=TRUE)

      x_var <- data_median$doy
      covar_label <- "Day-of-year (DOY)"

      #observed data
      wrangled_observed_data <- observed_data %>%
        dplyr::group_by(species, rear_type, transport, doy) %>%
        dplyr::summarise(
          n.sar.pit = sum(n, na.rm = TRUE),  # Sum 'n' per year
          med.sar.pit = median(sar.pit, na.rm = TRUE),  # Median SAR.pit
          sar.pit.lo = quantile(sar.pit, probs = 0.025, na.rm = TRUE),  # Lower bound (2.5th percentile)
          sar.pit.hi = quantile(sar.pit, probs = 0.975, na.rm = TRUE)   # Upper bound (97.5th percentile)
        ) %>%
        dplyr::ungroup()

      x_var_obs<- wrangled_observed_data$doy

      x_breaks <- seq(90, 160, by = 10)

    } else if (selected_covariate == "Temperature (°C)") {

      #prediction data
      data_median<- data %>%
        dplyr::group_by(covariate, species, rear_type, transport, mean.temp) %>%
        ggdist::median_qi(SAR, na.rm=TRUE)

      x_var <- data_median$mean.temp
      covar_label <- "Temperature (°C)"

      #observed data
      wrangled_observed_data <- observed_data %>%
        dplyr::mutate(rounded.mean.temp = round(mean.temp, 0)) %>%
        dplyr::group_by(species, rear_type, transport, rounded.mean.temp) %>%
        dplyr::summarise(
          n.sar.pit = sum(n, na.rm = TRUE),  # Sum 'n' per year
          med.sar.pit = median(sar.pit, na.rm = TRUE),  # Median SAR.pit
          sar.pit.lo = quantile(sar.pit, probs = 0.025, na.rm = TRUE),  # Lower bound (2.5th percentile)
          sar.pit.hi = quantile(sar.pit, probs = 0.975, na.rm = TRUE)   # Upper bound (97.5th percentile)
        ) %>%
        dplyr::ungroup()

      x_var_obs<- wrangled_observed_data$rounded.mean.temp

      x_breaks <- seq(6, 18, by = 2)

    }


    # plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = data_median, ggplot2::aes(x = x_var, y = SAR, fill = transport, color = transport)) +
      tidybayes::geom_lineribbon(data = data_median, ggplot2::aes(x = x_var, y = SAR, ymin = .lower, ymax = .upper, fill = transport, color = transport),
                                 alpha = .25) +
      ggdist::geom_pointinterval( data = wrangled_observed_data, ggplot2::aes(
        y = ifelse(n.sar.pit > 7, med.sar.pit, NA),
        x = x_var_obs,
        ymin = sar.pit.lo,
        ymax = sar.pit.hi,
        shape = transport,
        color = transport
       ),
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
        labels = c("In-river,\nmedian per year", "Transported,\nmedian per year")) +
      ggplot2::labs(
        x = covar_label,
        y = "Smolt-to-Adult Ratio\n(SAR)",
        color = "Predicted SAR",
        fill = "Predicted SAR",
        title = NULL,
        shape = "Observed data"
      ) +
      ggplot2::scale_x_continuous(breaks = x_breaks) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(color = c("steelblue4", "#b47747")))) + # change predator legend to squares
      ggplot2::theme_light() +
      ggplot2::facet_grid(rear_type ~ species, scales = "free") +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgrey"),
                     strip.text = ggplot2::element_text(colour = "black"),
                     panel.grid.minor = ggplot2::element_blank(),
                     text = ggplot2::element_text(size = 15))

  }
  return(p)

}





# filtered_data<- as.data.frame(all$doy[1])
# # observed_data<- as.data.frame(all$observed[1])
# # #"Day-of-year (DOY)"
# # #
# fct_SAR_all_years_plot(data = filtered_data, observed_data = observed_data, selected_covariate = "Day-of-year (DOY)" , observed = "yes")
