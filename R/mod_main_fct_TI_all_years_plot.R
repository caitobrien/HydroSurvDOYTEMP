#' TI_all_years_plot
#'
#' @description A function to plot SAR with all years combined--plot called within submodule mod_main_submodule_select_TI_plot.R
#'
#' @return The return value is a plot with median TI (and 95 CI ?) for all years of data per species, rear type and covariate (DOY, TEMP)--included on first tab within mod_main_page.R
#'
#' @noRd
#'
fct_TI_all_years_plot <- function(data, selected_covariate) {

  if (selected_covariate == "Day-of-year (DOY)") {
    data_median<- data %>%
      dplyr::group_by(covariate, species, rear_type, doy) %>%
      ggdist::median_qi(ti, na.rm=TRUE)

    x_var <- data_median$doy
    covar_label <- "Day-of-year (DOY)"

    x_breaks <- seq(90, 160, by = 10)

  } else if (selected_covariate == "Temperature (°C)") {
    data_median<- data %>%
      dplyr::group_by(covariate, species, rear_type, mean.temp) %>%
      ggdist::median_qi(ti, na.rm=TRUE)

    x_var <- data_median$mean.temp
    covar_label <- "Temperature (°C)"

    x_breaks <- seq(6, 18, by = 2)
  }


  # plot
  p <-
    ggplot2::ggplot(data_median, ggplot2::aes(x = x_var, y = ti)) +
    tidybayes::geom_lineribbon(ggplot2::aes(y = ti, ymin = .lower, ymax = .upper, fill = "Predicted median,\nwith 95% CI"), alpha = .1) +
    ggplot2::geom_point(ggplot2::aes(color = "Predicted median,\nwith 95% CI")) +
    ggplot2::labs(
      x = covar_label,
      y = "Transport to Bypass Ratio\n(T:B)",
      title = NULL
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "black" ) +
    ggplot2::scale_color_manual(name = NULL, values = c("Predicted median,\nwith 95% CI" = "black")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("Predicted median,\nwith 95% CI" = "black")) +
    ggplot2::theme_light()+
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
                   strip.text = ggplot2::element_text(colour = 'black'),
                   text = ggplot2::element_text(size = 15))

  p
}
