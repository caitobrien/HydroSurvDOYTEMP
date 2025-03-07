#' TI_by_years_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return the return value is a plot with median TI and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd
fct_TI_by_years_plot <- function(data, selected_covariate) {


  if (selected_covariate == "Day-of-year (DOY)") {
    data_summarized<- data %>%
      dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
      dplyr::group_by(year)

    x_var <- data_summarized$doy
    covar_label <- "Day-of-year (DOY)"

    x_breaks <- seq(90, 160, by = 10)

  } else if (selected_covariate == "Temperature (°C)") {
    data_summarized<- data %>%
      dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
      dplyr::group_by(year)

    x_var <- data_summarized$mean.temp
    covar_label <- "Temperature (°C)"

    x_breaks <- seq(6, 18, by = 2)
  }

  # plot
  p <-
    ggplot2::ggplot(data_summarized, ggplot2::aes(x = x_var, y = ti, group = year)) +
    ggdist::geom_lineribbon(ggplot2::aes(y = ti, ymin = ti_lower90, ymax = ti_upper90, fill = "Predicted median,\nwith 95% CI"), alpha = 0.1) +
    ggplot2::geom_point(ggplot2::aes(fill = "Predicted median,\nwith 95% CI"))+
    ggplot2::labs(
      x = covar_label,
      y = "Transport to Bypass Ratio\n(T:B)",
      title = NULL
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "black" ) +
    ggplot2::scale_fill_manual(name = NULL, values = c("Predicted median,\nwith 95% CI" = "black")) +  # Fill legend
    ggplot2::scale_color_manual(name = NULL, values = c("Predicted median,\nwith 95% CI" = "black")) +  # Fill legend
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::theme_light()+
    ggplot2::facet_wrap(~year + species_rear, scales = "free_y", ncol = 4) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
          strip.text = ggplot2::element_text(colour = 'black'),
          panel.spacing = ggplot2::unit(2, "lines"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(size = 15)
          )

  p
}


# df<-as.data.frame(all$doy_ti[1])
# fct_TI_by_years_plot(df, "Day-of-year (DOY)")
