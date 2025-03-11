#'@title TI_by_years_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return the return value is a plot with median TI and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#' @param data unlisted Data frame containing the TI data
#' @param selected_covariate user-selected covariate for the plot (either "Day-of-year (DOY)" or "Temperature (°C)")
#'
#' @noRd
fct_TI_by_years_plot <- function(data, selected_covariate) {

  # Define the adu return year threshold dynamically
  last_outmigration_year <- 2024
  first_return_year <- last_outmigration_year - 3

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
    ggdist::geom_lineribbon(ggplot2::aes(y = ti, ymin = ti_lower95, ymax = ti_upper95, fill = "Predicted median,\nwith 95% CI"), alpha = 0.1) +
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

  #add text for years without all adult returns
  if (any(as.numeric(as.character(data_summarized$year)) >= first_return_year)) {
    p <- p +
      ggplot2::geom_text(data = data_summarized %>% dplyr::filter(as.numeric(as.character(year)) >= first_return_year),
                         ggplot2::aes(x = Inf, y = Inf, label = "*Out-of-sample prediction until all adults return"),
                         hjust = 1.1, vjust = 2, size = 3, color = "red", inherit.aes = FALSE)
  }
  return(p)
}


# df<-as.data.frame(all$doy_ti[1])
# fct_TI_by_years_plot(df, "Day-of-year (DOY)")
