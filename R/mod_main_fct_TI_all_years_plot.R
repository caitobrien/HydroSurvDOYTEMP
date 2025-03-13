#' TI_all_years_plot
#'
#' @description A function to plot SAR with all years combined--plot called within submodule mod_main_submodule_select_TI_plot.R
#'
#' @return The return value is a plot with median TI (and 95 CI ?) for all years of data per species, rear type and covariate (DOY, TEMP)--included on first tab within mod_main_page.R
#'
#' @noRd
#'
fct_TI_all_years_plot <- function(data, selected_covariate, credible_interval) {
  # Remove outmigration year data that does not include all adult returns (i.e, 3 years since last outmigration year have not passed)
  last_outmigration_year<-data %>%
    dplyr::mutate(year = as.numeric(as.character(year))) %>%
    dplyr::pull(year) %>%
    max()

  current_year<-lubridate::year(Sys.Date())
  current_doy <- lubridate::yday(Sys.Date())


  adjusted_complete_adult_returns<-if (current_year > last_outmigration_year && current_doy < 160) {
    adjusted_years<- current_year-3
    adjusted_complete_adult_returns<- c(adjusted_years:last_outmigration_year)
  } else if (current_year > last_outmigration_year && current_doy > 160){
    adjusted_years<- last_outmigration_year-3
    adjusted_complete_adult_returns<- c(adjusted_years:current_year)
  }

  data <- dplyr::filter(data, !year %in% adjusted_complete_adult_returns) %>%
    dplyr::mutate(year = as.factor(year))


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

    x_breaks <- seq(6, 15, by = 1)
  }


  # plot
  p <-
    ggplot2::ggplot(data_median, ggplot2::aes(x = x_var, y = ti)) +
    ggplot2::geom_line(aes(color = "Predicted median")) +
    # tidybayes::geom_lineribbon(ggplot2::aes(y = ti, ymin = .lower, ymax = .upper, fill = "Predicted median,\nwith 95% CI"), alpha = .1) +
    ggplot2::geom_point(ggplot2::aes(color = "Predicted median")) +
    ggplot2::labs(
      x = covar_label,
      y = "Transport to Bypass Ratio\n(T:B)",
      title = NULL,
      caption = paste("\nData excludes years with incomplete adult returns.")
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "black" ) +
    ggplot2::scale_color_manual(name = "T:B ratio:", values = c("Predicted median" = "black")) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::theme_light()+
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
                   strip.text = ggplot2::element_text(colour = 'black'),
                   text = ggplot2::element_text(size = 15))

  # Add credible interval ribbon if set to TRUE
  if (credible_interval) {
    p <- p + ggplot2::geom_ribbon(data = data_median, ggplot2::aes( y = ti, ymin = .lower, ymax = .upper, fill = "95% CI"), alpha = 0.1) +
      ggplot2::scale_fill_manual(name = NULL, values = c("95% CI" = "black")) +  # Fill legend
      ggplot2::guides(color = guide_legend(order = 1),
                     fill = guide_legend(order = 2))
  }

  return(p)

}
