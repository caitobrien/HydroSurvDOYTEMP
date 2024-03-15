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
    dplyr::group_by(covariate, x_var, species, rear_type,doy) %>%
    tidybayes::median_qi(TI, na.rm = TRUE)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <-
    ggplot2::ggplot(data_summarized, ggplot2::aes(x = x_var, y = TI)) +
    ggplot2::geom_point(ggplot2::aes(color=.point))+
    ggplot2::geom_line(ggplot2::aes(color = .point))+
    tidybayes::geom_lineribbon(ggplot2::aes(y = TI, ymin = .lower, ymax = .upper, fill=.point), alpha = .25) +
    ggplot2::labs(
      x = covar_label,
      y = "Transport to Bypass Ratio\n(T:B)",
      title = NULL,
      color = NULL,
      fill= NULL
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "black" ) +
    ggplot2::scale_color_manual(values =  "black",
                       labels = "Predicted median,\nwith 95% CI")+
    ggplot2::scale_fill_manual(values =  "black",
                      labels = "Predicted median,\nwith 95% CI")+
    ggplot2::theme_light()+
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
                   strip.text = ggplot2::element_text(colour = 'black'))

  p
}
