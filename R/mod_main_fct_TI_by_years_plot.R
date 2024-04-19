#' TI_by_years_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return the return value is a plot with median TI and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd
fct_TI_by_years_plot <- function(data) {

  # wrangle data to plot median per year per grouping
  data_summarized <- data %>%
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    dplyr::filter( transport == 1) %>% #remove in-river fish for transport:barge ratio
    dplyr::mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species),
      species_rear = interaction(species, rear_type)
    ) %>%
    dplyr::group_by(year)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <-
    ggplot2::ggplot(data_summarized, ggplot2::aes(x = x_var, y = TI)) +
    ggplot2::geom_point(ggplot2::aes(color = transport))+
    # ggplot2::geom_line(ggplot2::aes(group = year, color = transport))+
    tidybayes::geom_lineribbon(ggplot2::aes(y = TI, ymin = TI.lo, ymax = TI.hi, color = transport),
                               alpha = .25,
                               fill = "darkgrey"
    ) +
    ggplot2::labs(
      x = covar_label,
      y = "Transport to Bypass Ratio\n(T:B)",
      title = NULL,
      color = NULL
    ) +
    ggplot2::geom_hline(yintercept = 1, color = "black" ) +
    ggplot2::scale_color_manual(values =  c("black"),
                       labels = "Predicted median\nper year")+
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
