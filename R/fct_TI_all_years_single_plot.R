#' TI_all_years_single_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_TI_all_years_single_plot <- function(data) {
  data_summarized <- data %>%
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    dplyr::filter( transport == 1) %>% #remove in-river fish for transport:barge ratio plot
    dplyr::mutate(transport = as.factor(transport),
           rear_type = as.factor(rear_type),
           covariate = as.factor(covariate),
           species = as.factor(species)
    )

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  p <-
    ggplot2::ggplot(data_summarized, ggplot2::aes(x= x_var, y= TI)) +
    ggplot2::geom_point(ggplot2::aes(color = transport))+
    ggplot2::geom_line(ggplot2::aes(group = year, color = transport))+
    ggplot2::stat_summary(geom = "line", ggplot2::aes(group = year), alpha =.25) +
    ggplot2::labs(x = covar_label,
         y = "Transport to Bypass Ratio\n(T:B)",
         color = NULL,
         title = "Predicted Transport to Bypass Ratio (T:B)",
         subtitle = "Comparison of selected years") +
    ggplot2::geom_hline(yintercept = 1, color = "black" ) +
    ggrepel::geom_text_repel(data = . %>% dplyr::group_by(covariate, species, rear_type, year) %>% dplyr::filter(TI == max(TI)),
                             ggplot2::aes( label = year),
                             nudge_y = 0.1  # Adjust the nudge in the y-direction
    )+
    ggplot2::scale_color_manual(values =  c("black"),
                       labels = "Predicted median\nper year")+
    ggplot2::theme_light() +
    ggplot2::facet_grid(rear_type ~ species, scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
                   strip.text = ggplot2::element_text(colour = 'black'))
  p
}
