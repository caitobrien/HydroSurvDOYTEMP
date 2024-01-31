#' TI_all_years_single_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_TI_all_years_single_plot <- function(data) {
  data_summarized <- data %>%
    mutate(x_var = case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    filter( transport == 1) %>% #remove in-river fish for transport:barge ratio plot
    mutate(transport = as.factor(transport),
           rear_type = as.factor(rear_type),
           covariate = as.factor(covariate),
           species = as.factor(species)
    )

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  p <- ggplot(data_summarized, aes(x= x_var, y= TI)) +
    geom_point(aes(color = transport))+
    geom_line(aes(group = year, color = transport))+
    stat_summary(geom = "line", aes(group = year), alpha =.25) +
    # stat_summary(data = data.pred, aes(y= TI, x = doy),
    #              fun = median,
    #              geom = "line",
    #              color = "#545454",
    #              linetype = "dashed")+
    labs(x = covar_label,
         y = "Transport to Bypass Ratio\n(T:B)",
         color = NULL,
         title = "Predicted Transport to Bypass Ratio (T:B)",
         subtitle = "Comparison of selected years") +
    geom_hline(yintercept = 1, color = "black" ) +
    ggrepel::geom_text_repel(data = . %>% group_by(covariate, species, rear_type, year) %>% filter(TI == max(TI)),
                             aes( label = year),
                             nudge_y = 0.1  # Adjust the nudge in the y-direction
    )+
    scale_color_manual(values =  c("black"),
                       labels = "Predicted median\nper year")+
    theme_light()+ facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background =element_rect(fill="lightgrey"))+
    theme(strip.text = element_text(colour = 'black'))
  p
}
