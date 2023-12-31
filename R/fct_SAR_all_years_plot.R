#' SAR_all_years_plot
#'
#' @description A function to plot SAR with all years combined.
#'
#' @return The return value is a plot with median SAR and 95 CI for all years of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd



fct_SAR_all_years_plot <- function(data) {
  # Use isolate() to avoid reactivity
  # This prevents infinite loops
  # data_isolate <- isolate(data)


  # wrangle data to plot median per year per grouping
  data_summarized <- data %>%
    #filter(covariate == "Day-of-year (DOY)") %>%
    mutate(x_var = case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species)
    ) %>%
    group_by(covariate, x_var, species, rear_type, transport, doy) %>%
    summarise(SAR.median = median(SAR, na.rm = TRUE),
              sd.SAR = sd(SAR, na.rm = TRUE),
              n.SAR = n()) %>%
    mutate(se.SAR = sd.SAR / sqrt(n.SAR),
           SAR.lower = SAR.median - qnorm(0.975) * se.SAR,
           SAR.upper = SAR.median + qnorm(0.975) * se.SAR)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_summarized$covariate)

  # plot
  p <- ggplot(data_summarized, aes(x = x_var, color = transport)) +
    labs(
      x = covar_label,
      y = "Smolt-to-Adult Ratio\n(SAR)",
      shape = "Observed data",
      size = "Number of fish observed",
      color = "Predicted SAR",
      fill = "Predicted SAR",
      # linetype = "Combined years",
      title = NULL
    ) +
    geom_point(aes(y = SAR.median, fill = transport)) +
    geom_line(aes(y = SAR.median)) +
    geom_ribbon(aes(y = SAR.median, ymin = SAR.lower, ymax = SAR.upper, fill = transport), alpha = .25) +
    # geom_point(aes(
    #   y = ifelse(n.obs > 7, sar.pit, NA),
    #   shape = transport, size = n.obs
    # ), alpha = .25) +
    scale_color_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI")
    ) +
    scale_fill_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI")
    ) +
    # scale_shape_manual(
    #   values = c(21, 21),
    #   breaks = c("0", "1"),
    #   labels = c("In-river", "Transported")
    # ) +
    # scale_linetype_manual(values = c("solid","dashed"),
    #                       breaks = c("0", "1"),
    #                       labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
    # scale_size_continuous(range = c(.25, 1),
    #                       breaks = c(1,1000, 2000)) +
    # guides(shape = guide_legend(override.aes = list(color = c("steelblue4", "#b47747") ),
    #                             order = 1),
    #        size = guide_legend(override.aes = list(
    #          label = list(size = 8)),
    #          order = 2),
    #        color = guide_legend(order = 3),
    #        fill = guide_legend(order = 3)) +
    theme_light() +
    facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background = element_rect(fill = "lightgrey")) +
    theme(strip.text = element_text(colour = "black"))
    # theme(plot.margin = margin(1, 0, 0, 1.5, "cm"),
    #       panel.spacing = unit(2, "lines"))

  # convert to plotly object
  plotly::ggplotly(p)
}
