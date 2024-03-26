#' SAR_by_year_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return The return value is a plot with median SAR and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd

fct_SAR_by_year_plot<-function(data, selected_years){


  #wrangle data to plot median per year per grouping
  data_summarized<- data %>%
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ temp
    )) %>%
    dplyr::mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species),
      species_rear = interaction(species, rear_type)) %>%
    dplyr::group_by(year)

  # Convert data_summarized to data frame
  data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <-  unique(data_summarized$covariate)



  p <-
    ggplot2::ggplot(data_summarized, ggplot2::aes( x= x_var)) +
    ggplot2::geom_point(ggplot2::aes(y =SAR, fill =  transport, color = transport))+
    ggplot2::geom_ribbon( ggplot2::aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, color = transport), alpha = .25)+
    ggplot2::geom_point(ggplot2::aes(y =sar.pit, size = n.obs, shape =  transport, color = transport), alpha = .7)+
    ggplot2::labs( x = covar_label,
          y = "Smolt-to-Adult Ratio\n(SAR)",
          shape = "Observed data",
          size = "Number of fish observed",
          color = "Predicted SAR",
          fill = "Predicted SAR",
          # linetype = "Combined years",
          title = NULL
    ) +
    ggplot2::scale_color_manual(breaks = c("0", "1"),
                       values = c("steelblue4", "#b47747"),
                       labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    ggplot2::scale_fill_manual(breaks = c("0", "1"),
                      values = c("steelblue4", "#b47747"),
                      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    ggplot2::scale_shape_manual(values = c(21,21),
                       breaks = c("0", "1"),
                       labels = c("In-river", "Transported")) +
    ggplot2::scale_linetype_manual(values = c("solid","dashed"),
                          breaks = c("0", "1"),
                          labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
    ggplot2::scale_size_continuous(range = c(1, 5),
                          breaks = c(1, pretty(c(1, max(stats::na.omit(data_summarized$n.obs)), n = 3)))) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(color = c("steelblue4", "#b47747") ),
                                order = 1),
           size = ggplot2::guide_legend(override.aes = list(
             label = list(size = 8),
             shape = 21),
             order = 2),
           color = ggplot2::guide_legend(order = 3),
           fill = ggplot2::guide_legend(order = 3)) +
    # linetype = guide_legend(order = 4)) +
    ggplot2::theme_light()+
    ggplot2::facet_wrap(~year + species_rear, scales = "free_y", ncol = 4) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
          strip.text = ggplot2::element_text(colour = 'black'),
          panel.spacing = ggplot2::unit(2, "lines"),
          panel.grid.minor = ggplot2::element_blank())

p

}

