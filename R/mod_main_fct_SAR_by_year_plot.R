#' SAR_by_year_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return The return value is a plot with median SAR and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#'
#' @noRd

fct_SAR_by_year_plot<-function(data, observed_data, selected_years, selected_covariate, observed = "no"){



  #wrangle data to plot median per year per grouping
  data_summarized<- data %>%
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ mean.temp#temp
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
    # ggplot2::geom_line(ggplot2::aes(y = SAR)) +
    # ggplot2::geom_ribbon( ggplot2::aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, color = transport), alpha = .25)+
    tidybayes::geom_lineribbon(ggplot2::aes(y = SAR, ymin = SAR.lo, ymax = SAR.hi, fill = transport, color = transport),
                               alpha = .25
    ) +
    ggplot2::labs( x = covar_label,
          y = "Smolt-to-Adult Ratio\n(SAR)",
          size = "Number of fish observed",
          color = "Predicted SAR",
          fill = "Predicted SAR",
          title = NULL
    ) +
    ggplot2::scale_color_manual(breaks = c("0", "1"),
                       values = c("steelblue4", "#b47747"),
                       labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    ggplot2::scale_fill_manual(breaks = c("0", "1"),
                      values = c("steelblue4", "#b47747"),
                      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    ggplot2::scale_linetype_manual(values = c("solid","dashed"),
                          breaks = c("0", "1"),
                          labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(color = c("steelblue4", "#b47747") ),
                                order = 1),
           size = ggplot2::guide_legend(override.aes = list(
             label = list(size = 8),
             shape = 21),
             order = 2),
           color = ggplot2::guide_legend(order = 3),
           fill = ggplot2::guide_legend(order = 3)) +
    ggplot2::theme_light()+
    ggplot2::facet_wrap(~year + species_rear, scales = "free_y", ncol = 4) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightgrey"),
          strip.text = ggplot2::element_text(colour = 'black'),
          panel.spacing = ggplot2::unit(2, "lines"),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(size = 15))


  #functionality to include obs data or not in plot
  if (observed == "yes") {
    if(selected_covariate == "Day-of-year (DOY)"){
      observed_data<- observed_data %>% mutate(covariate = "Day-of-year (DOY)")
    } else if(selected_covariate == "Temperature (°C)"){
      observed_data<- observed_data %>% mutate(covariate = "Temperature (°C)")
    }

    wrangled_observed_data<-observed_data %>%
      dplyr::mutate(x_var = dplyr::case_when(
        covariate == "Day-of-year (DOY)" ~ doy,
        TRUE ~ mean.temp#temp
      )) %>%
      dplyr::mutate(
        transport = as.factor(transport),
        year = as.factor(year),
        rear_type = as.factor(rear_type),
        covariate = as.factor(covariate),
        species = as.factor(species),
        species_rear = interaction(species, rear_type)) %>%
      dplyr::group_by(year)

    p.obs <- p +  ggplot2::geom_point(data = wrangled_observed_data,
                                      ggplot2::aes(y =sar.pit,
                                                   size = n,
                                                   shape =  transport,
                                                   color = transport),
                                      alpha = .7)+
      ggplot2::labs(shape = "Observed data") +
      ggplot2::scale_shape_manual(values = c(21,21),
                                  breaks = c("0", "1"),
                                  labels = c("In-river", "Transported")) +
      ggplot2::scale_size_continuous(range = c(1, 5),
                                     breaks = c(1, pretty(c(1, max(stats::na.omit(wrangled_observed_data$n)), n = 3))))


    return(p.obs)

  } else {
    return(p)
  }

}


# #example
# set_species<-"Chinook"
# set_rear_type<- "Natural-origin"
# set_covariate<- "Temperature (°C)" #"Day-of-year (DOY)"
# filtered_data<-df_mod_predict %>%
#   select(doy, temp, transport, year, SAR, SAR.lo, SAR.hi, rear_type, covariate, species) %>%
#   filter(species == set_species, rear_type == set_rear_type, covariate == set_covariate) %>%
#   rename(mean.temp = temp) %>%
#   filter(between(year, 2010, 2018))
#
# selected_covariate<-"Temperature (°C)" #"Day-of-year (DOY)"
# filtered_observed_data<-df_observed %>%
#   filter(species == set_species, rear_type == set_rear_type) %>%
#   filter(between(year, 2010, 2018))
#
# covar
# fct_SAR_by_year_plot(data = filtered_data, observed_data = filtered_observed_data, selected_covariate = "Temperature (°C)", selected_years = c(2010:2018), observed = "yes")
