#'@title SAR_by_year_plot
#'
#' @description A function to plot SAR separated by years.
#'
#' @return The return value is a plot with median SAR and 95 CI for each year of data per species, rear type and covariate (DOY, TEMP)
#' @param data_pred unlisted Data frame containing the SAR data, reactive filtered object
#' @param observed_data unlisted Data frame containing the observed SAR data, reactive filtered object
#' @param selected_covariate user-selected covariate for the plot (either "Day-of-year (DOY)" or "Temperature (°C)")
#' @param observed manually set to "yes" for SAR tab plots in app and no for walkthrough tab exluding observed data.
#'
#'
#' @noRd

fct_SAR_by_year_plot<-function(data_pred, observed_data, selected_covariate, observed = "no", legend_location){

  # Define the adu return year threshold dynamically
  last_outmigration_year <- 2024
  first_return_year <- last_outmigration_year - 3

  if (observed == "no") {

    if (selected_covariate == "Day-of-year (DOY)") {
      data_summarized<- data_pred %>%
        dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
        dplyr::group_by(year)

      x_var <- data_summarized$doy
      covar_label <- "Day-of-year (DOY)"

      x_breaks <- seq(90, 160, by = 10)

    } else if (selected_covariate == "Temperature (°C)") {
      data_summarized<- data_pred %>%
        dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
        dplyr::group_by(year)

      x_var <- data_summarized$mean.temp
      covar_label <- "Temperature (°C)"

      x_breaks <- seq(6, 15, by = 1)
    }

    unique_transport <- unique(data_summarized$transport)


  p <-
    ggplot2::ggplot(data_summarized, ggplot2::aes( x= x_var)) +
    ggplot2::geom_point(ggplot2::aes(y =SAR, fill =  transport, color = transport))+
    tidybayes::geom_lineribbon(ggplot2::aes(y = SAR, ymin = SAR.lo, ymax = SAR.hi, fill = transport, color = transport),
                               alpha = .25
    ) +
    ggplot2::labs( x = covar_label,
          y = "Smolt-to-Adult Ratio\n(SAR)",
          size = "Number of fish observed:",
          color = "Predicted SAR:",
          fill = "Predicted SAR:",
          shape = "Observed data:",
          title = NULL
    ) +
    ggplot2::scale_color_manual(
      values = c("0" = "steelblue4", "1" = "#b47747"),
      labels = c("0" = "In-river, \npredicted with 95% CI", "1" = "Transported, \npredicted with 95% CI")
    ) +
    ggplot2::scale_fill_manual(
      values = c("0" = "steelblue4", "1" = "#b47747"),
      labels = c("0" = "In-river, \npredicted with 95% CI", "1" = "Transported, \npredicted with 95% CI")

    ) +
    ggplot2::scale_linetype_manual(
      values = c("0" = "solid", "1" = "dashed"),
      labels = c("0" = "In-river", "1" = "Transported")
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(color = c("steelblue4", "#b47747") ),
                                order = 1),
           size = ggplot2::guide_legend(override.aes = list(
             label = list(size = 8),
             shape = 21),
             order = 2),
           color = ggplot2::guide_legend(order = 3),
           fill = ggplot2::guide_legend(order = 3)) +
    ggplot2::theme_light()+
    # ggplot2::facet_wrap(~year + species_rear, scales = "free_y", ncol = 4) +
    ggh4x::facet_wrap2(~year + species_rear, scales = "free_y", axes = "all", ncol = 4) +
    ggplot2::theme(
        strip.background = ggplot2::element_rect(fill="lightgrey"),
        strip.text = ggplot2::element_text(colour = 'black'),
        panel.spacing = ggplot2::unit(2, "lines"),
        panel.grid.minor = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 15))

  if(legend_location == "top"){
    p <-p + ggplot2::theme(
      legend.position = "top",  # Top right corner
      legend.justification.top = c("left", "top"),  # Adjusts legend alignment to the top right corner
      legend.direction = "vertical"
    )
  } else if(legend_location == "right"){
    p
  }

  #*does not include annotation text--since using in walkthrough for specific year example.

  } else if (observed == "yes") {


    if (selected_covariate == "Day-of-year (DOY)") {
      #predicted data
      data_summarized<- data_pred %>%
        dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
        dplyr::group_by(year)

      x_var <- data_summarized$doy
      covar_label <- "Day-of-year (DOY)"

      #observed data
      wrangled_observed_data <- observed_data %>%
        dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
        dplyr::group_by(year) %>%
        dplyr::filter(n > 7) #remove data points with less than 7 observations

      x_var_obs<- wrangled_observed_data$doy

      x_breaks <- seq(90, 160, by = 10)

    } else if (selected_covariate == "Temperature (°C)") {

      #prediction data
      data_summarized<- data_pred %>%
        dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
        dplyr::group_by(year)

      x_var <- data_summarized$mean.temp
      covar_label <- "Temperature (°C)"

      #observed data
      wrangled_observed_data <- observed_data %>%
        dplyr::mutate(species_rear = interaction(species, rear_type)) %>%
        dplyr::group_by(year) %>%
        dplyr::filter(n > 7) #remove data points with less than 7 observations

      x_var_obs<- wrangled_observed_data$mean.temp

      x_breaks <- seq(6, 15, by = 1)

    }

    unique_transport_obs <- unique(wrangled_observed_data$transport)

    p <-
      ggplot2::ggplot() +

      ggplot2::geom_point( data = data_summarized, ggplot2::aes( x = x_var, y =SAR, fill =  transport, color = transport))+
      tidybayes::geom_lineribbon(data = data_summarized, ggplot2::aes(x = x_var,y = SAR, ymin = SAR.lo, ymax = SAR.hi, fill = transport, color = transport),
                                 alpha = .25
      ) +
      ggplot2::geom_point(data = wrangled_observed_data,
                          ggplot2::aes(
                            x = x_var_obs,
                            y =sar.pit,
                            size = n,
                            shape =  transport,
                            color = transport),
                          alpha = .7)+
      ggplot2::labs( x = covar_label,
                     y = "Smolt-to-Adult Ratio\n(SAR)",
                     size = "Number of fish observed:",
                     color = "Predicted SAR:",
                     fill = "Predicted SAR:",
                     shape = "Observed data:",
                     title = NULL
      ) +
      ggplot2::scale_color_manual(
        values = c("0" = "steelblue4", "1" = "#b47747"),
        labels = c("0" = "In-river, \npredicted with 95% CI", "1" = "Transported, \npredicted with 95% CI")
      ) +
      ggplot2::scale_fill_manual(
        values = c("0" = "steelblue4", "1" = "#b47747"),
        labels = c("0" = "In-river, \npredicted with 95% CI", "1" = "Transported, \npredicted with 95% CI")
      ) +
      ggplot2::scale_linetype_manual(
        values = c("0" = "solid", "1" = "dashed"),
        labels = c("0" = "In-river", "1" = "Transported")
      ) +
      ggplot2::scale_shape_manual(
        values = c("0" = 21, "1" = 21),
        labels = c("0" = "In-river", "1" = "Transported")
      ) +
      ggplot2::scale_size_continuous(
        range = c(1, 5),
        breaks = c(10, pretty(c(10, max(stats::na.omit(wrangled_observed_data$n)), n = 3)))
      ) +
      ggplot2::scale_x_continuous(breaks = x_breaks) +
      ggplot2::guides(
        # Fix the shape guide to dynamically use only the available transport values
        shape = ggplot2::guide_legend(
          override.aes = list(color = c("steelblue4", "#b47747")[as.numeric(as.character(unique_transport_obs)) + 1]),
          order = 1,
          nrow = 1
        ),
        size = ggplot2::guide_legend(
          override.aes = list(
            label = list(size = 8),
            shape = 21
          ),
          order = 2,
          nrow = 1
        ),
        color = ggplot2::guide_legend(order = 3, nrow = 1),
        fill = ggplot2::guide_legend(order = 3, nrow = 1)
      ) +
      ggplot2::theme_light()+
      # ggplot2::facet_wrap(~year + species_rear, scales = "free_y", ncol = 4) +
      ggh4x::facet_wrap2(~year + species_rear, scales = "free_y", axes = "all", ncol = 4) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill="lightgrey"),
        strip.text = ggplot2::element_text(colour = 'black'),
        panel.spacing = ggplot2::unit(2, "lines"),
        panel.grid.minor = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 15))

    #add text for years without all adult returns
    if (any(as.numeric(as.character(data_summarized$year)) >= first_return_year)) {
    p <- p +
      ggplot2::geom_text(data = data_summarized %>% dplyr::filter(as.numeric(as.character(year)) >= first_return_year),
                         ggplot2::aes(x = Inf, y = Inf, label = "*Out-of-sample prediction until all adults return"),
                         hjust = 1.1, vjust = 2, size = 3, color = "red", inherit.aes = FALSE)
    }

    if(legend_location == "top"){
      p <-p + ggplot2::theme(
        legend.position = "top",  # Top right corner
        legend.justification.top = c("left", "top"),  # Adjusts legend alignment to the top right corner
        legend.direction = "vertical"
      )
    } else if(legend_location == "right"){
      p
    }

  }
    return(p)

}


# # # #example
# df<-as.data.frame(model_output$doy_pred[1]) %>% filter(year == 1994)
# obs<- as.data.frame(model_output$observed[1])%>% filter(year == 1994)
# fct_SAR_by_year_plot(data_pred = df, observed_data = obs, selected_covariate = "Day-of-year (DOY)", observed = "yes", legend_location = "top")
