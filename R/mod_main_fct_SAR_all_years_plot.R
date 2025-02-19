#' SAR_all_years_plot
#'
#' @description A function to plot SAR with all years combined.-- plot called within submodule mod_main_submodule_select_SAR_plot.R
#'
#' @return The return value is a plot with median SAR and 95 CI for all years of data per species, rear type and covariate (DOY, TEMP)--included on first tab within mod_main_page.R,
#'
#' @noRd

# set_species<-"Chinook"
# set_rear_type<- "Natural-origin"
# set_covariate<- "Temperature (°C)" #"Day-of-year (DOY)"#"Temperature (°C)"
# filtered_data<-all_combined %>% filter(species == set_species, rear_type == set_rear_type, covariate == set_covariate)
# # fct_SAR_by_year_plot(data = filtered_data, selected_years = c(1993:2018), observed = "yes")

fct_SAR_all_years_plot <- function(data, observed_data, selected_covariate, observed = "no") {


  data_median<- data %>%
    dplyr::mutate(x_var = dplyr::case_when(
      covariate == "Day-of-year (DOY)" ~ doy,
      TRUE ~ mean.temp
    )) %>%
    dplyr::mutate(
      transport = as.factor(transport),
      year = as.factor(year),
      rear_type = as.factor(rear_type),
      covariate = as.factor(covariate),
      species = as.factor(species)
    ) %>%
    dplyr::group_by(covariate, x_var, species, rear_type, transport, doy) %>%
    ggdist::median_qi(SAR, na.rm=TRUE)


  # data_summarized <- data_median %>%
  #   dplyr::left_join(data_n.obs, by = c("covariate", "species", "rear_type", "transport", "doy"))
  #
  # # Convert data_summarized to data frame
  # data_summarized <- as.data.frame(data_summarized)

  # Extract unique covariate name
  covar_label <- unique(data_median$covariate)

  # plot
  p <- ggplot2::ggplot(data_median, ggplot2::aes(x = x_var, color = transport)) +
    ggplot2::geom_point(ggplot2::aes(y = SAR, fill = transport)) +
    # ggplot2::geom_line(ggplot2::aes(y = SAR)) +
    # ggplot2::geom_ribbon(ggplot2::aes(y = SAR, ymin = SAR.lower, ymax = SAR.upper, fill = transport), alpha = .25) +
    tidybayes::geom_lineribbon(ggplot2::aes(y = SAR, ymin = .lower, ymax = .upper, fill = transport),
                               alpha = .25
    ) +
    # ggdist::geom_pointinterval(ggplot2::aes(
    #   y = ifelse(n.sar.pit > 7, sar.pit, NA),
    #   ymin = sar.pit.lower,
    #   ymax = sar.pit.upper,
    #   shape = transport,
    #   color = transport),
    #   alpha = .25
    # ) +
    ggplot2::scale_color_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \nmedian with 95% CI", "Transported, \nmedian with 95% CI")
    ) +
    ggplot2::scale_fill_manual(
      breaks = c("0", "1"),
      values = c("steelblue4", "#b47747"),
      labels = c("In-river, \nmedian with 95% CI", "Transported, \nmedian with 95% CI")
    ) +
    ggplot2::labs(
      x = covar_label,
      y = "Smolt-to-Adult Ratio\n(SAR)",
      color = "Predicted SAR",
      fill = "Predicted SAR",
      title = NULL
    ) +
    ggplot2::theme_light() +
    ggplot2::facet_grid(rear_type ~ species, scales = "free") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgrey"),
                   strip.text = ggplot2::element_text(colour = "black"),
                   panel.grid.minor = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 15))

  #functionality to include obs data or not in plot
  if (observed == "yes") {

    if(selected_covariate == "Day-of-year (DOY)"){
      observed_data<- observed_data %>% mutate(covariate = "Day-of-year (DOY)")

      wrangled_observed_data <- observed_data %>%
        dplyr::mutate(x_var = dplyr::case_when(
        covariate == "Day-of-year (DOY)" ~ doy,
        TRUE ~ mean.temp
      )) %>%
        dplyr::mutate(
          transport = as.factor(transport),
          year = as.factor(year),
          rear_type = as.factor(rear_type),
          covariate = as.factor(covariate),
          species = as.factor(species)
        ) %>%
        dplyr::group_by(covariate,x_var, species, rear_type, transport, doy) %>%
        dplyr::summarise(
          n.sar.pit = sum(n, na.rm = TRUE),  # Sum 'n' per year
          sar.pit = median(sar.pit, na.rm = TRUE),  # Median SAR.pit
          sar.pit.lo = quantile(sar.pit, probs = 0.025, na.rm = TRUE),  # Lower bound (2.5th percentile)
          sar.pit.hi = quantile(sar.pit, probs = 0.975, na.rm = TRUE)   # Upper bound (97.5th percentile)
        ) %>%
        ungroup()

    } else if(selected_covariate == "Temperature (°C)"){
      observed_data<- observed_data %>% mutate(covariate = "Temperature (°C)")

      wrangled_observed_data <- observed_data %>%
        dplyr::mutate(x_var = dplyr::case_when(
          covariate == "Day-of-year (DOY)" ~ doy,
          TRUE ~ mean.temp
        )) %>%
        dplyr::mutate(
          transport = as.factor(transport),
          year = as.factor(year),
          rear_type = as.factor(rear_type),
          covariate = as.factor(covariate),
          species = as.factor(species)
        ) %>%
        dplyr::group_by(covariate,x_var, species, rear_type, transport, mean.temp) %>%
        dplyr::summarise(
          n.sar.pit = sum(n, na.rm = TRUE),  # Sum 'n' per year
          sar.pit = median(sar.pit, na.rm = TRUE),  # Median SAR.pit
          sar.pit.lo = quantile(sar.pit, probs = 0.025, na.rm = TRUE),  # Lower bound (2.5th percentile)
          sar.pit.hi = quantile(sar.pit, probs = 0.975, na.rm = TRUE)   # Upper bound (97.5th percentile)
        ) %>%
        ungroup()
    }
    print(wrangled_observed_data)
    p.obs <- p +
      ggdist::geom_pointinterval( data = wrangled_observed_data, ggplot2::aes(
      y = ifelse(n.sar.pit > 7, sar.pit, NA),
      ymin = sar.pit.lo,
      ymax = sar.pit.hi,
      shape = transport,
      color = transport),
      alpha = .25
    ) +
      ggplot2::labs(shape = "Observed data") +
      ggplot2::scale_shape_manual(
        values = c(21, 21),
        breaks = c("0", "1"),
        labels = c("In-river,\nmedian per year", "Transported,\nmedian per year")) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(color = c("steelblue4", "#b47747")))) # change predator legend to squares

    return(p.obs)

    } else {
      return(p)
      }

}


# #example
<<<<<<< HEAD
# fct_SAR_all_years_plot(data = filtered_data, observed = "yes")
=======

set_species<-"Chinook"
set_rear_type<- "Natural-origin"
set_covariate<- "Day-of-year (DOY)"#"Temperature (°C)" #"Day-of-year (DOY)"
filtered_data<-df_mod_predict %>%
  select(doy, mean.temp, transport, year, SAR, SAR.lo, SAR.hi, rear_type, covariate, species) %>%
  filter(species == set_species, rear_type == set_rear_type, covariate == set_covariate)

selected_covariate<-"Day-of-year (DOY)"#"Temperature (°C)" #"Day-of-year (DOY)"
filtered_observed_data<-df_aggregated_observed %>%
  filter(species == set_species, rear_type == set_rear_type)

fct_SAR_all_years_plot(data = filtered_data, observed_data = filtered_observed_data, selected_covariate = selected_covariate,  observed = "yes")
>>>>>>> 6c85a78 (edits to methods)
