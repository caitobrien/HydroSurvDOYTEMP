#' SAR_all_years_single_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


fct_compare_SAR_TI_plot <- function(data_pred, data_ti, observed_data, selected_year, selected_covariate) {

  # input data for sar plot function
  sar_plot<- fct_SAR_by_year_plot(data =data_pred, observed_data = observed_data, observed = "yes", selected_covariate = selected_covariate)

  # set title for plot being showed
  sar_plot <- sar_plot + ggplot2::ggtitle(paste0("SAR & T:B, year of interest: ", selected_year)) +
                         ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  # input data for ti plot function
  ti_plot<- fct_TI_by_years_plot(data = data_ti, selected_covariate = selected_covariate)


  # arrange plots with patchwork
  combine_plot <- (
    patchwork::wrap_plots(sar_plot, ti_plot,ncol = 1 ) +
      patchwork::plot_layout(guides = "keep")
  )

  # display the combined plot
  combine_plot
}
