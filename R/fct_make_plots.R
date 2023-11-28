#' make_plots
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


plot_fx<- function(spp, rtype, cov, SAR_year){
    data %>%
    filter(year == SAR_year,
           species == spp,
           rear_type == rtype,
           covariate == cov) %>%
    ggplot(aes( x= date, color = as.factor(transport), group = year)) +
    geom_point(aes(y =SAR, fill = as.factor(transport)))+
    geom_jitter(aes(y =sar.pit, shape = as.factor(transport)), alpha = .7)+
    geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill = as.factor(transport)), alpha = .25) +
    labs( x = "Date", y = "SAR", color = NULL,
          fill = NULL, shape = NULL,
          title = "Predicted SAR versus observed SAR from PIT tag recoveries",
          subtitle = paste("Year:",SAR_year)) +
    scale_color_manual(breaks = c(0, 1),
                       values = c("steelblue4", "#b47747"),
                       labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    scale_fill_manual(breaks = c(0, 1),
                      values = c("steelblue4", "#b47747"),
                      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    scale_shape_manual(values = c(21,21),
                       breaks = c(0,1),
                       labels = c("In-river, observed", "Transported, observed")) +
    guides(shape = "legend") +
    theme_minimal()

}
