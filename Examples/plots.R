library(ggrepel)
library(ggdist)


#plotly best optioon still needs work on multiple year CI and grouping levels
plotly::ggplotly(
  data() %>%
    mutate(transport = as.factor(transport),
           year = as.factor(year),
           rear_type = as.factor(rear_type),
           covariate = as.factor(covariate),
           species = as.factor(species)
    ) %>%
    group_by( year) %>%
    ggplot( aes( x= doy, color = transport)) +
    geom_point(aes(y =SAR, fill =  transport, text = paste ("Year", year, "\nDay-of-year:",doy, "\nPassage type:", transport, "\nPredicted SAR with 95% CI:", round(SAR,2))))+
    geom_line(aes(y = SAR, fill= transport)) +
    geom_ribbon(aes(ymin = SAR.lo, ymax = SAR.hi, fill= transport), alpha =.25, color = NA) +
    geom_point(aes(y =sar.pit, shape =  transport, text = paste ("Year", year, "\nDay-of-year:",doy, "\nPassage type:", transport,"\nObserved SAR:", round(sar.pit,2))), alpha = .7)+
    labs( x = "Day-of-year\n(DOY)", y = "Smolt-to-Adult Ratio\n(SAR)", color = "Per year",
          fill = "Per year", shape = NULL, #linetype = "Combined years",
          title = NULL
    ) +
    scale_color_manual(breaks = c("0", "1"),
                       values = c("steelblue4", "#b47747"),
                       labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    scale_fill_manual(breaks = c("0", "1"),
                      values = c("steelblue4", "#b47747"),
                      labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
    scale_shape_manual(values = c(21,21),
                       breaks = c("0", "1"),
                       labels = c("In-river, observed", "Transported, observed")) +
    scale_linetype_manual(values = c("dashed","dashed"),
                          breaks = c("0", "1"),
                          labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
    theme_light()+ facet_grid(rear_type ~ species, scales = "free_y") +
    theme(strip.background =element_rect(fill="lightgrey"))+
    theme(strip.text = element_text(colour = 'black')) +
    theme(plot.margin = margin(1, 0, 0, 1.5, "cm")),
  tooltip =  "text"
) %>%
  layout(
    hovermode = "x",
    showlegend = FALSE
  ) %>%
  plotly::config(displayModeBar = FALSE) %>%
  plotly::config(showLink = FALSE)

############

data.pred %>%
  filter(covariate == "Day-of-year (DOY)") %>%
  mutate(transport = as.factor(transport),
         year = as.factor(year),
         rear_type = as.factor(rear_type),
         covariate = as.factor(covariate),
         species = as.factor(species)
  ) %>%
  group_by( year) %>%
  ggplot( aes( x= if_else(covariate == "Day-of-Year (DOY)", doy, temp), color = transport)) +
  geom_point(aes(y =SAR, fill =  transport))+
  tidybayes::geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill =  transport, group = year), alpha = .25) +
  #geom_point(aes(y =sar.pit), alpha = .7)+
  labs(  y = "Smolt-to-Adult Ratio\n(SAR)", color = "Per year",
        fill = "Per year", shape = NULL, #linetype = "Combined years",
        title = NULL
  ) +
  scale_color_manual(breaks = c("0", "1"),
                     values = c("steelblue4", "#b47747"),
                     labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
  scale_fill_manual(breaks = c("0", "1"),
                    values = c("steelblue4", "#b47747"),
                    labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
  scale_shape_manual(values = c(21,21),
                     breaks = c("0", "1"),
                     labels = c("In-river, observed", "Transported, observed")) +
  scale_linetype_manual(values = c("dashed","dashed"),
                        breaks = c("0", "1"),
                        labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
  # ggrepel::geom_text_repel(data = . %>% group_by(year, transport) %>% filter(doy == 160),aes(y = SAR, label = year, color = transport),
  #                          force        = 0.1,
  #                          nudge_x      = 0.1,
  #                          direction    = "y",
  #                          hjust        = -.7,
  #                          segment.size = 0.1,
  #                          min.segment.length = 0, #draw all line segments
  #                          xlim = c(-Inf,Inf), #allow values to extend to edges
  #                          max.overlaps = 30
  # )+
  # coord_cartesian(clip = "off") + #disable clipping labels
  # xlim(NA, 170)+
  theme_light()+ facet_grid(rear_type ~ species, scales = "free_y") +
  theme(strip.background =element_rect(fill="lightgrey"))+
  theme(strip.text = element_text(colour = 'black')) +
  theme(plot.margin = margin(1, 0, 0, 1.5, "cm"))









  plot_data <- data.pred %>%
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
    group_by(year) %>%

    filter(covariate == "Day-of-year (DOY)")

  # Create plotly plot
plot_data %>%
    plot_ly(x = ~x_var, color = ~transport, group = ~year) %>%
    add_trace(y = ~SAR, group = ~year, type = "scatter",  mode = "line+markers", name = "SAR") %>%
    add_ribbons( y = ~SAR, group = ~year,  ymin = ~SAR.lo, ymax = ~SAR.hi, fill = ~transport, name = "SAR.CI") %>%
    # add_trace(y = ~sar.pit, mode = "markers", name = "Obs", size = ~n.obs, alpha = 0.7) %>%
    layout(
      xaxis = list(title = unique(plot_data$covariate)),
      yaxis = list(title = "Smolt-to-Adult Ratio (SAR)"),
      title = list(text = ""),
      # shapes = list(
      #   type = 'line',
      #   x0 = min(plot_data$x_var), x1 = max(plot_data$x_var),
      #   y0 = median(plot_data$SAR), y1 = median(plot_data$SAR),
      #   line = list(color = 'black', dash = 'dash')
      # ),
      showlegend = TRUE
    )

# Create subplot for each combination of species and rear_type with facet-like headers
plot_list <- list()
for (spec in unique(plot_data$species)) {
  for (rear in unique(plot_data$rear_type)) {
    subset_data <- plot_data %>%
      filter(species == spec, rear_type == rear)

    plot <- plot_ly(subset_data, x = ~x_var, color = ~as.factor(transport), group = ~year) %>%
      add_trace(y = ~SAR, type = "scatter", mode = "markers", name = paste(spec, rear)) %>%
      add_ribbons( y = ~SAR, group = ~year,  ymin = ~SAR.lo, ymax = ~SAR.hi, fill = ~transport, name = "SAR.CI")

    # Add annotation as a header
    plot <- plot %>%
      layout(
        annotations = list(
          list(
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 1.1,
            xanchor = "center",
            yanchor = "bottom",
            text = paste(spec, rear),
            showarrow = FALSE,
            font = list(size = 14)
          )
        )
      )

    plot_list[[paste(spec, rear)]] <- plot
  }
}

# Create subplots
subplot(plot_list, nrows = length(unique(plot_data$species)), shareX = TRUE, shareY = TRUE)

######

# Create subplot for each combination of species and rear_type without connecting lines
plot_list <- list()
legend_text <- c()  # To store legend labels

for (spec in unique(plot_data$species)) {
  for (rear in unique(plot_data$rear_type)) {
    subset_data <- plot_data %>%
      filter(species == spec, rear_type == rear)

    plot <- plot_ly(subset_data, x = ~x_var, color = ~as.factor(transport), group = ~year) %>%
      add_trace(y = ~SAR, type = "scatter", mode = "markers", name = paste(spec, rear), showlegend = FALSE) %>%
      add_ribbons( y = ~SAR, group = ~year,  ymin = ~SAR.lo, ymax = ~SAR.hi, fill = ~transport, name = "SAR.CI", showlegend = FALSE)

    plot_list[[paste(spec, rear)]] <- plot
    legend_text <- c(legend_text, paste(spec, rear))
  }
}

# Create subplots
subplot(plot_list, nrows = length(unique(plot_data$species)), shareX = TRUE, shareY = TRUE) %>%
  layout(annotations = list(
    list(
      x = 1.1,
      y = 1,
      xref = "paper",
      yref = "paper",
      text = paste(legend_text, collapse = "<br>"),
      showarrow = FALSE,
      font = list(size = 12),
      align = "left"
    )
  ))


#####

p<-ggplotly(
  plot_data %>%
    mutate(transport = as.factor(transport)) %>%
    ggplot( aes( x= x_var, color = transport, )) +
      geom_point(aes(y =SAR, fill =  transport, legendgroup = transport), name = "Predicted")+
      geom_ribbon( aes( ymin =SAR.lo, ymax = SAR.hi, fill =  transport, group = year), alpha = .25) +
      geom_point(aes(y =sar.pit, shape = transport), alpha = .7)+
      labs(shape = "Observed", fill = "predicted", color = NULL)+
      #
      # scale_color_manual(breaks = c("0", "1"),
      #                    values = c("steelblue4", "#b47747"),
      #                    labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
      # scale_fill_manual(breaks = c("0", "1"),
      #                   values = c("steelblue4", "#b47747"),
      #                   labels = c("In-river, \npredicted with 95% CI", "Transported, \npredicted with 95% CI"))+
      # scale_shape_manual(values = c(21,21),
      #                    breaks = c("0", "1"),
      #                    labels = c("In-river, observed", "Transported, observed")) +
      # scale_linetype_manual(values = c("dashed","dashed"),
      #                       breaks = c("0", "1"),
      #                       labels = c("In-river,\nmedian predicted probability", "Transported,\nmedian predicted probability"))+
      facet_grid(rear_type ~ species, scales = "free_y") +
    heme(legend.title = element_blank())
)


p %>%
  layout(
    legend = list(
      x = 0.5,
      y = 1.2,
      traceorder = "normal",
      bgcolor = "#f8f9fa",
      bordercolor = "black",
      borderwidth = 1,
      itemsizing = "constant",
      itemwidth = 30
    )
  )


######
library(ggplot2)
library(plotly)
library(dplyr)

# Assuming plot_data contains the necessary columns x_var, transport, SAR, SAR.lo, SAR.hi, sar.pit, year, species, rear_type

# Create the ggplot object
gg <- plot_data %>%
  ggplot(aes(x = x_var, color = as.factor(transport))) +
  geom_point(aes(y = SAR, fill = as.factor(transport), shape = as.factor(transport),
                 legendgroup = as.factor(transport), name = "Transported")) +
  geom_ribbon(aes(ymin = SAR.lo, ymax = SAR.hi, fill = as.factor(transport),
                  group = year, legendgroup = as.factor(transport), name = "Transported CI"), alpha = 0.25) +
  geom_point(aes(y = sar.pit, shape = as.factor(transport),
                 legendgroup = as.factor(transport), name = "Another Transported Point"), alpha = 0.7) +
  facet_grid(rear_type ~ species, scales = "free_y") +
  theme(legend.title = element_blank())  # Hide the default legend title

# Convert ggplot to plotly
p <- ggplotly(gg)

# Display the interactive plot
p


##########

library(plotly)
library(dplyr)

# Example data generation
set.seed(123)
year <- rep(2022, 365)
doy <- 1:365
survival <- runif(365, 0.3, 0.8)
migrating_fish <- sample(50:200, 365, replace = TRUE)

data <- data.frame(year, doy, survival, migrating_fish)

data.pred.x<-data.pred %>%
  group_by(covariate, species, doyz) %>%
  mutate(total_observed = sum(n.obs, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(prop_obs = n.obs/total_observed )


# Calculate total migration and survival for the year
total_migration <- sum(data$migrating_fish)
total_survival <- sum(data$survival * data$migrating_fish)

# Create plot
 data.pred.x %>%
  filter(
         species == "Chinook",
         rear_type == "Natural-origin",
         covariate == "Day-of-year") %>%
   # ggplot(aes(x= date))+
   # geom_line(aes(y=SAR, color= transport))+
   # geom_bar(aes(y = prop_obs))

  plot_ly( x = ~date) %>%
  add_trace(y = ~SAR, type = 'scatter', mode = 'lines', name = 'Predicted Survival') %>%

# %>%
#   layout(
#     title = 'Survival and Migration for 2022',
#     xaxis = list(title = 'Day of Year'),
#     yaxis = list(title = 'Proportion'),
#     annotations = list(
#       x = 365, y = 1, xref = 'x', yref = 'y',
#       text = paste("Total Migration: ", total_migration, "<br>Total Survival: ", total_survival),
#       showarrow = FALSE,
#       font = list(size = 12)
#     )
#   )

# Display the plot
plot



ggplot
