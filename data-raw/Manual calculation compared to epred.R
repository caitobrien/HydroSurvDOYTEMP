#load libraries
library(tidyverse)
library(brms)
library(tidybayes)
library(here)


# example to compare epred to manual calculation to get predictions for natural chinook with doy run in brms
# model # Family: binomial
# Links: mu = logit
# Formula: alive | trials(n) ~ doyz + I(doyz^2) + transport + doyz:transport + I(doyz^2):transport + (1 + doyz + I(doyz^2) + transport | year)
# Data: df.agg (Number of observations: 1533)
# Draws: 3 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 3000


# import model
mod_natural_chinook_doy<- readRDS(here::here("data-raw/models", "mod_natural_chinook_doy.rds"))
posterior_draws <- as_draws_df(mod_natural_chinook_doy)
colnames(posterior_draws)


generate_predictions <- function( newdata) {
  # Extract fixed effects
  b0 <- posterior_draws$b_Intercept
  b_doyz <- posterior_draws$b_doyz
  b_doyz2 <- posterior_draws$b_IdoyzE2
  b_transport <- posterior_draws$b_transport1
  b_doyz_transport <- posterior_draws$`b_doyz:transport1`
  b_doyz2_transport <- posterior_draws$`b_IdoyzE2:transport1`

  all_predictions <- list()

  # set empty matrix to store values
  logit_matrix <- matrix(NA, nrow = nrow(newdata), ncol = length(b0))

  # Loop through each row of newdata
  for (i in 1:nrow(newdata)) {

    # Get the year, doyz, and transport values for the current row
    doyz_value <- newdata$doyz[i]
    transport_value <- newdata$transport[i]
    year_value <- as.character(newdata$year[i])

    # get naming to extract the random effects for the given year from posterior draws
    year_col_intercept <- paste0("r_year[", year_value, ",Intercept]")
    year_col_doyz <- paste0("r_year[", year_value, ",doyz]")
    year_col_doyz2 <- paste0("r_year[", year_value, ",IdoyzE2]")
    year_col_transport <- paste0("r_year[", year_value, ",transport1]")

    # cal based on naming above
    b_year_intercept <- posterior_draws[[year_col_intercept]]
    b_year_doyz <- posterior_draws[[year_col_doyz]]
    b_year_doyz2 <- posterior_draws[[year_col_doyz2]]
    b_year_transport <- posterior_draws[[year_col_transport]]


    #predict for the current posterior draw and row of newdata
    logit_matrix[i, ] <-  b0 +
      b_doyz * doyz_value +
      b_doyz2 * doyz_value^2 +
      b_transport * transport_value +
      b_doyz_transport * doyz_value * transport_value +
      b_doyz2_transport * doyz_value^2 * transport_value +
      # Add random effects for the given year
      b_year_intercept +
      b_year_doyz * doyz_value +
      b_year_doyz2 * doyz_value^2 +
      b_year_transport * transport_value
  }

  # get median for for each row
  logit_survival_med <- apply(logit_matrix, 1, function(x) {
    c(median = stats::median(x),
      .lower90 = stats::quantile(x, 0.05), .upper90 = stats::quantile(x, 0.95),
      .lower50 = stats::quantile(x, 0.25), .upper50 = stats::quantile(x, 0.75))
  })

  pred_summary <- as.data.frame(t(logit_survival_med))
  colnames(pred_summary) <- c("median", ".lower90", ".upper90", ".lower50", ".upper50")

  pred_summary_df <- data.frame(pred_summary,
                                year = newdata$year,
                                doyz = newdata$doyz,
                                transport = newdata$transport)

  pred_summary_df <- pred_summary_df %>%
    mutate(across(c(median, .lower90, .upper90, .lower50, .upper50), plogis))

  return(pred_summary_df)
}

#run manual function
# get pre-aggregated data for doy scaling
df<-read_csv(here("data-raw/HydroSurvDOYTEMP_data_subset", "natural_chinook_data_subset.csv"))

scale.doy <- scale(as.numeric(df$doy))
scale.x.doy <- attr(scale.doy, "scaled:center")
scale.sd.doy <- attr(scale.doy, "scaled:scale")

# set new data
newdata <- expand_grid(
  doyz = ((90:160) - scale.x.doy) / scale.sd.doy,
  transport = c(0, 1),
  year = c(1993:1996,1998:2018) #no 1997 data
)  %>%
  mutate(year = as.character(year))


# run predictions
predictions <- generate_predictions( newdata = newdata)


#plot predictions
predictions %>%
  ggplot(aes(x = doyz, y = median, color = factor(transport))) +
  geom_point()+
  geom_line() +
  facet_wrap(~year)

#calculate TI
ti_predictions<- predictions %>%
  pivot_wider(
    names_from = transport,
    values_from = c(median, .lower90, .upper90, .lower50, .upper50),
    names_glue = "{.value}_{transport}"
  ) %>%
  mutate(ti = median_1 / median_0) %>%
  select(year, doyz, ti)

#plot TI
ti_predictions %>%
  ggplot(aes(x = doyz, y = ti)) +
  geom_point()+
  geom_line() +
  facet_wrap(~year)


####-------------compare to epred----####
newdata_epred <- expand_grid(
  doyz = ((90:160) - scale.x.doy) / scale.sd.doy,
  transport = c(0, 1),
  year = c(1993:1996,1998:2018),
  n = 1 #need to add n trial
)  %>%
  mutate(year = as.character(year))

epred_predictions<-newdata_epred %>%
  tidybayes::add_epred_draws(mod_natural_chinook_doy, re_formula = NULL, allow_new_levels = TRUE) %>%
  tidybayes::median_qi()

epred_predictions %>%
  ggplot(aes(x = doyz, y =.epred, color = factor(transport))) +
  geom_point()+
  geom_line() +
  facet_wrap(~year)

#calculate epred TI
ti_epred_predictions<- epred_predictions %>%
  select(year, doyz, transport, .epred,.lower,.upper) %>%
  pivot_wider(
    names_from = transport,
    values_from = c(.epred, .lower, .upper),
    names_glue = "{.value}_{transport}"
  ) %>%
  mutate(ti = .epred_1 / .epred_0) %>%
  select(year, doyz, ti)

#plot epred TI
ti_epred_predictions %>%
  filter(year == 1996) %>%
  ggplot(aes(x = doyz, y = ti)) +
  geom_point()+
  geom_line() +
  facet_wrap(~year)


# dips still present in the predictions-- see year 1996
ti_predictions %>%
  filter(year == 1996) %>%
  ggplot(aes(x = doyz, y = ti)) +
  geom_point()+
  geom_line() +
  facet_wrap(~year)

ti_epred_predictions %>%
  filter(year == 1996) %>%
  ggplot(aes(x = doyz, y = ti)) +
  geom_point()+
  geom_line() +
  facet_wrap(~year)


