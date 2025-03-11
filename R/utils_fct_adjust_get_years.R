#' @title Adjust Max Year Based on Current Year and Day of Year
#' @description returns same range from reactive get_years() based on speciesxrun but adjusts max year to remove years where all adults have not returned (i.e. 3 years past last outmigration year)
#' @returns range of years
#' @param years list of year ranges sourced from get_years() reactive
#' @param max_year maximum outmigration year of the input data
#'

fct_adjust_get_years <- function(years) {

  max_year <- max(years)
  current_year <- lubridate::year(Sys.Date())
  current_doy <- lubridate::yday(Sys.Date())

  if (current_year > max_year && current_doy < 160) {
    adjusted_max_year <- current_year - 4
  } else if (current_year <= max_year && current_doy > 160) {
    adjusted_max_year <- current_year - 3
  } else {
    return(years)
  }

  # Adjust the years based on the adjusted_max_year
  adjusted_years <- years[years <= adjusted_max_year]

  return(adjusted_years)
}

# # Example usage
# years <- c(1996:1997, 1999:2001, 2003:2024)
#
#
# adjusted_years <- fct_adjust_get_years(years)
# print(adjusted_years)
