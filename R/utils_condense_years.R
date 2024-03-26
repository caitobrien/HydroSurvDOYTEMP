#'
#' @description A utils function to condense years_selected for reactive title
#'
#' @noRd
#'
#'
#' @param years years_selected input
#'
#' @return A data frame with the summary data and labels in format needed for:
condense_years <- function(years) {
  if (length(years) == 0) {
    return("No year selected")
  }
  years <- sort(unique(as.numeric(years))) # Convert years to numeric
  breaks <- which(c(TRUE, diff(years) > 1, TRUE))
  ranges <- mapply(function(i, j) {
    if (i != j) {
      paste(years[i], years[j], sep = "-")
    } else {
      as.character(years[i])
    }
  }, breaks[-length(breaks)], breaks[-1] - 1)
  paste(ranges, collapse = ", ")
}
