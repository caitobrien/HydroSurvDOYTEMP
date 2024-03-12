#' model_tables
#'
#' @description A fct function to import models results and export summary results in HTML table format for mod_Background.R. Not included in app--move to R markdown?
#'
#'
#' @return A summary table per model in HTML format
#'
#' @noRd

#function to import models
import_models <- function(models_folder) {
  model_files <- list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)
  model_list <- lapply(model_files, readRDS)
  names(model_list) <- gsub("\\.rds$", "", basename(model_files))
  return(model_list)
}

#define model folder path
models_folder<- here::here("data/models")

#import models from the designated folder path
model_list <- import_models(models_folder)


#function to generate and save tables from imported models
create_model_tables <- function(model_list, output_directory) {
  for (i in seq_along(model_list)) {
    #extract model name for file output
    model_name <- basename(names(model_list)[i])
    #extract coefficients and rename variables
    tbl <- as.data.frame(brms::fixef(model_list[[i]]))

    # Conditional row names based on model name
    if (grepl("doy", model_name, ignore.case = TRUE)) {
      row_names <- c("Intercept", "Day-of-years (DOY)", "DOY^2", "Transport", "DOY:Transport", "doyz2:transport")
    } else {
      row_names <- c("Intercept", "Temperature", "Temp^2", "Transport", "Temperature:Transport", "tempz2:transport")
    }

    rownames(tbl) <- row_names

    # Extracting variables from the model name for the table header
    model_vars <- strsplit(model_name, "_")[[1]][2:4]  # Extracts species, rear_type, and covariate

    # Creating the table header
    header <-  paste("Table", i, "Covariates of model for",
                     paste(paste0(tools::toTitleCase(model_vars[1]),"-origin"), tools::toTitleCase(model_vars[2]), collapse = " "),
                     "including the covariate",
                     ifelse(tolower(model_vars[3]) == "doy", "day-of-year (DOY)", "temperature (C)"))


    #create table
    table <- tbl %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      knitr::kable("html", caption = paste0(header)) %>%
      kableExtra::kable_styling(c("striped", "hover"), full_width = TRUE)


    #export to new file folder at html
    file_path <- file.path(output_directory, paste0("table_", model_name, ".html"))
    write_file(table, file_path)
  }
}


# Replace 'output_directory' with your desired output directory
output_directory <- here::here("inst/app/www")
# Create tables
create_model_tables(model_list, output_directory)


