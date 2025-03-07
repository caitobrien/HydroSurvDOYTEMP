#' submodule_dataselect UI Function
#'
#' @description sub module within main page module for the data selection only
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_main_submodule_dataselect_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # div("To plot survival predictions:", class = "instructions"),

    fluidRow(
      column(
        width = 3,

    # select covariate
    selectInput(
      inputId = ns("select_cov"),
      label = "Select covariate",
      choices = c("Day-of-year (DOY)", "Temperature (°C)"),
      selected = "Day-of-year (DOY)",
      width = "200px",
      multiple = F
    )
    ),

    column(
      width = 3,

    # select species
    shinyWidgets::prettyCheckboxGroup(
      inputId = ns("select_spp"),
      label = "Select species",
      shape = "curve",
      outline = TRUE,
      choices = c("Chinook", "Steelhead"),
      selected = "Chinook",
      width = "200px"
      )
    ),

    column(
      width = 3,

    # select rear type
    shinyWidgets::prettyCheckboxGroup(
      inputId = ns("select_rear"),
      label = "Select rearing type",
      shape = "curve",
      outline = TRUE,
      choices = c("Natural-origin", "Hatchery-origin"),
      selected = "Natural-origin",
      width = "200px"
      )
    ),

    column(
      width = 3,

    # prompt to select all years or by year
    selectInput(
      inputId = ns("year_display"),
      label = "View by",
      choices = c("All Years", "Year"),
      selected = "All Years"
    ),
    uiOutput(ns("year_picker"))
  )
      )
  )
}

#' dataselect Server Functions
#'
#' @noRd
mod_main_submodule_dataselect_server <- function(id, all) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    load("data/all.rda")
    get("all")

    get_years <- reactive({
      species <- input$select_spp
      rear_type <- input$select_rear

      if (length(species) > 1 || length(rear_type) > 1) {
        return(c(1993:2024))
      }

      if (species == "Chinook" && rear_type == "Natural-origin") {
        unique(all$observed[[1]]$year) # c(1993:1996, 1998:2019, 2021:2024)
      } else if (species == "Chinook" && rear_type == "Hatchery-origin") {
        unique(all$observed[[2]]$year) # c(1993:1996, 1998:2019, 2021:2024)
      } else if (species == "Steelhead" && rear_type == "Natural-origin") {
        unique(all$observed[[3]]$year) # c(1994:2019, 2021:2024)
      } else if (species == "Steelhead" && rear_type == "Hatchery-origin") {
        unique(all$observed[[4]]$year) # c(1993:2019, 2021:2024)
      } else {
        c(1993:2024)
      }
    })


    # Render the UI for the year picker
    output$year_picker <- renderUI({

      if (input$year_display == "Year") {
        shinyWidgets::pickerInput(
          inputId = ns("select_years"),
          label = "Select Year(s)",
          choices = get_years(),
          selected = 2021,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      } else {
        NULL
      }
    })

    # Reactive for year_display
    year_display <- reactive({
      input$year_display
    })

    # Reactive for selected_years
    years_selected <- reactive({
      input$select_years
    })

    # Reactive for covariate
    selected_covariate <- reactive({
      input$select_cov
    })


    # Reactive for plot height
    plot_height <- reactive({
      if (input$year_display == "All Years") {
        return(375)  # Reset to default height when viewing all years
      } else {
        req(input$select_years)
        nyears <- length(input$select_years)
        if (nyears > 2) {
          return(375 + (nyears-2)*150)
        } else {
          return(375)
        }
      }
    })


    # Filter data based on selections
    filtered_data_pred <- reactive({

      # extract list for covariate
      selected_list <- if (input$select_cov == "Day-of-year (DOY)") all$doy_pred else if(input$select_cov == "Temperature (°C)") all$temp_pred
      # Filter based on inputs
      filtered_list <- purrr::map(selected_list, ~ .x %>%
                                    dplyr::filter(
                                      species %in% c(input$select_spp),
                                      rear_type %in% c(input$select_rear),
                                      if (input$year_display == "Year" && !is.null(input$select_years)) year %in% c(input$select_years) else TRUE
                                    ) %>%
                                    mutate(transport = as.factor(transport))
      )

      # Combine filtered list into a single data frame
      filtered_data <- dplyr::bind_rows(filtered_list)
      return(filtered_data)
    })

    # Filter data based on selections
    filtered_data_ti <- reactive({

      # extract list for covariate
      selected_list <- if (input$select_cov == "Day-of-year (DOY)") all$doy_ti else if(input$select_cov == "Temperature (°C)") all$temp_ti
      # Filter based on inputs
      filtered_list <- purrr::map(selected_list, ~ .x %>%
                                    dplyr::filter(
                                      species %in% c(input$select_spp),
                                      rear_type %in% c(input$select_rear),
                                      if (input$year_display == "Year" && !is.null(input$select_years)) year %in% c(input$select_years) else TRUE
                                    )
      )

      # Combine filtered list into a single data frame
      filtered_data <- dplyr::bind_rows(filtered_list)
      return(filtered_data)
    })

    filtered_observed_data <- reactive({

      # Filter each list element based on user selections
      filtered_list <- purrr::map(all$observed, ~ .x %>%
                                    dplyr::filter(
                                      species %in% c(input$select_spp),
                                      rear_type %in% c(input$select_rear),
                                      if (input$year_display == "Year" && !is.null(input$select_years)) year %in% c(input$select_years) else TRUE
                                    ) %>%
                                    mutate(transport = as.factor(transport))

      )

      # Combine filtered list into a single data frame
      filtered_observed_data <- as.data.frame(dplyr::bind_rows(filtered_list))
      return(filtered_observed_data)
    })

    # Return the filtered data as a reactive list
    return(list(
      filtered_data_pred = reactive(filtered_data_pred),
      filtered_data_ti = reactive(filtered_data_ti),
      filtered_observed_data = reactive(filtered_observed_data),
      year_display = reactive(year_display),
      plot_height = reactive(plot_height),
      years_selected = reactive(years_selected),
      selected_covariate = reactive(selected_covariate)
    ))
  })
}

## To be copied in the UI
# mod_main_submodule_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_main_submodule_dataselect_server("dataselect_1")
