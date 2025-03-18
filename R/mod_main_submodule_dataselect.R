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
  ),
  # Add update button row
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: center; margin-top: 15px;",
          actionButton(
            inputId = ns("update_plot"),
            label = "Update Plot",
            color = "default",
            icon = icon("sync")
            # class = "btn-primary"
          )
      )
    )
  )
  )
  )
}

#' dataselect Server Functions
#'
#' @noRd
mod_main_submodule_dataselect_server <- function(id, model_output) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns



    # reactive values to track input changes
    rct_update_vals <- reactiveVal(list(
      select_cov = NULL,
      select_spp = NULL,
      select_rear = NULL,
      year_display = NULL,
      select_years = NULL
    ))

    # starting and intial values
    inputs_changed <- reactiveVal(TRUE)
    initial_load <- reactiveVal(TRUE)

    # get current input values
    current_inputs <- reactive({
      list(
        select_cov = input$select_cov,
        select_spp = input$select_spp,
        select_rear = input$select_rear,
        year_display = input$year_display,
        select_years = if(input$year_display == "Year") input$select_years else NULL
      )
    })

    # get reactive for changing title changes in different modules
    update_clicked <- reactiveVal(0)

    # watch for changes in inputs
    observe({
      current <- current_inputs()
      last_applied <- rct_update_vals()

      # If initial load or values don't match, inputs have changed
      if (is.null(last_applied$select_cov) ||
          !identical(current, last_applied)) {
        inputs_changed(TRUE)
      }
    })

    # load plot for initial load
    observe({
      # Only run once on initial load
      if (initial_load()) {
        # Set initial values so it shows the first plot
        rct_update_vals(current_inputs())
        update_clicked(update_clicked() + 1)
        initial_load(FALSE)
        inputs_changed(FALSE)
      }
    })

    # Update button observer
    observeEvent(input$update_plot, {
      # Store the current inputs as the last applied
      rct_update_vals(current_inputs())
      # Inputs no longer changed from applied values
      inputs_changed(FALSE)
      update_clicked(update_clicked() + 1)
    })

    # Control update button state-- working?
    observe({
      # Disable if no changes since last update
      if (!inputs_changed()) {
        shinyjs::disable("update_plot")
      } else {
        shinyjs::enable("update_plot")
      }
    })

    #
    get_years <- reactive({
      species <- input$select_spp
      rear_type <- input$select_rear

      if (length(species) > 1 || length(rear_type) > 1) {
        return(c(1993:2019,2021:2024))
      }

      if (species == "Chinook" && rear_type == "Natural-origin") {
        unique(model_output$observed[[1]]$year) # c(1993:1996, 1998:2019, 2021:2024)
      } else if (species == "Chinook" && rear_type == "Hatchery-origin") {
        unique(model_output$observed[[2]]$year) # c(1993:1996, 1998:2019, 2021:2024)
      } else if (species == "Steelhead" && rear_type == "Natural-origin") {
        unique(model_output$observed[[3]]$year) # c(1994:2019, 2021:2024)
      } else if (species == "Steelhead" && rear_type == "Hatchery-origin") {
        unique(model_output$observed[[4]]$year) # c(1993:2019, 2021:2024)
      } else {
        c(1993:2019,2021:2024) #no 2020 data for any spp/rear combo
      }
    })

    # UI for the year picker
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

    # Adjust year based on input
    year_display <- eventReactive(
      c(input$update_plot, initial_load()), {
        input$year_display
      },
      ignoreNULL = FALSE
    )

    years_selected <- eventReactive(
      c(input$update_plot, initial_load()), {
        input$select_years
      },
      ignoreNULL = FALSE
    )

    selected_covariate <- eventReactive(
      c(input$update_plot, initial_load()), {
        input$select_cov
      },
      ignoreNULL = FALSE
    )

    # Reactive for plot height
    plot_height <- reactive({
      year_display_val <- year_display()
      if (year_display_val == "All Years") {
        return(375)  # Reset to default height when viewing all years
      } else {
        years_selected_val <- years_selected()
        req(years_selected_val)
        nyears <- length(years_selected_val)
        if (nyears > 2) {
          return(375 + (nyears-2)*150)
        } else {
          return(375)
        }
      }
    })

    # Adjust all data frames for each plot (DOY and TI predictions, and Observed)
    # Modify the filtered data reactives to respond to either update button or initial load
    filtered_data_pred <- eventReactive(
      c(input$update_plot, initial_load()), {
        # Get the current values that will be used for filtering
        selected_cov_val <- input$select_cov
        select_spp_val <- input$select_spp
        select_rear_val <- input$select_rear
        year_display_val <- input$year_display
        select_years_val <- input$select_years

        # extract list dataframe for covariate
        selected_list <- if (selected_cov_val == "Day-of-year (DOY)") model_output$doy_pred else if(selected_cov_val == "Temperature (°C)") model_output$temp_pred

        # Filter based on inputs
        filtered_list <- purrr::map(selected_list, ~ .x %>%
                                      dplyr::filter(
                                        species %in% c(select_spp_val),
                                        rear_type %in% c(select_rear_val),
                                        if (year_display_val == "Year" && !is.null(select_years_val)) year %in% c(select_years_val) else TRUE
                                      ) %>%
                                      dplyr::mutate(transport = as.factor(transport))
        )

        # Combine filtered list into a single data frame
        filtered_data <- dplyr::bind_rows(filtered_list)

        return(filtered_data)
      },
      ignoreNULL = FALSE
    )

    filtered_data_ti <- eventReactive(
      c(input$update_plot, initial_load()), {
        # Get the current values that will be used for filtering
        selected_cov_val <- input$select_cov
        select_spp_val <- input$select_spp
        select_rear_val <- input$select_rear
        year_display_val <- input$year_display
        select_years_val <- input$select_years

        # extract list for covariate
        selected_list <- if (selected_cov_val == "Day-of-year (DOY)") model_output$doy_ti else if(selected_cov_val == "Temperature (°C)") model_output$temp_ti

        # Filter based on inputs
        filtered_list <- purrr::map(selected_list, ~ .x %>%
                                      dplyr::filter(
                                        species %in% c(select_spp_val),
                                        rear_type %in% c(select_rear_val),
                                        if (year_display_val == "Year" && !is.null(select_years_val)) year %in% c(select_years_val) else TRUE
                                      )
        )

        # Combine filtered list into a single data frame
        filtered_data <- dplyr::bind_rows(filtered_list)
        return(filtered_data)
      },
      ignoreNULL = FALSE
    )

    filtered_observed_data <- eventReactive(
      c(input$update_plot, initial_load()), {
        # Get the current values that will be used for filtering
        select_spp_val <- input$select_spp
        select_rear_val <- input$select_rear
        year_display_val <- input$year_display
        select_years_val <- input$select_years

        # Filter each list element based on user selections
        filtered_list <- purrr::map(model_output$observed, ~ .x %>%
                                      dplyr::filter(
                                        species %in% c(select_spp_val),
                                        rear_type %in% c(select_rear_val),
                                        if (year_display_val == "Year" && !is.null(select_years_val)) year %in% c(select_years_val) else TRUE
                                      ) %>%
                                      dplyr::mutate(transport = as.factor(transport))

        )

        # Combine filtered list into a single data frame
        filtered_observed_data <- as.data.frame(dplyr::bind_rows(filtered_list))
        return(filtered_observed_data)
      },
      ignoreNULL = FALSE
    )

    # Return the filtered data as a reactive list
    return(list(
      filtered_data_pred = reactive(filtered_data_pred),
      filtered_data_ti = reactive(filtered_data_ti),
      filtered_observed_data = reactive(filtered_observed_data),
      year_display = reactive(year_display),
      plot_height = reactive(plot_height),
      years_selected = reactive(years_selected),
      selected_covariate = reactive(selected_covariate),
      get_years = reactive(get_years),
      update_button = reactive(update_clicked())
    ))
  })
}

## To be copied in the UI
# mod_main_submodule_dataselect_ui("dataselect_1")

## To be copied in the server
# mod_main_submodule_dataselect_server("dataselect_1")
