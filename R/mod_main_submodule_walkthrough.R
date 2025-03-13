#' main_submodule_walkthrough UI Function
#'
#' @description A working shiny submodule that allows the user to walk through the steps of a shiny app with results and supplementary material
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_main_submodule_walkthrough_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        br(),
        h5("As you explore this application, below outlines examples of trends you may notice in the data. Click on the boxes to expand and see more information."),
      )
    ),

    # information to add
    fluidRow(
      shinydashboard::box(
        title = "Exploring predicted survival, you may notice how:",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        HTML("<ul>
                      <li>In many years, SAR survival declines through the smolt outmigration season</li>
               <li>Often, SAR survival is higher in in-river migration smolts early in the migration season, whereas later in the season transported smolts have higher SAR survival.</li>
               </ul>"),
        # SAR example1 plot
        fluidRow(
          width = 12,
          column(width = 1),
          column(
            width = 10,
            plotOutput(outputId = ns("SAR_example1_plot"))
          ),
          column(width = 1)
        ),
        br(),
        HTML("<ul>
                      <li>In some years, SAR survival was relatively flat through the season (1994), compared to others (2000 & 2006).</li>
             </ul>"),
        # SAR example2 plot
        fluidRow(
          width = 12,
          column(width = 1),
          column(
            width = 10,
            plotOutput(outputId = ns("SAR_example2_plot"))
          ),
          column(width = 1)
        ),
        br(),
        HTML("<ul>
              <li>When comparing between species, you may notice that there are more years with bell-shaped curved patterns of SAR in Steelhead than in Chinook Salmon.</li>
              </ul>"),
        # SAR example3 plot
        fluidRow(
          width = 12,
          column(width = 1),
          column(
            width = 10,
            plotOutput(outputId = ns("SAR_example3_plot"))
          ),
          column(width = 1)
        ),

        # summary text
        br(),
        HTML("In addition, some questions you may want to ask yourself as you explore the Shiny app:
               <ul>
                      <li>Why are SAR survival patterns different year to year?</li>
                      <li>How does the T:B index differ between Chinook Salmon and Steelhead?</li>
                      <li>What other freshwater, estuarine and marine conditions may be affecting their survival?</li>
                      <li>How much confidence do we have in differences in SAR survival between transported and in-river migrating fish?</li>
                      <li>And, would you consider these improvements to survival sufficient?</li>
                    </ul>
               To provide additional context and support for our predicted survival estimates, explore the model parameter estimates below.")
      )
    ),
    fluidRow(
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Model parameter estimates",
        "Using a Bayesian binomial model, we estimated survival for each species and rear type based on the covariates: day-of-year or temperature.
        The parameter estimates below provide insight into the relationship between survival
        and each variable within the model. Similar to Scheuerell et al. (2009), we included a quadratic term for each covariate to allow for a non-linear relationship.",
        br(),
        br(),
        "Tabs below show the estimated coefficients for each model output:",
        br(),
        tabsetPanel(
          id = ns("modelTabs"),
          tabPanel(
            "Natural-origin Chinook",
            shiny::includeHTML(system.file("app/www/table_mod_natural_chinook_doy.html", package = "HydroSurvDOYTEMP")),
            br(),
            shiny::includeHTML(system.file("app/www/table_mod_natural_chinook_temp.html", package = "HydroSurvDOYTEMP"))
          ),
          tabPanel(
            "Hatchery-origin Chinook",
            shiny::includeHTML(system.file("app/www/table_mod_hatchery_chinook_doy.html", package = "HydroSurvDOYTEMP")),
            br(),
            shiny::includeHTML(system.file("app/www/table_mod_hatchery_chinook_temp.html", package = "HydroSurvDOYTEMP"))
          ),
          tabPanel(
            "Natural-origin Steelhead",
            shiny::includeHTML(system.file("app/www/table_mod_natural_steelhead_doy.html", package = "HydroSurvDOYTEMP")),
            br(),
            shiny::includeHTML(system.file("app/www/table_mod_natural_steelhead_temp.html", package = "HydroSurvDOYTEMP"))
          ),
          tabPanel(
            "Hatchery-origin Steelhead",
            shiny::includeHTML(system.file("app/www/table_mod_hatchery_steelhead_doy.html", package = "HydroSurvDOYTEMP")),
            br(),
            shiny::includeHTML(system.file("app/www/table_mod_hatchery_steelhead_temp.html", package = "HydroSurvDOYTEMP"))
          )
        ),
        br(),
        br(),
        "Conditional effects highlight the individual covariates included within each model if all other variables are held constant.",
        "To plot conditional effects, select a covariate and species below:",
        br(),

        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("select_cov"),
              label = "Select covariate",
              choices = c("Day-of-year (DOY)", "Temperature (°C)"),
              selected = "Day-of-year (DOY)",
              multiple = FALSE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("select_spp"),
              label = "Select species",
              choices = c("Chinook", "Steelhead"),
              selected = "Chinook",
              multiple = FALSE
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("select_rear"),
              label = "Select rear type",
              choices = c("Natural-origin", "Hatchery"),
              selected = "Hatchery",
              multiple = FALSE
            )
          ),

           fluidRow(
            column(
              width = 12,
              offset = 1,
              imageOutput(outputId = ns("condeff_plot"), width = "100%", height = "auto")
          )
           )
        )
      )
    )
  )
}

#' main_submodule_walkthrough Server Functions
#'
#' @noRd
mod_main_submodule_walkthrough_server <- function(id, model_output) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$SAR_example1_plot <- renderPlot({
      example1_pred_data <- model_output$doy_pred[[1]] #pull natural-origin chinook
      example1_obs_data <- model_output$observed_data[[1]]

      fct_SAR_all_years_plot(data_pred = example1_pred_data, observed_data = example1_obs_data, selected_covariate = "Day-of-year (DOY)", observed = "no") +
        ggplot2::ggtitle("Predicted SAR compared to smolt outmigration season",
          subtitle = "Years: 1993 to 2021; Species: Chinook salmon; Rear type: Natural-origin"
        )
    })

    output$SAR_example2_plot <- renderPlot({

      example2_pred_data<- model_output$doy_pred[[1]] %>% dplyr::filter( year %in% c(1994, 2000, 2006))
      example2_obs_data<- model_output$observed[[1]] %>% dplyr::filter( year %in% c(1994, 2000, 2006))


      fct_SAR_by_year_plot(data_pred = example2_pred_data, observed_data = example2_obs_data, selected_covariate = "Day-of-year (DOY)", observed = "no", legend_location = "right") +
        ggplot2::ggtitle("Predicted SAR compared to smolt outmigration season",
          subtitle = "Years: 1994, 2000, 2006; Species: Chinook salmon; Rear type: Natural-origin"
        )
    })

    output$SAR_example3_plot <- renderPlot({
      # Extract datasets 1 and 3, bind them together, and filter for year 2018
      example3_pred_data <- dplyr::bind_rows(model_output$doy_pred[[1]], model_output$doy_pred[[3]]) %>%
        dplyr::filter(year == 2018)

      example3_obs_data <- dplyr::bind_rows(model_output$observed[[1]], model_output$observed[[3]]) %>%
        dplyr::filter(year == 2018)

      fct_SAR_by_year_plot(data_pred = example3_pred_data, observed_data = exampled3_obs_data, selected_covariate = "Day-of-year (DOY)",  observed = "no", legend_location = "right") +
        ggplot2::ggtitle("Predicted SAR compared to smolt outmigration season",
          subtitle = "Year: 2018, Species: Chinook salmon & Steelhead; Rear type: Natural-origin"
        )
    })

    output$condeff_plot <- renderImage({
      # Construct file name based on input selections
      file_name <- paste0(
        "mod_", ifelse(input$select_rear == "Natural-origin", "natural", "hatchery"), "_", tolower(input$select_spp), "_",
        ifelse(input$select_cov == "Day-of-year (DOY)", "doy", "temp"), ".png"
      )

      # Construct full path to the file
      file_path <- system.file("app/www/cond_eff_plots", file_name, package = "HydroSurvDOYTEMP")

      # Return a list suitable for renderImage
      list(
        src = file_path,
        contentType = "image/png",
        alt = "Conditional effects plot",
        deleteFile = FALSE
      )
    })

  })
}

## To be copied in the UI
# mod_main_submodule_walkthrough_ui("main_submodule_walkthrough_1")

## To be copied in the server
# mod_main_submodule_walkthrough_server("main_submodule_walkthrough_1")
