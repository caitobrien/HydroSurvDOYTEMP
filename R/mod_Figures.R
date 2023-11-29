#' Figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Figures_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(

      fluidRow(
        h2("Header"),
        shinydashboard::box(
          title = "X",
          width = 6,
          plotOutput(outputId = ns("plot1"))),

        shinydashboard::box(
          title = "X2",
          width = 6,
          plotOutput(outputId = "plot2")
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "X",
          width = 6,
          plotOutput(outputId = "plot3")),

        shinydashboard::box(
          title = "X2",
          width = 6,
          plotOutput(outputId = "plot4")
        )
      )
    )
  )
}

#' Figures Server Functions
#'
#' @noRd
mod_Figures_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

     # plot1 <- reactive({plot_fx(spp = input$select_spp,
     #                 rtype = input$select_rear,
     #                 cov = input$select_cov,
     #                 SAR_year = input$select_year)})

     output$plot1 <- renderPlot({
       df<-data %>%
         filter(species == input$select_spp)

         ggplot(df, aes( x= doy, color = as.factor(transport), group = year)) +
         geom_point(aes(y =SAR, fill = as.factor(transport)))+
         geom_jitter(aes(y =sar.pit, shape = as.factor(transport)), alpha = .7)+
         #geom_lineribbon( aes(y = SAR, ymin =SAR.lo, ymax = SAR.hi, fill = as.factor(transport)), alpha = .25) +
         labs( x = "Date", y = "SAR", color = NULL,
               fill = NULL, shape = NULL,
               title = "Predicted SAR versus observed SAR from PIT tag recoveries"
               ) +
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
    })

    output$plot2 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })

    output$plot3 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })

    output$plot4 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })

  })
}

