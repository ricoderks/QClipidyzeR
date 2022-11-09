#' pca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bs4Dash
#' @import plotly
#'
mod_pca_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        width = 5,
        collapsible = FALSE,
        headerBorder = FALSE,
        plotlyOutput(outputId = ns("scores"))
      ),
      bs4Dash::box(
        width = 5,
        collapsible = FALSE,
        headerBorder = FALSE,
        plotlyOutput(outputId = ns("loadings"))
      ),
      bs4Dash::box(
        width = 2,
        collapsible = FALSE,
        headerBorder = FALSE,
        plotOutput(outputId = ns("sumfit"))
      )
    )
  )
}

#' pca Server Functions
#'
#' @noRd
mod_pca_server <- function(id, r, sheet){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$scores <- renderPlotly({
      req(r$clean_data,
          r$pca_model)


      scores_plot(model = r$pca_model[[sheet]],
                  meta_data = r$clean_data[[sheet]][, r$meta_columns])
    })

    output$loadings <- renderPlotly({
      req(r$clean_data,
          r$pca_model)

      loadings_plot(model = r$pca_model[[sheet]])
    })

    output$sumfit <- renderPlot({
      req(r$clean_data)

      sumfit_plot(model = r$pca_model[[sheet]])
    })
  })
}
