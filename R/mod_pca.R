#' pca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information.
#' @param sheet integer, indicates what sheet to use.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets prettyCheckboxGroup
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
        uiOutput(outputId = ns("ui_scores"))
        # plotlyOutput(outputId = ns("scores"))
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

    output$ui_scores <- renderUI({
      req(r$clean_data,
          r$pca_model)

      # get the batch information to create the checkboxes
      meta_data <- r$clean_data[[sheet]][, r$meta_columns]
      batches <- as.integer(unique(meta_data$batch))
      # create the CSS for coloring the checkboxes
      CSS <- create_cb_css()

      tagList(
        tags$head(tags$style(HTML(CSS))),
        fluidRow(
          column(width = 3,
                 prettyCheckboxGroup(
                   inputId = ns("batch"),
                   label = "Batch:",
                   icon = icon("check"),
                   choiceNames = as.character(batches),
                   choiceValues = batches,
                   selected = as.character(batches)
                 ),
                 prettyCheckboxGroup(
                   inputId = ns("sample_type"),
                   label = "Sample type:",
                   icon = icon("check"),
                   choiceNames = c("Pooled sample (●)", "Samples(▲)"),
                   choiceValues = c("Pooled sample", "Samples"),
                   selected = c("Pooled sample", "Samples")
                 )
          ),
          column(width = 9,
                 plotlyOutput(outputId = ns("scores"))
          )
        )
      )
    })

    output$scores <- renderPlotly({
      req(r$clean_data,
          r$pca_model)

      scores_plot(model = r$pca_model[[sheet]],
                  meta_data = r$clean_data[[sheet]][, r$meta_columns],
                  batch = input$batch,
                  sample_type = input$sample_type)
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
