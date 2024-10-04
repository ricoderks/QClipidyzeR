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
#' @import plotly
#'
mod_pca_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(outputId = ns("show_title")),
    bslib::layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 3fr"),
      bslib::card(
        shinyWidgets::prettyCheckboxGroup(
          inputId = ns("batch"),
          label = "Batch:",
          icon = shiny::icon("check"),
          choiceNames = c("1", "2"),
          choiceValues = c("1", "2"),
          selected = c("1", "2"),
          inline = TRUE
        ),
        shinyWidgets::prettyCheckboxGroup(
          inputId = ns("sample_type"),
          label = "Sample type:",
          icon = shiny::icon("check"),
          choiceNames = c("QC samples (●)", "Samples (▲)"),
          choiceValues = c("qc", "sample"),
          selected = c("qc", "sample"),
          inline = TRUE
        )
      ),
      bslib::card(
        plotly::plotlyOutput(outputId = ns("scores")),
        plotly::plotlyOutput(outputId = ns("loadings"))
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

    shiny::observe({
      shiny::req(r$clean_data,
                 r$pca_model)

      meta_data <- r$clean_data[[sheet]][, r$meta_columns]
      batches <- as.integer(unique(meta_data$batch))

      shinyWidgets::updatePrettyCheckboxGroup(
        inputId = "batch",
        choiceNames = as.character(batches),
        choiceValues = batches,
        selected = as.character(batches),
        inline = TRUE,
        prettyOptions = list(
          icons = shiny::icon("check")
        )
      )
    })


    output$show_title <- shiny::renderUI({
      title <- switch(
        sheet,
        "Species concentration",
        "Species composition",
        "Class concentration",
        "Class composition",
        "FA concentration",
        "FA composition"
      )

      shiny::h5(title)
    })


    # output$ui_scores <- shiny::renderUI({
    #   req(r$clean_data,
    #       r$pca_model)
    #
    #   # get the batch information to create the checkboxes
    #   meta_data <- r$clean_data[[sheet]][, r$meta_columns]
    #   batches <- as.integer(unique(meta_data$batch))
    #   print(batches)
    #   # create the CSS for coloring the checkboxes
    #   CSS <- create_cb_css()
    #
    #   shiny::tagList(
    #     tags$head(tags$style(HTML(CSS))),
    #
    #     shinyWidgets::prettyCheckboxGroup(
    #       inputId = "batch",
    #       label = "Batch:",
    #       icon = icon("check"),
    #       choiceNames = as.character(batches),
    #       choiceValues = batches,
    #       selected = as.character(batches)
    #     ),
    #     shinyWidgets::prettyCheckboxGroup(
    #       inputId = ns("sample_type"),
    #       label = "Sample type:",
    #       icon = icon("check"),
    #       choiceNames = c("Pooled sample (●)", "Samples(▲)"),
    #       choiceValues = c("Pooled sample", "Samples"),
    #       selected = c("Pooled sample", "Samples")
    #     )
    #   )
    # })

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
