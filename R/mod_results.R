#' plots UI Function
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
#' @importFrom DT DTOutput renderDT
#'
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("plot_tabs"))
  )
}

#' plots Server Functions
#'
#' @noRd
mod_results_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_tabs <- renderUI({
      bs4Dash::tabsetPanel(
        id = "plot_tabset",
        tabPanel(
          title = r$sheet_names_short[1],
          fluidRow(
            bs4Dash::box(
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("plot_sheet1"))
            ),
          ),
          fluidRow(
            bs4Dash::box(
              width = 5,
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("scores_sheet1"))
            ),
            bs4Dash::box(
              width = 5,
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("loadings_sheet1"))
            ),
            bs4Dash::box(
              width = 2,
              collapsible = FALSE,
              headerBorder = FALSE,
              plotOutput(outputId = ns("sumfit_sheet1"))
            )
          )
        ),
        tabPanel(
          title = r$sheet_names_short[2],
          plotlyOutput(outputId = ns("plot_sheet2")),
          plotlyOutput(outputId = ns("scores_sheet2"))
        ),
        tabPanel(
          title = r$sheet_names_short[3],
          DT::DTOutput(outputId = ns("table_sheet3"))
        ),
        tabPanel(
          title = r$sheet_names_short[4],
          DT::DTOutput(outputId = ns("table_sheet4"))
        ),
        tabPanel(
          title = r$sheet_names_short[5],
          plotlyOutput(outputId = ns("plot_sheet5")),
          plotlyOutput(outputId = ns("scores_sheet5"))
        ),
        tabPanel(
          title = r$sheet_names_short[6],
          plotlyOutput(outputId = ns("plot_sheet6")),
          plotlyOutput(outputId = ns("scores_sheet6"))
        )
      )
    })

    ##### RSD stuff #####
    output$plot_sheet1 <- renderPlotly({
      req(r$clean_data)

      # calculate all RSD values
      plot_data <- calc_rsd(data = r$clean_data$clean_data[[1]],
                            meta_data = r$meta_columns,
                            lipid_class = TRUE)

      # create the plot
      create_rsd_hist(data = plot_data)
    })

    output$plot_sheet2 <- renderPlotly({
      req(r$clean_data)

      # calculate all RSD values
      plot_data <- calc_rsd(data = r$clean_data$clean_data[[2]],
                            meta_data = r$meta_columns,
                            lipid_class = TRUE)

      # create the plot
      create_rsd_hist(data = plot_data)
    })

    output$table_sheet3 <- DT::renderDT({
      req(r$clean_data)

      # calculate all RSD values
      table_data <- calc_rsd(data = r$clean_data$clean_data[[3]],
                             meta_data = r$meta_columns,
                             lipid_class = FALSE)

      table_DT <- create_rsd_table(data = table_data)

      table_DT
    })

    output$table_sheet4 <- DT::renderDT({
      req(r$clean_data)

      # calculate all RSD values
      table_data <- calc_rsd(data = r$clean_data$clean_data[[4]],
                             meta_data = r$meta_columns,
                             lipid_class = FALSE)

      table_DT <- create_rsd_table(data = table_data)

      table_DT
    })

    output$plot_sheet5 <- renderPlotly({
      req(r$clean_data)

      # calculate all RSD values
      plot_data <- calc_rsd(data = r$clean_data$clean_data[[5]],
                            meta_data = r$meta_columns,
                            lipid_class = TRUE)

      # create the plot
      create_rsd_hist(data = plot_data)
    })

    output$plot_sheet6 <- renderPlotly({
      req(r$clean_data)

      # calculate all RSD values
      plot_data <- calc_rsd(data = r$clean_data$clean_data[[6]],
                            meta_data = r$meta_columns,
                            lipid_class = TRUE)

      # create the plot
      create_rsd_hist(data = plot_data)
    })
    #####################

    ##### PCA stuff #####
    output$scores_sheet1 <- renderPlotly({
      req(r$clean_data)

      # do pca analysis
      mod <- do_pca(data = r$clean_data$clean_data[[1]],
                    meta_data = r$meta_columns)

      scores_plot(model = mod,
                  meta_data = r$clean_data$clean_data[[1]][, r$meta_columns])
    })

    output$loadings_sheet1 <- renderPlotly({
      req(r$clean_data)

      # do pca analysis
      mod <- do_pca(data = r$clean_data$clean_data[[1]],
                    meta_data = r$meta_columns)


      loadings_plot(model = mod)
    })

    output$sumfit_sheet1 <- renderPlot({
      req(r$clean_data)

      # do pca analysis
      mod <- do_pca(data = r$clean_data$clean_data[[1]],
                    meta_data = r$meta_columns)

      sumfit_plot(model = mod)
    })

    output$scores_sheet2 <- renderPlotly({
      req(r$clean_data)

      # do pca analysis
      mod <- do_pca(data = r$clean_data$clean_data[[2]],
                    meta_data = r$meta_columns)

      scores_plot(model = mod,
                  meta_data = r$clean_data$clean_data[[2]][, r$meta_columns])
    })

    output$scores_sheet5 <- renderPlotly({
      req(r$clean_data)

      # do pca analysis
      mod <- do_pca(data = r$clean_data$clean_data[[5]],
                    meta_data = r$meta_columns)

      scores_plot(model = mod,
                  meta_data = r$clean_data$clean_data[[5]][, r$meta_columns])
    })

    output$scores_sheet6 <- renderPlotly({
      req(r$clean_data)

      # do pca analysis
      mod <- do_pca(data = r$clean_data$clean_data[[6]],
                    meta_data = r$meta_columns)

      scores_plot(model = mod,
                  meta_data = r$clean_data$clean_data[[6]][, r$meta_columns])
    })

    #####################


  }) # end server module
}
