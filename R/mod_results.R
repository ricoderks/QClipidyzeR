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
          mod_pca_ui(id = ns("pca_sheet1"))
        ),
        tabPanel(
          title = r$sheet_names_short[2],
          fluidRow(
            bs4Dash::box(
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("plot_sheet2"))
            ),
          ),
          mod_pca_ui(id = ns("pca_sheet2"))
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
          fluidRow(
            bs4Dash::box(
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("plot_sheet5"))
            ),
          ),
          mod_pca_ui(id = ns("pca_sheet5"))
        ),
        tabPanel(
          title = r$sheet_names_short[6],
          fluidRow(
            bs4Dash::box(
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("plot_sheet6"))
            ),
          ),
          mod_pca_ui(id = ns("pca_sheet6"))
        )
      )
    })

    ##### RSD stuff #####
    output$plot_sheet1 <- renderPlotly({
      req(r$rsd_data)

      # create the plot
      create_rsd_hist(data = r$rsd_data[[1]])
    })

    output$plot_sheet2 <- renderPlotly({
      req(r$rsd_data)

      # create the plot
      create_rsd_hist(data = r$rsd_data[[2]])
    })

    output$table_sheet3 <- DT::renderDT({
      req(r$rsd_data)

      table_DT <- create_rsd_table(data = r$rsd_data[[3]])

      table_DT
    })

    output$table_sheet4 <- DT::renderDT({
      req(r$rsd_data)

      table_DT <- create_rsd_table(data = r$rsd_data[[4]])

      table_DT
    })

    output$plot_sheet5 <- renderPlotly({
      req(r$rsd_data)

      # create the plot
      create_rsd_hist(data = r$rsd_data[[5]])
    })

    output$plot_sheet6 <- renderPlotly({
      req(r$rsd_data)

      # create the plot
      create_rsd_hist(data = r$rsd_data[[6]])
    })
    #####################

    ##### PCA stuff #####
    mod_pca_server(id = "pca_sheet1",
                   r = r,
                   sheet = 1)

    mod_pca_server(id = "pca_sheet2",
                   r = r,
                   sheet = 2)

    mod_pca_server(id = "pca_sheet3",
                   r = r,
                   sheet = 3)

    mod_pca_server(id = "pca_sheet4",
                   r = r,
                   sheet = 4)

    mod_pca_server(id = "pca_sheet5",
                   r = r,
                   sheet = 5)

    mod_pca_server(id = "pca_sheet6",
                   r = r,
                   sheet = 6)
    #####################


  }) # end server module
}
