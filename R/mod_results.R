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
          plotlyOutput(outputId = ns("plot_sheet1"))
        ),
        tabPanel(
          title = r$sheet_names_short[2],
          plotlyOutput(outputId = ns("plot_sheet2"))
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
          plotlyOutput(outputId = ns("plot_sheet5"))
        ),
        tabPanel(
          title = r$sheet_names_short[6],
          plotlyOutput(outputId = ns("plot_sheet6"))
        )
      )
    })

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

  }) # end server module
}
