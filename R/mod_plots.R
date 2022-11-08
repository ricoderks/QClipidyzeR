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
#'
mod_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("plot_tabs"))
  )
}

#' plots Server Functions
#'
#' @noRd
mod_plots_server <- function(id, r){
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
        ),
        tabPanel(
          title = r$sheet_names_short[3],
        ),
        tabPanel(
          title = r$sheet_names_short[4],
        ),
        tabPanel(
          title = r$sheet_names_short[5],
        ),
        tabPanel(
          title = r$sheet_names_short[6],
        )
      )
    })

    output$plot_sheet1 <- renderPlotly({
      my_df <- data.frame(x = 1:10,
                          y = rnorm(10))
      plot_ly(my_df, x = ~x, y = ~y, type = "scatter")
    })
  })
}
