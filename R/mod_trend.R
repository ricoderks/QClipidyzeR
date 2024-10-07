#' trend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_sidebar sidebar
mod_trend_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(inputId = ns("select_trend"),
                           label = "Show trend:",
                           choices = c("Overall" = "overall",
                                       "Per batch" = "batch"),
                           selected = "overall")
      ),
      shiny::plotOutput(outputId = ns("trend_plot"))
    )
  )
}

#' trend Server Functions
#'
#' @noRd
mod_trend_server <- function(id, r, sheet){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$trend_plot <- shiny::renderPlot({
      shiny::req(r$trend_data,
                 input$select_trend)

      trend_plot(data = r$trend_data[[sheet]],
                 trend = input$select_trend)

    })
  })
}
