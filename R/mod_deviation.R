#' deviation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_deviation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(inputId = ns("select_deviation"),
                           label = "Show deviation:",
                           choices = c("Overall" = "overall",
                                       "Per batch" = "batch"),
                           selected = "overall")
      ),
      shiny::plotOutput(outputId = ns("deviation_plot"))
    )
  )
}

#' deviation Server Functions
#'
#' @noRd
mod_deviation_server <- function(id, r, sheet){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$deviation_plot <- shiny::renderPlot({
      req(r$deviation_data,
          input$select_deviation)

      deviation_plot(data = r$deviation_data[[sheet]],
                     trend = input$select_deviation)
    })

  })
}
