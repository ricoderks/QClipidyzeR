#' rsd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information.
#' @param sheet integer(1), indicates what sheet to use.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rsd_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(outputId = ns("show_title")),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(inputId = ns("select_histogram"),
                           label = "Show histogram:",
                           choices = c("Overall" = "overall",
                                       "Per batch" = "batch"),
                           selected = "overall")
      ),
      shiny::uiOutput(outputId = ns("show_rsd"))
    )
  )
}

#' rsd Server Functions
#'
#' @noRd
mod_rsd_server <- function(id, r, sheet){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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


    output$show_rsd <- shiny::renderUI({
      shiny::req(r$rsd_data)

      if(sheet != 3 & sheet != 4) {
        shiny::tagList(
          # plotly is not working??
          # plotly::plotlyOutput(outputId = ns("rsd_plot"))
          shiny::plotOutput(outputId = ns("rsd_plot"),
                            height = "100%")
        )
      } else {
        shiny::tagList(
          DT::DTOutput(outputId = ns("rsd_table"))
        )
      }
    })


    output$rsd_plot <- shiny::renderPlot({#plotly::renderPlotly({
      shiny::req(r$rsd_data,
                 input$select_histogram)

      if(!is.null(r$rsd_data[[sheet]])) {
        p <- create_rsd_hist(data = r$rsd_data[[sheet]],
                             batch = input$select_histogram)
        p
      }
    })


    output$rsd_table <- DT::renderDT({
      shiny::req(r$rsd_data)

      if(!is.null(r$rsd_data[[sheet]])) {
        table_DT <- create_rsd_table(data = r$rsd_data[[sheet]],
                                     batch = input$select_histogram)
        table_DT
      }
    })

  })
}
