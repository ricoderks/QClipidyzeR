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
    shiny::uiOutput(outputId = ns("show_rsd"))
  )
}

#' rsd Server Functions
#'
#' @noRd
mod_rsd_server <- function(id, r, sheet){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$show_rsd <- shiny::renderUI({
      shiny::req(r$rsd_data)

      print("Render plot or table")
      print(sheet)
      if(sheet != 3 & sheet != 4) {
        print("plot")
        shiny::tagList(
          plotly::plotlyOutput(outputId = ns("rsd_plot"))
        )
      } else {
        print("table")
        shiny::tagList(
          DT::DTOutput(outputId = ns("rsd_table"))
        )
      }
    })


    output$rsd_plot <- plotly::renderPlotly({
      shiny::req(r$rsd_data)

      print("Create the plot 1")
      print(sheet)

      if(!is.null(r$rsd_data[[sheet]])) {
        # if(!(sheet == 3 | sheet == 4)) {
          print("Create the plot 2")
          p <- create_rsd_hist(data = r$rsd_data[[sheet]])
          p
        # }

      }
    })


    output$rsd_table <- DT::renderDT({
      shiny::req(r$rsd_data)

      if(!is.null(r$rsd_data[[sheet]])) {
        # if(sheet == 3 | sheet == 4) {
          table_DT <- create_rsd_table(data = r$rsd_data[[sheet]])

          table_DT
        # }
      }
    })

  })
}
