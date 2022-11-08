#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,r,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import DT
#' @importFrom shiny NS tagList
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(outputId = ns("table1"))
  )
}

#' data Server Functions
#'
#' @noRd
mod_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table1 <- DT::renderDataTable({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data()$data[[1]]

        show_data
      }
    })
  })
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
