#' tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(outputId = ns("data_sheet"))
  )
}

#' tables Server Functions
#'
#' @noRd
mod_tables_server <- function(id, r, sheet){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$data_sheet <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$clean_data[[sheet]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )
  })
}

## To be copied in the UI
# mod_tables_ui("tables_1")

## To be copied in the server
# mod_tables_server("tables_1")
