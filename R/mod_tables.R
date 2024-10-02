#' tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information.
#' @param sheet integer, indicates what sheet to use.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tables_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
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
      req(r$clean_data)

      if(!is.null(r$clean_data)) {
        show_data <- r$clean_data[[sheet]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )
  })
}
