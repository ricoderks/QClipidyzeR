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

  )
}

#' deviation Server Functions
#'
#' @noRd
mod_deviation_server <- function(id, r, sheet){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
