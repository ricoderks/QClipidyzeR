#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card
mod_about_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      shiny::h3("Issues"),
      shiny::p("If you have any ideas to extend this shiny app please send me an email. If you have any issue please send me an email or go to the ",
               shiny::a("issue tracker.", href = "http://github.com/ricoderks/QClipidyzeR/issues", target = "_blank"),
               "Cheers, Rico"),
      shiny::hr(),
      shiny::h3("Session info"),
      shiny::verbatimTextOutput(
        outputId = ns("about_session")
      )
    )
  )
}

#' about Server Functions
#'
#' @importFrom sessioninfo session_info
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$about_session <- shiny::renderPrint({
      sessioninfo::session_info()
    })
  })
}
