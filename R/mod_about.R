#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::column(
        width = 12,
        h3("Issues"),
        p("If you have any ideas to extend this shiny app please send me an email. If you have any issue please send me an email or go to the ",
          a("issue tracker.", href = "http://github.com/ricoderks/QClipidyzeR/issues", target = "_blank"),
          "Cheers, Rico"))
    ),
    fluidRow(
      bs4Dash::column(
        width = 12,
        h3("Session info"),
        verbatimTextOutput(ns("about_session")))
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

    output$about_session <- renderPrint({
      session_info()
    })
  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
