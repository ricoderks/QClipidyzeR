#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      bs4Dash::column(
        width = 12,
        h3("Data files"),
        HTML("<p>There are 2 requirements on the data files from the SLA software :<br>
                  <ul>
                  <li>The name should contain the batch number as: Batch 1. Batch is case insensitive and  ' -_' is allowed as separator.</li>
                  <li>2 columns should be present <b>SampleID</b> and <b>NormType</b>. <b>NormType</b> will contain the information about the
                  sample type, i.e. 'Pooled sample' or 'Samples'.</li></p>")
      )
    )
  )
}

#' help Server Functions
#'
#' @noRd
mod_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_help_ui("help_1")

## To be copied in the server
# mod_help_server("help_1")
