#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information
#' @noRd
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS tagList
#'
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("data_tabs"))
  )
}

#' data Server Functions
#'
#' @importFrom DT renderDT
#' @import bs4Dash
#'
#' @noRd
mod_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$data_tabs <- renderUI({
      bs4Dash::tabsetPanel(
        id = "data_tabset",
        tabPanel(
          title = r$sheet_names_short[1],
          mod_tables_ui(id = ns("data_sheet1"))
        ),
        tabPanel(
          title = r$sheet_names_short[2],
          mod_tables_ui(id = ns("data_sheet2"))
        ),
        tabPanel(
          title = r$sheet_names_short[3],
          mod_tables_ui(id = ns("data_sheet3"))
        ),
        tabPanel(
          title = r$sheet_names_short[4],
          mod_tables_ui(id = ns("data_sheet4"))
        ),
        tabPanel(
          title = r$sheet_names_short[5],
          mod_tables_ui(id = ns("data_sheet5"))
        ),
        tabPanel(
          title = r$sheet_names_short[6],
          mod_tables_ui(id = ns("data_sheet6"))
        )
      )
    })

    mod_tables_server(id = "data_sheet1",
                      r = r,
                      sheet = 1)

    mod_tables_server(id = "data_sheet2",
                      r = r,
                      sheet = 2)

    mod_tables_server(id = "data_sheet3",
                      r = r,
                      sheet = 3)

    mod_tables_server(id = "data_sheet4",
                      r = r,
                      sheet = 4)

    mod_tables_server(id = "data_sheet5",
                      r = r,
                      sheet = 5)

    mod_tables_server(id = "data_sheet6",
                      r = r,
                      sheet = 6)


  }) # end module server
}
