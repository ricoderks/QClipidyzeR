#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
#'
mod_data_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(outputId = ns("data_tabs"))
  )
}

#' data Server Functions
#'
#' @importFrom bslib navset_card_tab nav_panel card
#' @importFrom shiny uiOutput
#'
#' @noRd
mod_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$data_tabs <- shiny::renderUI({
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = r$sheet_names_short[1],
          bslib::card(
            bslib::card_body(
              mod_tables_ui(id = ns("data_sheet1"))
            )
          )
        ),
        bslib::nav_panel(
          title = r$sheet_names_short[2],
          bslib::card(
            mod_tables_ui(id = ns("data_sheet2"))
          )
        ),
        bslib::nav_panel(
          title = r$sheet_names_short[3],
          bslib::card(
            mod_tables_ui(id = ns("data_sheet3"))
          )
        ),
        bslib::nav_panel(
          title = r$sheet_names_short[4],
          bslib::card(
            mod_tables_ui(id = ns("data_sheet4"))
          )
        ),
        bslib::nav_panel(
          title = r$sheet_names_short[5],
          bslib::card(
            mod_tables_ui(id = ns("data_sheet5"))
          )
        ),
        bslib::nav_panel(
          title = r$sheet_names_short[6],
          bslib::card(
            mod_tables_ui(id = ns("data_sheet6"))
          )
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
