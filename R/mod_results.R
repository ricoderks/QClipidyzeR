#' plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bs4Dash
#' @import plotly
#' @importFrom DT DTOutput renderDT
#'
mod_results_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Species conc.",
        bslib::card(
          mod_rsd_ui(id = ns("rsd_sheet1"))
        )
      ),
      bslib::nav_panel(
        title = "Species comp.",
        bslib::card(
          bslib::card_body(
            mod_rsd_ui(id = ns("rsd_sheet2"))
          )
        )
      ),
      bslib::nav_panel(
        title = "Classes conc.",
        bslib::card(
          bslib::card_body(
            mod_rsd_ui(id = ns("rsd_sheet3"))
          )
        )
      ),
      bslib::nav_panel(
        title = "Classes comp.",
        bslib::card(
          bslib::card_body(
            mod_rsd_ui(id = ns("rsd_sheet4"))
          )
        )
      )
    )
  )
}

#' plots Server Functions
#'
#' @noRd
mod_results_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### RSD stuff #####
    mod_rsd_server(id = "rsd_sheet1",
                   r = r,
                   sheet = 1)

    mod_rsd_server(id = "rsd_sheet2",
                   r = r,
                   sheet = 2)

    mod_rsd_server(id = "rsd_sheet3",
                   r = r,
                   sheet = 3)

    mod_rsd_server(id = "rsd_sheet4",
                   r = r,
                   sheet = 4)

    # mod_rsd_server(id = "rsd_sheet5",
    #                r = r,
    #                sheet = 5)
    #
    # mod_rsd_server(id = "rsd_sheet6",
    #                r = r,
    #                sheet = 6)
    #####################

    # ##### PCA stuff #####
    # mod_pca_server(id = "pca_sheet1",
    #                r = r,
    #                sheet = 1)
    #
    # mod_pca_server(id = "pca_sheet2",
    #                r = r,
    #                sheet = 2)
    #
    # mod_pca_server(id = "pca_sheet3",
    #                r = r,
    #                sheet = 3)
    #
    # mod_pca_server(id = "pca_sheet4",
    #                r = r,
    #                sheet = 4)
    #
    # mod_pca_server(id = "pca_sheet5",
    #                r = r,
    #                sheet = 5)
    #
    # mod_pca_server(id = "pca_sheet6",
    #                r = r,
    #                sheet = 6)
    #####################


  }) # end server module
}
