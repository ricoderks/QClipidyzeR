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
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("plot_tabs"))
  )
}

#' plots Server Functions
#'
#' @noRd
mod_results_server <- function(id, r, res_auth){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_tabs <- renderUI({
      bs4Dash::tabsetPanel(
        id = "plot_tabset",
        tabPanel(
          title = r$sheet_names_short[1],
          mod_rsd_ui(id = ns("rsd_sheet1")),
          mod_pca_ui(id = ns("pca_sheet1"))
        ),
        tabPanel(
          title = r$sheet_names_short[2],
          mod_rsd_ui(id = ns("rsd_sheet2")),
          mod_pca_ui(id = ns("pca_sheet2"))
        ),
        tabPanel(
          title = r$sheet_names_short[3],
          mod_pca_ui(id = ns("pca_sheet3")),
          mod_rsd_ui(id = ns("rsd_sheet3"))
        ),
        tabPanel(
          title = r$sheet_names_short[4],
          mod_pca_ui(id = ns("pca_sheet4")),
          mod_rsd_ui(id = ns("rsd_sheet4"))
        ),
        tabPanel(
          title = r$sheet_names_short[5],
          mod_rsd_ui(id = ns("rsd_sheet5")),
          mod_pca_ui(id = ns("pca_sheet5"))
        ),
        tabPanel(
          title = r$sheet_names_short[6],
          mod_rsd_ui(id = ns("rsd_sheet6")),
          mod_pca_ui(id = ns("pca_sheet6"))
        )
      )
    })

    ##### RSD stuff #####
    mod_rsd_server(id = "rsd_sheet1",
                   r = r,
                   sheet = 1,
                   res_auth = res_auth)

    mod_rsd_server(id = "rsd_sheet2",
                   r = r,
                   sheet = 2,
                   res_auth = res_auth)

    mod_rsd_server(id = "rsd_sheet3",
                   r = r,
                   sheet = 3,
                   res_auth = res_auth)

    mod_rsd_server(id = "rsd_sheet4",
                   r = r,
                   sheet = 4,
                   res_auth = res_auth)

    mod_rsd_server(id = "rsd_sheet5",
                   r = r,
                   sheet = 5,
                   res_auth = res_auth)

    mod_rsd_server(id = "rsd_sheet6",
                   r = r,
                   sheet = 6,
                   res_auth = res_auth)
    #####################

    ##### PCA stuff #####
    mod_pca_server(id = "pca_sheet1",
                   r = r,
                   sheet = 1,
                   res_auth = res_auth)

    mod_pca_server(id = "pca_sheet2",
                   r = r,
                   sheet = 2,
                   res_auth = res_auth)

    mod_pca_server(id = "pca_sheet3",
                   r = r,
                   sheet = 3,
                   res_auth = res_auth)

    mod_pca_server(id = "pca_sheet4",
                   r = r,
                   sheet = 4,
                   res_auth = res_auth)

    mod_pca_server(id = "pca_sheet5",
                   r = r,
                   sheet = 5,
                   res_auth = res_auth)

    mod_pca_server(id = "pca_sheet6",
                   r = r,
                   sheet = 6,
                   res_auth = res_auth)
    #####################


  }) # end server module
}
