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
#'
mod_results_ui <- function(id){
  ns <- shiny::NS(id)

  if(id == "sheet3" | id == "sheet4") {
    title <- "RSD table"
  } else {
    title = "RSD histogram"
  }

  shiny::tagList(
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = title,
        bslib::card(
          mod_rsd_ui(id = ns(paste0("rsd_", id))),
        )
      ),
      bslib::nav_panel(
        title = "PCA",
        bslib::card(
          bslib::card_body(
            mod_pca_ui(id = ns(paste0("pca_", id)))
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

    mod_rsd_server(id = "rsd_sheet5",
                   r = r,
                   sheet = 5)

    mod_rsd_server(id = "rsd_sheet6",
                   r = r,
                   sheet = 6)
    #####################

    ##### PCA stuff #####
    mod_pca_server(id = "pca_sheet1",
                   r = r,
                   sheet = 1)

    mod_pca_server(id = "pca_sheet2",
                   r = r,
                   sheet = 2)

    mod_pca_server(id = "pca_sheet3",
                   r = r,
                   sheet = 3)

    mod_pca_server(id = "pca_sheet4",
                   r = r,
                   sheet = 4)

    mod_pca_server(id = "pca_sheet5",
                   r = r,
                   sheet = 5)

    mod_pca_server(id = "pca_sheet6",
                   r = r,
                   sheet = 6)
    #####################


  }) # end server module
}
