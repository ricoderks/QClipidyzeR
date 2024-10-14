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
#' @importFrom bsicons bs_icon
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
        title = "Trend plot",
        bslib::card(
          bslib::card_body(
            mod_trend_ui(id = ns(paste0("trend_", id)))
          )
        )
      ),
      bslib::nav_panel(
        title = "Deviation",
        bslib::card(
          bslib::card_body(
            mod_deviation_ui(id = ns(paste0("deviation_", id)))
          )
        )
      ),
      bslib::nav_panel(
        title = "PCA",
        bslib::card(
          bslib::card_body(
            mod_pca_ui(id = ns(paste0("pca_", id)))
          )
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::uiOutput(
          outputId = ns("download_ui")
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

    ##### Trend stuff ####
    mod_trend_server(id = "trend_sheet1",
                   r = r,
                   sheet = 1)

    mod_trend_server(id = "trend_sheet2",
                   r = r,
                   sheet = 2)

    mod_trend_server(id = "trend_sheet3",
                   r = r,
                   sheet = 3)

    mod_trend_server(id = "trend_sheet4",
                   r = r,
                   sheet = 4)

    mod_trend_server(id = "trend_sheet5",
                   r = r,
                   sheet = 5)

    mod_trend_server(id = "trend_sheet6",
                   r = r,
                   sheet = 6)
    ######################

    ##### Deviation stuff ####
    mod_deviation_server(id = "deviation_sheet1",
                   r = r,
                   sheet = 1)

    mod_deviation_server(id = "deviation_sheet2",
                   r = r,
                   sheet = 2)

    mod_deviation_server(id = "deviation_sheet3",
                   r = r,
                   sheet = 3)

    mod_deviation_server(id = "deviation_sheet4",
                   r = r,
                   sheet = 4)

    mod_deviation_server(id = "deviation_sheet5",
                   r = r,
                   sheet = 5)

    mod_deviation_server(id = "deviation_sheet6",
                   r = r,
                   sheet = 6)
    #########################

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

    ##### Download stuff #####
    output$download_ui <- shiny::renderUI({
      shiny::req(r$data$clean_data,
                 r$data$rsd_data,
                 r$data$trend_data,
                 r$data$pca_model)

      shiny::tagList(
        bslib::popover(
          bsicons::bs_icon(name = "cloud-download-fill",
                           size = "2em"),
          shiny::downloadButton(
            outputId = ns("download_report"),
            label = "Download overview report"
          )
        )
      )
    })


    # output$download_report <- shiny::downloadHandler(
    #   filename = function() {
    #     paste(Sys.Date(), "_data_overview.html", sep = "")
    #   },
    #   content = function(file) {
    #     temp_report <- file.path(tempdir(), "data_overview.Rmd")
    #     report_file <- system.file("reports", "data_overview.Rmd",
    #                                package = "BatchCorrection")
    #     file.copy(from = report_file,
    #               to = temp_report,
    #               overwrite = TRUE)
    #
    #     e <- new.env()
    #
    #     e$params <- list(
    #       data_file = r$data_file,
    #       meta_file = r$meta_file,
    #       clean_data = r$tables$clean_data,
    #       meta_data = r$tables$meta_data,
    #       trend_data = r$data$trend,
    #       histogram_data = r$data$histogram,
    #       pca_data = r$data$pca,
    #       heatmap_data = r$data$heatmap,
    #       rle_data = r$data$rle,
    #       sampleid_raw_col = r$indices$raw_id_col,
    #       sampleid_meta_col = r$indices$meta_id_col,
    #       meta_type_col = r$indices$meta_type_col,
    #       meta_acqorder_col = r$indices$meta_acqorder_col,
    #       meta_batch_col = r$indices$meta_batch_col,
    #       sample_ids = r$indices$id_samples,
    #       qcpool_ids = r$indices$id_qcpool,
    #       blank_ids = r$indices$id_blanks,
    #       settings_data = r$settings_data
    #     )
    #
    #     shiny::withProgress(
    #       message = "Rendering report.....",
    #       value = 0,
    #       {
    #         shiny::incProgress(1/10)
    #         Sys.sleep(1)
    #         shiny::incProgress(5/10)
    #         rmarkdown::render(input = temp_report,
    #                           output_file = file,
    #                           envir = e)
    #       }
    #     )
    #   }
    # )


    ##########################


  }) # end server module
}
