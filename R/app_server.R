#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- reactiveValues(
    settings = list(files = NULL,
                    batches = NULL,
                    sampleid_col = NULL,
                    sampletype_col = NULL,
                    qc_regex = NULL,
                    sample_regex = NULL),
    all_data = NULL,
    clean_data = NULL,
    rsd_data = vector("list", 6),
    trend_data = vector("list", 6),
    deviation_data = vector("list", 6),
    pca_model = vector("list", 6),
    meta_columns = NULL,
    sheet_names = c("Lipid Species Conc (nmol_g)",
                    "Lipid Species Composition (%)",
                    "Lipid Class Conc (nmol_g)",
                    "Lipid Class Composition (%)",
                    "Fatty Acid Conc (nmol_g)",
                    "Fatty Acid Composition (%)"),
    sheet_names_short = c("Species conc.",
                          "Species comp.",
                          "Classes conc.",
                          "Classes comp.",
                          "FA conc.",
                          "FA comp."),
    errors = NULL
  )

  # import files
  mod_files_server(id = "file",
                   r = r)

  # show the data
  mod_data_server(id = "data",
                  r = r)

  # show the results
  mod_results_server(id = "sheet1",
                     r = r)

  mod_results_server(id = "sheet2",
                     r = r)

  mod_results_server(id = "sheet3",
                     r = r)

  mod_results_server(id = "sheet4",
                     r = r)

  mod_results_server(id = "sheet5",
                     r = r)

  mod_results_server(id = "sheet6",
                     r = r)

  # help module
  mod_help_server(id = "help")

  # about module
  mod_about_server(id = "about")
}
