#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      data.frame(
        user = c("shiny", "shinymanager"), # mandatory
        password = c("azerty", "12345"), # mandatory
        start = c("2019-04-15"), # optinal (all others)
        expire = c(NA, "2019-12-31"),
        admin = c(FALSE, TRUE),
        comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
        stringsAsFactors = FALSE
      )
    )
  )

  r <- reactiveValues(files = NULL,
                      all_data = NULL,
                      clean_data = NULL,
                      rsd_data = vector("list", 6),
                      pca_model = vector("list", 6),
                      meta_columns = NULL,
                      sheet_names = c("Lipid Species Conc (nmolg)",
                                      "Lipid Species Composition (%)",
                                      "Lipid Class Conc (nmolg)",
                                      "Lipid Class Composition (%)",
                                      "Fatty Acid Conc (nmolg)",
                                      "Fatty Acid Composition (%)"),
                      sheet_names_short = c("Species conc.",
                                            "Species comp.",
                                            "Classes conc.",
                                            "Classes comp.",
                                            "FA conc.",
                                            "FA comp."))

  # import files
  mod_files_server(id = "file",
                   r = r,
                   res_auth = res_auth)

  # show the data
  mod_data_server(id = "data",
                  r = r,
                  res_auth = res_auth)

  # show the plots
  mod_results_server(id = "results",
                     r = r,
                     res_auth = res_auth)

  # help module
  mod_help_server(id = "help,
                   res_auth = res_auth")

  # about module
  mod_about_server(id = "about",
                   res_auth = res_auth)
}
