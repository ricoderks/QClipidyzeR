#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- reactiveValues(all_data = NULL,
                      clean_data = NULL,
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
                   r = r)

  # show the data
  mod_data_server(id = "data",
                  r = r)

  # show the plots
  mod_plots_server(id = "plots",
                   r = r)

  # help module
  mod_help_server(id = "help")

  # about module
  mod_about_server(id = "about")
}
