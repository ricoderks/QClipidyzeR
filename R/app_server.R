#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # import files
  mod_files_server("file")

  # help module
  mod_help_server("help")

  # about module
  mod_about_server("about")
}
