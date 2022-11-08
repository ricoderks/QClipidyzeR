#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- reactiveValues(all_data = NULL)

  # import files
  mod_files_server(id = "file",
                   r = r)

  # show the data
  mod_data_server(id = "data",
                  r = r)

  # help module
  mod_help_server(id = "help")

  # about module
  mod_about_server(id = "about")
}
