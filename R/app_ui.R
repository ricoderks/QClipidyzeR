#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs
#' @importFrom utils packageVersion
#'
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shinyjs::useShinyjs(),

    # Your application UI logic
    bslib::page_navbar(
      title = paste0("QClipidyzeR | v", packageVersion(pkg = "QClipidyzeR")),
      underline = TRUE,
      bslib::nav_panel(
        title = "Welcome",
        bslib::card(
          shiny::p("This app is intended to give a quick overview of the quality of a Lipidyzer study. E.g. it will show you the RSD values and a PCA analysis is to
                  to see if there is any batch efftect in the data. For more information see the Help section.")
        )
      ),
      bslib::nav_panel(
        title = "Files",
        mod_files_ui(id = "file")
      ),
      bslib::nav_panel(
        title = "Data",
        mod_data_ui(id = "data")
      ),
      bslib::nav_menu(
        title = "Results",
        bslib::nav_panel(
          title = "Species conc.",
          mod_results_ui(id = "sheet1")
        ),
        bslib::nav_panel(
          title = "Species comp.",
          mod_results_ui(id = "sheet2")
        ),
        bslib::nav_panel(
          title = "Class conc.",
          mod_results_ui(id = "sheet3")
        ),
        bslib::nav_panel(
          title = "Conc comp.",
          mod_results_ui(id = "sheet4")
        ),
        bslib::nav_panel(
          title = "FA conc.",
          mod_results_ui(id = "sheet5")
        ),
        bslib::nav_panel(
          title = "FA comp.",
          mod_results_ui(id = "sheet6")
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "Help",
        bslib::nav_panel(
          title = "Help",
          mod_help_ui(id = "help")
        ),
        "----",
        bslib::nav_panel(
          title = "About",
          mod_about_ui(id = "about")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "QClipidyzeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
