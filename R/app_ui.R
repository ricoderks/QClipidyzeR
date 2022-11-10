#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @importFrom utils packageVersion
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      #### Header
      dashboardHeader(
        title = paste0("QClipidyzeR | v", packageVersion(pkg = "QClipidyzeR"))
      ),

      #### Sidebarmenu
      bs4DashSidebar(
        skin = "light",
        sidebarMenu(
          menuItem(text = "Welcome",
                   tabName = "welcome",
                   icon = icon("home")),
          menuItem(text = "File",
                   tabName = "file",
                   icon = icon("file-excel")),
          menuItem(text = "Data",
                   tabName = "data",
                   icon = icon("table")),
          menuItem(text = "Results",
                   tabName = "results",
                   icon = icon("chart-simple")),
          menuItem(text = "Help",
                   tabName = "main_help",
                   icon = icon("circle-info"),
                   menuSubItem(
                     text = "help",
                     tabName = "help"
                   ),
                   menuSubItem(
                     text = "About",
                     tabName = "about"
                   )
          )
        )
      ),

      #### Body
      bs4DashBody(
        tabItems(
          tabItem(
            tabName = "welcome",
            fluidRow(
              bs4Dash::column(
                width = 10,
                p("This app is intended to give a quick overview of the quality of a Lipidyzer study. E.g. it will show you the RSD values and a PCA analysis is to
                  to see if there is any batch efftect in the data. For more information see the Help section.")
              ),
              column(
                width = 2,
                img(src = "./www/CPM-logo.png", alt = "CPM logo", width = "100px")
              )
            )

          ),
          tabItem(
            tabName = "file",
            mod_files_ui(id = "file")
          ),
          tabItem(
            tabName = "data",
            mod_data_ui(id = "data")
          ),
          tabItem(
            tabName = "results",
            mod_results_ui(id = "results")
          ),
          tabItem(
            tabName = "help",
            mod_help_ui(id = "help")
          ),
          tabItem(
            tabName = "about",
            mod_about_ui(id = "about")
          )
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
