#' files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,r,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom stringr str_detect
#' @importFrom shiny NS tagList
mod_files_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidPage(
      fluidRow(
        column = 12,
        fileInput(
          inputId = ns("import_files"),
          label = "Select .xlsx files:",
          accept = ".xlsx",
          multiple = TRUE
        )
      ),
      fluidRow(
        column = 12,
        uiOutput(outputId = ns("files_imported")),
        textOutput(outputId = ns("debug"))
      )
    )
  )
}

#' files Server Functions
#'
#' @importFrom stringr str_extract
#'
#' @noRd
mod_files_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # sort the file names
    my_files <- reactive({
      req(input$import_files)

      # get the file names
      my_files <- input$import_files

      if (is.null(my_files)) {
        # no files selected yet
        return(NULL)
      } else {
        batches <- str_extract(string = my_files$name,
                               pattern = "[bB][aA][tT][cC][hH][ -_]?[0-9]{1,2}")

        # sort the files according to the batch order
        my_files <- my_files[order(batches), ]

        return(my_files)
      }
    })

    # show the imported file names
    output$files_imported <- renderUI({
      req(my_files)

      if(!is.null(my_files())) {
        my_list <- "<ul>"

        for(a in 1:length(my_files()$name)) {
          my_list <- paste(my_list, "<li>", my_files()$name[a], "</li>")
        }

        my_list <- paste(my_list, "</ul>")

        HTML(
          "<h3>Imported files:</h3>",
          my_list
        )
      }
    })

    # r$all_data <- reactive({
    observe({
      req(my_files)

      if(!is.null(my_files())) {
        # read all the files
        r$all_data <- read_files(files = my_files(),
                                 sheet_names = r$sheet_names)

        # clean the data, every column is kept (for now)
        # only keep pooled samples and samples
        # remove features which are NOT present in all pooled samples
        r$clean_data <- clean_data(data = r$all_data)

        # determine the meta data columns
        r$meta_columns <- which(!str_detect(string = colnames(r$all_data$data[[1]]),
                                            pattern = "^[a-zA-Z]* [dPO]?-?[0-9]{1,2}:[0-9]{1,2}"))
      }
    })

    # just for some debugging
    output$debug <- renderText({
      req(my_files)

      # print(my_files()$name)
      # print(class(r$all_data()))
      NULL
    })
  })
}

## To be copied in the UI
# mod_files_ui("files_1")

## To be copied in the server
# mod_files_server("files_1")
