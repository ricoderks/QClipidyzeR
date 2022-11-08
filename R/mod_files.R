#' files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
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

    # sheet_names <- reactiveVal(c("Lipid Species Concentrations",
    #                              "Lipid Species Composition",
    #                              "Lipid Class Concentration",
    #                              "Lipid Class Composition",
    #                              "Fatty Acid Concentration",
    #                              "Fatty Acid Composition"))

    sheet_names <- reactiveVal(c("Lipid Species Conc (nmolg)",
                                 "Lipid Species Composition (%)",
                                 "Lipid Class Conc (nmolg)",
                                 "Lipid Class Composition (%)",
                                 "Fatty Acid Conc (nmolg)",
                                 "Fatty Acid Composition (%)"))

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

    r$all_data <- reactive({
      req(my_files)

      if(!is.null(my_files())) {
        # read all the files
        all_data <- read_files(files = my_files(),
                               sheet_names = sheet_names())

        return(all_data)
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
