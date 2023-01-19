#' files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information
#'
#' @noRd
#'
#' @import waiter
#' @importFrom stringr str_detect
#' @importFrom shiny NS tagList
mod_files_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidPage(
      # initialize hostess for file loading
      waiter::useHostess(),
      fluidRow(
        column = 12,
        fileInput(
          inputId = ns("import_files"),
          label = "Select .xlsx files:",
          accept = ".xlsx",
          multiple = TRUE
        ),
      ),
      fluidRow(
        column = 12,
        uiOutput(outputId = ns("files_imported")),
        textOutput(outputId = ns("debug"))
      ),
      # place the hostess on the page
      waiter::hostess_loader(
        id = "loader",
        preset = "circle",
        text_color = "black",
        class = "label-center",
        center_page = TRUE
      )
    )
  )
}

#' files Server Functions
#'
#' @importFrom stringr str_extract
#' @importFrom waiter Waitress
#'
#' @noRd
mod_files_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create new hostess
    file_hostess <- waiter::Hostess$new(id = "loader")

    # import the files
    observeEvent(input$import_files, {
      # file_waitress$notify()

      # get the file names
      my_files <- input$import_files

      file_hostess$set(10)

      if (!is.null(my_files)) {
        batches <- str_extract(string = my_files$name,
                               pattern = "[bB][aA][tT][cC][hH][ -_]?[0-9]{1,2}")

        file_hostess$set(15)

        # sort the files according to the batch order
        r$files <- my_files[order(batches), ]
      }

      file_hostess$set(20)

      # read all the files
      r$all_data <- read_files(files = r$files,
                               sheet_names = r$sheet_names)

      file_hostess$set(60)

      # clean the data, every column is kept (for now)
      # only keep pooled samples and samples
      # remove features which are NOT present in all pooled samples
      r$clean_data <- clean_data(data = r$all_data)

      file_hostess$set(70)

      # determine the meta data columns
      r$meta_columns <- which(!str_detect(string = colnames(r$all_data$data[[1]]),
                                          pattern = "^[a-zA-Z]* [dPO]?-?[0-9]{1,2}:[0-9]{1,2}"))

      file_hostess$set(80)

      for(a in 1:6) {
        # calculate the RSD stuff
          r$rsd_data[[a]] <- calc_rsd(data = isolate(r$clean_data[[a]]),
                                      meta_data = isolate(r$meta_columns),
                                      lipid_class = ifelse(a == 3 | a == 4, FALSE, TRUE))

          r$pca_model[[a]] <- do_pca(data = r$clean_data[[a]],
                                     meta_data = r$meta_columns)
      }
      file_hostess$set(100)
      file_hostess$close()
    })

    # show the imported file names
    output$files_imported <- renderUI({
      req(r$files,
          r$rsd_data)

      if(!is.null(r$files)) {
        my_list <- "<ul>"

        for(a in 1:length(r$files$name)) {
          my_list <- paste(my_list, "<li>", r$files$name[a], "</li>")
        }

        my_list <- paste(my_list, "</ul>")

        HTML(
          "<h3>Imported files:</h3>",
          my_list
        )
      }
    })

    # just for some debugging
    output$debug <- renderText({

      # print(my_files()$name)
      # print(class(r$all_data()))
      NULL
    })
  })
}
