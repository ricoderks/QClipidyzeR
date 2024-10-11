#' files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information
#'
#' @noRd
#'
#' @importFrom stringr str_detect
#' @importFrom shiny NS tagList
#' @importFrom shinyjs disabled enable
mod_files_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),

    # disable upload bar fileinput
    tags$style(".shiny-file-input-progress {display: none}"),
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::textInput(
            inputId = ns("batch_regex"),
            label = "Batch regex:",
            value = "[0-9]{12}"
          ),
          shiny::hr(),
          shiny::selectInput(
            inputId = ns("sampleid_col"),
            label = "Sample ID column:",
            choices = "Select a column"
          ),
          shiny::selectInput(
            inputId = ns("sampletype_col"),
            label = "Sample type column:",
            choices = "Select a column"
          ),
          shiny::textInput(
            inputId = ns("qc_regex"),
            label = "QC sample regex:",
            value = "Quality"
          ),
          shiny::textInput(
            inputId = ns("sample_regex"),
            label = "Sample regex:",
            value = "(Ref|Bench|lg-term|AS|f/t)"
          )
        ),
        shiny::fileInput(
          inputId = ns("import_files"),
          label = "Select .xlsx files:",
          accept = ".xlsx",
          multiple = TRUE
        ),
        uiOutput(outputId = ns("files_imported")),
        textOutput(outputId = ns("debug")),
        shinyjs::disabled(
          shiny::actionButton(
            inputId = ns("import_data"),
            label = "Import data",
            width = "20%"
          )
        )
      )
    )
  )
}

#' files Server Functions
#'
#' @importFrom stringr str_extract str_detect
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner show_modal_progress_line
#'     update_modal_progress remove_modal_progress
#'
#' @noRd
mod_files_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # import the files
    shiny::observeEvent(input$import_files, {
      shiny::req(input$batch_regex)

      r$errors <- NULL

      tryCatch({
        shinybusy::show_modal_spinner(text = "Uploading / reading files...")
        my_files <- input$import_files
        batch_regex <- input$batch_regex

        if (!is.null(my_files)) {
          batches <- stringr::str_extract(string = my_files$name,
                                          pattern = batch_regex)

          print("Show batches:")
          print(batches)

          # sort the files according to the batch order
          r$files <- my_files[order(batches), ]
        }

        # read all the files
        r$all_data <- read_files(files = r$files,
                                 sheet_names = r$sheet_names)

        r$meta_columns <-
          colnames(r$all_data$data[[1]])[!stringr::str_detect(string = colnames(r$all_data$data[[1]]),
                                                              pattern = "^[a-zA-Z]* [dPO]?-?[0-9]{1,2}:[0-9]{1,2}")]

        shiny::updateSelectInput(
          inputId = "sampleid_col",
          choices = r$meta_columns,
          selected = r$meta_columns[1]
        )
        shiny::updateSelectInput(
          inputId = "sampletype_col",
          choices = r$meta_columns,
          selected = r$meta_columns[1]
        )

        shinybusy::remove_modal_spinner()
        shinyjs::enable(id = "import_data")
      },
      error = function(e) {
        # empty some old data
        r$all_data <- NULL
        r$meta_columns <- NULL
        # pass the error on
        r$errors <- e
      })
    })


    shiny::observeEvent(input$import_data, {
      shiny::req(r$all_data,
                 input$sampletype_col,
                 input$qc_regex,
                 input$sample_regex)

      tryCatch({
        shinyjs::disable(id = "import_data")
        # shinybusy::show_modal_spinner(text = "Processing data....")
        shinybusy::show_modal_progress_line(value = 0,
                                            text = "Processing data....")

        print("Start data import")
        # clean the data, every column is kept (for now)
        # only keep pooled samples and samples
        # remove features which are NOT present in all pooled samples
        r$clean_data <- clean_data(data = r$all_data,
                                   sample_type = input$sampletype_col,
                                   qc_regex = input$qc_regex,
                                   sample_regex = input$sample_regex)

        # an extra meta column was added by clean_data(), add here
        r$meta_columns <- c(r$meta_columns, "sample_type")

        shinybusy::update_modal_progress(value = 0.4)
        progress <- 0.4
        for(a in 1:6) {
          # calculate everything
          r$rsd_data[[a]] <- calc_rsd(data = r$clean_data[[a]][grepl(x = r$clean_data[[a]][, input$sampletype_col],
                                                                     pattern = input$qc_regex), ],
                                      meta_data = r$meta_columns,
                                      lipid_class = ifelse(a == 3 | a == 4, FALSE, TRUE))

          progress <- progress + 0.025
          shinybusy::update_modal_progress(value = progress)

          r$pca_model[[a]] <- do_pca(data = r$clean_data[[a]],
                                     meta_data = r$meta_columns)

          progress <- progress + 0.025
          shinybusy::update_modal_progress(value = progress)

          r$trend_data[[a]] <- calc_trend(data = r$clean_data[[a]],
                                          meta_data = r$meta_columns)

          progress <- progress + 0.025
          shinybusy::update_modal_progress(value = progress)

          r$deviation_data[[a]] <- calc_deviation(data = r$clean_data[[a]],
                                                  meta_data = r$meta_columns)

          progress <- progress + 0.025
          shinybusy::update_modal_progress(value = progress)
        }
        print("done")
        shinybusy::remove_modal_progress()
        # shinybusy::remove_modal_spinner()
      },
      error = function(e) {
        # empty some old data
        r$clean_data <- NULL
        r$rsd_data <- vector("list", 6)
        r$pca_model <- vector("list", 6)
        r$trend_data <- vector("list", 6)
        # pass the error on
        r$errors <- e
      })

    })


    # show the imported file names
    output$files_imported <- shiny::renderUI({
      shiny::req(r$files)

      if(!is.null(r$files)) {
        my_list <- "<ul>"

        for(a in 1:length(r$files$name)) {
          my_list <- paste(my_list, "<li>", r$files$name[a], "</li>")
        }

        my_list <- paste(my_list, "</ul>")

        if (is.null(r$errors)) {
          HTML(
            "<h3>Imported files:</h3>",
            my_list
          )
        } else {
          HTML(
            "<h3>Imported files:</h3>",
            my_list,
            "<p style='color:red;'>", r$errors$message, "</p>"
          )
        }
      }
    })


    shiny::observeEvent(c(input$sampleid_col,
                          input$sampletype_col,
                          input$qc_regex,
                          input$sample_regex), {
                            # after changing something make possible to re-import
                            # the data
                            shinyjs::enable(id = "import_data")
                          },
                        ignoreInit = TRUE)


    # just for some debugging
    output$debug <- shiny::renderText({

      # print(my_files()$name)
      # print(class(r$all_data()))
      NULL
    })
  })
}
