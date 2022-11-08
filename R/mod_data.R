#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,r,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS tagList
#'
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("data_tabs"))
  )
}

#' data Server Functions
#'
#' @importFrom DT renderDT
#' @import bs4Dash
#'
#' @noRd
mod_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$data_tabs <- renderUI({
      bs4Dash::tabsetPanel(
        id = "data_tabset",
        tabPanel(
          title = r$sheet_names_short[1],
          DT::DTOutput(outputId = ns("data_sheet1"))
        ),
        tabPanel(
          title = r$sheet_names_short[2],
          DT::DTOutput(outputId = ns("data_sheet2"))
        ),
        tabPanel(
          title = r$sheet_names_short[3],
          DT::DTOutput(outputId = ns("data_sheet3"))
        ),
        tabPanel(
          title = r$sheet_names_short[4],
          DT::DTOutput(outputId = ns("data_sheet4"))
        ),
        tabPanel(
          title = r$sheet_names_short[5],
          DT::DTOutput(outputId = ns("data_sheet5"))
        ),
        tabPanel(
          title = r$sheet_names_short[6],
          DT::DTOutput(outputId = ns("data_sheet6"))
        )
      )
    })


    output$data_sheet1 <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data$data[[1]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )

    output$data_sheet2 <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data$data[[2]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )

    output$data_sheet3 <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data$data[[3]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )

    output$data_sheet4 <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data$data[[4]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )

    output$data_sheet5 <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data$data[[5]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )

    output$data_sheet6 <- DT::renderDT({
      req(r$all_data)

      if(!is.null(r$all_data)) {
        show_data <- r$all_data$data[[6]]

        show_data
      }
    },
    options = list(pageLength = 20,
                   dom = "tpr")
    )

  }) # end module server
}
