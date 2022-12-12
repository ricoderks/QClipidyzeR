#' rsd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r is a reactiveValues object containing all information.
#' @param sheet integer, indicates what sheet to use.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rsd_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("show_rsd"))
  )
}

#' rsd Server Functions
#'
#' @noRd
mod_rsd_server <- function(id, r, sheet, res_auth){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # show graph or table
    output$show_rsd <- renderUI({
      if(sheet == 3 | sheet == 4) {
        tagList(
          fluidRow(
            bs4Dash::box(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              DT::DTOutput(outputId = ns("rsd_table"))
            )
          )
        )
      } else {
        tagList(
          fluidRow(
            bs4Dash::box(
              collapsible = FALSE,
              headerBorder = FALSE,
              plotlyOutput(outputId = ns("rsd_plot"))
            )
          )
        )
      }
    })

    output$rsd_plot <- renderPlotly({
      req(r$rsd_data)

      if(!is.null(r$rsd_data[[sheet]])) {
        # create the plot
        if(sheet != 3 | sheet != 4) {
          create_rsd_hist(data = r$rsd_data[[sheet]])
        }
      }
    })

    output$rsd_table <- DT::renderDT({
      req(r$rsd_data)

      if(!is.null(r$rsd_data[[sheet]])) {
        if(sheet == 3 | sheet == 4) {
          table_DT <- create_rsd_table(data = r$rsd_data[[sheet]])

          table_DT
        }
      }
    })

  })
}
