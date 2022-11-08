#' plots
#'
#' @title Calculate the RSD for all features over the pooled samples
#'
#' @description Calculate the RSD for all features over the pooled samples.
#'
#' @param data data.frame, wide data.frame with all the data.
#' @param meta_data numeric vector, with indices of the meta data columns.
#' @param lipid_class logical, extract lipid class information (default = FALSE).
#'
#' @return long data.frame
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr if_else
#'
#' @noRd
#'
calc_rsd <- function(data = NULL, meta_data = NULL, lipid_class = FALSE) {
  rsd_data <- data |>
    filter(GroupName == "Pooled sample") |>
    pivot_longer(
      cols = !meta_data,
      names_to = "lipid",
      values_to = "value"
    ) |>
    group_by(lipid) |>
    summarise(mean = mean(value, na.rm = TRUE),
              stdev = sd(value, na.rm = TRUE),
              rsd = stdev / mean)

  # extract lipid class information for lipid species
  if(lipid_class) {
    rsd_data <- rsd_data |>
      mutate(lipid_class = str_extract(string = lipid,
                                       pattern = "^[a-zA-Z]* O?"),
             lipid_class = if_else(lipid_class == "Cer ",
                                   str_extract(string = lipid,
                                               pattern = "^[a-zA-Z]* d18:[01]"),
                                   lipid_class))
  }

  return(rsd_data)
}

#' @title Create histogram of RSD values
#'
#' @description Create plotly histogram of RSD values.
#'
#' @param data data.frame, with all rsd values
#'
#' @return plotly object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_minimal guides guide_legend .data
#' @importFrom plotly ggplotly
#'
#' @noRd
#'
create_rsd_hist <- function(data = NULL) {
  p <- data |>
    mutate(rsd = rsd * 100) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                 fill = .data$lipid_class)) +
    ggplot2::geom_histogram(bins = 100) +
    ggplot2::geom_vline(xintercept = 30,
                        colour = "red",
                        linetype = 2) +
    ggplot2::labs(x = "RSD [%]") +
    # this one doesn't convert well to plotly
    # ggplot2::guides(fill = ggplot2::guide_legend(title = "Lipid class")) +
    ggplot2::theme_minimal()

  ply <- ggplotly(p)

  return(ply)
}

#' @title Create nice table with RSD values
#'
#' @description Create nice table with RSD values.
#'
#' @param data data.frame, with all rsd values
#'
#' @return datatable object
#'
#' @author Rico Derks
#'
#' @importFrom DT datatable formatStyle styleInterval formatRound formatPercentage
#'
#' @noRd
#'
create_rsd_table <- function(data = NULL) {
  dt_table <- DT::datatable(data,
                            options = list(pageLength = 20,
                                           dom = "tr")) |>
    DT::formatRound(columns = c("mean", "stdev"),
                    digits = 1) |>
    DT::formatPercentage(columns = "rsd",
                         digits = 1) |>
    DT::formatStyle(columns = "rsd",
                    color = DT::styleInterval(cuts = 0.2,
                                              values = c("black", "red")),
                    fontWeight = DT::styleInterval(cuts = 0.2,
                                                   values = c("normal", "bold")))

  return(dt_table)
}
