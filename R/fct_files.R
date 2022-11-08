#' files
#'
#' @title Read multiple xlsx files
#'
#' @description Read multiple xlsx files from the Lipidyzer / SLA software.
#'
#' @param files data.frame, from fileInput().
#' @param sheet_names character vector, containing all the names of the different sheets.
#'
#' @return data.frame containing all data from all sheets
#'
#' @author Rico Derks
#'
#' @importFrom dplyr mutate relocate n
#' @importFrom purrr map2 reduce
#' @importFrom readxl read_xlsx
#'
#' @noRd
#'
read_files <- function(files = NULL, sheet_names = NULL) {

  all_data <- data.frame(sheet_name = sheet_names) |>
    dplyr::mutate(full_path = list(files$datapath),
                  file = list(files$name),
                  data = purrr::map2(.x = full_path,
                                     .y = sheet_name,
                                     .f = ~ purrr::map2(.x = .x,
                                                        .y = .y,
                                                        .f = ~ readxl::read_xlsx(path = .x,
                                                                                 sheet = .y,
                                                                                 col_names = TRUE,
                                                                                 na = ".") |>
                                                          # add batch information
                                                          dplyr::mutate(batch = factor(.x),
                                                                        read_order = 1:dplyr::n()) |>
                                                          dplyr::relocate(batch, read_order, .after = SampleID)) |>
                                       purrr::reduce(function(...) merge(...), all = TRUE)
                  )
    )

  return(all_data)
}
