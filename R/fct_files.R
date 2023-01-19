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
#' @importFrom dplyr mutate relocate n arrange
#' @importFrom purrr map2 reduce
#' @importFrom readxl read_xlsx
#' @importFrom rlang .data
#'
#' @noRd
#'
read_files <- function(files = NULL, sheet_names = NULL) {

  all_data <- data.frame(sheet_name = sheet_names) |>
    dplyr::mutate(full_path = list(files$datapath),
                  file = list(files$name),
                  data = purrr::map2(.x = .data$full_path,
                                     .y = .data$sheet_name,
                                     .f = ~ purrr::map2(.x = .x,
                                                        .y = .y,
                                                        .f = ~ readxl::read_xlsx(path = .x,
                                                                                 sheet = .y,
                                                                                 col_names = TRUE,
                                                                                 na = ".") |>
                                                          # add batch information
                                                          dplyr::mutate(batch = factor(.x),
                                                                        read_order = 1:dplyr::n()) |>
                                                          dplyr::relocate(batch, read_order, .after = SampleID))
                  ),
                  # check if all columns are correct
                  check_columns = purrr::map(.x = data,
                                             .f = ~ sapply(.x, function(x) {
                                               all(c("NormType", "SampleID") %in% colnames(x))
                                             }))
    )

  # check if there is a wrong column somewhere
  check_columns <- all(sapply(all_data$check_columns, function(x) all(x)))

  if(check_columns) {
    all_data <- all_data |>
      dplyr::mutate(data = purrr::map(.x = data,
                                      .f = ~ .x |>
                                        purrr::reduce(function(...) merge(...), all = TRUE) |>
                                        # sort and convert to batch number
                                        dplyr::arrange(.data$batch) |>
                                        dplyr::mutate(batch = as.factor(as.integer(.data$batch)))
      ))
  } else {
    stop("Not all files / sheets contain the correct column names (i.e. NormType and SampleID)!!")
  }

  return(all_data)
}


#' @title Clean the data
#'
#' @description Clean the data by removing everything except pooled samples and samples
#'
#' @param data data.frame, containing all the data
#'
#' @details Only the pooled samples and samples are kept. Remove also all species
#'     which are not present in all pooled samples.
#'
#' @return data.frame containing all cleaned data from all sheets
#'
#' @author Rico Derks
#'
#' @importFrom dplyr mutate relocate n filter select_if
#' @importFrom purrr map
#'
#' @noRd
#'
clean_data <- function(data = NULL) {
  # keep only pooled samples and samples
  clean_data <- data |>
    dplyr::mutate(clean_data = purrr::map(.x = data,
                                          .f = ~ .x |>
                                            dplyr::filter(NormType %in% c("Pooled sample", "Samples"))))

  # which features
  for(a in 1:6) {
    data_df <- clean_data$clean_data[[a]]
    na_pooled_idx <- apply(data_df[data_df$NormType == "Pooled sample", ], 2, function(x) {
      sum(is.na(x))
    })
    names(na_pooled_idx) <- NULL

    # correct  for meta data
    # na_pooled_idx[1:4] <- 0
    # convert to TRUE / FALSE, keep = TRUE, for now assuming that the meta data doesn't contain any NA's
    na_pooled_idx <- na_pooled_idx == 0

    clean_data$clean_data[[a]] <- data_df[, na_pooled_idx]
  }

  return(clean_data$clean_data)
}
