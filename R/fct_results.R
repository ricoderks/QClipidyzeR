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
#' @importFrom rlang .data
#' @importFrom dplyr if_else group_by summarise mutate
#' @importFrom stats sd
#' @importFrom stringr str_extract
#'
#' @noRd
#'
calc_rsd <- function(data = NULL, meta_data = NULL, lipid_class = FALSE) {
  rsd_data <- data |>
    tidyr::pivot_longer(
      cols = !meta_data,
      names_to = "lipid",
      values_to = "value"
    ) |>
    dplyr::group_by(.data$lipid) |>
    dplyr::summarise(mean = mean(.data$value, na.rm = TRUE),
                     stdev = stats::sd(.data$value, na.rm = TRUE),
                     rsd = .data$stdev / .data$mean,
                     .groups = "drop")

  rsd_data_batch <- data |>
    tidyr::pivot_longer(
      cols = !meta_data,
      names_to = "lipid",
      values_to = "value"
    ) |>
    dplyr::group_by(.data$batch, .data$lipid) |>
    dplyr::summarise(mean = mean(.data$value, na.rm = TRUE),
                     stdev = stats::sd(.data$value, na.rm = TRUE),
                     rsd = .data$stdev / .data$mean,
                     .groups = "drop")

  # extract lipid class information for lipid species
  if(lipid_class) {
    rsd_data <- rsd_data |>
      dplyr::mutate(lipid_class = stringr::str_extract(string = .data$lipid,
                                                       pattern = "^[a-zA-Z]* O?"),
                    lipid_class = dplyr::if_else(lipid_class == "Cer ",
                                                 stringr::str_extract(string = .data$lipid,
                                                                      pattern = "^[a-zA-Z]* d18:[01]"),
                                                 .data$lipid_class))

    rsd_data_batch <- rsd_data_batch |>
      dplyr::mutate(lipid_class = stringr::str_extract(string = .data$lipid,
                                                       pattern = "^[a-zA-Z]* O?"),
                    lipid_class = dplyr::if_else(lipid_class == "Cer ",
                                                 stringr::str_extract(string = .data$lipid,
                                                                      pattern = "^[a-zA-Z]* d18:[01]"),
                                                 .data$lipid_class))
  }

  return(list("overall" = rsd_data,
              "batch" = rsd_data_batch))
}

#' @title Create histogram of RSD values
#'
#' @description Create plotly histogram of RSD values.
#'
#' @param data data.frame, with all rsd values.
#' @param batch character(1), show the histograms for all data or per batch.
#'
#' @return plotly object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_minimal
#'     guides guide_legend .data theme facet_wrap labeller
#' @importFrom plotly ggplotly
#' @importFrom dplyr mutate
#'
#' @noRd
#'
create_rsd_hist <- function(data = NULL,
                            batch = c("overall", "batch")) {
  p <- data[[batch]] |>
    dplyr::mutate(rsd = .data$rsd * 100) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                 fill = .data$lipid_class)) +
    ggplot2::geom_histogram(bins = 100,
                            alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 30,
                        colour = "red",
                        linetype = 2) +
    ggplot2::labs(x = "RSD [%]") +
    # this one doesn't convert well to plotly
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Lipid class",
                                                 nrow = 2))

    if(batch == "batch") {
      strip_labels <- function(value) {
        return(paste0("Batch ", value))
      }

      p <- p +
        ggplot2::facet_wrap(~ .data$batch,
                            scales = "free_y",
                            labeller = ggplot2::labeller(batch = strip_labels))
    }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  # ply <- plotly::ggplotly(p)

  return(p)
}

#' @title Create nice table with RSD values
#'
#' @description Create nice table with RSD values.
#'
#' @param data data.frame, with all rsd values.
#' @param batch character(1), show the tables for all data or per batch.
#'
#' @return datatable object
#'
#' @author Rico Derks
#'
#' @importFrom DT datatable formatStyle styleInterval formatRound formatPercentage
#'     JS
#'
#' @noRd
#'
create_rsd_table <- function(data = NULL,
                             batch = c("overall", "batch")) {
  num_pages <- length(unique(data[[batch]]$lipid))

  if(batch == "batch") {
    col_names <- c("Batch" = "batch", "Lipid class" = "lipid", "Mean" = "mean", "St. dev." = "stdev", "RSD" = "rsd")
    rsd_limit <- 0.2
  } else {
    col_names <- c("Lipid class" = "lipid", "Mean" = "mean", "St. dev." = "stdev", "RSD" = "rsd")
    rsd_limit <- 0.3
  }

  dt_table <- data[[batch]] |>
    DT::datatable(options = list(pageLength = num_pages,
                                 dom = '<"top" p>t',
                                 ordering = FALSE),
                  rownames = FALSE,
                  colnames = col_names) |>
    DT::formatRound(columns = c("Mean", "St. dev."),
                    digits = 1) |>
    DT::formatPercentage(columns = "RSD",
                         digits = 1) |>
    # Workaround to get the highlighting to work properly, see:
    # https://github.com/rstudio/DT/issues/1102    and
    # https://stackoverflow.com/questions/78140942/is-there-a-way-to-change-color-of-specific-dt-lines-when-using-bslib-and-bootstr/78141878#78141878
    DT::formatStyle(
      columns = 1:ncol(data[[batch]]),
      target = "cell",
      color = DT::JS("\"unset\""),
      backgroundColor = DT::JS("\"unset\"")
    ) |>
    DT::formatStyle(columns = "RSD",
                    target = "row", # not working, color is not working
                    fontWeight = DT::styleInterval(cuts = rsd_limit,
                                                   values = c("normal", "bold")),
                    color = DT::styleInterval(cuts = rsd_limit,
                                              values = c("black", "red")))

  return(dt_table)
}


#' @title Do PCA analysis
#'
#' @description Create nice table with RSD values.
#'
#' @param data data.frame, with all the data
#' @param meta_data numeric vector, with indices of the meta data columns.
#'
#' @return pcaRes object
#'
#' @author Rico Derks
#'
#' @importFrom pcaMethods pca
#'
#' @noRd
#'
do_pca <- function(data = NULL, meta_data = NULL) {
  pca_data <- data
  # log transform
  pca_data[is.na(pca_data)] <- 1
  # pca_data[, -meta_data] <- log(pca_data[, -meta_data])
  # put the few NA's back
  pca_data[pca_data == 0] <- NA

  mod <- pcaMethods::pca(object = pca_data, #pca_data[, -meta_data],
                         nPcs = 2,
                         scale = "uv",
                         cv = "q2")

  return(mod)
}


#' @title Create scores plot
#'
#' @description Show scores plot.
#'
#' @param model pcaRes object.
#' @param meta_data data.frame containing the meta data.
#' @param batch character vector, containing the numbers (as character) of which batches to show.
#' @param sample_type character vector, containing the names of the sample types to show.
#'
#' @return plotly object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 aes geom_path geom_point geom_vline geom_hline labs
#'     theme_minimal theme scale_color_manual scale_shape_manual
#' @importFrom plotly ggplotly
#' @importFrom RColorBrewer brewer.pal
#'
#' @noRd
#'
scores_plot <- function(model = NULL,
                        meta_data = NULL,
                        batch = NULL,
                        sample_type = NULL) {
  batch <- as.integer(batch)

  plot_data <- cbind(meta_data, model@scores)
  # copy the data to make sure the Hoteling T2 is correct
  full_data <- plot_data

  if(!is.null(batch)) {
    plot_data <- plot_data[plot_data$batch %in% batch, ]
  }

  if(!is.null(sample_type)) {
    plot_data <- plot_data[plot_data$sample_type %in% sample_type, ]
  }

  # create the colors for the batches
  batch_colors <- c(
    RColorBrewer::brewer.pal(n = 9, name = "Set1"),
    RColorBrewer::brewer.pal(n = 8, name = "Set2"),
    RColorBrewer::brewer.pal(n = 12, name = "Set3")
  )
  names(batch_colors) <- as.character(1:length(batch_colors))

  # create the shapes for the sample type, 16 = circle, 17 = triangle
  sample_types <- c("qc" = 16,
                    "sample" = 17)

  p <- plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_path(data = simple_ellipse(x = full_data$PC1,
                                             y = full_data$PC2),
                       ggplot2::aes(x = .data$x,
                                    y = .data$y),
                       colour = "gray") +
    ggplot2::geom_vline(xintercept = 0,
                        colour = "gray") +
    ggplot2::geom_hline(yintercept = 0,
                        colour = "gray") +
    ggplot2::geom_point(data = plot_data,
                        ggplot2::aes(x = .data$PC1,
                                     y = .data$PC2,
                                     colour = .data$batch,
                                     shape = .data$sample_type),
                        size = 3) +
    ggplot2::scale_color_manual(values = batch_colors) +
    ggplot2::scale_shape_manual(values = sample_types) +
    ggplot2::labs(title = "Scores plot",
                  caption = "Note: log transform / uv scaling / lipid species present in all pooled samples",
                  x = sprintf("PC 1 (%0.1f %%)", model@R2[1] * 100),
                  y = sprintf("PC 2 (%0.1f %%)", model@R2[2] * 100)) +
    ggplot2::theme_minimal()
  # ggplot2::theme(legend.position = "none")

  ply <- plotly::ggplotly(p)

  return(ply)
}


#' @title Create loadings plot
#'
#' @description Show loadings plot.
#'
#' @param model pcaRes object.
#'
#' @return plotly object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 aes geom_path geom_point geom_vline geom_hline labs theme_minimal theme .data
#' @importFrom plotly ggplotly
#'
#' @noRd
#'
loadings_plot <- function(model = NULL) {
  plot_data <- data.frame("lipid" = rownames(model@loadings),
                          model@loadings) |>
    mutate(lipid_class = str_extract(string = .data$lipid,
                                     pattern = "^[a-zA-Z]* ?O?"),
           lipid_class = if_else(.data$lipid_class == "Cer ",
                                 str_extract(string = .data$lipid,
                                             pattern = "^[a-zA-Z]* d18:[01]"),
                                 .data$lipid_class))

  p <- plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0,
                        colour = "gray") +
    ggplot2::geom_hline(yintercept = 0,
                        colour = "gray") +
    ggplot2::geom_text(ggplot2::aes(x = .data$PC1,
                                    y = .data$PC2,
                                    label = .data$lipid_class,
                                    colour = .data$lipid_class)) +
    ggplot2::labs(title = "Loadings plot",
                  caption = "Note: log transform / uv scaling / lipid species present in all pooled samples",
                  x = sprintf("PC 1 (%0.1f %%)", model@R2[1] * 100),
                  y = sprintf("PC 2 (%0.1f %%)", model@R2[2] * 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  ply <- plotly::ggplotly(p)

  return(ply)
}


#' @title Create summary of fit plot
#'
#' @description Show summary of fit plot.
#'
#' @param model pcaRes object.
#'
#' @return ggplot2 object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 aes geom_col labs theme_minimal theme .data
#'
#' @noRd
#'
sumfit_plot <- function(model = NULL) {
  plot_data <- data.frame(PC = paste("PC", 1:length(model@R2cum), sep = ""),
                          R2cum = model@R2cum,
                          Q2cum = model@cvstat) |>
    pivot_longer(cols = c(.data$R2cum, .data$Q2cum),
                 names_to = "variable",
                 values_to = "value")

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PC,
                                 y = .data$value,
                                 fill = .data$variable)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Summary of fit") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}


#' @title Create a Hotelling T2 ellipse for a PCA score plot
#'
#' @description This function can be used to create a confidence (Hotelling T2) interval for a
#' PCA score plot.
#'
#' @param x x vector
#' @param y y vector
#' @param alpha confidence interval
#' @param len number of points to create the ellipse
#'
#' @return A data frame is returned with the points to create the ellipse.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a
#' PCA score plot.
#'
#' @importFrom stats var qf
#'
#' @author Rico Derks
simple_ellipse <- function(x, y, alpha = 0.95, len = 200) {
  N <- length(x)
  mypi <- seq(0, 2 * pi, length = len)

  r1 <- sqrt(var(x) * qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 <- sqrt(var(y) * qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))

  result <- data.frame(x = (r1 * cos(mypi) + mean(x)),
                       y = (r2 * sin(mypi) + mean(y)))

  return(result)
}


#' @title Create the CSS for coloring the checkboxes
#'
#' @description Create the CSS code for coloring checkboxes from prettyCheckboxGroup
#'     from shinyWidgets.
#'
#' @return Character(1) containing the CSS code.
#'
#' @noRd
#'
#' @author Rico Derks
#'
#' @importFrom RColorBrewer brewer.pal
#'
create_cb_css <- function() {
  batch_colors <- c(
    RColorBrewer::brewer.pal(n = 9, name = "Set1"),
    RColorBrewer::brewer.pal(n = 8, name = "Set2"),
    RColorBrewer::brewer.pal(n = 12, name = "Set3")
  )
  names(batch_colors) <- as.character(1:length(batch_colors))

  CSS <- ""
  for(a in 1:length(batch_colors)) {
    CSS <- paste0(CSS, ".pretty input[value='", a, "']~.state label:after,
                        .pretty input[value='", a,"']~.state label:before {
                           background-color: ", batch_colors[a] ,";
                        }")
  }

  return(CSS)
}


#' @title Do the calculations for a trend plot
#'
#' @description Do the calculations for a trend plot
#'
#' @param data data.frame, with all data.
#' @param meta_data character() vector with column names of the meta data.
#'
#' @returns data.frame in long format with log2fc column
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate select left_join group_by ungroup
#'
#' @author Rico Derks
#'
calc_trend <- function(data = NULL,
                       meta_data = NULL) {
  trend_data <- data |>
    tidyr::pivot_longer(
      cols = !meta_data,
      names_to = "lipid",
      values_to = "value"
    ) |>
    dplyr::filter(.data$sample_type == "qc")

  # ref data overall
  ref_data <- trend_data |>
    dplyr::filter(.data$sample_type == "qc",
                  .data$batch == 1) |>
    dplyr::filter(.data$read_order == min(.data$read_order)) |>
    dplyr::select(.data$lipid, .data$value) |>
    dplyr::rename(ref_value = .data$value)

  # ref data per batch
  ref_batch_data <- trend_data |>
    dplyr::filter(.data$sample_type == "qc") |>
    dplyr::group_by(.data$batch) |>
    dplyr::filter(.data$read_order == min(.data$read_order)) |>
    dplyr::ungroup() |>
    dplyr::select(.data$batch, .data$lipid, .data$value) |>
    dplyr::rename(ref_batch_value = .data$value)

  trend_data <- trend_data |>
    dplyr::left_join(
      y = ref_data,
      by = "lipid"
    ) |>
    dplyr::left_join(
      y = ref_batch_data,
      by = c("lipid" = "lipid",
             "batch" = "batch")
    ) |>
    dplyr::mutate(log2fc = log2(.data$value / .data$ref_value),
                  log2fc_batch = log2(.data$value / .data$ref_batch_value),
                  Sample_name = paste0(.data$Sample_name, " - ", .data$batch))

  # sort the samples in correct order
  trend_data$Sample_name <- factor(
    x = trend_data$Sample_name,
    levels = unique(trend_data$Sample_name[order(trend_data$batch, trend_data$read_order)]),
    labels = unique(trend_data$Sample_name[order(trend_data$batch, trend_data$read_order)])
  )

  return(trend_data)
}


#' @title Create the trend plot
#'
#' @description Create the trend plot, overall or per batch.
#'
#' @param data data.frame, with all data.
#' @param trend character(1) show the overall trend or per batch.
#'
#' @returns ggplot2 object
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line theme_minimal .data labs
#'     geom_point theme element_text facet_wrap vars labeller
#'
#' @author Rico Derks
#'
trend_plot <- function(data = NULL,
                       trend = c("overall", "batch")) {

  data$yaxis <- switch(
    trend,
    "overall" = data$log2fc,
    "batch" = data$log2fc_batch
  )

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Sample_name,
                                 y = .data$yaxis,
                                 group = .data$lipid,
                                 color = .data$batch)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::geom_point(size = 2,
                        alpha = 0.5) +
    ggplot2::geom_hline(yintercept = c(-0.5, 0.5),
                        linetype = 2,
                        color = "red") +
    ggplot2::labs(x = "Sample name",
                  y = "Log2(fold change)")

  if(trend == "batch") {
    strip_labels <- function(value) {
      return(paste0("Batch ", value))
    }

    p <- p +
      ggplot2::facet_wrap(~ .data$batch,
                          scales = "free",
                          labeller = ggplot2::labeller(batch = strip_labels))
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60,
                                                       hjust = 1))

  return(p)
}


#' @title Calculate the deviation from the mean
#'
#' @description Calculate the deviation from the mean for each QC sample overall
#'    and per batch.
#'
#' @param data data.frame, with all data.
#' @param meta_data character() vector with column names of the meta data.
#'
#' @returns data.frame in long format
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate select left_join group_by ungroup
#'
#' @author Rico Derks
#'
calc_deviation <- function(data = NULL,
                           meta_data = NULL) {
  dev_data <- data |>
    tidyr::pivot_longer(
      cols = !meta_data,
      names_to = "lipid",
      values_to = "value"
    ) |>
    dplyr::filter(.data$sample_type == "qc")

  dev_data <- dev_data |>
    dplyr::group_by(.data$batch, .data$lipid) |>
    dplyr::mutate(mean_value = mean(.data$value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(diff_mean = (.data$value - .data$mean_value) / .data$mean_value)

  return(dev_data)
}


#' @title Create a deviation from the mean plot
#'
#' @description Create a deviation from the mean histogram, overall or per batch.
#'
#' @param data data.frame, with all data.
#' @param trend character(1) show the overall trend or per batch.
#'
#' @returns ggplot2 object
#'
#' @importFrom ggplot2 ggplot aes geom_vline geom_histogram theme_minimal .data
#'     labs labeller
#'
#' @author Rico Derks
#'
deviation_plot <- function(data = NULL,
                           trend = c("overall", "batch")) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$diff_mean)) +
    ggplot2::geom_histogram(binwidth = 0.05) +
    ggplot2::geom_vline(xintercept = c(-0.25, 0.25),
                        linetype = 2,
                        color = "red") +
    ggplot2::labs(x = "Difference from the mean [proportion]")

  if(trend == "batch") {
    strip_labels <- function(value) {
      return(paste0("Batch ", value))
    }

    p <- p +
      ggplot2::facet_wrap(~ .data$batch,
                          scales = "free_y",
                          labeller = ggplot2::labeller(batch = strip_labels))
  }

  p <- p +
    ggplot2::theme_minimal()

  return(p)
}
