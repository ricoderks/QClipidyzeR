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
#' @importFrom dplyr if_else
#' @importFrom stats sd
#'
#' @noRd
#'
calc_rsd <- function(data = NULL, meta_data = NULL, lipid_class = FALSE) {
  rsd_data <- data |>
    filter(.data$NormType == "Pooled sample") |>
    pivot_longer(
      cols = !meta_data,
      names_to = "lipid",
      values_to = "value"
    ) |>
    group_by(.data$lipid) |>
    summarise(mean = mean(.data$value, na.rm = TRUE),
              stdev = sd(.data$value, na.rm = TRUE),
              rsd = .data$stdev / mean)

  # extract lipid class information for lipid species
  if(lipid_class) {
    rsd_data <- rsd_data |>
      mutate(lipid_class = str_extract(string = .data$lipid,
                                       pattern = "^[a-zA-Z]* O?"),
             lipid_class = if_else(lipid_class == "Cer ",
                                   str_extract(string = .data$lipid,
                                               pattern = "^[a-zA-Z]* d18:[01]"),
                                   .data$lipid_class))
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
    mutate(rsd = .data$rsd * 100) |>
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
  pca_data[, -meta_data] <- log(pca_data[, -meta_data])
  # put the few NA's back
  pca_data[pca_data == 0] <- NA

  mod <- pcaMethods::pca(object = pca_data[, -meta_data],
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
#' @importFrom ggplot2 aes geom_path geom_point geom_vline geom_hline labs theme_minimal theme scale_color_manual
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
    plot_data <- plot_data[plot_data$NormType %in% sample_type, ]
  }

  # create the colors for the batches
  batch_colors <- c(
    RColorBrewer::brewer.pal(n = 9, name = "Set1"),
    RColorBrewer::brewer.pal(n = 8, name = "Set2"),
    RColorBrewer::brewer.pal(n = 12, name = "Set3")
  )
  names(batch_colors) <- as.character(1:length(batch_colors))

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
                                     shape = .data$NormType),
                        size = 3) +
    ggplot2::scale_color_manual(values = batch_colors) +
    ggplot2::labs(title = "Scores plot",
                  caption = "Note: log transform / uv scaling / lipid species present in all pooled samples",
                  x = sprintf("PC 1 (%0.1f %%)", model@R2[1] * 100),
                  y = sprintf("PC 2 (%0.1f %%)", model@R2[2] * 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

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
