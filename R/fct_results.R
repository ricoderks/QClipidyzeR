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
#'
#' @return plotly object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 aes geom_path geom_point geom_vline geom_hline labs theme_minimal
#' @importFrom plotly ggplotly
#'
#' @noRd
#'
scores_plot <- function(model = NULL,
                        meta_data = NULL) {
  plot_data <- cbind(meta_data, model@scores)

  p <- plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_path(data = simple_ellipse(x = plot_data$PC1,
                                             y = plot_data$PC2),
                       ggplot2::aes(x = x,
                                    y = y),
                       colour = "gray") +
    ggplot2::geom_vline(xintercept = 0,
                        colour = "gray") +
    ggplot2::geom_hline(yintercept = 0,
                        colour = "gray") +
    ggplot2::geom_point(data = plot_data,
                        ggplot2::aes(x = PC1,
                                     y = PC2,
                                     colour = batch,
                                     shape = GroupName),
                        size = 3) +
    ggplot2::labs(title = "Scores plot",
                  caption = "Note: log transform / uv scaling / lipid species present in all pooled samples",
                  x = sprintf("PC 1 (%0.1f %%)", model@R2[1] * 100),
                  y = sprintf("PC 2 (%0.1f %%)", model@R2[2] * 100)) +
    ggplot2::theme_minimal()

  ply <- plotly::ggplotly(p)

  return(ply)
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
