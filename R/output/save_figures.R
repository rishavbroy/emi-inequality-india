# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' save figures
#'
#' @return A tibble, model object, list, or file path depending on context.
save_figures <- function(figures, cfg) {
  dir <- "outputs/figures/main"
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(dir, "figure_manifest.csv")
  utils::write.csv(data.frame(name = names(figures)), p, row.names = FALSE)
  p
}

#' save figure pdf png
#'
#' @return A tibble, model object, list, or file path depending on context.
save_figure_pdf_png <- function(plot, path_base, width = 7, height = 5, dpi = 300) {
  ggplot2::ggsave(paste0(path_base, ".png"), plot, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(paste0(path_base, ".pdf"), plot, width = width, height = height)
  c(paste0(path_base, ".png"), paste0(path_base, ".pdf"))
}

#' save map pdf png
#'
#' @return A tibble, model object, list, or file path depending on context.
save_map_pdf_png <- function(plot, path_base) {
  save_figure_pdf_png(plot, path_base)
}

#' save diagnostic figure
#'
#' @return A tibble, model object, list, or file path depending on context.
save_diagnostic_figure <- function(plot, path_base) {
  save_figure_pdf_png(plot, path_base)
}
