# Render public-facing Quarto artifacts from within the {targets} graph.

#' Render the final report PDF
#'
#' @param report_qmd Path to the generated public report QMD.
#' @param report_values Report inline values target. Forced so stale inline values
#'   invalidate the render target.
#' @param figure_files Public figure file target paths. Forced so figure changes
#'   invalidate the render target.
#' @param table_files Public table file target paths. Forced so table changes
#'   invalidate the render target.
#' @return Character vector of rendered PDF output paths for a `format = "file"`
#'   target.
render_report_pdf <- function(report_qmd, report_values, figure_files, table_files) {
  force(report_values)
  force(figure_files)
  force(table_files)

  if (!file.exists(report_qmd)) {
    stop("Report source QMD does not exist: ", report_qmd, call. = FALSE)
  }
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto CLI was not found on PATH; cannot render ", report_qmd, call. = FALSE)
  }

  pdf_path <- file.path(dirname(report_qmd), paste0(tools::file_path_sans_ext(basename(report_qmd)), ".pdf"))
  status <- system2("quarto", c("render", report_qmd, "--to", "pdf"))
  if (!identical(status, 0L)) {
    stop("quarto render ", report_qmd, " --to pdf failed with status ", status, call. = FALSE)
  }
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) {
    stop("quarto render did not create a non-empty ", pdf_path, call. = FALSE)
  }

  pdf_path
}
