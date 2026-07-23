# Render public-facing Quarto artifacts from within the {targets} graph.

#' Render the final report PDF
#'
#' @param report_qmd Path to the public report QMD.
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


#' Render a public Quarto HTML document
#'
#' @param qmd Path to the source QMD.
#' @param dependencies Optional objects to force as target dependencies before
#'   rendering. This keeps source-only notes explicit when they read targets at
#'   render time, such as named report values.
#' @return Character vector of rendered HTML output paths for a `format = "file"`
#'   target.
render_public_html <- function(qmd, dependencies = list()) {
  force(dependencies)

  if (!file.exists(qmd)) {
    stop("Public note source QMD does not exist: ", qmd, call. = FALSE)
  }
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto CLI was not found on PATH; cannot render ", qmd, call. = FALSE)
  }

  html_path <- file.path(dirname(qmd), paste0(tools::file_path_sans_ext(basename(qmd)), ".html"))
  status <- system2("quarto", c("render", qmd, "--to", "html"))
  if (!identical(status, 0L)) {
    stop("quarto render ", qmd, " --to html failed with status ", status, call. = FALSE)
  }
  if (!file.exists(html_path) || file.info(html_path)$size <= 0L) {
    stop("quarto render did not create a non-empty ", html_path, call. = FALSE)
  }

  html_path
}

#' Render the conference poster PDF
#'
#' @param poster_qmd Path to the poster QMD.
#' @param figure_files Generated figure dependencies.
#' @return Rendered poster PDF path.
render_poster_pdf <- function(poster_qmd, figure_files) {
  force(figure_files)
  if (!file.exists(poster_qmd)) stop("Poster source QMD does not exist: ", poster_qmd, call. = FALSE)
  if (!nzchar(Sys.which("quarto"))) stop("Quarto CLI was not found on PATH; cannot render ", poster_qmd, call. = FALSE)
  required_assets <- c("assets/uw-logo-horizontal-full-color-print.pdf", "assets/repo-qr.svg")
  missing <- required_assets[!file.exists(required_assets)]
  if (length(missing)) stop("Poster asset(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  pdf_path <- file.path(dirname(poster_qmd), "poster.pdf")
  status <- system2("quarto", c("render", poster_qmd))
  if (!identical(status, 0L)) stop("quarto render ", poster_qmd, " failed with status ", status, call. = FALSE)
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) stop("Poster render did not create a non-empty ", pdf_path, call. = FALSE)
  pdf_path
}
