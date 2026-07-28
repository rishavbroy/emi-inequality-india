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

poster_typst_template_paths <- function(poster_qmd) {
  extension_dir <- file.path(dirname(poster_qmd), "_extensions", "poster")
  c(
    template = file.path(extension_dir, "typst-template.typ"),
    show = file.path(extension_dir, "typst-show.typ")
  )
}



validate_poster_typst_templates <- function(poster_qmd) {
  paths <- poster_typst_template_paths(poster_qmd)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Poster Typst template file(s) missing: ",
      paste(unname(missing), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(paths)
}

poster_required_assets <- function(poster_logo_pdf) {
  c(poster_logo_pdf, "assets/repo-qr.svg")
}


#' Flatten the official print logo for Typst compatibility
#'
#' The UW print PDF contains an optional-content group. Typst warns when it
#' embeds such PDFs, so the build writes a visually equivalent PDF 1.3
#' derivative while retaining the official PDF as the source asset.
#'
#' @param source_pdf Official print PDF.
#' @param output_pdf Generated flattened PDF path.
#' @param ghostscript Ghostscript executable.
#' @return The generated PDF path.
flatten_poster_logo_pdf <- function(
    source_pdf,
    output_pdf = "outputs/derived/poster/uw-logo-horizontal-full-color-print-flat.pdf",
    ghostscript = Sys.which("gs")) {
  if (!file.exists(source_pdf) || file.info(source_pdf)$size <= 0L) {
    stop("Official poster logo PDF is missing or empty: ", source_pdf, call. = FALSE)
  }
  if (!nzchar(ghostscript)) {
    stop("Ghostscript is required to prepare the print-safe poster logo PDF.", call. = FALSE)
  }

  dir.create(dirname(output_pdf), recursive = TRUE, showWarnings = FALSE)
  status <- system2(
    ghostscript,
    c(
      "-q",
      "-dNOPAUSE",
      "-dBATCH",
      "-sDEVICE=pdfwrite",
      "-dCompatibilityLevel=1.3",
      shQuote(paste0("-sOutputFile=", normalizePath(output_pdf, mustWork = FALSE))),
      shQuote(normalizePath(source_pdf, mustWork = TRUE))
    )
  )
  if (!identical(status, 0L)) {
    stop("Ghostscript failed to flatten the poster logo PDF with status ", status, call. = FALSE)
  }
  if (!file.exists(output_pdf) || file.info(output_pdf)$size <= 0L) {
    stop("Poster logo preprocessing did not create a non-empty ", output_pdf, call. = FALSE)
  }

  output_pdf
}

#' Render the conference poster PDF and a PNG preview
#'
#' @param poster_qmd Path to the poster QMD.
#' @param figure_files Generated figure dependencies.
#' @param poster_logo_pdf Flattened print-logo dependency.
#' @return Rendered poster paths.
render_poster_pdf <- function(poster_qmd, figure_files, poster_logo_pdf) {
  force(figure_files)
  force(poster_logo_pdf)
  if (!file.exists(poster_qmd)) stop("Poster source QMD does not exist: ", poster_qmd, call. = FALSE)
  if (!nzchar(Sys.which("quarto"))) stop("Quarto CLI was not found on PATH; cannot render ", poster_qmd, call. = FALSE)
  validate_poster_typst_templates(poster_qmd)
  required_assets <- poster_required_assets(poster_logo_pdf)
  missing <- required_assets[!file.exists(required_assets)]
  if (length(missing)) stop("Poster asset(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  pdf_path <- file.path(dirname(poster_qmd), "poster.pdf")
  status <- system2("quarto", c("render", poster_qmd))
  if (!identical(status, 0L)) stop("quarto render ", poster_qmd, " failed with status ", status, call. = FALSE)
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) stop("Poster render did not create a non-empty ", pdf_path, call. = FALSE)
  png_path <- render_poster_png(pdf_path)
  c(pdf_path, png_path)
}

render_poster_png <- function(pdf_path, png_path = sub("\\.pdf$", ".png", pdf_path), dpi = 220) {
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) {
    stop("Poster PDF does not exist or is empty: ", pdf_path, call. = FALSE)
  }
  if (nzchar(Sys.which("pdftoppm"))) {
    prefix <- sub("\\.png$", "", png_path)
    status <- system2("pdftoppm", c("-singlefile", "-png", "-r", as.character(dpi), pdf_path, prefix))
    if (!identical(status, 0L)) stop("pdftoppm failed to create poster PNG with status ", status, call. = FALSE)
  } else {
    need_pkg("magick", "poster PNG preview rendering")
    image <- magick::image_read_pdf(pdf_path, density = dpi)
    image <- magick::image_background(image[[1]], "white", flatten = TRUE)
    magick::image_write(image, path = png_path, format = "png")
  }
  if (!file.exists(png_path) || file.info(png_path)$size <= 0L) {
    stop("Poster PNG preview was not created: ", png_path, call. = FALSE)
  }
  png_path
}
