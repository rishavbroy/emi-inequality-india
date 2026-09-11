# Render public-facing Quarto artifacts from within the {targets} graph.

#' Render a public PDF from a Quarto source
#'
#' @param qmd Path to the public QMD.
#' @param dependencies Objects or file targets that must be current before the
#'   document renders. The renderer does not interpret them; forcing the list
#'   makes the publication dependency explicit to {targets}.
#' @return Character vector of rendered PDF output paths for a `format = "file"`
#'   target.
render_public_pdf <- function(qmd, dependencies = list()) {
  force(dependencies)

  if (!file.exists(qmd)) {
    stop("Public PDF source QMD does not exist: ", qmd, call. = FALSE)
  }
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto CLI was not found on PATH; cannot render ", qmd, call. = FALSE)
  }

  pdf_path <- file.path(dirname(qmd), paste0(tools::file_path_sans_ext(basename(qmd)), ".pdf"))
  status <- system2("quarto", c("render", qmd, "--to", "pdf"))
  if (!identical(status, 0L)) {
    stop("quarto render ", qmd, " --to pdf failed with status ", status, call. = FALSE)
  }
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) {
    stop("quarto render did not create a non-empty ", pdf_path, call. = FALSE)
  }

  pdf_path
}


#' Render a paper PDF
#'
#' Backward-compatible paper wrapper around `render_public_pdf()`. Keeping the
#' domain-specific arguments here makes the legacy-paper dependency contract easy
#' to read. The current paper calls the shared renderer directly because its
#' in-document appendices add explicit exhibit dependencies.
render_paper_pdf <- function(paper_qmd, report_values, figure_files, table_files) {
  render_public_pdf(
    paper_qmd,
    dependencies = list(report_values, figure_files, table_files)
  )
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

poster_required_assets <- function() {
  c(
    "assets/uw-logo-horizontal-full-color-print.pdf",
    "assets/repo-qr.svg"
  )
}


#' Render the conference poster PDF and a PNG preview
#'
#' @param poster_qmd Path to the poster QMD.
#' @param figure_files Generated figure dependencies.
#' @param poster_assets Poster image dependencies.
#' @param project_root Repository root used as Typst's project root.
#' @return Rendered poster paths.
render_poster_pdf <- function(poster_qmd, figure_files, poster_assets, project_root = ".") {
  force(figure_files)
  force(poster_assets)
  if (!file.exists(poster_qmd)) stop("Poster source QMD does not exist: ", poster_qmd, call. = FALSE)
  if (!nzchar(Sys.which("quarto"))) stop("Quarto CLI was not found on PATH; cannot render ", poster_qmd, call. = FALSE)
  validate_poster_typst_templates(poster_qmd)
  typst_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  required_assets <- poster_required_assets()
  missing <- required_assets[!file.exists(file.path(typst_root, required_assets))]
  if (length(missing)) stop("Poster asset(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  pdf_path <- file.path(dirname(poster_qmd), "poster.pdf")
  status <- system2(
    "quarto",
    c("render", poster_qmd),
    env = paste0("TYPST_ROOT=", shQuote(typst_root))
  )
  if (!identical(status, 0L)) stop("quarto render ", poster_qmd, " failed with status ", status, call. = FALSE)
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) stop("Poster render did not create a non-empty ", pdf_path, call. = FALSE)
  png_path <- render_poster_png(pdf_path, file.path(dirname(pdf_path), "RishavRoy-Education.png"))
  c(pdf_path, png_path)
}

render_poster_png <- function(pdf_path, png_path = sub("\\.pdf$", ".png", pdf_path), dpi = 220) {
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) {
    stop("Poster PDF does not exist or is empty: ", pdf_path, call. = FALSE)
  }
  if (nzchar(Sys.which("pdftoppm"))) {
    prefix <- sub("\\.png$", "", png_path)
    status <- system2(
      "pdftoppm",
      c("-singlefile", "-png", "-r", as.character(dpi), shQuote(pdf_path), shQuote(prefix))
    )
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
