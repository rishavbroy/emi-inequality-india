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

poster_typst_bundle_error <- function(message) {
  structure(
    list(message = message, call = NULL),
    class = c("poster_typst_bundle_error", "error", "condition")
  )
}

abort_poster_typst_bundle <- function(...) {
  stop(poster_typst_bundle_error(paste0(...)))
}

poster_typst_bundle_paths <- function(poster_qmd) {
  poster_dir <- dirname(poster_qmd)
  package_root <- file.path(
    poster_dir, "_extensions", "poster", "typst", "packages", "local"
  )
  package_names <- list.dirs(package_root, recursive = FALSE, full.names = TRUE)
  if (length(package_names) != 1L) {
    abort_poster_typst_bundle("Poster extension must bundle exactly one local Typst package.")
  }
  package_versions <- list.dirs(package_names[[1L]], recursive = FALSE, full.names = TRUE)
  if (length(package_versions) != 1L) {
    abort_poster_typst_bundle("Poster local Typst package must contain exactly one version.")
  }

  list(
    template = file.path(poster_dir, "_extensions", "poster", "typst-template.typ"),
    manifest = file.path(package_versions[[1L]], "typst.toml"),
    entrypoint = file.path(package_versions[[1L]], "poster.typ")
  )
}

typst_manifest_value <- function(lines, field) {
  pattern <- paste0('^\\s*', field, '\\s*=\\s*"([^"]+)"\\s*$')
  hit <- grep(pattern, lines, value = TRUE)
  if (length(hit) != 1L) {
    abort_poster_typst_bundle("Typst package manifest must define exactly one `", field, "` value.")
  }
  sub(pattern, "\\1", hit)
}

validate_poster_typst_bundle <- function(poster_qmd) {
  paths <- poster_typst_bundle_paths(poster_qmd)
  required <- unlist(paths, use.names = FALSE)
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    abort_poster_typst_bundle("Poster Typst bundle file(s) missing: ", paste(missing, collapse = ", "))
  }

  manifest <- readLines(paths$manifest, warn = FALSE)
  package_name <- typst_manifest_value(manifest, "name")
  package_version <- typst_manifest_value(manifest, "version")
  entrypoint <- typst_manifest_value(manifest, "entrypoint")
  if (!identical(basename(paths$entrypoint), entrypoint)) {
    abort_poster_typst_bundle("Poster Typst package entrypoint does not match its manifest.")
  }

  package_reference <- paste0("@local/", package_name, ":", package_version)
  template <- readLines(paths$template, warn = FALSE)
  imports <- grep('^#import\\s+"[^"]+"', template, value = TRUE)
  if (length(imports) != 1L) {
    abort_poster_typst_bundle("Poster Typst template must contain exactly one package import.")
  }
  imported_reference <- sub('^#import\\s+"([^"]+)".*$', '\\1', imports)
  if (!identical(imported_reference, package_reference)) {
    abort_poster_typst_bundle(
      "Poster Typst template must import the bundled package as `",
      package_reference, "`."
    )
  }

  invisible(c(paths, package_reference = package_reference))
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
  validate_poster_typst_bundle(poster_qmd)
  required_assets <- c("assets/uw-logo-horizontal-full-color-print.pdf", "assets/repo-qr.svg")
  missing <- required_assets[!file.exists(required_assets)]
  if (length(missing)) stop("Poster asset(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  pdf_path <- file.path(dirname(poster_qmd), "poster.pdf")
  status <- system2("quarto", c("render", poster_qmd))
  if (!identical(status, 0L)) stop("quarto render ", poster_qmd, " failed with status ", status, call. = FALSE)
  if (!file.exists(pdf_path) || file.info(pdf_path)$size <= 0L) stop("Poster render did not create a non-empty ", pdf_path, call. = FALSE)
  pdf_path
}
