# Render writing samples from excerpt markers in paper/report.qmd.

#' Render all writing samples described by YAML specs
#'
#' @param spec_dir Directory containing `writing-*.yml` specs.
#' @return Character vector of output PDF paths.
render_writing_samples <- function(spec_dir = "application-samples/specs") {
  specs <- list.files(spec_dir, pattern = "^writing-.*\\.yml$", full.names = TRUE)
  vapply(specs, render_one_writing_sample, character(1))
}

#' Render one writing sample
#'
#' @param spec_path YAML spec path.
#' @return Output PDF path.
render_one_writing_sample <- function(spec_path) {
  spec <- yaml::read_yaml(spec_path)
  output <- spec$output
  work_dir <- file.path("application-samples", ".work")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  output_qmd <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(output)), ".qmd"))

  if (identical(spec$mode, "full")) {
    source_lines <- readLines(spec$source, warn = FALSE)
    assemble_writing_sample_qmd(spec$cover_note, source_lines, output_qmd)
  } else {
    excerpts <- extract_qmd_excerpts(spec$source, unlist(spec$excerpts, use.names = FALSE))
    assemble_writing_sample_qmd(spec$cover_note, excerpts, output_qmd)
  }

  render_qmd_to_pdf(output_qmd, output)
  output
}

#' Render a QMD file to a PDF path
#'
#' @param input_qmd Assembled temporary QMD.
#' @param output_file Desired PDF output path.
#' @return Output PDF path invisibly.
render_qmd_to_pdf <- function(input_qmd, output_file) {
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  output_dir <- dirname(normalizePath(output_file, mustWork = FALSE))
  output_name <- basename(output_file)

  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto CLI was not found on PATH; cannot render ", input_qmd, call. = FALSE)
  }

  status <- system2(
    "quarto",
    c("render", input_qmd, "--to", "pdf", "--output", output_name, "--output-dir", output_dir)
  )

  if (!identical(status, 0L)) stop("quarto render failed for ", input_qmd, call. = FALSE)
  invisible(output_file)
}
