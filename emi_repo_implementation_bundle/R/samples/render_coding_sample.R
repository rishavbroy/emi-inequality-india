# Render coding samples from sample-start/sample-end markers in R source files.

#' Render all coding samples described by YAML specs
#'
#' @param spec_dir Directory containing `coding-*.yml` specs.
#' @return Character vector of output PDF paths.
render_coding_samples <- function(spec_dir = "application-samples/specs") {
  specs <- list.files(spec_dir, pattern = "^coding-.*\\.yml$", full.names = TRUE)
  vapply(specs, render_one_coding_sample, character(1))
}

#' Render one coding sample
#'
#' @param spec_path YAML spec path.
#' @return Output PDF path.
render_one_coding_sample <- function(spec_path) {
  spec <- yaml::read_yaml(spec_path)
  output <- spec$output
  work_dir <- file.path("application-samples", ".work")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  output_qmd <- file.path(work_dir, paste0(tools::file_path_sans_ext(basename(output)), ".qmd"))
  body <- extract_code_excerpts(spec)
  assemble_coding_sample_qmd(spec$cover_note, body, output_qmd)
  render_qmd_to_pdf(output_qmd, output)
  output
}
