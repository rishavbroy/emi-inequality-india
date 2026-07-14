# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' list analysis notebooks
#'
#' @return Character vector of analysis QMD paths.
list_analysis_qmd_files <- function(root = "analysis") {
  if (!dir.exists(root)) stop("Missing analysis directory: ", root, call. = FALSE)
  qmds <- list.files(root, pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
  qmds <- qmds[!grepl("(^|/)_[^/]+[.]qmd$", qmds)]
  normalizePath(sort(qmds), mustWork = TRUE)
}

#' list rendered-analysis runtime inputs
#'
#' The analysis notebooks read diagnostic CSV/PNG artifacts directly from the
#' filesystem through analysis/_analysis_helpers.R.  Keep those filesystem reads
#' visible to {targets} by registering the generated diagnostic/benchmark files
#' as file dependencies of the rendered analysis Markdown targets.
#'
#' @return Character vector of existing analysis-input file paths.
list_analysis_runtime_input_files <- function(root = ".") {
  roots <- file.path(root, c(
    "analysis/_analysis_helpers.R",
    "outputs/diagnostics/public",
    "outputs/diagnostics/extended",
    "outputs/benchmarking"
  ))
  files <- unlist(lapply(roots, function(path) {
    if (file.exists(path) && !dir.exists(path)) return(path)
    if (!dir.exists(path)) return(character())
    list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  }), use.names = FALSE)
  files <- files[file.exists(files) & !dir.exists(files)]
  normalizePath(sort(unique(files)), mustWork = TRUE)
}

#' render one analysis notebook to GitHub-flavored Markdown
#'
#' @return Path to the rendered Markdown file.
render_analysis_markdown_file <- function(qmd, runtime_inputs = character()) {
  qmd <- normalizePath(qmd, mustWork = TRUE)
  if (!nzchar(Sys.which("quarto"))) stop("quarto is required to render analysis notebooks.", call. = FALSE)
  invisible(runtime_inputs)
  message("Rendering ", qmd, " to GitHub-flavored Markdown")
  status <- system2("quarto", c("render", qmd, "--to", "gfm"))
  if (!identical(status, 0L)) stop("quarto render failed for ", qmd, " with status ", status, call. = FALSE)
  out <- sub("[.]qmd$", ".md", qmd)
  if (!file.exists(out) || file.info(out)$size <= 0) {
    stop("Analysis render did not create non-empty Markdown output: ", out, call. = FALSE)
  }
  normalizePath(out, mustWork = TRUE)
}
