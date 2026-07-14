# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' list analysis notebooks
#'
#' @return Character vector of analysis QMD paths relative to the project root.
list_analysis_qmd_files <- function(root = "analysis") {
  if (!dir.exists(root)) stop("Missing analysis directory: ", root, call. = FALSE)
  qmds <- list.files(root, pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
  qmds <- qmds[!grepl("(^|/)_[^/]+[.]qmd$", qmds)]
  sort(qmds)
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

  # Render from the notebook directory and pass Quarto only the basename.
  # This avoids Quarto CLI edge cases with absolute paths containing spaces
  # while preserving {targets}' file tracking on the absolute QMD path.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dirname(qmd))
  status <- system2("quarto", c("render", basename(qmd), "--to", "gfm"))
  if (!identical(status, 0L)) stop("quarto render failed for ", qmd, " with status ", status, call. = FALSE)

  out <- sub("[.]qmd$", ".md", qmd)
  if (!file.exists(out) || file.info(out)$size <= 0) {
    stop("Analysis render did not create non-empty Markdown output: ", out, call. = FALSE)
  }
  normalizePath(out, mustWork = TRUE)
}


#' construct a stable target name for an analysis file
#'
#' Static per-note render targets avoid illegal dynamic branching over a vector
#' of file paths and let {targets} skip each rendered Markdown file separately.
#'
#' @return A syntactically valid {targets} target name.
analysis_note_target_name <- function(prefix, path, root = "analysis") {
  rel <- gsub("\\\\", "/", path)
  root_prefix <- paste0(gsub("\\\\", "/", root), "/")
  rel <- sub(paste0("^", root_prefix), "", rel)
  stem <- tools::file_path_sans_ext(rel)
  slug <- gsub("[^A-Za-z0-9]+", "_", stem)
  slug <- gsub("^_+|_+$", "", slug)
  if (!nzchar(slug)) stop("Could not derive analysis target name for: ", path, call. = FALSE)
  paste0(prefix, "_", slug)
}

#' define cached analysis-note render targets
#'
#' Each QMD is first declared as a `format = "file"` target, then rendered by
#' its own Markdown file target. The final `analysis_markdown_files` target is
#' a convenience aggregate used by Makefile wrappers and audit scripts.
#'
#' @return List of {targets} target definitions.
analysis_markdown_target_definitions <- function(root = "analysis") {
  qmds <- list_analysis_qmd_files(root)
  if (!length(qmds)) stop("No analysis QMD files found under: ", root, call. = FALSE)

  qmd_target_names <- vapply(qmds, analysis_note_target_name, character(1), prefix = "analysis_qmd", root = root)
  md_target_names <- vapply(qmds, analysis_note_target_name, character(1), prefix = "analysis_md", root = root)

  qmd_targets <- Map(function(target_name, qmd) {
    targets::tar_target_raw(
      name = target_name,
      command = substitute(qmd_path, list(qmd_path = qmd)),
      format = "file"
    )
  }, qmd_target_names, qmds)

  md_targets <- Map(function(target_name, qmd_target_name) {
    targets::tar_target_raw(
      name = target_name,
      command = substitute(
        render_analysis_markdown_file(qmd_target, analysis_runtime_input_files),
        list(qmd_target = as.name(qmd_target_name))
      ),
      format = "file"
    )
  }, md_target_names, qmd_target_names)

  aggregate_command <- as.call(c(as.name("c"), lapply(md_target_names, as.name)))

  c(
    list(
      targets::tar_target_raw(
        name = "analysis_runtime_input_files",
        command = quote(list_analysis_runtime_input_files()),
        format = "file",
        cue = targets::tar_cue(mode = "always")
      )
    ),
    qmd_targets,
    md_targets,
    list(
      targets::tar_target_raw(
        name = "analysis_markdown_files",
        command = aggregate_command,
        format = "file"
      )
    )
  )
}
