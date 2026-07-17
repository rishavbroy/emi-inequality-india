# Verify that public render dependencies exist before Quarto starts.
# This catches incomplete pipelines with one concise error.

source("scripts/public_output_contract.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
require_stamp <- "--require-final-stamp" %in% args

failures <- character()
add_failure <- function(...) failures <<- c(failures, paste0(...))

if (require_stamp && !file.exists(".pipeline-final-ok")) {
  add_failure("Missing .pipeline-final-ok; run `make pipeline-final` successfully before rendering public outputs.")
}

missing <- missing_or_empty_files(required_public_render_inputs())
if (length(missing)) add_failure("Missing required public file(s): ", paste(missing, collapse = ", "))

check_bibliography_paths <- function(path) {
  if (!file.exists(path)) return()
  lines <- readLines(path, warn = FALSE)
  idx <- grep("^bibliography:\\s*", lines)
  for (i in idx) {
    val <- trimws(sub("^bibliography:\\s*", "", lines[[i]]))
    if (!nzchar(val)) next
    val <- gsub("^['\"]|['\"]$", "", val)
    full <- file.path(dirname(path), val)
    if (!file.exists(full)) add_failure(path, " points to missing bibliography: ", val)
  }
}

for (qmd in public_qmd_sources()) check_bibliography_paths(qmd)

if (file.exists("paper/report.qmd")) {
  report <- paste(readLines("paper/report.qmd", warn = FALSE), collapse = "\n")
  if (grepl("render_public_table\\(", report) && !grepl("source_public_qmd_helpers", report, fixed = TRUE)) {
    add_failure("paper/report.qmd calls render_public_table() but does not source public QMD helpers.")
  }
}

if (!file.exists("R/output/public_qmd_helpers.R")) {
  add_failure("Missing shared public QMD helper file: R/output/public_qmd_helpers.R")
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Required public render dependencies are missing.", call. = FALSE)
}

message("Required public render dependencies exist.")
