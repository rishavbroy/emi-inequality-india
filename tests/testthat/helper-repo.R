repo_file <- function(...) {
  candidates <- c(
    file.path(Sys.getenv("EMI_PROJECT_ROOT", unset = getwd()), ...),
    file.path(getwd(), ...),
    file.path(getwd(), "..", ...),
    file.path(getwd(), "..", "..", ...)
  )
  hits <- candidates[file.exists(candidates)]
  if (!length(hits)) {
    stop("Could not locate repository file: ", file.path(...), call. = FALSE)
  }
  normalizePath(hits[[1]], mustWork = TRUE)
}

repo_text <- function(...) {
  paste(readLines(repo_file(...), warn = FALSE), collapse = "\n")
}

repo_pipeline_target_files <- function(pattern = ".*_targets\\.R$") {
  pipeline_dir <- repo_file("R", "pipeline")
  sort(list.files(pipeline_dir, pattern = pattern, full.names = TRUE))
}

repo_target_definition_text <- function() {
  files <- c(repo_file("_targets.R"), repo_pipeline_target_files())
  paste(
    unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )
}

repo_extended_target_text <- function() {
  files <- repo_pipeline_target_files("^extended_.*_targets\\.R$")
  paste(
    unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )
}
