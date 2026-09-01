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

# Cache the canonical {targets} manifest so architecture tests assert pipeline
# semantics rather than the physical placement/formatting of target declarations.
.repo_target_manifest_cache <- new.env(parent = emptyenv())

repo_target_manifest <- function() {
  if (!exists("manifest", envir = .repo_target_manifest_cache, inherits = FALSE)) {
    manifest <- targets::tar_manifest(
      fields = tidyselect::any_of(c("name", "command")),
      script = repo_file("_targets.R")
    )
    assign("manifest", manifest, envir = .repo_target_manifest_cache)
  }
  get("manifest", envir = .repo_target_manifest_cache, inherits = FALSE)
}

repo_target_command <- function(name) {
  manifest <- repo_target_manifest()
  rows <- manifest$name == name
  if (sum(rows) != 1L) {
    stop("Expected exactly one target named ", name, call. = FALSE)
  }
  command <- manifest$command[[which(rows)]]
  if (is.character(command)) {
    return(paste(command, collapse = "\n"))
  }
  paste(deparse(command, width.cutoff = 500L), collapse = "\n")
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

repo_core_target_text <- function() {
  files <- repo_pipeline_target_files("^core_.*_targets\\.R$")
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
