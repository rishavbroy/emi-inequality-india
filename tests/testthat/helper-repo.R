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
    script <- repo_file("_targets.R")
    root <- dirname(script)
    old_wd <- setwd(root)
    on.exit(setwd(old_wd), add = TRUE)

    # tar_manifest() normally evaluates the target script in a clean callr
    # process. For tests, its documented callr_function = NULL mode lets us
    # control the working directory explicitly while keeping evaluation in an
    # isolated environment. This matters because the target script deliberately
    # uses project-relative source paths.
    manifest <- targets::tar_manifest(
      fields = tidyselect::any_of(c("name", "command")),
      callr_function = NULL,
      envir = new.env(parent = globalenv()),
      script = "_targets.R"
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
