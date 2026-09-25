# Validate the tracked application-sample manifest against the current paper and code.

source("R/io/utils_data_frame.R")
source("R/application_samples/sample_manifest.R")
source("R/application_samples/writing_sample_sections.R")
source("R/application_samples/extract_code_excerpts.R")

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required for application-sample configuration. Run `make restore`.", call. = FALSE)
}

manifest <- read_application_sample_manifest()
source_path <- manifest$paper$source
source_lines <- readLines(source_path, warn = FALSE)
index <- qmd_section_index(source_lines)
failures <- character()

for (spec in manifest$writing) {
  if (identical(spec$mode %||% "excerpt", "full")) next
  ids <- unlist(spec$sections, use.names = FALSE)
  missing <- setdiff(ids, index$id)
  if (length(missing)) {
    failures <- c(failures, paste0("writing sample ", spec$id, " missing section IDs: ", paste(missing, collapse = ", ")))
  }
  if (is.null(spec$target_pages) || !is.finite(as.numeric(spec$target_pages)) || as.integer(spec$target_pages) < 1L) {
    failures <- c(failures, paste0("writing sample ", spec$id, " needs a positive target_pages value"))
  }
}

for (spec in manifest$coding) {
  result <- tryCatch({
    validate_code_excerpt_markers(spec)
    TRUE
  }, error = function(e) {
    failures <<- c(failures, paste0("coding sample ", spec$id, ": ", conditionMessage(e)))
    FALSE
  })
  if (isTRUE(result)) {
    for (excerpt in spec$excerpts) {
      lines <- extract_between_sample_markers(excerpt$file, excerpt$id)
      if (sum(nzchar(trimws(lines))) < 2L) {
        failures <- c(failures, paste0("coding sample ", spec$id, " has near-empty excerpt: ", excerpt$id))
      }
    }
  }
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Application-sample manifest is inconsistent with the current paper or code markers.", call. = FALSE)
}

message("Application-sample manifest matches current paper sections and code markers.")
