# Check that application-sample YAML specs refer to real, nonempty excerpts.

source("R/application_samples/extract_qmd_excerpts.R")
source("R/application_samples/extract_code_excerpts.R")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required for application-sample specs. Run `make init-renv`.", call. = FALSE)
}

failures <- character()

writing_specs <- list.files("application-samples/specs", pattern = "^writing-.*\\.yml$", full.names = TRUE)
if (!length(writing_specs)) stop("No writing-sample specs found in application-samples/specs.", call. = FALSE)

for (spec_path in writing_specs) {
  spec <- yaml::read_yaml(spec_path)
  source <- spec$source %||% "paper/report.qmd"
  excerpts <- unlist(spec$excerpts, use.names = FALSE)
  blocks <- extract_marked_divs(readLines(source, warn = FALSE))

  missing <- setdiff(excerpts, names(blocks))
  if (length(missing)) {
    failures <- c(failures, paste0(spec_path, " missing writing excerpts: ", paste(missing, collapse = ", ")))
  }

  for (id in intersect(excerpts, names(blocks))) {
    nonblank <- sum(nzchar(trimws(blocks[[id]])))
    if (nonblank < 2L) {
      failures <- c(failures, paste0(spec_path, " has near-empty writing excerpt: ", id))
    }
  }
}

coding_specs <- list.files("application-samples/specs", pattern = "^coding-.*\\.yml$", full.names = TRUE)
if (!length(coding_specs)) stop("No coding-sample specs found in application-samples/specs.", call. = FALSE)

for (spec_path in coding_specs) {
  spec <- yaml::read_yaml(spec_path)
  result <- tryCatch({
    validate_code_excerpt_markers(spec)
    TRUE
  }, error = function(e) {
    failures <<- c(failures, paste0(spec_path, " coding marker error: ", conditionMessage(e)))
    FALSE
  })

  if (isTRUE(result)) {
    for (excerpt in spec$excerpts) {
      lines <- extract_between_sample_markers(excerpt$file, excerpt$id)
      nonblank <- sum(nzchar(trimws(lines)))
      if (nonblank < 2L) {
        failures <- c(failures, paste0(spec_path, " has near-empty coding excerpt: ", excerpt$id))
      }
    }
  }
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Application-sample specs are inconsistent with report/code excerpt markers.", call. = FALSE)
}

message("Writing and coding sample specs match nonempty excerpts.")
