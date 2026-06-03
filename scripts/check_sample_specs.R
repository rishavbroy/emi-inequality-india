# Check that application-sample YAML specs refer to real, nonempty excerpts.

source("R/samples/extract_qmd_excerpts.R")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required for application-sample specs. Run `make init-renv`.", call. = FALSE)
}

specs <- list.files("application-samples/specs", pattern = "^writing-.*\\.yml$", full.names = TRUE)
if (!length(specs)) stop("No writing-sample specs found in application-samples/specs.", call. = FALSE)

failures <- character()
for (spec_path in specs) {
  spec <- yaml::read_yaml(spec_path)
  source <- spec$source %||% "paper/report.qmd"
  excerpts <- unlist(spec$excerpts, use.names = FALSE)
  blocks <- extract_marked_divs(readLines(source, warn = FALSE))

  missing <- setdiff(excerpts, names(blocks))
  if (length(missing)) {
    failures <- c(failures, paste0(spec_path, " missing: ", paste(missing, collapse = ", ")))
  }

  for (id in intersect(excerpts, names(blocks))) {
    nonblank <- sum(nzchar(trimws(blocks[[id]])))
    if (nonblank < 2L) {
      failures <- c(failures, paste0(spec_path, " has near-empty excerpt: ", id))
    }
  }
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Application-sample specs are inconsistent with paper/report.qmd.", call. = FALSE)
}

message("Writing-sample specs match nonempty report excerpts.")
