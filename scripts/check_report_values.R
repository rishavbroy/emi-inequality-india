# Check that current public documents use named report values backed by the targets graph.

source("scripts/public_output_contract.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% args
allow_status_placeholders <- "--allow-status-placeholders" %in% args

is_report_value_status <- function(value) {
  is.list(value) && !is.null(value$status) && !is.null(value$reason)
}

extract_report_value_keys <- function(paths) {
  keys <- character()
  pattern <- "report_value\\s*\\(\\s*['\"][^'\"]+['\"]\\s*\\)"
  for (path in paths[file.exists(paths)]) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (matches[[1]] == -1L) next
    found <- regmatches(text, list(matches))[[1]]
    found <- sub("^report_value\\s*\\(\\s*['\"]", "", found, perl = TRUE)
    found <- sub("['\"]\\s*\\)$", "", found, perl = TRUE)
    keys <- c(keys, found)
  }
  sort(unique(keys))
}

report_sources <- public_report_value_sources()
keys <- extract_report_value_keys(report_sources)

report_values <- tryCatch(
  targets::tar_read(report_values),
  error = function(e) stop("Could not read report_values target. Run `make pipeline-final` first. ", conditionMessage(e), call. = FALSE)
)
if (is.null(report_values) || !is.list(report_values)) {
  stop("report_values target did not return a named list.", call. = FALSE)
}

names_report_values <- names(report_values)
missing <- setdiff(keys, names_report_values)
placeholder <- vapply(report_values[keys[keys %in% names_report_values]], is_report_value_status, logical(1))
placeholder_keys <- names(placeholder)[placeholder]

cat("Report values check\n")
cat("  keys used by public QMDs:", length(keys), "\n")
cat("  mapped:", length(keys) - length(missing), "\n")
cat("  unmapped:", length(missing), "\n")
cat("  placeholder/status values:", length(placeholder_keys), "\n")
cat("  status placeholders allowed for build-gate check:", if (allow_status_placeholders) "yes" else "no", "\n")

if (length(keys)) {
  result <- data.frame(
    key = keys,
    mapped = keys %in% names_report_values,
    placeholder = keys %in% placeholder_keys,
    stringsAsFactors = FALSE
  )
  print(result, row.names = FALSE)
}

if (length(missing) || (strict && length(placeholder_keys) && !allow_status_placeholders)) {
  if (length(missing)) cat("Unmapped report value key(s):", paste(missing, collapse = ", "), "\n")
  if (length(placeholder_keys) && strict && !allow_status_placeholders) {
    cat("Placeholder/status report value key(s):", paste(placeholder_keys, collapse = ", "), "\n")
  }
  stop("Current report values are incomplete.", call. = FALSE)
}
