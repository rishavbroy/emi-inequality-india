args <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% args || identical(Sys.getenv("EMI_CONFIG"), "config/final.yml")

if (strict && !file.exists(".pipeline-final-ok")) {
  stop("Strict report-value audit requires .pipeline-final-ok. Run `make pipeline-final` successfully first.", call. = FALSE)
}

if (strict && !file.exists(".pipeline-final-ok")) {
  stop("Strict report-value audit requires a successful current final pipeline run. Run `make pipeline-final` first.", call. = FALSE)
}

find_targets_store <- function(start = getwd()) {
  here <- normalizePath(start, mustWork = TRUE)
  repeat {
    candidate <- file.path(here, "_targets")
    if (dir.exists(candidate)) return(candidate)
    parent <- dirname(here)
    if (identical(parent, here)) return("_targets")
    here <- parent
  }
}

extract_legacy_inline_expressions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  start <- grep("^legacy_inline_expressions <- list\\(", lines)
  if (!length(start)) return(list())

  close <- which(seq_along(lines) > start[[1]] & trimws(lines) == ")")
  if (!length(close)) stop("Could not parse legacy_inline_expressions in ", path, call. = FALSE)
  end <- close[[1]]

  env <- new.env(parent = baseenv())
  eval(parse(text = paste(lines[start[[1]]:end], collapse = "\n")), envir = env)
  env$legacy_inline_expressions
}

load_report_values <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) {
    warning("Package 'targets' is not available; treating report_values as missing.", call. = FALSE)
    return(list())
  }
  tryCatch(
    targets::tar_read(report_values, store = find_targets_store()),
    error = function(e) {
      warning("Could not read report_values target: ", conditionMessage(e), call. = FALSE)
      list()
    }
  )
}

is_status_value <- function(value) {
  is.list(value) && !is.null(value$status) && !is.null(value$reason)
}

scalar_text <- function(value) {
  if (is_status_value(value)) {
    display <- value$value
    if (is.null(display) || length(display) == 0L || all(is.na(display))) display <- value$display
    value <- display
  }
  paste(as.character(unlist(value)), collapse = ", ")
}

is_placeholder_value <- function(value) {
  if (is.null(value) || length(value) == 0L) return(TRUE)
  if (is_status_value(value)) {
    if (!identical(value$status, "ok")) return(TRUE)
    value <- value$value
  }
  if (length(value) == 0L || all(is.na(value))) return(TRUE)
  text <- trimws(scalar_text(value))
  !nzchar(text) ||
    text %in% c("—", "-", "NA", "NaN", "Inf", "-Inf") ||
    grepl("not yet available|not run in current draft pipeline|out_of_active_pipeline|unavailable", text, ignore.case = TRUE)
}

expressions <- extract_legacy_inline_expressions("paper/report.qmd")
report_values <- load_report_values()
if (is.null(report_values) || !is.list(report_values)) report_values <- list()

rows <- lapply(names(expressions), function(key) {
  expr <- expressions[[key]]
  mapped_by <- if (key %in% names(report_values)) {
    "key"
  } else if (expr %in% names(report_values)) {
    "expression"
  } else {
    NA_character_
  }
  value <- if (identical(mapped_by, "key")) {
    report_values[[key]]
  } else if (identical(mapped_by, "expression")) {
    report_values[[expr]]
  } else {
    NULL
  }
  status <- if (is_status_value(value)) value$status else if (is.na(mapped_by)) "unmapped" else "mapped"
  reason <- if (is_status_value(value)) value$reason else NA_character_
  placeholder <- is.na(mapped_by) || is_placeholder_value(value)

  data.frame(
    key = key,
    expression = expr,
    mapped_by = mapped_by,
    status = status,
    placeholder = placeholder,
    value = if (is.null(value)) NA_character_ else scalar_text(value),
    reason = reason,
    stringsAsFactors = FALSE
  )
})

audit <- if (length(rows)) do.call(rbind, rows) else data.frame()

cat("Report values audit\n")
cat("  expressions: ", nrow(audit), "\n", sep = "")
cat("  mapped: ", sum(!is.na(audit$mapped_by)), "\n", sep = "")
cat("  unmapped: ", sum(is.na(audit$mapped_by)), "\n", sep = "")
cat("  placeholder/status values: ", sum(audit$placeholder), "\n", sep = "")

if (nrow(audit)) {
  print(audit[order(audit$placeholder, is.na(audit$mapped_by), audit$key, decreasing = TRUE), ], row.names = FALSE)
}

fail <- is.na(audit$mapped_by) | audit$placeholder
if (strict && any(fail)) {
  bad <- audit[fail, c("key", "expression", "status", "reason", "value")]
  cat("\nStrict report-value audit failures:\n")
  print(bad, row.names = FALSE)
  stop("Final report has unmapped or placeholder-valued legacy inline expressions.", call. = FALSE)
}
