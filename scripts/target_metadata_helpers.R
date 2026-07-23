# Shared {targets} metadata handling for strict and selected-target runs.

`%||%` <- function(x, y) if (is.null(x)) y else x

target_env_flag <- function(name, default = FALSE) {
  fallback <- if (isTRUE(default)) "true" else "false"
  !tolower(trimws(Sys.getenv(name, fallback))) %in% c("0", "false", "no", "off")
}

normalize_target_metadata <- function(meta) {
  meta <- data.frame(meta, check.names = FALSE, stringsAsFactors = FALSE)
  for (field in intersect(c("error", "warnings"), names(meta))) {
    if (is.list(meta[[field]])) {
      meta[[field]] <- vapply(meta[[field]], function(value) {
        value <- as.character(value %||% character())
        value <- value[!is.na(value) & nzchar(value)]
        paste(value, collapse = "; ")
      }, character(1))
    } else {
      meta[[field]] <- as.character(meta[[field]])
    }
  }
  meta
}


target_metadata_snapshot <- function(target_names = NULL) {
  if (!requireNamespace("targets", quietly = TRUE)) return(data.frame())
  names_expr <- if (is.null(target_names)) {
    NULL
  } else {
    tidyselect::any_of(unique(as.character(target_names)))
  }
  tryCatch(
    targets::tar_meta(
      names = names_expr,
      fields = c("name", "time", "error", "warnings"),
      targets_only = TRUE
    ),
    error = function(e) data.frame()
  )
}

target_run_metadata_scope <- function(selected_target_names, progress) {
  progress <- data.frame(progress, check.names = FALSE, stringsAsFactors = FALSE)
  executed <- if (all(c("name", "progress") %in% names(progress))) {
    as.character(progress$name[
      progress$progress %in% c("dispatched", "completed", "errored", "canceled")
    ])
  } else {
    character()
  }
  unique(c(as.character(selected_target_names), executed))
}

metadata_value_key <- function(x) {
  if (inherits(x, "POSIXt")) return(format(x, tz = "UTC", usetz = TRUE))
  if (is.list(x)) {
    return(vapply(x, function(value) paste(as.character(value %||% character()), collapse = "; "), character(1)))
  }
  as.character(x)
}

changed_target_metadata_names <- function(before, after) {
  before <- normalize_target_metadata(before)
  after <- normalize_target_metadata(after)
  if (!nrow(after) || !"name" %in% names(after)) return(character())
  if (!nrow(before) || !"name" %in% names(before)) return(as.character(after$name))

  fields <- intersect(c("time", "error", "warnings"), union(names(before), names(after)))
  before_index <- match(as.character(after$name), as.character(before$name))
  changed <- is.na(before_index)
  for (field in fields) {
    before_value <- if (field %in% names(before)) metadata_value_key(before[[field]]) else rep(NA_character_, nrow(before))
    after_value <- if (field %in% names(after)) metadata_value_key(after[[field]]) else rep(NA_character_, nrow(after))
    old <- before_value[before_index]
    different <- xor(is.na(old), is.na(after_value)) | (!is.na(old) & !is.na(after_value) & old != after_value)
    changed <- changed | different
  }
  as.character(after$name[changed])
}

select_target_metadata <- function(meta, target_names = NULL) {
  meta <- normalize_target_metadata(meta)
  if (!nrow(meta) || is.null(target_names)) return(meta)
  meta[as.character(meta$name) %in% as.character(target_names), , drop = FALSE]
}

target_metadata_issue_rows <- function(meta, field) {
  meta <- normalize_target_metadata(meta)
  if (!nrow(meta) || !field %in% names(meta)) return(meta[FALSE, , drop = FALSE])
  value <- as.character(meta[[field]])
  meta[!is.na(value) & nzchar(trimws(value)), , drop = FALSE]
}

safe_target_run_label <- function(label) {
  label <- gsub("[^A-Za-z0-9._-]+", "_", as.character(label))
  label <- gsub("^_+|_+$", "", label)
  if (nzchar(label)) label else "selected_targets"
}

write_target_run_metadata <- function(meta, label, dir = "outputs/diagnostics/build") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0("target_meta_after_", safe_target_run_label(label), "_run.csv"))
  utils::write.csv(normalize_target_metadata(meta), path, row.names = FALSE, na = "")
  path
}

record_target_warnings <- function(meta, label, path = "outputs/diagnostics/build/target_warnings.csv") {
  warnings <- target_metadata_issue_rows(meta, "warnings")
  if (!nrow(warnings)) return(invisible(data.frame()))
  fields <- intersect(c("name", "warnings"), names(warnings))
  out <- data.frame(warnings[, fields, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  out$run_label <- safe_target_run_label(label)
  out$recorded_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  prior <- if (file.exists(path) && file.info(path)$size > 0) {
    tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else {
    data.frame()
  }
  combined <- if (nrow(prior)) rbind(prior[names(out)], out) else out
  key <- paste(combined$name, combined$warnings, combined$run_label, sep = "\r")
  combined <- combined[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(combined, path, row.names = FALSE, na = "")
  invisible(out)
}

print_target_issues <- function(rows, field, heading) {
  if (!nrow(rows)) return(invisible(FALSE))
  cat(heading, "\n", sep = "")
  print(rows[, intersect(c("name", field), names(rows)), drop = FALSE], row.names = FALSE)
  invisible(TRUE)
}
