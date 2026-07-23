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


target_metadata_selection <- function(target_names) {
  target_names <- unique(as.character(target_names))
  rlang::expr(tidyselect::any_of(!!target_names))
}

target_metadata_snapshot <- function(target_names = NULL) {
  if (!requireNamespace("targets", quietly = TRUE)) return(data.frame())
  if (is.null(target_names)) {
    return(targets::tar_meta(
      fields = c("name", "time", "error", "warnings"),
      targets_only = TRUE
    ))
  }

  selection <- target_metadata_selection(target_names)
  rlang::inject(
    targets::tar_meta(
      names = !!selection,
      fields = c("name", "time", "error", "warnings"),
      targets_only = TRUE
    )
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
