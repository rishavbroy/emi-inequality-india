# Run an explicitly selected group of {targets} targets and fail if any selected
# target records an error or warning. Warnings can be acknowledged only through
# the explicit EMI_ALLOW_TARGET_WARNINGS=true escape hatch.

source("scripts/target_metadata_helpers.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
starts_with_arg <- ""
targets_arg <- ""
for (i in seq_along(args)) {
  if (identical(args[[i]], "--starts-with") && i < length(args)) starts_with_arg <- args[[i + 1L]]
  if (identical(args[[i]], "--targets") && i < length(args)) targets_arg <- args[[i + 1L]]
}
if (!nzchar(starts_with_arg) && !nzchar(targets_arg)) {
  stop("Usage: Rscript scripts/run_targets_checked.R --starts-with PREFIX | --targets TARGET[,TARGET...]", call. = FALSE)
}
if (nzchar(starts_with_arg) && nzchar(targets_arg)) {
  stop("Use either --starts-with or --targets, not both.", call. = FALSE)
}
if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}
if (!requireNamespace("tidyselect", quietly = TRUE)) {
  stop("Package 'tidyselect' is required. Run `make init-renv`.", call. = FALSE)
}

manifest <- tryCatch(
  targets::tar_manifest(fields = "name"),
  error = function(e) data.frame(name = character())
)
active_target_names <- as.character(manifest$name)
if (nzchar(starts_with_arg)) {
  selected_target_names <- active_target_names[startsWith(active_target_names, starts_with_arg)]
  run_label <- starts_with_arg
  if (!length(selected_target_names)) stop("No active targets match prefix: ", starts_with_arg, call. = FALSE)
} else {
  selected_target_names <- trimws(unlist(strsplit(targets_arg, ",", fixed = TRUE), use.names = FALSE))
  selected_target_names <- selected_target_names[nzchar(selected_target_names)]
  missing_targets <- setdiff(selected_target_names, active_target_names)
  if (length(missing_targets)) {
    stop("Requested targets are not active in the current target graph: ", paste(missing_targets, collapse = ", "), call. = FALSE)
  }
  run_label <- "selected_targets"
}

meta_before <- target_metadata_snapshot()

status <- 0L
tryCatch(
  targets::tar_make(names = tidyselect::all_of(selected_target_names)),
  error = function(e) {
    message("targets::tar_make() errored: ", conditionMessage(e))
    status <<- 1L
  }
)

meta <- target_metadata_snapshot()
changed_target_names <- changed_target_metadata_names(meta_before, meta)
metadata_scope <- unique(c(selected_target_names, changed_target_names))
meta_selected <- select_target_metadata(meta, metadata_scope)
write_target_run_metadata(meta_selected, run_label)

errors <- target_metadata_issue_rows(meta_selected, "error")
warnings <- target_metadata_issue_rows(meta_selected, "warnings")
if (print_target_issues(errors, "error", "Errored selected targets:")) status <- 1L
if (nrow(warnings)) {
  record_target_warnings(meta_selected, run_label)
  print_target_issues(warnings, "warnings", "Warnings from selected targets:")
  if (!target_env_flag("EMI_ALLOW_TARGET_WARNINGS")) status <- 1L
}

if (status != 0L) quit(status = status)
