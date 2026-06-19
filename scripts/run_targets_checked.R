# Run a selected group of {targets} targets and fail if any selected target
# records an error.  This wrapper is used for optional diagnostics/benchmarks
# because targets::tar_make(error = "abridge") can record an errored target in
# metadata without causing a shell-level failure in all configurations.

args <- commandArgs(trailingOnly = TRUE)
starts_with_arg <- ""
for (i in seq_along(args)) {
  if (identical(args[[i]], "--starts-with") && i < length(args)) {
    starts_with_arg <- args[[i + 1L]]
  }
}
if (!nzchar(starts_with_arg)) {
  stop("Usage: Rscript scripts/run_targets_checked.R --starts-with PREFIX", call. = FALSE)
}

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}
library(targets)

manifest <- tryCatch(
  targets::tar_manifest(fields = "name"),
  error = function(e) data.frame(name = character())
)
selected_target_names <- as.character(manifest$name)
selected_target_names <- selected_target_names[grepl(paste0("^", starts_with_arg), selected_target_names)]
if (!length(selected_target_names)) {
  stop("No active targets match prefix: ", starts_with_arg, call. = FALSE)
}

status <- 0L
tryCatch(
  targets::tar_make(names = all_of(selected_target_names)),
  error = function(e) {
    message("targets::tar_make() errored: ", conditionMessage(e))
    status <<- 1L
  }
)

meta <- tryCatch(
  targets::tar_meta(fields = c("name", "error", "warnings")),
  error = function(e) data.frame()
)

dir.create("outputs/diagnostics/build", recursive = TRUE, showWarnings = FALSE)
if (nrow(meta)) {
  selected <- as.character(meta$name) %in% selected_target_names
  meta_selected <- meta[selected, , drop = FALSE]
  utils::write.csv(
    meta_selected,
    file.path("outputs/diagnostics/build", paste0("target_meta_after_", starts_with_arg, "run.csv")),
    row.names = FALSE
  )
  if (nrow(meta_selected) && "error" %in% names(meta_selected)) {
    err <- !is.na(meta_selected$error) & nzchar(as.character(meta_selected$error))
    if (any(err)) {
      cat("Errored selected targets:\n")
      print(meta_selected[err, intersect(c("name", "error"), names(meta_selected)), drop = FALSE], row.names = FALSE)
      status <- 1L
    }
  }
}

if (status != 0L) quit(status = status)
