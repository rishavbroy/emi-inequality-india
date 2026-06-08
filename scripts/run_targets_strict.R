# Run targets and return a non-zero exit code if any target errored.
# Final workflows use this wrapper so downstream audits cannot pass against stale target state.

config <- Sys.getenv("EMI_CONFIG", "config/draft.yml")
is_final <- identical(basename(config), "final.yml")
stamp <- if (is_final) ".pipeline-final-ok" else ".pipeline-draft-ok"
unlink(stamp)
if (is_final) unlink(".public-final-ok")

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}

status <- 0L
tryCatch(
  targets::tar_make(),
  error = function(e) {
    message("targets::tar_make() errored: ", conditionMessage(e))
    status <<- 1L
  }
)

meta <- tryCatch(
  targets::tar_meta(fields = c("name", "error", "warnings")),
  error = function(e) data.frame()
)

dir.create("outputs/diagnostics", recursive = TRUE, showWarnings = FALSE)
if (nrow(meta)) {
  utils::write.csv(meta, "outputs/diagnostics/target_meta_after_strict_run.csv", row.names = FALSE)
}

if (nrow(meta) && "warnings" %in% names(meta)) {
  warn <- !is.na(meta$warnings) & nzchar(as.character(meta$warnings))
  if (any(warn)) {
    warning_rows <- meta[warn, intersect(c("name", "warnings"), names(meta)), drop = FALSE]
    utils::write.csv(warning_rows, "outputs/diagnostics/target_warnings.csv", row.names = FALSE)
    cat("Target warnings:\n")
    print(warning_rows, row.names = FALSE)
    if (is_final && !identical(Sys.getenv("EMI_ALLOW_TARGET_WARNINGS"), "true")) {
      status <- 1L
    }
  }
}

if (nrow(meta) && "error" %in% names(meta)) {
  err <- !is.na(meta$error) & nzchar(as.character(meta$error))
  if (any(err)) {
    cat("Errored targets:\n")
    print(meta[err, intersect(c("name", "error"), names(meta)), drop = FALSE], row.names = FALSE)
    status <- 1L
  }
}

if (status != 0L) quit(status = status)
file.create(stamp)
message("Targets completed successfully; wrote ", stamp)
