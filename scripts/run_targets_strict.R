# Run targets and return a non-zero exit code if an active target errored.
# Final workflows use this wrapper so downstream audits cannot pass against
# stale target state for the targets selected by the current configuration.

config <- Sys.getenv("EMI_CONFIG", "config/draft.yml")
is_final <- identical(basename(config), "final.yml")
stamp <- if (is_final) ".pipeline-final-ok" else ".pipeline-draft-ok"
unlink(stamp)
if (is_final) unlink(".public-final-ok")

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

active_manifest <- tryCatch(
  targets::tar_manifest(fields = "name"),
  error = function(e) data.frame(name = character())
)
active_target_names <- as.character(active_manifest$name %||% character())

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

dir.create("outputs/diagnostics/build", recursive = TRUE, showWarnings = FALSE)
if (nrow(meta)) {
  utils::write.csv(meta, "outputs/diagnostics/build/target_meta_after_strict_run.csv", row.names = FALSE)
}

# targets::tar_meta() includes historical metadata for targets that are not in
# the active target graph. Optional diagnostics/benchmarks can therefore leave
# stale warnings/errors behind after an earlier opt-in run. Strict public builds
# should fail on targets selected in this run, not on omitted opt-in targets.
# The dedicated optional wrappers still check diag_ext_* and bench_* targets when
# those groups are explicitly requested.
meta_active <- meta
if (nrow(meta_active) && length(active_target_names)) {
  meta_active <- meta_active[as.character(meta_active$name) %in% active_target_names, , drop = FALSE]
}

if (nrow(meta_active) && "warnings" %in% names(meta_active)) {
  warn <- !is.na(meta_active$warnings) & nzchar(as.character(meta_active$warnings))
  if (any(warn)) {
    warning_rows <- meta_active[warn, intersect(c("name", "warnings"), names(meta_active)), drop = FALSE]
    utils::write.csv(warning_rows, "outputs/diagnostics/build/target_warnings.csv", row.names = FALSE)
    cat("Target warnings:\n")
    print(warning_rows, row.names = FALSE)
    if (is_final && !identical(Sys.getenv("EMI_ALLOW_TARGET_WARNINGS"), "true")) {
      status <- 1L
    }
  }
}

if (nrow(meta_active) && "error" %in% names(meta_active)) {
  err <- !is.na(meta_active$error) & nzchar(as.character(meta_active$error))
  if (any(err)) {
    cat("Errored active targets:\n")
    print(meta_active[err, intersect(c("name", "error"), names(meta_active)), drop = FALSE], row.names = FALSE)
    status <- 1L
  }
}

if (status != 0L) quit(status = status)
file.create(stamp)
message("Targets completed successfully; wrote ", stamp)
