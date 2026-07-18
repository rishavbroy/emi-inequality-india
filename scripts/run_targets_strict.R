# Run the active {targets} graph and return a non-zero exit code if an active
# target records an error or, in final mode, an unacknowledged warning.

source("scripts/target_metadata_helpers.R", local = TRUE)

config <- Sys.getenv("EMI_CONFIG", "config/draft.yml")
is_final <- identical(basename(config), "final.yml")
stamp <- if (is_final) ".pipeline-final-ok" else ".pipeline-draft-ok"
unlink(stamp)
if (is_final) unlink(".public-final-ok")
unlink("outputs/diagnostics/build/target_warnings.csv")

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}

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
meta_active <- select_target_metadata(meta, active_target_names)
write_target_run_metadata(meta_active, "strict")

errors <- target_metadata_issue_rows(meta_active, "error")
warnings <- target_metadata_issue_rows(meta_active, "warnings")
if (print_target_issues(errors, "error", "Errored active targets:")) status <- 1L
if (nrow(warnings)) {
  record_target_warnings(meta_active, "strict")
  print_target_issues(warnings, "warnings", "Target warnings:")
  if (is_final && !target_env_flag("EMI_ALLOW_TARGET_WARNINGS")) status <- 1L
}

if (status != 0L) quit(status = status)
file.create(stamp)
message("Targets completed successfully; wrote ", stamp)
