#!/usr/bin/env Rscript

required <- c("targets", "ps")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Targets process preflight requires installed package(s): ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

pid <- tryCatch(
  suppressWarnings(targets::tar_pid()),
  error = function(e) NA_integer_
)
pid <- if (length(pid)) suppressWarnings(as.integer(pid[[1L]])) else NA_integer_

if (!is.finite(pid) || pid <= 0L) {
  message("No recorded {targets} process.")
  quit(status = 0L)
}

process_is_running <- tryCatch(
  ps::ps_is_running(ps::ps_handle(pid)),
  error = function(e) FALSE
)

if (isTRUE(process_is_running)) {
  message(
    "A live {targets} process is already recorded for this data store (PID ",
    pid,
    ").\n",
    "Do not run two pipelines against the same _targets store.\n",
    "If this is the audit you intentionally interrupted, inspect it and then run:\n",
    "  kill ", pid
  )
  quit(status = 3L)
}

message(
  "Recorded {targets} PID ", pid,
  " is no longer running; clearing stale process metadata."
)
targets::tar_unblock_process()
message("Stale {targets} process metadata cleared.")
