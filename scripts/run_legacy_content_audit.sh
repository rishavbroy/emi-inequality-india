#!/usr/bin/env bash
set -euo pipefail

echo "=== LEGACY CONTENT AUDIT: Python parser ==="
python3 -m py_compile scripts/audit_legacy_parity.py

echo "=== LEGACY CONTENT AUDIT: target diagnostics ==="
Rscript -e '
if (file.exists("outputs/diagnostics/target_warnings.csv") && file.info("outputs/diagnostics/target_warnings.csv")$size > 0) {
  warnings <- utils::read.csv("outputs/diagnostics/target_warnings.csv", stringsAsFactors = FALSE)
  if (nrow(warnings)) stop("target_warnings.csv is non-empty", call. = FALSE)
}
if (file.exists("outputs/diagnostics/target_meta_after_strict_run.csv")) {
  meta <- utils::read.csv("outputs/diagnostics/target_meta_after_strict_run.csv", stringsAsFactors = FALSE)
  if ("error" %in% names(meta)) {
    errored <- meta[!is.na(meta$error) & nzchar(meta$error), , drop = FALSE]
    if (nrow(errored)) stop("target metadata contains errored targets: ", paste(errored$name, collapse = ", "), call. = FALSE)
  }
}
cat("target diagnostics are clean or absent\n")
'

echo "=== LEGACY CONTENT AUDIT: parity checks ==="
python3 scripts/audit_legacy_parity.py
echo "=== LEGACY CONTENT AUDIT PASSED ==="
