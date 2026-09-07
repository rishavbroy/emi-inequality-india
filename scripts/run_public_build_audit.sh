#!/usr/bin/env bash
set -euxo pipefail

render_samples="false"
archive_out="review.zip"
skip_clean="false"
skip_tests="false"
incremental="false"
with_extended_diagnostics="false"
with_benchmarks="false"
with_analysis_notes="false"
archive_on_failure="false"
current_stage="argument-parsing"
audit_completed="false"
audit_started_at_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

usage() {
  cat <<'USAGE'
Usage: bash scripts/run_public_build_audit.sh [--with-samples|--without-samples] [--with-extended-diagnostics] [--with-benchmarks] [--with-analysis-notes] [--archive-on-error|--archive-always] [--incremental|--skip-clean] [--skip-tests] [-o OUT.zip]

Runs the final public build audit. The default is --without-samples for a faster
report/data/output audit that omits application-sample rendering and excludes
application-samples/output from the review archive. Use --with-samples before a
full submission/review bundle. This script is the canonical end-to-end audit for
the active current pipeline.

The audit restores the project R library from the tracked renv.lock before
checking synchronization, then checks source whitespace without editing source
files. Every successful review archive contains
outputs/diagnostics/build/audit_status.json. By default, failed runs preserve the
last verified review archive unchanged. --archive-always and --archive-on-error
request a current incomplete review.zip on failure; successful runs always
replace the archive only after all warning, integrity, and manifest gates pass.

Use --incremental to preserve generated renders and the {targets} store while
debugging; use a non-incremental run for the final reviewer-facing proof build.
Optional extended diagnostics and benchmarks are included only when requested
and respect the {targets} cache. Use --with-analysis-notes to render the
human-readable analysis notebooks; this also requests the diagnostics and
benchmarks those notebooks read.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --with-samples) render_samples="true"; shift ;;
    --without-samples|--no-samples) render_samples="false"; shift ;;
    # Failed audits preserve the last verified archive unless the caller requests
    # a current incomplete snapshot with one of these failure-archive flags.
    --archive-on-error|--archive-always) archive_on_failure="true"; shift ;;
    --incremental) incremental="true"; skip_clean="true"; shift ;;
    --skip-clean) skip_clean="true"; shift ;;
    --with-extended-diagnostics) with_extended_diagnostics="true"; shift ;;
    --with-benchmarks) with_benchmarks="true"; shift ;;
    --with-analysis-notes)
      with_analysis_notes="true"
      with_extended_diagnostics="true"
      with_benchmarks="true"
      shift
      ;;
    --skip-tests) skip_tests="true"; shift ;;
    -o|--output)
      if [[ $# -lt 2 ]]; then echo "Missing argument for $1" >&2; exit 2; fi
      archive_out="$2"
      shift 2
      ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [[ "$render_samples" == "true" ]]; then
  sample_mode="with application samples"
  clean_target="clean-renders"
  check_target="check-public-final"
  archive_sample_flag="--with-samples"
else
  sample_mode="without application samples"
  clean_target="clean-renders-no-samples"
  check_target="check-public-final-no-samples"
  archive_sample_flag="--without-samples"
fi

write_audit_status() {
  local status="$1"
  local stage="$2"
  local exit_code="$3"
  local archive_mode="$4"
  mkdir -p outputs/diagnostics/build
  AUDIT_STATUS="$status" \
  AUDIT_STAGE="$stage" \
  AUDIT_EXIT_CODE="$exit_code" \
  AUDIT_ARCHIVE_MODE="$archive_mode" \
  AUDIT_STARTED_AT_UTC="$audit_started_at_utc" \
  AUDIT_ARCHIVE_OUT="$archive_out" \
  AUDIT_RENDER_SAMPLES="$render_samples" \
  AUDIT_INCREMENTAL="$incremental" \
  AUDIT_SKIP_CLEAN="$skip_clean" \
  AUDIT_SKIP_TESTS="$skip_tests" \
  AUDIT_EXTENDED="$with_extended_diagnostics" \
  AUDIT_BENCHMARKS="$with_benchmarks" \
  AUDIT_ANALYSIS_NOTES="$with_analysis_notes" \
  AUDIT_ARCHIVE_ON_FAILURE="$archive_on_failure" \
  python3 - <<'PY'
import json
import os
import subprocess
from datetime import datetime, timezone
from pathlib import Path

def flag(name):
    return os.environ.get(name, "false").lower() == "true"

def git_value(*args):
    try:
        return subprocess.check_output(["git", *args], text=True, stderr=subprocess.DEVNULL).strip()
    except Exception:
        return None

status = {
    "schema_version": 1,
    "status": os.environ["AUDIT_STATUS"],
    "stage": os.environ["AUDIT_STAGE"],
    "exit_code": int(os.environ["AUDIT_EXIT_CODE"]),
    "archive_mode": os.environ["AUDIT_ARCHIVE_MODE"],
    "started_at_utc": os.environ["AUDIT_STARTED_AT_UTC"],
    "updated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    "archive_output": os.environ["AUDIT_ARCHIVE_OUT"],
    "options": {
        "with_samples": flag("AUDIT_RENDER_SAMPLES"),
        "incremental": flag("AUDIT_INCREMENTAL"),
        "skip_clean": flag("AUDIT_SKIP_CLEAN"),
        "skip_tests": flag("AUDIT_SKIP_TESTS"),
        "with_extended_diagnostics": flag("AUDIT_EXTENDED"),
        "with_benchmarks": flag("AUDIT_BENCHMARKS"),
        "with_analysis_notes": flag("AUDIT_ANALYSIS_NOTES"),
        "archive_on_failure": flag("AUDIT_ARCHIVE_ON_FAILURE"),
    },
    "git": {
        "branch": git_value("branch", "--show-current"),
        "commit": git_value("rev-parse", "HEAD"),
        "dirty": bool(git_value("status", "--porcelain")),
    },
}
path = Path("outputs/diagnostics/build/audit_status.json")
path.write_text(json.dumps(status, indent=2, sort_keys=True) + "\n", encoding="utf-8")
PY
}

check_source_whitespace() {
  local tmp
  tmp="$(mktemp)"
  find paper docs scripts R tests posters config \
    -type f \
    \( -name '*.qmd' -o -name '*.R' -o -name '*.r' -o -name '*.sh' -o -name '*.md' \) \
    -print0 | xargs -0 grep -nH -E '[[:blank:]]+$' >"$tmp" || true
  if [[ -s "$tmp" ]]; then
    echo "Source files contain trailing whitespace; the audit is read-only and will not rewrite them:" >&2
    cat "$tmp" >&2
    rm -f "$tmp"
    return 1
  fi
  rm -f "$tmp"
}

dump_diagnostics() {
  local exit_code="$1"
  trap - EXIT
  if [[ "$audit_completed" != "true" && "$exit_code" -eq 0 ]]; then
    echo "Audit exited before completion without a nonzero status; normalizing to exit code 1." >&2
    exit_code=1
  fi
  echo "=== EXIT CODE: ${exit_code} ==="

  if [[ "$audit_completed" == "true" && "$exit_code" -eq 0 ]]; then
    write_audit_status "passed" "complete" 0 "verified"
  else
    write_audit_status "failed" "$current_stage" "$exit_code" "incomplete"
    if [[ "$archive_on_failure" == "true" ]]; then
      echo "=== FAILURE REVIEW ARCHIVE (${archive_out}) ==="
      if bash scripts/make_review_archive.sh \
          "$archive_sample_flag" --allow-incomplete --output "$archive_out"; then
        echo "Wrote current failed-run review archive: $archive_out"
      else
        archive_exit_code=$?
        # A requested current archive must never be confused with an older run.
        # If packaging itself fails, remove the stale destination and record that
        # the archive contract failed while preserving the audit's original exit.
        rm -f -- "$archive_out"
        write_audit_status "failed" "$current_stage" "$exit_code" "archive_failed"
        echo "Failed to build requested review archive (archive exit ${archive_exit_code}); removed stale $archive_out." >&2
      fi
    fi
  fi

  echo "=== DIAGNOSTICS: audit_status.json ==="
  cat outputs/diagnostics/build/audit_status.json 2>/dev/null || true
  echo "=== DIAGNOSTICS: target_warnings.csv ==="
  if [[ -f outputs/diagnostics/build/target_warnings.csv ]]; then
    cat outputs/diagnostics/build/target_warnings.csv
  else
    echo "No target_warnings.csv found"
  fi
  echo "=== DIAGNOSTICS: target_meta_after_strict_run.csv tail ==="
  if [[ -f outputs/diagnostics/build/target_meta_after_strict_run.csv ]]; then
    tail -120 outputs/diagnostics/build/target_meta_after_strict_run.csv
  else
    echo "No target_meta_after_strict_run.csv found"
  fi
  echo "=== END: git state ==="
  git status --short

  exit "$exit_code"
}
trap 'audit_exit_code=$?; dump_diagnostics "$audit_exit_code"' EXIT

current_stage="initialize-diagnostics"
echo "=== START: git state ==="
git status --short
# Preserve the existing archive until a replacement has been fully built and
# validated. Failed runs replace it only when --archive-always/--archive-on-error
# explicitly requests a current incomplete snapshot.
bash scripts/clean_audit_workspace.sh
write_audit_status "running" "$current_stage" 0 "pending"

current_stage="source-whitespace-check"
echo "=== READ-ONLY SOURCE WHITESPACE CHECK ==="
check_source_whitespace
git diff --check -- . \
  ':(exclude)*.html' \
  ':(exclude)outputs/**' \
  ':(exclude)data/processed/**' \
  ':(exclude)application-samples/output/**'

if [[ "$incremental" == "true" ]]; then
  echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} (incremental/cache-preserving) ==="
else
  echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} ==="
fi

current_stage="restore-project-library"
echo "=== RESTORE PROJECT LIBRARY FROM RENV.LOCK ==="
make restore

current_stage="targets-process-preflight"
echo "=== TARGETS PROCESS PREFLIGHT ==="
Rscript scripts/check_targets_process.R

current_stage="clean-generated-renders"
if [[ "$skip_clean" == "true" ]]; then
  echo "=== CLEAN GENERATED RENDERS: skipped by --skip-clean ==="
else
  echo "=== CLEAN GENERATED RENDERS ==="
  make "$clean_target"
fi

current_stage="static-parse-checks"
echo "=== STATIC/PARSE CHECKS ==="
check_source_whitespace
bash scripts/check_source_syntax.sh

current_stage="unit-tests"
if [[ "$skip_tests" == "true" ]]; then
  echo "=== UNIT TESTS: skipped by --skip-tests ==="
else
  echo "=== UNIT TESTS ==="
  make test
fi

current_stage="lineage-geometry"
echo "=== LINEAGE GEOMETRY ==="
make lineage-geometry-build

if [[ "$with_extended_diagnostics" == "true" ]]; then
  current_stage="extended-diagnostics"
  echo "=== EXTENDED DIAGNOSTICS ==="
  make extended-diagnostics
  Rscript scripts/check_required_outputs.R --extended-diagnostics-only
fi

current_stage="public-final-check"
export EMI_CONSUMPTION_DOMAIN_CORES="${EMI_CONSUMPTION_DOMAIN_CORES:-4}"
echo "=== CONSUMPTION DOMAIN WORKERS: ${EMI_CONSUMPTION_DOMAIN_CORES} requested (clamped to physical cores in R) ==="
echo "=== PUBLIC FINAL CHECK (${sample_mode}) ==="
make "$check_target"

if [[ "$with_benchmarks" == "true" ]]; then
  current_stage="benchmarks"
  echo "=== BENCHMARKS ==="
  make benchmarking
fi

if [[ "$with_analysis_notes" == "true" ]]; then
  current_stage="analysis-notes"
  echo "=== ANALYSIS NOTES ==="
  make render-analysis
fi

current_stage="strict-target-warning-check"
echo "=== STRICT TARGET WARNING CHECK ==="
if [[ -s outputs/diagnostics/build/target_warnings.csv ]]; then
  echo "target_warnings.csv is non-empty"
  cat outputs/diagnostics/build/target_warnings.csv
  exit 1
fi

current_stage="output-manifest"
echo "=== OUTPUT MANIFEST ==="
set --
if [[ "$render_samples" == "true" ]]; then set -- "$@" --with-samples; fi
if [[ "$with_analysis_notes" == "true" ]]; then set -- "$@" --with-analysis-notes; fi
Rscript scripts/write_output_manifest.R "$@"
if [[ ! -s outputs/diagnostics/build/output_manifest.csv ]]; then
  echo "Output manifest was not created or is empty." >&2
  exit 1
fi

current_stage="review-archive"
write_audit_status "passed" "complete" 0 "verified"
echo "=== VERIFIED REVIEW ARCHIVE ==="
bash scripts/make_review_archive.sh "$archive_sample_flag" --output "$archive_out"
audit_completed="true"
