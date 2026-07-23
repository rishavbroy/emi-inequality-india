#!/usr/bin/env bash
set -euxo pipefail

render_samples="false"
archive_out="review.zip"
archive_on_error="false"
archive_each_step="false"
skip_clean="false"
skip_tests="false"
incremental="false"
with_extended_diagnostics="false"
with_benchmarks="false"
with_analysis_notes="false"
current_stage="argument-parsing"
audit_completed="false"
audit_started_at_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

usage() {
  cat <<'USAGE'
Usage: bash scripts/run_public_build_audit.sh [--with-samples|--without-samples] [--with-extended-diagnostics] [--with-benchmarks] [--with-analysis-notes] [--archive-on-error|--archive-always] [--archive-each-step] [--incremental|--skip-clean] [--skip-tests] [-o OUT.zip]

Runs the final public build audit. The default is --without-samples for a faster
report/data/output audit that omits application-sample rendering and excludes
application-samples/output from the review archive. Use --with-samples before a
full submission/review bundle. This script is the canonical end-to-end audit for
the active current pipeline.

The audit checks source whitespace without editing source files. Every review
archive contains outputs/diagnostics/build/audit_status.json. Failed runs can
still produce an explicitly failed/incomplete review archive with
--archive-on-error (or its synonym --archive-always). Successful runs create the
verified archive only after all warning, integrity, and manifest gates pass.

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
    --archive-on-error|--archive-always) archive_on_error="true"; shift ;;
    --archive-each-step) archive_each_step="true"; archive_on_error="true"; shift ;;
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

make_debug_archive() {
  local label="$1"
  if [[ "$archive_on_error" != "true" && "$archive_each_step" != "true" ]]; then return 0; fi
  echo "=== DEBUG REVIEW ARCHIVE (${label}) ==="
  bash scripts/make_review_archive.sh "$archive_sample_flag" --allow-incomplete --output "$archive_out" || \
    echo "Could not create debug review archive ${archive_out}" >&2
}

checkpoint_archive() {
  local label="$1"
  if [[ "$archive_each_step" == "true" ]]; then
    write_audit_status "running" "$label" 0 "checkpoint"
    make_debug_archive "$label"
  fi
}

check_source_whitespace() {
  local tmp
  tmp="$(mktemp)"
  find paper docs scripts R tests \
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
  local exit_code=$?
  trap - EXIT
  echo "=== EXIT CODE: ${exit_code} ==="

  if [[ "$audit_completed" == "true" && "$exit_code" -eq 0 ]]; then
    write_audit_status "passed" "complete" 0 "verified"
  else
    write_audit_status "failed" "$current_stage" "$exit_code" "incomplete"
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

  if [[ "$exit_code" -ne 0 && "$archive_on_error" == "true" ]]; then
    make_debug_archive "error-${current_stage}"
  fi
  exit "$exit_code"
}
trap dump_diagnostics EXIT

current_stage="initialize-diagnostics"
echo "=== START: git state ==="
git status --short
rm -rf outputs/diagnostics/build outputs/diagnostics/public
rm -f outputs/diagnostics/*.csv
mkdir -p outputs/diagnostics/build outputs/diagnostics/public outputs/diagnostics/extended outputs/benchmarking
write_audit_status "running" "$current_stage" 0 "pending"

current_stage="source-whitespace-check"
echo "=== READ-ONLY SOURCE WHITESPACE CHECK ==="
check_source_whitespace
git diff --check -- . ':(exclude)*.html' ':(exclude)outputs/**' ':(exclude)application-samples/output/**'

if [[ "$incremental" == "true" ]]; then
  echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} (incremental/cache-preserving) ==="
else
  echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} ==="
fi

current_stage="clean-generated-renders"
if [[ "$skip_clean" == "true" ]]; then
  echo "=== CLEAN GENERATED RENDERS: skipped by --skip-clean ==="
else
  echo "=== CLEAN GENERATED RENDERS ==="
  make "$clean_target"
fi
checkpoint_archive "after-clean"

current_stage="static-parse-checks"
echo "=== STATIC/PARSE CHECKS ==="
check_source_whitespace
bash scripts/check_source_syntax.sh
checkpoint_archive "after-static-parse"

current_stage="unit-tests"
if [[ "$skip_tests" == "true" ]]; then
  echo "=== UNIT TESTS: skipped by --skip-tests ==="
else
  echo "=== UNIT TESTS ==="
  make test
fi
checkpoint_archive "after-unit-tests"

current_stage="public-final-check"
echo "=== PUBLIC FINAL CHECK (${sample_mode}) ==="
make "$check_target"
checkpoint_archive "after-public-final-check"

if [[ "$with_extended_diagnostics" == "true" ]]; then
  current_stage="lineage-geometry"
  echo "=== LINEAGE GEOMETRY ==="
  make lineage-geometry-build
  checkpoint_archive "after-lineage-geometry"

  current_stage="extended-diagnostics"
  echo "=== EXTENDED DIAGNOSTICS ==="
  make extended-diagnostics
  checkpoint_archive "after-extended-diagnostics"
fi

if [[ "$with_benchmarks" == "true" ]]; then
  current_stage="benchmarks"
  echo "=== BENCHMARKS ==="
  make benchmarking
  checkpoint_archive "after-benchmarks"
fi

if [[ "$with_analysis_notes" == "true" ]]; then
  current_stage="analysis-notes"
  echo "=== ANALYSIS NOTES ==="
  make render-analysis
  checkpoint_archive "after-analysis-notes"
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
manifest_roots=(paper outputs docs)
if [[ "$render_samples" == "true" ]]; then manifest_roots+=(application-samples/output); fi
if [[ "$with_analysis_notes" == "true" ]]; then manifest_roots+=(analysis); fi
find "${manifest_roots[@]}" \
  -maxdepth 3 \
  -type f \
  \( -name '*.pdf' -o -name '*.html' -o -name '*.md' -o -name '*.csv' -o -name '*.tex' -o -name '*.png' -o -name 'audit_status.json' \) \
  -print | sort

current_stage="review-archive"
write_audit_status "passed" "complete" 0 "verified"
echo "=== VERIFIED REVIEW ARCHIVE ==="
bash scripts/make_review_archive.sh "$archive_sample_flag" --output "$archive_out"
audit_completed="true"
