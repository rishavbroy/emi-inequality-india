#!/usr/bin/env bash
set -euo pipefail

render_samples="true"
archive_enabled="true"
archive_out="review.zip"
from_clean_slate="false"
fast_mode="false"
skip_tests="false"
with_extended_diagnostics="false"
with_benchmarks="false"
with_analysis="false"
with_poster="false"
current_stage="argument-parsing"
build_completed="false"
build_started_at_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

usage() {
  cat <<'USAGE'
Usage: bash scripts/run_full_build.sh [options]

Builds and validates the project using the final research configuration by default.
The ordinary run preserves the {targets} store and existing generated files, includes
application samples, omits analysis reports and the conference poster, and writes
review.zip on both success and failure.

Options:
  --no-samples              Omit application-sample rendering and checks.
  --with-analysis           Render analysis reports and their required extended
                            results and benchmarks.
  --with-poster             Render and require the conference poster.
  --with-extended-diagnostics
                            Run the opt-in extended diagnostic target family.
  --with-benchmarks         Run the opt-in benchmark target family.
  --from-clean-slate        Remove generated outputs and destroy the {targets}
                            store before rebuilding.
  --fast                    Use config/fast.yml instead of config/final.yml.
  --skip-tests              Skip the unit-test stage.
  --no-archive              Do not create a review archive on success or failure.
  -o, --output OUT.zip      Set the review-archive path (default: review.zip).
  -h, --help                Show this help.

A fresh clone already has no {targets} store, so --from-clean-slate is mainly a
maintainer check that reconstruction does not depend on cached state.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --no-samples|--without-samples) render_samples="false"; shift ;;
    --with-samples) render_samples="true"; shift ;;
    --with-analysis)
      with_analysis="true"
      with_extended_diagnostics="true"
      with_benchmarks="true"
      shift
      ;;
    --with-poster) with_poster="true"; shift ;;
    --with-extended-diagnostics) with_extended_diagnostics="true"; shift ;;
    --with-benchmarks) with_benchmarks="true"; shift ;;
    --from-clean-slate) from_clean_slate="true"; shift ;;
    --fast) fast_mode="true"; shift ;;
    --skip-tests) skip_tests="true"; shift ;;
    --no-archive) archive_enabled="false"; shift ;;
    -o|--output)
      if [[ $# -lt 2 ]]; then echo "Missing argument for $1" >&2; exit 2; fi
      archive_out="$2"
      shift 2
      ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [[ "$fast_mode" == "true" ]]; then
  config_path="config/fast.yml"
  build_mode="fast"
else
  config_path="config/final.yml"
  build_mode="final"
fi

archive_args=()
if [[ "$render_samples" == "true" ]]; then
  archive_args+=(--with-samples)
else
  archive_args+=(--no-samples)
fi
if [[ "$with_analysis" == "true" ]]; then archive_args+=(--with-analysis); fi
if [[ "$with_poster" == "true" ]]; then archive_args+=(--with-poster); fi

write_build_status() {
  local status="$1"
  local stage="$2"
  local exit_code="$3"
  local archive_mode="$4"
  mkdir -p outputs/build
  BUILD_STATUS="$status" \
  BUILD_STAGE="$stage" \
  BUILD_EXIT_CODE="$exit_code" \
  BUILD_ARCHIVE_MODE="$archive_mode" \
  BUILD_STARTED_AT_UTC="$build_started_at_utc" \
  BUILD_ARCHIVE_OUT="$archive_out" \
  BUILD_ARCHIVE_ENABLED="$archive_enabled" \
  BUILD_CONFIG="$config_path" \
  BUILD_SAMPLES="$render_samples" \
  BUILD_CLEAN_SLATE="$from_clean_slate" \
  BUILD_FAST="$fast_mode" \
  BUILD_SKIP_TESTS="$skip_tests" \
  BUILD_EXTENDED="$with_extended_diagnostics" \
  BUILD_BENCHMARKS="$with_benchmarks" \
  BUILD_ANALYSIS="$with_analysis" \
  BUILD_POSTER="$with_poster" \
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
    "schema_version": 2,
    "status": os.environ["BUILD_STATUS"],
    "stage": os.environ["BUILD_STAGE"],
    "exit_code": int(os.environ["BUILD_EXIT_CODE"]),
    "archive_mode": os.environ["BUILD_ARCHIVE_MODE"],
    "started_at_utc": os.environ["BUILD_STARTED_AT_UTC"],
    "updated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    "archive_output": os.environ["BUILD_ARCHIVE_OUT"],
    "config": os.environ["BUILD_CONFIG"],
    "options": {
        "archive": flag("BUILD_ARCHIVE_ENABLED"),
        "with_samples": flag("BUILD_SAMPLES"),
        "from_clean_slate": flag("BUILD_CLEAN_SLATE"),
        "fast": flag("BUILD_FAST"),
        "skip_tests": flag("BUILD_SKIP_TESTS"),
        "with_extended_diagnostics": flag("BUILD_EXTENDED"),
        "with_benchmarks": flag("BUILD_BENCHMARKS"),
        "with_analysis": flag("BUILD_ANALYSIS"),
        "with_poster": flag("BUILD_POSTER"),
    },
    "git": {
        "branch": git_value("branch", "--show-current"),
        "commit": git_value("rev-parse", "HEAD"),
        "dirty": bool(git_value("status", "--porcelain")),
    },
}
Path("outputs/build/build_status.json").write_text(
    json.dumps(status, indent=2, sort_keys=True) + "\n", encoding="utf-8"
)
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
    echo "Source files contain trailing whitespace:" >&2
    cat "$tmp" >&2
    rm -f "$tmp"
    return 1
  fi
  rm -f "$tmp"
}

package_review_archive() {
  local allow_incomplete="${1:-false}"
  [[ "$archive_enabled" == "true" ]] || return 0
  local args=("${archive_args[@]}" --output "$archive_out")
  if [[ "$allow_incomplete" == "true" ]]; then args=("${archive_args[@]}" --allow-incomplete --output "$archive_out"); fi
  bash scripts/make_review_archive.sh "${args[@]}"
}

dump_build_state() {
  local exit_code="$1"
  trap - EXIT
  if [[ "$build_completed" != "true" && "$exit_code" -eq 0 ]]; then
    echo "Build exited before completion without a nonzero status; normalizing to exit code 1." >&2
    exit_code=1
  fi
  echo "=== EXIT CODE: ${exit_code} ==="

  if [[ "$build_completed" == "true" && "$exit_code" -eq 0 ]]; then
    write_build_status "passed" "complete" 0 "$([[ "$fast_mode" == "true" ]] && echo fast || echo verified)"
  else
    write_build_status "failed" "$current_stage" "$exit_code" "incomplete"
    if [[ "$archive_enabled" == "true" ]]; then
      echo "=== FAILURE REVIEW ARCHIVE (${archive_out}) ==="
      if package_review_archive true; then
        echo "Wrote current failed-run review archive: $archive_out"
      else
        archive_exit_code=$?
        rm -f -- "$archive_out"
        write_build_status "failed" "$current_stage" "$exit_code" "archive_failed"
        echo "Failed to build current review archive (archive exit ${archive_exit_code}); removed stale $archive_out." >&2
      fi
    fi
  fi

  echo "=== BUILD STATUS ==="
  cat outputs/build/build_status.json 2>/dev/null || true
  echo "=== TARGET WARNINGS ==="
  if [[ -f outputs/build/target_warnings.csv ]]; then
    cat outputs/build/target_warnings.csv
  else
    echo "No target_warnings.csv found"
  fi
  echo "=== TARGET METADATA TAIL ==="
  if [[ -f outputs/build/target_meta_after_strict_run.csv ]]; then
    tail -120 outputs/build/target_meta_after_strict_run.csv
  else
    echo "No target_meta_after_strict_run.csv found"
  fi
  echo "=== END: git state ==="
  git status --short
  exit "$exit_code"
}
trap 'build_exit_code=$?; dump_build_state "$build_exit_code"' EXIT

current_stage="initialize"
echo "=== START: git state ==="
git status --short
write_build_status "running" "$current_stage" 0 "pending"

echo "=== BUILD PROFILE ==="
printf '  config: %s\n' "$config_path"
printf '  application samples: %s\n' "$render_samples"
printf '  conference poster: %s\n' "$with_poster"
printf '  extended diagnostics: %s\n' "$with_extended_diagnostics"
printf '  benchmarks: %s\n' "$with_benchmarks"
printf '  analysis reports: %s\n' "$with_analysis"
printf '  from clean slate: %s\n' "$from_clean_slate"
printf '  review archive: %s\n' "$archive_enabled"

current_stage="source-whitespace-check"
echo "=== SOURCE WHITESPACE CHECK ==="
check_source_whitespace
git diff --check -- . \
  ':(exclude)*.html' \
  ':(exclude)outputs/**' \
  ':(exclude)data/processed/**' \
  ':(exclude)application-samples/output/**'

current_stage="restore-project-library"
echo "=== RESTORE PROJECT LIBRARY FROM RENV.LOCK ==="
make restore

if [[ "$from_clean_slate" == "true" ]]; then
  current_stage="clean-slate"
  echo "=== FROM CLEAN SLATE ==="
  make clean-all
fi

# Always clear short-lived build metadata and retired outputs after any optional
# generated-state reset. This does not delete the {targets} store.
current_stage="initialize-generated-state"
bash scripts/clean_audit_workspace.sh
write_build_status "running" "$current_stage" 0 "pending"

current_stage="prepare-data"
echo "=== PREPARE AUTOMATIC DATA SOURCES ==="
make prepare-data

current_stage="targets-process-preflight"
echo "=== TARGETS PROCESS PREFLIGHT ==="
Rscript scripts/check_targets_process.R

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
  make extended-diagnostics CONFIG="$config_path"
  Rscript scripts/check_required_outputs.R --extended-diagnostics-only
fi

current_stage="main-build"
export EMI_CONSUMPTION_DOMAIN_CORES="${EMI_CONSUMPTION_DOMAIN_CORES:-4}"
echo "=== MAIN BUILD ==="
echo "Consumption-domain workers requested: ${EMI_CONSUMPTION_DOMAIN_CORES} (clamped to physical cores in R)."
make pipeline \
  CONFIG="$config_path" \
  RENDER_SAMPLES="$render_samples" \
  RENDER_POSTER="$with_poster"

if [[ "$fast_mode" == "true" ]]; then
  current_stage="fast-checks"
  echo "=== FAST BUILD CHECKS ==="
  Rscript scripts/check_report_values.R
  Rscript scripts/audit_crossrefs.R
  Rscript scripts/check_rendered_text.R
else
  current_stage="final-checks"
  echo "=== FINAL BUILD CHECKS ==="
  EMI_CONFIG="$config_path" Rscript scripts/check_report_values.R --strict
  Rscript scripts/audit_crossrefs.R --strict-report
  EMI_REQUIRE_APPLICATION_SAMPLES="$render_samples" \
    EMI_REQUIRE_POSTER="$with_poster" \
    Rscript scripts/check_required_outputs.R --require-final-stamp
  EMI_CONFIG="$config_path" \
    EMI_REQUIRE_APPLICATION_SAMPLES="$render_samples" \
    EMI_REQUIRE_POSTER="$with_poster" \
    Rscript scripts/check_rendered_text.R --final
  EMI_REQUIRE_APPLICATION_SAMPLES="$render_samples" \
    EMI_REQUIRE_POSTER="$with_poster" \
    Rscript scripts/check_public_final.R
  touch .public-final-ok
fi

if [[ "$with_benchmarks" == "true" ]]; then
  current_stage="benchmarks"
  echo "=== BENCHMARKS ==="
  make benchmarking CONFIG="$config_path"
fi

if [[ "$with_analysis" == "true" ]]; then
  current_stage="analysis"
  echo "=== ANALYSIS REPORTS ==="
  make render-analysis CONFIG="$config_path"
fi

current_stage="strict-target-warning-check"
echo "=== TARGET WARNING CHECK ==="
if [[ -s outputs/build/target_warnings.csv ]]; then
  echo "target_warnings.csv is non-empty"
  cat outputs/build/target_warnings.csv
  exit 1
fi

current_stage="output-manifest"
echo "=== OUTPUT MANIFEST ==="
manifest_args=(scripts/write_output_manifest.R)
if [[ "$render_samples" == "true" ]]; then manifest_args+=(--with-samples); fi
if [[ "$with_analysis" == "true" ]]; then manifest_args+=(--with-analysis); fi
if [[ "$with_poster" == "true" ]]; then manifest_args+=(--with-poster); fi
Rscript "${manifest_args[@]}"
if [[ ! -s outputs/build/output_manifest.csv ]]; then
  echo "Output manifest was not created or is empty." >&2
  exit 1
fi

current_stage="review-archive"
write_build_status "passed" "complete" 0 "$([[ "$fast_mode" == "true" ]] && echo fast || echo verified)"
if [[ "$archive_enabled" == "true" ]]; then
  echo "=== REVIEW ARCHIVE ==="
  if [[ "$fast_mode" == "true" ]]; then
    package_review_archive true
  else
    package_review_archive false
  fi
fi
build_completed="true"
