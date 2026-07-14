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

usage() {
  cat <<'USAGE'
Usage: bash scripts/run_public_build_audit.sh [--with-samples|--without-samples] [--with-extended-diagnostics] [--with-benchmarks] [--with-analysis-notes] [--archive-on-error|--archive-always] [--archive-each-step] [--incremental|--skip-clean] [--skip-tests] [-o OUT.zip]

Runs the final public build audit. The default is --without-samples for a faster
report/data/output audit that omits application-sample rendering and excludes
application-samples/output from the review archive. Use --with-samples before a
full submission/review bundle. This script is the canonical end-to-end audit; use
scripts/run_legacy_content_audit.sh for the narrower post-build legacy-results
parity audit. Debug-only options are off by default so reviewers do not see
incomplete archives or cache-preserving shortcuts unless requested. Use --incremental
to preserve generated renders and the {targets} store while debugging content parity;
use a non-incremental run for the final reviewer-facing proof build. Use
--archive-on-error, or the synonym --archive-always, to write a debug review.zip
if the audit fails; successful audits always write the final review archive.

Optional extended diagnostics and benchmarks are included only when requested and
respect the {targets} cache. Ordinary public builds clear only short-lived,
Git-ignored outputs/diagnostics/build and outputs/diagnostics/public files;
longer outputs/diagnostics/extended and outputs/benchmarking artifacts are
preserved unless explicitly cleaned. Use --with-analysis-notes to render the human-readable
analysis notebooks to GitHub-flavored Markdown in the same audit log; this also
requests the extended diagnostics and benchmarks that those notebooks read.
Analysis notes do not request application samples; add --with-samples only when
sample-generation code or sample-facing outputs may have changed.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --with-samples)
      render_samples="true"
      shift
      ;;
    --without-samples|--no-samples)
      render_samples="false"
      shift
      ;;
    --archive-on-error|--archive-always)
      archive_on_error="true"
      shift
      ;;
    --archive-each-step)
      archive_each_step="true"
      archive_on_error="true"
      shift
      ;;
    --incremental)
      incremental="true"
      skip_clean="true"
      shift
      ;;
    --skip-clean)
      skip_clean="true"
      shift
      ;;
    --with-extended-diagnostics)
      with_extended_diagnostics="true"
      shift
      ;;
    --with-benchmarks)
      with_benchmarks="true"
      shift
      ;;
    --with-analysis-notes)
      with_analysis_notes="true"
      with_extended_diagnostics="true"
      with_benchmarks="true"
      shift
      ;;
    --skip-tests)
      skip_tests="true"
      shift
      ;;
    -o|--output)
      if [[ $# -lt 2 ]]; then
        echo "Missing argument for $1" >&2
        exit 2
      fi
      archive_out="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

make_debug_archive() {
  local label="$1"
  if [[ "$archive_on_error" != "true" && "$archive_each_step" != "true" ]]; then
    return 0
  fi
  echo "=== DEBUG REVIEW ARCHIVE (${label}) ==="
  bash scripts/make_review_archive.sh "$archive_sample_flag" --allow-incomplete --output "$archive_out" || \
    echo "Could not create debug review archive ${archive_out}" >&2
}

checkpoint_archive() {
  local label="$1"
  if [[ "$archive_each_step" == "true" ]]; then
    make_debug_archive "$label"
  fi
}

dump_diagnostics() {
  exit_code=$?
  echo "=== EXIT CODE: ${exit_code} ==="

  echo "=== DIAGNOSTICS: target_warnings.csv ==="
  if [ -f outputs/diagnostics/build/target_warnings.csv ]; then
    cat outputs/diagnostics/build/target_warnings.csv
  else
    echo "No target_warnings.csv found"
  fi

  echo "=== DIAGNOSTICS: target_meta_after_strict_run.csv tail ==="
  if [ -f outputs/diagnostics/build/target_meta_after_strict_run.csv ]; then
    tail -120 outputs/diagnostics/build/target_meta_after_strict_run.csv
  else
    echo "No target_meta_after_strict_run.csv found"
  fi

  echo "=== END: git state ==="
  git status --short

  if [[ "${exit_code}" -ne 0 && "$archive_on_error" == "true" ]]; then
    make_debug_archive "error"
  fi

  exit "${exit_code}"
}
trap dump_diagnostics EXIT

normalize_source_whitespace() {
  if ! command -v perl >/dev/null 2>&1; then
    echo "perl is required for source whitespace normalization" >&2
    exit 2
  fi

  find paper docs scripts R tests \
    -type f \
    \( -name '*.qmd' -o -name '*.R' -o -name '*.r' -o -name '*.sh' -o -name '*.md' \) \
    -print0 | xargs -0 perl -pi -e 's/[ \t]+$//'
}

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

echo "=== START: git state ==="
git status --short

echo "=== RESET PUBLIC/BUILD DIAGNOSTICS ==="
rm -rf outputs/diagnostics/build outputs/diagnostics/public
mkdir -p outputs/diagnostics/build outputs/diagnostics/public outputs/diagnostics/extended outputs/benchmarking

echo "=== NORMALIZE SOURCE WHITESPACE ==="
normalize_source_whitespace

git diff --check -- \
  . \
  ':(exclude)*.html' \
  ':(exclude)outputs/**' \
  ':(exclude)application-samples/output/**'

if [[ "$incremental" == "true" ]]; then
  echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} (incremental/cache-preserving) ==="
else
  echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} ==="
fi

if [[ "$skip_clean" == "true" ]]; then
  echo "=== CLEAN GENERATED RENDERS: skipped by --skip-clean ==="
else
  echo "=== CLEAN GENERATED RENDERS ==="
  make "$clean_target"
fi
checkpoint_archive "after-clean"

echo "=== REBUILD GENERATED QMD SOURCES ==="
make rebuild-qmds
checkpoint_archive "after-rebuild-qmds"

echo "=== SOURCE WHITESPACE CHECK AFTER QMD REBUILD ==="
normalize_source_whitespace
git diff --check -- \
  paper/report.qmd \
  paper/appendix.qmd \
  docs/district-matching.qmd \
  docs/long-paths-and-8-3-filenames.qmd \
  scripts/postprocess_public_qmds.R \
  scripts/check_required_outputs.R

echo "=== STATIC/PARSE CHECKS ==="
Rscript -e 'parse("scripts/check_required_outputs.R"); cat("check_required_outputs.R parses\n")'
Rscript -e 'tmp <- tempfile(fileext = ".R"); knitr::purl("paper/report.qmd", output = tmp, quiet = TRUE); parse(tmp); cat("paper/report.qmd R chunks parse\n")'
python3 -m py_compile scripts/audit_legacy_parity.py
checkpoint_archive "after-static-parse"

if [[ "$skip_tests" == "true" ]]; then
  echo "=== UNIT TESTS: skipped by --skip-tests ==="
else
  echo "=== UNIT TESTS ==="
  make test
fi
checkpoint_archive "after-unit-tests"

echo "=== PUBLIC FINAL CHECK (${sample_mode}) ==="
make "$check_target"
checkpoint_archive "after-public-final-check"

echo "=== LEGACY CONTENT PARITY AUDIT ==="
python3 -m py_compile scripts/audit_legacy_parity.py
python3 scripts/audit_legacy_parity.py
checkpoint_archive "after-legacy-content-audit"

if [[ "$with_extended_diagnostics" == "true" ]]; then
  echo "=== EXTENDED DIAGNOSTICS ==="
  make extended-diagnostics
  checkpoint_archive "after-extended-diagnostics"
fi

if [[ "$with_benchmarks" == "true" ]]; then
  echo "=== BENCHMARKS ==="
  make benchmarking
  checkpoint_archive "after-benchmarks"
fi


if [[ "$with_analysis_notes" == "true" ]]; then
  echo "=== ANALYSIS NOTES ==="
  make clean-analysis
  make render-analysis
  checkpoint_archive "after-analysis-notes"
fi

echo "=== REVIEW ARCHIVE ==="
bash scripts/make_review_archive.sh "$archive_sample_flag" --output "$archive_out"

echo "=== STRICT TARGET WARNING CHECK ==="
if [ -s outputs/diagnostics/build/target_warnings.csv ]; then
  echo "target_warnings.csv is non-empty"
  cat outputs/diagnostics/build/target_warnings.csv
  exit 1
fi

echo "=== OUTPUT MANIFEST ==="
manifest_roots=(paper outputs docs)
if [[ "$render_samples" == "true" ]]; then
  manifest_roots+=(application-samples/output)
fi
if [[ "$with_analysis_notes" == "true" ]]; then
  manifest_roots+=(analysis)
fi
find "${manifest_roots[@]}" \
  -maxdepth 3 \
  -type f \
  \( -name '*.pdf' -o -name '*.html' -o -name '*.md' -o -name '*.csv' -o -name '*.tex' -o -name '*.png' \) \
  -print | sort
