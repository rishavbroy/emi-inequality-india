#!/usr/bin/env bash
set -euxo pipefail

render_samples="false"
archive_out="review.zip"

usage() {
  cat <<'USAGE'
Usage: bash scripts/run_public_build_audit.sh [--with-samples|--without-samples] [-o OUT.zip]

Runs the final public build audit. The default is --without-samples for a faster
report/data/output audit that omits application-sample rendering and excludes
application-samples/output from the review archive. Use --with-samples before a
full submission/review bundle.
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

dump_diagnostics() {
  exit_code=$?
  echo "=== EXIT CODE: ${exit_code} ==="

  echo "=== DIAGNOSTICS: target_warnings.csv ==="
  if [ -f outputs/diagnostics/target_warnings.csv ]; then
    cat outputs/diagnostics/target_warnings.csv
  else
    echo "No target_warnings.csv found"
  fi

  echo "=== DIAGNOSTICS: target_meta_after_strict_run.csv tail ==="
  if [ -f outputs/diagnostics/target_meta_after_strict_run.csv ]; then
    tail -120 outputs/diagnostics/target_meta_after_strict_run.csv
  else
    echo "No target_meta_after_strict_run.csv found"
  fi

  echo "=== END: git state ==="
  git status --short

  exit "${exit_code}"
}
trap dump_diagnostics EXIT

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
git diff --check -- \
  . \
  ':(exclude)*.html' \
  ':(exclude)outputs/**' \
  ':(exclude)application-samples/output/**'

echo "=== PUBLIC BUILD AUDIT MODE: ${sample_mode} ==="

echo "=== CLEAN GENERATED RENDERS ==="
make "$clean_target"

echo "=== REBUILD GENERATED QMD SOURCES ==="
make rebuild-qmds

echo "=== SOURCE WHITESPACE CHECK AFTER QMD REBUILD ==="
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

echo "=== UNIT TESTS ==="
make test

echo "=== PUBLIC FINAL CHECK (${sample_mode}) ==="
make "$check_target"

echo "=== REVIEW ARCHIVE ==="
bash scripts/make_review_archive.sh "$archive_sample_flag" --output "$archive_out"

echo "=== STRICT TARGET WARNING CHECK ==="
if [ -s outputs/diagnostics/target_warnings.csv ]; then
  echo "target_warnings.csv is non-empty"
  cat outputs/diagnostics/target_warnings.csv
  exit 1
fi

echo "=== OUTPUT MANIFEST ==="
manifest_roots=(paper outputs docs)
if [[ "$render_samples" == "true" ]]; then
  manifest_roots+=(application-samples/output)
fi
find "${manifest_roots[@]}" \
  -maxdepth 3 \
  -type f \
  \( -name '*.pdf' -o -name '*.html' -o -name '*.csv' -o -name '*.tex' -o -name '*.png' \) \
  -print | sort
