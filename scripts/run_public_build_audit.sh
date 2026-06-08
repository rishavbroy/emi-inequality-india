#!/usr/bin/env bash
set -euxo pipefail

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

echo "=== START: git state ==="
git status --short
git diff --check -- \
  . \
  ':(exclude)*.html' \
  ':(exclude)outputs/**' \
  ':(exclude)application-samples/output/**'

echo "=== CLEAN GENERATED RENDERS ==="
make clean-renders

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

echo "=== FINAL TARGETS PIPELINE ==="
make pipeline-final

echo "=== PUBLIC REPORT ==="
make report

echo "=== APPLICATION SAMPLES ==="
make samples

echo "=== PUBLIC FINAL CHECK ==="
make check-public-final

echo "=== REVIEW ARCHIVE ==="
bash scripts/make_review_archive.sh Archive.zip

echo "=== STRICT TARGET WARNING CHECK ==="
if [ -s outputs/diagnostics/target_warnings.csv ]; then
  echo "target_warnings.csv is non-empty"
  cat outputs/diagnostics/target_warnings.csv
  exit 1
fi

echo "=== OUTPUT MANIFEST ==="
find paper application-samples/output outputs docs \
  -maxdepth 3 \
  -type f \
  \( -name '*.pdf' -o -name '*.html' -o -name '*.csv' -o -name '*.tex' -o -name '*.png' \) \
  -print | sort
