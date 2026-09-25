#!/usr/bin/env bash
set -euo pipefail

out="review.zip"
include_samples="true"
include_poster="false"
allow_incomplete="false"

usage() {
  cat <<'USAGE'
Usage: bash scripts/make_review_archive.sh [--with-samples|--no-samples] [--with-poster] [--allow-incomplete] [-o OUT.zip]
       bash scripts/make_review_archive.sh OUT.zip

Creates a public review archive from the current working tree. By default the
archive is written to review.zip and includes application-sample PDFs. Conference
poster renders are included only when explicitly requested. Use --allow-incomplete
only for debugging failed builds; it packages the current state without requiring
final deliverables.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --with-samples)
      include_samples="true"
      shift
      ;;
    --without-samples|--no-samples)
      include_samples="false"
      shift
      ;;
    --with-poster)
      include_poster="true"
      shift
      ;;
    --allow-incomplete)
      allow_incomplete="true"
      shift
      ;;
    -o|--output)
      if [[ $# -lt 2 ]]; then
        echo "Missing argument for $1" >&2
        exit 2
      fi
      out="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --*)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      if [[ "$out" != "review.zip" ]]; then
        echo "Archive output already set to $out; unexpected extra argument: $1" >&2
        exit 2
      fi
      out="$1"
      shift
      ;;
  esac
done

if [[ "$allow_incomplete" != "true" && ! -f .public-final-ok ]]; then
  echo "Cannot build review archive because .public-final-ok is missing. Run make check-public-final successfully first." >&2
  exit 1
fi

if [[ "$out" = /* ]]; then
  out_path="$out"
else
  out_path="$PWD/$out"
fi

tmpdir="$(mktemp -d)"
out_dir="$(dirname "$out_path")"
archive_tmpdir="$(mktemp -d "${out_dir}/.review-archive.XXXXXX")"
tmp_archive="${archive_tmpdir}/review.zip"
trap 'rm -rf "$tmpdir" "$archive_tmpdir"' EXIT

# Copy the current working-tree versions of tracked files. This intentionally
# avoids git archive HEAD because public QMDs/outputs may have just been
# regenerated and not committed yet.
while IFS= read -r -d '' file; do
  mkdir -p "$tmpdir/$(dirname "$file")"
  if [[ -f "$file" ]]; then cp -p "$file" "$tmpdir/$file"; fi
done < <(git ls-files -z)

# Include regenerated public artifacts whether tracked or not.
mkdir -p "$tmpdir/paper" "$tmpdir/docs" "$tmpdir/outputs"
cp -f paper/paper.pdf paper/paper.html paper/paper.qmd "$tmpdir/paper/" 2>/dev/null || true
if [[ "$include_poster" == "true" ]]; then
  mkdir -p "$tmpdir/posters/2026_predoc_conference"
  cp -f posters/2026_predoc_conference/poster.pdf posters/2026_predoc_conference/RishavRoy-Education.png "$tmpdir/posters/2026_predoc_conference/" 2>/dev/null || true
else
  rm -f "$tmpdir/posters/2026_predoc_conference/poster.pdf" "$tmpdir/posters/2026_predoc_conference/RishavRoy-Education.png"
fi
# Preserve application-sample configuration and filters from the working tree even
# before they are committed; omit only transient render files. Generated PDFs are
# copied separately below so stale or retired outputs cannot leak into the archive.
if [[ -d application-samples ]]; then
  rm -rf "$tmpdir/application-samples"
  cp -R application-samples "$tmpdir/application-samples"
  rm -rf "$tmpdir/application-samples/.work" "$tmpdir/application-samples/output"
fi

if [[ "$include_samples" == "true" ]]; then
  mkdir -p "$tmpdir/application-samples/output"
  while IFS= read -r sample_output; do
    [[ -n "$sample_output" ]] || continue
    if [[ -f "$sample_output" ]]; then
      mkdir -p "$tmpdir/$(dirname "$sample_output")"
      cp -f "$sample_output" "$tmpdir/$sample_output"
    fi
  done < <(Rscript -e 'source("R/io/utils_data_frame.R"); source("R/application_samples/sample_manifest.R"); cat(paste(application_sample_expected_outputs(), collapse = "\n"), "\n")')
else
  rm -rf "$tmpdir/application-samples/output"
fi
cp -R outputs/figures "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/tables "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/diagnostics "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/replication "$tmpdir/outputs/" 2>/dev/null || true
mkdir -p "$tmpdir/outputs/build"
if [[ ! -s "$tmpdir/outputs/build/build_status.json" ]]; then
  ARCHIVE_ALLOW_INCOMPLETE="$allow_incomplete" ARCHIVE_INCLUDE_SAMPLES="$include_samples" ARCHIVE_INCLUDE_POSTER="$include_poster" python3 - "$tmpdir/outputs/build/build_status.json" <<'PY_STATUS'
import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

path = Path(sys.argv[1])
status = {
    "schema_version": 1,
    "status": "not_run",
    "stage": "standalone_archive",
    "exit_code": None,
    "archive_mode": "incomplete" if os.environ["ARCHIVE_ALLOW_INCOMPLETE"] == "true" else "artifact_only",
    "updated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    "options": {
        "with_samples": os.environ["ARCHIVE_INCLUDE_SAMPLES"] == "true",
        "with_poster": os.environ["ARCHIVE_INCLUDE_POSTER"] == "true"
    },
    "note": "The archive was created outside scripts/run_full_build.sh; no build result is asserted."
}
path.write_text(json.dumps(status, indent=2, sort_keys=True) + "\n", encoding="utf-8")
PY_STATUS
fi
cp -f "$tmpdir/outputs/build/build_status.json" "$tmpdir/build_status.json"
# Build metadata is stored separately from empirical diagnostic results.
# Drop stale root-level diagnostic CSVs from earlier layouts before zipping review.zip.
if [[ -d "$tmpdir/outputs/diagnostics" ]]; then
  find "$tmpdir/outputs/diagnostics" -maxdepth 1 -type f -name '*.csv' -delete
fi
cp -R outputs/benchmarking "$tmpdir/outputs/" 2>/dev/null || true
rm -rf "$tmpdir/data/processed"
if [[ -d data/processed ]]; then
  mkdir -p "$tmpdir/data"
  cp -R data/processed "$tmpdir/data/"
fi

# Manually remove the eight local-only/cache families identified in review, plus
# common render/cache byproducts.
rm -rf \
  "$tmpdir/_targets" \
  "$tmpdir/renv/library" \
  "$tmpdir/application-samples/.work" \
  "$tmpdir/scripts/__pycache__" \
  "$tmpdir/.quarto-home" \
  "$tmpdir/.texcache" \
  "$tmpdir/__MACOSX"
find "$tmpdir" -name '.DS_Store' -delete
rm -rf "$tmpdir/renv/staging" "$tmpdir/renv/cache" "$tmpdir/.quarto" "$tmpdir/.Rproj.user"
find "$tmpdir" -type d \( -name '*_cache' -o -name '*_files' -o -name '__pycache__' \) -prune -exec rm -rf {} +
find "$tmpdir" -type f \( -name '*.aux' -o -name '*.log' -o -name '*.fls' -o -name '*.fdb_latexmk' -o -name '*.synctex.gz' -o -name '*.toc' -o -name '*.out' -o -name '*.bbl' -o -name '*.blg' -o -name '*.nb.html' \) -delete

# Raw data and literature are intentionally omitted from the review archive.
rm -rf "$tmpdir/data/raw" "$tmpdir/data/raw_future" "$tmpdir/relevant-literature"
rm -rf "$tmpdir/archive/implementation-bundles"
rm -f "$tmpdir/docs/plan/THOROUGH NOTES Research Paper ECON 623.docx" \
      "$tmpdir/docs/plan/COMPACTED NOTES Research Paper ECON 623.docx"

required_public=(
  "paper/paper.pdf"
)
if [[ "$include_samples" == "true" ]]; then
  while IFS= read -r sample_output; do
    [[ -n "$sample_output" ]] && required_public+=("$sample_output")
  done < <(Rscript -e 'source("R/io/utils_data_frame.R"); source("R/application_samples/sample_manifest.R"); cat(paste(application_sample_expected_outputs(), collapse = "\n"), "\n")')
fi
if [[ "$include_poster" == "true" ]]; then
  required_public+=(
    "posters/2026_predoc_conference/poster.pdf"
    "posters/2026_predoc_conference/RishavRoy-Education.png"
  )
fi
if [[ "$allow_incomplete" != "true" ]]; then
  for f in "${required_public[@]}"; do
    if [[ ! -s "$tmpdir/$f" ]]; then
      echo "Review archive is missing required public artifact: $f" >&2
      exit 1
    fi
  done
fi

if [[ "$include_samples" != "true" && -d "$tmpdir/application-samples/output" ]]; then
  echo "Review archive unexpectedly contains application-samples/output." >&2
  exit 1
fi

(cd "$tmpdir" && zip -r "$tmp_archive" . >/dev/null)

if unzip -l "$tmp_archive" | grep -E '(^|/)(_targets|renv/library|application-samples/\.work|scripts/__pycache__|\.quarto-home|\.texcache|__MACOSX|\.DS_Store)(/|$)' >/dev/null; then
  echo "Review archive contains local-only cache artifacts." >&2
  exit 1
fi

if [[ "$include_samples" != "true" ]] && unzip -l "$tmp_archive" | grep -E '(^|/)application-samples/output/' >/dev/null; then
  echo "Review archive contains application-samples/output despite --no-samples." >&2
  exit 1
fi

# Keep the previous archive intact until the replacement has been fully built and
# validated. tmp_archive lives beside out_path, so this move is a same-filesystem
# rename on normal local filesystems.
mv -f -- "$tmp_archive" "$out_path"

echo "Wrote $out_path"
if [[ "$allow_incomplete" == "true" ]]; then
  echo "Archive was created in --allow-incomplete debug mode; final public artifacts may be absent."
fi
if [[ "$include_samples" != "true" ]]; then
  echo "Application-sample outputs were omitted; rerun with --with-samples to include them."
fi
if [[ "$include_poster" != "true" ]]; then
  echo "Conference-poster renders were omitted; rerun with --with-poster to include them."
fi
