#!/usr/bin/env bash
set -euo pipefail

out="review.zip"
include_samples="true"
include_analysis="false"
include_poster="false"
allow_incomplete="false"

usage() {
  cat <<'USAGE'
Usage: bash scripts/make_review_archive.sh [--with-samples|--no-samples] [--with-analysis] [--with-poster] [--allow-incomplete] [-o OUT.zip]
       bash scripts/make_review_archive.sh OUT.zip

Creates a public review archive from the current working tree. By default the
archive is written to review.zip and includes application-sample PDFs. Analysis
report renders and conference-poster renders are included only when explicitly
requested. Use --allow-incomplete only for debugging failed builds; it packages
the current state without requiring final deliverables.
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
    --with-analysis)
      include_analysis="true"
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
cp -f paper/paper.pdf paper/paper.html paper/paper.qmd paper/paper-new.pdf paper/paper-new.html paper/paper-new.qmd "$tmpdir/paper/" 2>/dev/null || true
if [[ "$include_poster" == "true" ]]; then
  mkdir -p "$tmpdir/posters/2026_predoc_conference"
  cp -f posters/2026_predoc_conference/poster.pdf posters/2026_predoc_conference/RishavRoy-Education.png "$tmpdir/posters/2026_predoc_conference/" 2>/dev/null || true
else
  rm -f "$tmpdir/posters/2026_predoc_conference/poster.pdf" "$tmpdir/posters/2026_predoc_conference/RishavRoy-Education.png"
fi
if [[ "$include_samples" == "true" ]]; then
  mkdir -p "$tmpdir/application-samples/output"
  cp -f application-samples/output/*.pdf "$tmpdir/application-samples/output/" 2>/dev/null || true
else
  rm -rf "$tmpdir/application-samples/output"
fi
cp -R outputs/figures "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/tables "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/diagnostics "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/replication "$tmpdir/outputs/" 2>/dev/null || true
mkdir -p "$tmpdir/outputs/build"
if [[ ! -s "$tmpdir/outputs/build/build_status.json" ]]; then
  ARCHIVE_ALLOW_INCOMPLETE="$allow_incomplete" ARCHIVE_INCLUDE_SAMPLES="$include_samples" ARCHIVE_INCLUDE_ANALYSIS="$include_analysis" ARCHIVE_INCLUDE_POSTER="$include_poster" python3 - "$tmpdir/outputs/build/build_status.json" <<'PY_STATUS'
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
        "with_analysis": os.environ["ARCHIVE_INCLUDE_ANALYSIS"] == "true",
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
# Analysis source files may be tracked, but generated Markdown is included only
# when this archive corresponds to a build that rendered analysis reports.
cp -R analysis "$tmpdir/" 2>/dev/null || true
if [[ -d "$tmpdir/analysis" ]]; then
  find "$tmpdir/analysis" -type f \( -name '*.html' -o -name '*.pdf' -o -name '*.tex' -o -name '*.log' \) -delete
  if [[ "$include_analysis" != "true" ]]; then
    find "$tmpdir/analysis" -type f -name '*.md' -delete
  fi
fi
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
  "paper/paper-new.pdf"
)
if [[ "$include_samples" == "true" ]]; then
  required_public+=(
    "application-samples/output/RishavRoy_WritingSample.pdf"
    "application-samples/output/RishavRoy_WritingSample10pg.pdf"
    "application-samples/output/RishavRoy_WritingSample5pg.pdf"
    "application-samples/output/RishavRoy_CodingSample.pdf"
    "application-samples/output/RishavRoy_CodingSample47pg.pdf"
    "application-samples/output/RishavRoy_CodingSample25pg.pdf"
  )
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
if [[ "$include_analysis" != "true" ]]; then
  echo "Rendered analysis reports were omitted; rerun with --with-analysis to include them."
fi
if [[ "$include_poster" != "true" ]]; then
  echo "Conference-poster renders were omitted; rerun with --with-poster to include them."
fi
