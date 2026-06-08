#!/usr/bin/env bash
set -euo pipefail

out="review.zip"
include_samples="true"

usage() {
  cat <<'USAGE'
Usage: bash scripts/make_review_archive.sh [--with-samples|--without-samples] [-o OUT.zip]
       bash scripts/make_review_archive.sh OUT.zip

Creates a public review archive from the current working tree. By default the
archive is written to review.zip and includes application-sample PDFs. Use
--without-samples for fast-audit archives that intentionally omit
application-samples/output.
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

if [[ ! -f .public-final-ok ]]; then
  echo "Cannot build review archive because .public-final-ok is missing. Run make check-public-final successfully first." >&2
  exit 1
fi

if [[ "$out" = /* ]]; then
  out_path="$out"
else
  out_path="$PWD/$out"
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

# Copy the current working-tree versions of tracked files. This intentionally
# avoids git archive HEAD because public QMDs/outputs may have just been
# regenerated and not committed yet.
while IFS= read -r -d '' file; do
  mkdir -p "$tmpdir/$(dirname "$file")"
  if [[ -f "$file" ]]; then cp -p "$file" "$tmpdir/$file"; fi
done < <(git ls-files -z)

# Include regenerated public artifacts whether tracked or not.
mkdir -p "$tmpdir/paper" "$tmpdir/docs" "$tmpdir/outputs"
cp -f paper/report.pdf paper/report.html paper/report.qmd "$tmpdir/paper/" 2>/dev/null || true
cp -f paper/appendix.pdf paper/appendix.html paper/appendix.qmd "$tmpdir/paper/" 2>/dev/null || true
cp -f docs/district-matching.html docs/district-matching.pdf docs/district-matching.qmd "$tmpdir/docs/" 2>/dev/null || true
cp -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf docs/long-paths-and-8-3-filenames.qmd "$tmpdir/docs/" 2>/dev/null || true
if [[ "$include_samples" == "true" ]]; then
  mkdir -p "$tmpdir/application-samples/output"
  cp -f application-samples/output/*.pdf "$tmpdir/application-samples/output/" 2>/dev/null || true
else
  rm -rf "$tmpdir/application-samples/output"
fi
cp -R outputs/figures "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/tables "$tmpdir/outputs/" 2>/dev/null || true

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
  "paper/report.pdf"
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
for f in "${required_public[@]}"; do
  if [[ ! -s "$tmpdir/$f" ]]; then
    echo "Review archive is missing required public artifact: $f" >&2
    exit 1
  fi
done

if [[ "$include_samples" != "true" && -d "$tmpdir/application-samples/output" ]]; then
  echo "Fast review archive unexpectedly contains application-samples/output." >&2
  exit 1
fi

rm -f "$out_path"
(cd "$tmpdir" && zip -r "$out_path" . >/dev/null)

if unzip -l "$out_path" | grep -E '(^|/)(_targets|renv/library|application-samples/\.work|scripts/__pycache__|\.quarto-home|\.texcache|__MACOSX|\.DS_Store)(/|$)' >/dev/null; then
  echo "Review archive contains local-only cache artifacts." >&2
  exit 1
fi

if [[ "$include_samples" != "true" ]] && unzip -l "$out_path" | grep -E '(^|/)application-samples/output/' >/dev/null; then
  echo "Fast review archive contains application-samples/output despite --without-samples." >&2
  exit 1
fi

echo "Wrote $out_path"
if [[ "$include_samples" != "true" ]]; then
  echo "Application-sample outputs were omitted; rerun with --with-samples for the full review archive."
fi
