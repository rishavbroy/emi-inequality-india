#!/usr/bin/env bash
set -euo pipefail

out="${1:-Archive.zip}"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

# Build from tracked files first so local caches, target stores, and working-tree
# byproducts cannot leak into the review bundle.
git archive --format=tar HEAD | tar -x -C "$tmpdir"

# Include public artifacts that may be generated locally after the last commit.
mkdir -p "$tmpdir/paper" "$tmpdir/docs" "$tmpdir/application-samples/output" "$tmpdir/outputs"
cp -f paper/report.pdf "$tmpdir/paper/" 2>/dev/null || true
cp -f paper/report.html "$tmpdir/paper/" 2>/dev/null || true
cp -f paper/appendix.pdf "$tmpdir/paper/" 2>/dev/null || true
cp -f docs/district-matching.html docs/district-matching.pdf "$tmpdir/docs/" 2>/dev/null || true
cp -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf "$tmpdir/docs/" 2>/dev/null || true
cp -f application-samples/output/*.pdf "$tmpdir/application-samples/output/" 2>/dev/null || true
cp -R outputs/figures "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/tables "$tmpdir/outputs/" 2>/dev/null || true

# Manually remove the eight local-only/cache families identified in review, plus
# common render/cache byproducts. The explicit eight are intentionally kept here
# as plain paths so a future reviewer can check this requirement line-by-line.
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
rm -f "$tmpdir/docs/plan/THOROUGH NOTES Research Paper ECON 623.docx"       "$tmpdir/docs/plan/COMPACTED NOTES Research Paper ECON 623.docx"

rm -f "$out"
(cd "$tmpdir" && zip -r "$OLDPWD/$out" . >/dev/null)

if unzip -l "$out" | grep -E '(^|/)(_targets|renv/library|application-samples/\.work|scripts/__pycache__|\.quarto-home|\.texcache|__MACOSX|\.DS_Store)(/|$)' >/dev/null; then
  echo "Review archive contains local-only cache artifacts." >&2
  exit 1
fi

echo "Wrote $out"
