#!/usr/bin/env bash
set -euo pipefail

out="${1:-review.zip}"

if [[ ! -f .public-final-ok ]]; then
  echo "Cannot build review archive because .public-final-ok is missing. Run make check-public-final successfully first." >&2
  exit 1
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
mkdir -p "$tmpdir/paper" "$tmpdir/docs" "$tmpdir/application-samples/output" "$tmpdir/outputs"
cp -f paper/report.pdf paper/report.html paper/report.qmd "$tmpdir/paper/" 2>/dev/null || true
cp -f paper/appendix.pdf paper/appendix.html paper/appendix.qmd "$tmpdir/paper/" 2>/dev/null || true
cp -f docs/district-matching.html docs/district-matching.pdf docs/district-matching.qmd "$tmpdir/docs/" 2>/dev/null || true
cp -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf docs/long-paths-and-8-3-filenames.qmd "$tmpdir/docs/" 2>/dev/null || true
cp -f application-samples/output/*.pdf "$tmpdir/application-samples/output/" 2>/dev/null || true
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
  "application-samples/output/RishavRoy_WritingSample.pdf"
  "application-samples/output/RishavRoy_WritingSample10pg.pdf"
  "application-samples/output/RishavRoy_WritingSample5pg.pdf"
  "application-samples/output/RishavRoy_CodingSample.pdf"
  "application-samples/output/RishavRoy_CodingSample47pg.pdf"
  "application-samples/output/RishavRoy_CodingSample25pg.pdf"
)
for f in "${required_public[@]}"; do
  if [[ ! -s "$tmpdir/$f" ]]; then
    echo "Review archive is missing required public artifact: $f" >&2
    exit 1
  fi
done

rm -f "$out"
(cd "$tmpdir" && zip -r "$OLDPWD/$out" . >/dev/null)

if unzip -l "$out" | grep -E '(^|/)(_targets|renv/library|application-samples/\.work|scripts/__pycache__|\.quarto-home|\.texcache|__MACOSX|\.DS_Store)(/|$)' >/dev/null; then
  echo "Review archive contains local-only cache artifacts." >&2
  exit 1
fi

echo "Wrote $out"
