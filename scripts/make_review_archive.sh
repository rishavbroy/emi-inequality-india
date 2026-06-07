#!/usr/bin/env bash
set -euo pipefail

out="${1:-Archive.zip}"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

git archive --format=tar HEAD | tar -x -C "$tmpdir"

# Include rendered public artifacts that may be intentionally ignored or locally regenerated.
mkdir -p "$tmpdir/paper" "$tmpdir/application-samples/output" "$tmpdir/outputs"
cp -f paper/report.pdf "$tmpdir/paper/" 2>/dev/null || true
cp -f application-samples/output/*.pdf "$tmpdir/application-samples/output/" 2>/dev/null || true
cp -R outputs/figures "$tmpdir/outputs/" 2>/dev/null || true
cp -R outputs/tables "$tmpdir/outputs/" 2>/dev/null || true

# Ensure the eight known local-only/cache families never enter the review archive.
rm -rf \
  "$tmpdir/_targets" \
  "$tmpdir/renv/library" \
  "$tmpdir/application-samples/.work" \
  "$tmpdir/scripts/__pycache__" \
  "$tmpdir/.quarto-home" \
  "$tmpdir/.texcache" \
  "$tmpdir/__MACOSX"
find "$tmpdir" -name '.DS_Store' -delete

rm -f "$out"
(cd "$tmpdir" && zip -r "$OLDPWD/$out" . >/dev/null)

if unzip -l "$out" | grep -E '(^|/)(_targets|renv/library|application-samples/\.work|scripts/__pycache__|\.quarto-home|\.texcache|__MACOSX|\.DS_Store)(/|$)' >/dev/null; then
  echo "Review archive contains local-only cache artifacts." >&2
  exit 1
fi
