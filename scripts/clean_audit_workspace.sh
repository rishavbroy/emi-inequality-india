#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
diagnostics_root="${root%/}/outputs/diagnostics"
derived_root="${root%/}/outputs/derived"
paper_root="${root%/}/paper"

rm -f "$paper_root"/*_bibertool.bib 2>/dev/null || true

rm -rf \
  "$diagnostics_root/build" \
  "$diagnostics_root/public" \
  "$diagnostics_root/extended/district_lineage_v2" \
  "$derived_root/district_lineage_v2"

find "$diagnostics_root" -maxdepth 1 -type f -name '*.csv' -delete 2>/dev/null || true
mkdir -p \
  "$diagnostics_root/build" \
  "$diagnostics_root/public" \
  "$diagnostics_root/extended" \
  "${root%/}/outputs/benchmarking"
