#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
diagnostics_root="${root%/}/outputs/diagnostics"
derived_root="${root%/}/outputs/derived"
paper_root="${root%/}/paper"

rm -f "$paper_root"/*_bibertool.bib 2>/dev/null || true
rm -f \
  "$paper_root/appendix.pdf" "$paper_root/appendix.html" "$paper_root/appendix.tex" \
  "${root%/}/docs/district-matching.html" "${root%/}/docs/district-matching.pdf" "${root%/}/docs/district-matching.tex" \
  "${root%/}/docs/long-paths-and-8-3-filenames.html" "${root%/}/docs/long-paths-and-8-3-filenames.pdf" "${root%/}/docs/long-paths-and-8-3-filenames.tex" \
  2>/dev/null || true
rm -f \
  "${root%/}/outputs/tables/appendix/appendix_a7_consumption_construction.csv" \
  "${root%/}/outputs/tables/appendix/appendix_a7_consumption_construction.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b3_consumption_reconstruction.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b3_consumption_reconstruction.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b4_hces_consistency_summary.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b4_hces_consistency_summary.tex" \
  "${root%/}/outputs/figures/appendix/appendix_b4_hces_consistency.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_b4_hces_consistency.png" \
  2>/dev/null || true

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
