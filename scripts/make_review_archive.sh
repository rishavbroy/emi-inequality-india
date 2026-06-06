#!/usr/bin/env bash
set -euo pipefail

out="${1:-Archive.zip}"

zip -r "$out" . \
  -x "docs/plan/THOROUGH NOTES Research Paper ECON 623.docx" \
  -x "docs/plan/COMPACTED NOTES Research Paper ECON 623.docx" \
  -x "__MACOSX/*" \
  -x ".DS_Store" \
  -x "*/.DS_Store" \
  -x ".git/*" \
  -x "Archive.zip" \
  -x "legacy.zip" \
  -x "data.zip" \
  -x "full_output.txt" \
  -x "file_list.txt" \
  -x "archive/implementation-bundles/*" \
  -x "_targets/*" \
  -x "renv/library/*" \
  -x "renv/staging/*" \
  -x "renv/cache/*" \
  -x ".quarto/*" \
  -x "*_cache/*" \
  -x "*_files/*" \
  -x "application-samples/.work/*" \
  -x "scripts/__pycache__/*" \
  -x "*/__pycache__/*" \
  -x ".Rproj.user/*" \
  -x ".RData" \
  -x ".Rhistory" \
  -x ".RDataTmp" \
  -x "*.nb.html" \
  -x "*.aux" \
  -x "*.log" \
  -x "*.fls" \
  -x "*.fdb_latexmk" \
  -x "*.synctex.gz" \
  -x "*.toc" \
  -x "*.out" \
  -x "*.bbl" \
  -x "*.blg" \
  -x "data/raw/*" \
  -x "data/raw_future/*" \
  -x "docs/*_files/*" \
  -x "analysis/**/*_files/*" \
  -x "paper/*_files/*" \
  -x "relevant-literature/*"
