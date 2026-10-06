#!/usr/bin/env bash
set -euo pipefail

site_dir="${1:-_site}"
sample_dir="application-samples/output"
code_sample="$sample_dir/RishavRoy_CodeSample.pdf"

mkdir -p "$site_dir/application-samples"
cp pages/index.html "$site_dir/index.html"
cp pages/404.html "$site_dir/404.html"
cp paper/paper.pdf "$site_dir/paper.pdf"
cp "$sample_dir"/RishavRoy_*.pdf "$site_dir/application-samples/"

for legacy_name in Long Short; do
  cp "$code_sample" "$site_dir/application-samples/RishavRoy_CodeSample_${legacy_name}.pdf"
done
