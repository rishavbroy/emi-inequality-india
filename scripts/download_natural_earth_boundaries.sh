#!/usr/bin/env bash
set -euo pipefail

repo_root="${EMI_PROJECT_ROOT:-$(git rev-parse --show-toplevel)}"
cd "$repo_root"

curl_bin="${CURL_BIN:-curl}"
if ! command -v "$curl_bin" >/dev/null 2>&1; then
  printf 'Required download command not found: %s\n' "$curl_bin" >&2
  exit 1
fi
if ! command -v unzip >/dev/null 2>&1; then
  printf 'Required extraction command not found: unzip\n' >&2
  exit 1
fi

base="data/raw/natural-earth/10m"
mkdir -p "$base"

names=(
  ne_10m_admin_0_boundary_lines_disputed_areas
)
base_url="https://naturalearth.s3.amazonaws.com/10m_cultural"

downloaded=0
skipped=0
for name in "${names[@]}"; do
  if [[ -s "$base/${name}.shp" && -s "$base/${name}.dbf" && -s "$base/${name}.shx" && -s "$base/${name}.prj" && -s "$base/${name}.cpg" ]]; then
    skipped=$((skipped + 1))
    continue
  fi
  archive="$base/${name}.zip"
  part="${archive}.part"
  rm -f "$part"
  printf 'Downloading Natural Earth %s\n' "$name"
  "$curl_bin" --fail --location --retry 5 --retry-delay 2 --connect-timeout 30 \
    --output "$part" "$base_url/${name}.zip"
  mv "$part" "$archive"
  unzip -oq "$archive" -d "$base"
  rm -f "$archive"
  for ext in shp dbf shx prj cpg; do
    if [[ ! -s "$base/${name}.${ext}" ]]; then
      printf 'Natural Earth archive did not contain %s.%s\n' "$name" "$ext" >&2
      exit 1
    fi
  done
  downloaded=$((downloaded + 1))
done

printf 'Natural Earth boundary downloads complete: %d downloaded, %d already present.\n' \
  "$downloaded" "$skipped"
