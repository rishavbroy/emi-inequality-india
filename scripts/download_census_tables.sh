#!/usr/bin/env bash
set -euo pipefail

repo_root="${EMI_PROJECT_ROOT:-$(git rev-parse --show-toplevel)}"
cd "$repo_root"

curl_bin="${CURL_BIN:-curl}"
if ! command -v "$curl_bin" >/dev/null 2>&1; then
  printf 'Required download command not found: %s\n' "$curl_bin" >&2
  exit 1
fi

if [[ $# -gt 0 ]]; then
  manifests=("$@")
else
  manifests=(
    data/metadata/census_2001_download_manifest.tsv
    data/metadata/census_2011_download_manifest.tsv
  )
fi

downloaded=0
skipped=0

for manifest in "${manifests[@]}"; do
  if [[ ! -f "$manifest" ]]; then
    printf 'Manifest not found: %s\n' "$manifest" >&2
    exit 1
  fi

  normalized_manifest="$(mktemp)"
  tr -d '\r' < "$manifest" > "$normalized_manifest"

  expected_header=$'table\tstate_code\trelative_path\turl'
  IFS= read -r header < "$normalized_manifest"
  if [[ "$header" != "$expected_header" ]]; then
    printf 'Unexpected manifest header in %s\nExpected: %s\nFound:    %s\n' \
      "$manifest" "$expected_header" "$header" >&2
    rm -f "$normalized_manifest"
    exit 1
  fi

  awk -F '\t' '
    NR == 1 { next }
    NF != 4 || $1 == "" || $2 == "" || $3 == "" || $4 == "" {
      printf "Malformed manifest row %d\n", NR > "/dev/stderr"
      bad = 1
      next
    }
    $3 !~ /^data\/raw\/census_(2001|2011)\// || $3 ~ /(^|\/)\.\.($|\/)/ {
      printf "Unsafe Census destination on row %d: %s\n", NR, $3 > "/dev/stderr"
      bad = 1
    }
    $4 !~ /^https:\/\/censusindia\.gov\.in\// {
      printf "Unexpected Census URL on row %d: %s\n", NR, $4 > "/dev/stderr"
      bad = 1
    }
    seen_path[$3]++ {
      printf "Duplicate destination on row %d: %s\n", NR, $3 > "/dev/stderr"
      bad = 1
    }
    seen_url[$4]++ {
      printf "Duplicate URL on row %d: %s\n", NR, $4 > "/dev/stderr"
      bad = 1
    }
    END { exit bad }
  ' "$normalized_manifest"

  while IFS=$'\t' read -r table state_code relative_path url; do
    [[ "$table" == "table" ]] && continue

    if [[ -s "$relative_path" ]]; then
      skipped=$((skipped + 1))
      continue
    fi

    mkdir -p "$(dirname "$relative_path")"
    part="${relative_path}.part"
    rm -f "$part"
    printf 'Downloading %s\n' "$relative_path"

    "$curl_bin" \
      --fail \
      --location \
      --retry 5 \
      --retry-delay 2 \
      --connect-timeout 30 \
      --output "$part" \
      "$url"

    if [[ ! -s "$part" ]]; then
      printf 'Downloaded file is empty: %s\n' "$part" >&2
      rm -f "$normalized_manifest"
      exit 1
    fi

    mv "$part" "$relative_path"
    downloaded=$((downloaded + 1))
  done < "$normalized_manifest"

  rm -f "$normalized_manifest"
done

printf 'Census downloads complete: %d downloaded, %d already present.\n' \
  "$downloaded" "$skipped"
