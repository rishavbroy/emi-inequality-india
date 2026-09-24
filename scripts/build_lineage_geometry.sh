#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

geometry_source="data/raw/datameet/Districts/Census_2001/2001_Dist.shp"
geometry_output="data/processed/geography/district_2001.gpkg"
geometry_qa="data/processed/geography/district_2001_qa.csv"
map_scaffold_output="data/processed/geography/district_2001_map_scaffold.gpkg"
legacy_geometry_output="outputs/derived/district_lineage/district_2001.gpkg"
legacy_geometry_qa="outputs/derived/district_lineage/district_2001_qa.csv"

if [[ -f "$legacy_geometry_output" || -f "$legacy_geometry_qa" ]]; then
  echo "=== LINEAGE GEOMETRY: relocating legacy processed geography ==="
  mkdir -p "$(dirname "$geometry_output")"
  if [[ ! -f "$geometry_output" && -f "$legacy_geometry_output" ]]; then
    mv "$legacy_geometry_output" "$geometry_output"
  fi
  if [[ ! -f "$geometry_qa" && -f "$legacy_geometry_qa" ]]; then
    mv "$legacy_geometry_qa" "$geometry_qa"
  fi
  rm -f "$legacy_geometry_output" "$legacy_geometry_qa"
  rmdir "$(dirname "$legacy_geometry_output")" 2>/dev/null || true
  rmdir outputs/derived 2>/dev/null || true
fi

needs_build=false
if [[ ! -f "$geometry_output" || ! -f "$map_scaffold_output" ]]; then
  needs_build=true
fi

required_boundary_files=(
  "$geometry_source"
  "data/raw/datameet/Districts/Census_2001/2001_Dist.dbf"
  "data/raw/datameet/Districts/Census_2001/2001_Dist.shx"
  "data/raw/datameet/Districts/Census_2001/2001_Dist.prj"
)

if [[ "$needs_build" == true ]]; then
  missing_boundary_files=()
  for path in "${required_boundary_files[@]}"; do
    [[ -f "$path" ]] || missing_boundary_files+=("$path")
  done
  if ((${#missing_boundary_files[@]})); then
    echo "Missing required processed Census-2001 map geometry and its DataMeet source bundle." >&2
    printf 'Missing: %s\n' "${missing_boundary_files[@]}" >&2
    echo "Restore the raw DataMeet boundary files so the canonical geometry and display scaffold can be built." >&2
    exit 1
  fi
fi

if [[ -f "$geometry_source" && "$needs_build" == false ]]; then
  shopt -s nullglob
  inputs=(
    "${required_boundary_files[@]}"
    data/raw/census_2001/languages/C16/PC01_C16_*.xls
    R/clean/clean_census_2001_languages.R
    R/districts/lineage_completion.R
    R/districts/lineage_sources.R
    scripts/build_lineage_geometry.R
    scripts/build_lineage_geometry.sh
  )
  shopt -u nullglob

  for input in "${inputs[@]}"; do
    if [[ "$input" -nt "$geometry_output" || "$input" -nt "$map_scaffold_output" ]]; then
      needs_build=true
      break
    fi
  done
fi

if [[ "$needs_build" == true ]]; then
  echo "=== LINEAGE GEOMETRY: building DataMeet Census 2001 GeoPackage ==="
  EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=false \
    Rscript scripts/run_targets_checked.R --targets census_2001_languages
  Rscript scripts/build_lineage_geometry.R
else
  echo "=== LINEAGE GEOMETRY: up to date ==="
fi
