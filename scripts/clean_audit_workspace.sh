#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
diagnostics_root="${root%/}/outputs/diagnostics"
build_root="${root%/}/outputs/build"
legacy_derived_root="${root%/}/outputs/derived"
paper_root="${root%/}/paper"

rm -f "$paper_root"/*_bibertool.bib 2>/dev/null || true
rm -f \
  "$paper_root/appendix.pdf" "$paper_root/appendix.html" "$paper_root/appendix.tex" \
  "${root%/}/docs/district-matching.html" "${root%/}/docs/district-matching.pdf" "${root%/}/docs/district-matching.tex" \
  "${root%/}/docs/long-paths-and-8-3-filenames.html" "${root%/}/docs/long-paths-and-8-3-filenames.pdf" "${root%/}/docs/long-paths-and-8-3-filenames.tex" \
  2>/dev/null || true
rm -f \
  "${root%/}/outputs/tables/appendix/appendix_a3_lineage_source_hierarchy.csv" \
  "${root%/}/outputs/tables/appendix/appendix_a3_lineage_source_hierarchy.tex" \
  "${root%/}/outputs/tables/appendix/appendix_a6_linguistic_measures.csv" \
  "${root%/}/outputs/tables/appendix/appendix_a6_linguistic_measures.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b1_lineage_readiness.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b1_lineage_readiness.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b2_lineage_sensitivity.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b2_lineage_sensitivity.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b6_language_source_validation.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b6_language_source_validation.tex" \
  "${root%/}/outputs/figures/appendix/appendix_b5_historical_language_persistence.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_b5_historical_language_persistence.png" \
  "${root%/}/outputs/figures/appendix/appendix_b7_nss_dise_agreement.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_b7_nss_dise_agreement.png" \
  "${root%/}/outputs/tables/appendix/appendix_b8_census_universe_reconciliation.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b8_census_universe_reconciliation.tex" \
  "${root%/}/outputs/tables/appendix/appendix_d3_housing_assets.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d3_housing_assets.tex" \
  "${root%/}/outputs/tables/appendix/appendix_d4_economic_census.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d4_economic_census.tex" \
  "${root%/}/outputs/tables/appendix/appendix_d5_labor.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d5_labor.tex" \
  "${root%/}/outputs/tables/appendix/appendix_d6_household_capacity.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d6_household_capacity.tex" \
  "${root%/}/outputs/tables/appendix/appendix_d7_social_heterogeneity.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d7_social_heterogeneity.tex" \
  "${root%/}/outputs/figures/appendix/appendix_d8_raw_spatial_geography.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_d8_raw_spatial_geography.png" \
  "${root%/}/outputs/figures/appendix/appendix_d8_raw_spatial_geography.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d9_residual_spatial_diagnostics.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d9_residual_spatial_diagnostics.tex" \
  "${root%/}/outputs/tables/appendix/appendix_a7_consumption_construction.csv" \
  "${root%/}/outputs/tables/appendix/appendix_a7_consumption_construction.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b3_consumption_reconstruction.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b3_consumption_reconstruction.tex" \
  "${root%/}/outputs/tables/appendix/appendix_b4_hces_consistency_summary.csv" \
  "${root%/}/outputs/tables/appendix/appendix_b4_hces_consistency_summary.tex" \
  "${root%/}/outputs/figures/appendix/appendix_b4_hces_consistency.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_b4_hces_consistency.png" \
  "${root%/}/outputs/tables/appendix/appendix_d1_migration.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d1_migration.tex" \
  "${root%/}/outputs/tables/appendix/appendix_d2_migration_context.csv" \
  "${root%/}/outputs/tables/appendix/appendix_d2_migration_context.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c1_full_absorption_ladder.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c1_full_absorption_ladder.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c3_control_block_absorption.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c3_control_block_absorption.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c4_geographic_scale_sensitivity.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c4_geographic_scale_sensitivity.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c5_alternative_scalar_distances.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c5_alternative_scalar_distances.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c6_mapping_composition_sensitivity.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c6_mapping_composition_sensitivity.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c7_historical_balance.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c7_historical_balance.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c9_historical_first_stage.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c9_historical_first_stage.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c11_multiple_instruments.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c11_multiple_instruments.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c13_robustness_family_census.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c13_robustness_family_census.tex" \
  "${root%/}/outputs/tables/appendix/appendix_c14_exclusion_sensitivity.csv" \
  "${root%/}/outputs/tables/appendix/appendix_c14_exclusion_sensitivity.tex" \
  "${root%/}/outputs/figures/appendix/appendix_c2_residual_geography.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_c2_residual_geography.png" \
  "${root%/}/outputs/figures/appendix/appendix_c8_historical_pretrends.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_c8_historical_pretrends.png" \
  "${root%/}/outputs/figures/appendix/appendix_c10_monotonicity.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_c10_monotonicity.png" \
  "${root%/}/outputs/figures/appendix/appendix_c12_consumption_iv_dynamics.pdf" \
  "${root%/}/outputs/figures/appendix/appendix_c12_consumption_iv_dynamics.png" \
  "${root%/}/outputs/tables/main/paper_identification_boundary.csv" \
  "${root%/}/outputs/tables/main/paper_identification_boundary.tex" \
  "${root%/}/outputs/figures/main/paper_first_stage_absorption.pdf" \
  "${root%/}/outputs/figures/main/paper_first_stage_absorption.png" \
  "${root%/}/outputs/figures/main/paper_language_schooling_maps.pdf" \
  "${root%/}/outputs/figures/main/paper_language_schooling_maps.png" \
  "${root%/}/outputs/tables/appendix/appendix_e1_selection_sample.csv" \
  "${root%/}/outputs/tables/appendix/appendix_e1_selection_sample.tex" \
  "${root%/}/outputs/tables/appendix/appendix_e4_missingness.csv" \
  "${root%/}/outputs/tables/appendix/appendix_e4_missingness.tex" \
  "${root%/}/outputs/figures/appendix/appendix_e5_missingness_predictability.png" \
  2>/dev/null || true

rm -rf \
  "$build_root" \
  "$diagnostics_root/build" \
  "$diagnostics_root/public" \
  "$diagnostics_root/extended/district_lineage_v2" \
  "$legacy_derived_root/district_lineage_v2" \
  "${root%/}/data/processed/geography_v2"

find "$diagnostics_root" -maxdepth 1 -type f -name '*.csv' -delete 2>/dev/null || true
mkdir -p \
  "$build_root" \
  "$diagnostics_root/public" \
  "$diagnostics_root/extended" \
  "${root%/}/outputs/benchmarking"
