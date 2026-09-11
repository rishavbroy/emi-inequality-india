# Shared public-output file contracts for final checks.

public_qmd_sources <- function() {
  c(
    "paper/paper.qmd",
    "paper/paper-new.qmd",
    "paper/appendix.qmd",
    "docs/district-matching.qmd",
    "docs/long-paths-and-8-3-filenames.qmd",
    "posters/2026_predoc_conference/poster.qmd"
  )
}

public_report_value_sources <- function() {
  c("paper/paper.qmd", "paper/paper-new.qmd", "paper/appendix.qmd", "docs/district-matching.qmd")
}

required_public_render_inputs <- function() {
  c(
    "paper/references.bib",
    "outputs/tables/main/sum_tbl_probit_quant.csv",
    "outputs/tables/main/sum_tbl_probit_cat.csv",
    "outputs/tables/main/probit_mfx.csv",
    "outputs/tables/main/sum_tbl_iv.csv",
    "outputs/tables/main/fs_cons.csv",
    "outputs/tables/main/cons_iv.csv",
    "outputs/tables/main/paper_core_summary.tex",
    "outputs/tables/main/paper_schooling_market_geography.tex",
    "outputs/tables/main/paper_language_behavior.tex",
    "outputs/tables/main/paper_economic_conversion.tex",
    "outputs/tables/main/paper_local_development.tex",
    "outputs/tables/main/paper_identification_boundary.tex",
    "outputs/figures/main/paper_language_schooling_maps.pdf",
    "outputs/figures/main/paper_unequal_schooling_access.pdf",
    "outputs/figures/main/paper_first_stage_absorption.pdf",
    "outputs/figures/main/consumption_iv_dynamics.pdf",
    "outputs/figures/main/fig_ilo_trends.png",
    "outputs/figures/main/district_carveouts_shifts.png",
    "outputs/figures/main/poster_emie_expected_values.pdf",
    "outputs/figures/main/poster_first_stage_specs.pdf",
    "outputs/figures/main/poster_second_stage_specs.pdf",
    "outputs/figures/main/map_emi_exposure.pdf",
    "outputs/figures/main/map_residual_emi_exposure.pdf",
    "outputs/figures/main/map_linguistic_distance.pdf",
    "outputs/figures/main/map_residual_linguistic_distance.pdf",
    "assets/uw-logo-horizontal-full-color-print.pdf",
    "assets/repo-qr.svg"
  )
}

application_sample_outputs <- function() {
  c(
    "application-samples/output/RishavRoy_WritingSample.pdf",
    "application-samples/output/RishavRoy_WritingSample10pg.pdf",
    "application-samples/output/RishavRoy_WritingSample5pg.pdf",
    "application-samples/output/RishavRoy_CodingSample.pdf",
    "application-samples/output/RishavRoy_CodingSample47pg.pdf",
    "application-samples/output/RishavRoy_CodingSample25pg.pdf"
  )
}

required_final_documents <- function(require_application_samples = TRUE) {
  files <- c(
    "paper/paper.pdf",
    "paper/paper-new.pdf",
    "docs/district-matching.html",
    "docs/long-paths-and-8-3-filenames.html",
    "posters/2026_predoc_conference/poster.pdf",
    "posters/2026_predoc_conference/RishavRoy-Education.png"
  )
  if (isTRUE(require_application_samples)) files <- c(files, application_sample_outputs())
  files
}

required_extended_diagnostic_outputs <- function() {
  c(
    "outputs/diagnostics/extended/iv/construct_registry.csv",
    "outputs/diagnostics/extended/instrument_relevance/census_1991_st_language_coverage.csv",
    "outputs/diagnostics/extended/instrument_relevance/census_1991_st_language_estimates.csv",
    "outputs/diagnostics/extended/schooling_access/nss64_social_group_access_summary.csv",
    "outputs/diagnostics/extended/schooling_access/nss64_social_group_access_crosscuts.csv",
    "outputs/diagnostics/extended/schooling_access/nss64_social_group_distance_heterogeneity.csv",
    "outputs/diagnostics/extended/census_households/household_capacity_trajectory_specifications.csv",
    "outputs/diagnostics/extended/census_households/household_capacity_trajectory_estimates.csv",
    "outputs/diagnostics/extended/census_migration/hindi_belt_skilled_migration.csv",
    "outputs/diagnostics/extended/economic_census/ec05_it_opportunity_specifications.csv",
    "outputs/diagnostics/extended/economic_census/ec05_it_opportunity_estimates.csv",
    "outputs/diagnostics/extended/mechanisms/st_concentration_heterogeneity_registry.csv",
    "outputs/diagnostics/extended/mechanisms/st_concentration_heterogeneity_estimates.csv",
    "outputs/diagnostics/extended/consumption/schooling_consumption_bridge_specifications.csv",
    "outputs/diagnostics/extended/consumption/schooling_consumption_bridge_estimates.csv",
    "outputs/diagnostics/extended/consumption/schooling_consumption_conversion_specifications.csv",
    "outputs/diagnostics/extended/consumption/schooling_consumption_conversion_estimates.csv",
    "outputs/diagnostics/extended/consumption/consumption_exclusion_sensitivity_summary.csv",
    "outputs/diagnostics/extended/consumption/consumption_exclusion_sensitivity_grid.csv",
    "outputs/diagnostics/extended/instrument_relevance/iv_falsification_adaptive_set_summary.csv",
    "outputs/diagnostics/extended/instrument_relevance/iv_falsification_adaptive_set_components.csv",
    "outputs/diagnostics/extended/mechanisms/evidence_grid.csv",
    "outputs/diagnostics/extended/mechanisms/family_summary.csv"
  )
}

required_final_artifacts <- function() {
  c(
    "paper/references.bib",
    "outputs/tables/main/sum_tbl_probit_quant.csv",
    "outputs/tables/main/sum_tbl_probit_cat.csv",
    "outputs/tables/main/probit_mfx.csv",
    "outputs/tables/main/sum_tbl_iv.csv",
    "outputs/tables/main/fs_cons.csv",
    "outputs/tables/main/cons_iv.csv",
    "outputs/tables/main/paper_core_summary.csv",
    "outputs/tables/main/paper_core_summary.tex",
    "outputs/tables/main/paper_schooling_market_geography.csv",
    "outputs/tables/main/paper_schooling_market_geography.tex",
    "outputs/tables/main/paper_language_behavior.csv",
    "outputs/tables/main/paper_language_behavior.tex",
    "outputs/tables/main/paper_economic_conversion.csv",
    "outputs/tables/main/paper_economic_conversion.tex",
    "outputs/tables/main/paper_local_development.csv",
    "outputs/tables/main/paper_local_development.tex",
    "outputs/tables/main/paper_identification_boundary.csv",
    "outputs/tables/main/paper_identification_boundary.tex",
    "outputs/tables/appendix/appendix_a1_data_source_timing.csv",
    "outputs/tables/appendix/appendix_a1_data_source_timing.tex",
    "outputs/figures/appendix/appendix_a2_lineage_logic.pdf",
    "outputs/tables/appendix/appendix_a3_lineage_source_hierarchy.csv",
    "outputs/tables/appendix/appendix_a3_lineage_source_hierarchy.tex",
    "outputs/tables/appendix/appendix_a4_nss_schooling_constructs.csv",
    "outputs/tables/appendix/appendix_a4_nss_schooling_constructs.tex",
    "outputs/tables/appendix/appendix_a5_dise_construction.csv",
    "outputs/tables/appendix/appendix_a5_dise_construction.tex",
    "outputs/tables/appendix/appendix_a6_linguistic_measures.csv",
    "outputs/tables/appendix/appendix_a6_linguistic_measures.tex",
    "outputs/tables/appendix/appendix_a7_consumption_construction.csv",
    "outputs/tables/appendix/appendix_a7_consumption_construction.tex",
    "outputs/tables/appendix/appendix_a8_other_outcome_panels.csv",
    "outputs/tables/appendix/appendix_a8_other_outcome_panels.tex",
    "outputs/tables/appendix/appendix_b1_lineage_readiness.csv",
    "outputs/tables/appendix/appendix_b1_lineage_readiness.tex",
    "outputs/tables/appendix/appendix_b2_lineage_sensitivity.csv",
    "outputs/tables/appendix/appendix_b2_lineage_sensitivity.tex",
    "outputs/tables/appendix/appendix_b3_consumption_reconstruction.csv",
    "outputs/tables/appendix/appendix_b3_consumption_reconstruction.tex",
    "outputs/tables/appendix/appendix_b4_hces_consistency_summary.csv",
    "outputs/tables/appendix/appendix_b4_hces_consistency_summary.tex",
    "outputs/figures/appendix/appendix_b4_hces_consistency.pdf",
    "outputs/figures/appendix/appendix_b5_historical_language_persistence.pdf",
    "outputs/tables/appendix/appendix_b6_language_source_validation.csv",
    "outputs/tables/appendix/appendix_b6_language_source_validation.tex",
    "outputs/figures/appendix/appendix_b7_nss_dise_agreement.pdf",
    "outputs/tables/appendix/appendix_b8_census_universe_reconciliation.csv",
    "outputs/tables/appendix/appendix_b8_census_universe_reconciliation.tex",
    "outputs/tables/appendix/appendix_b9_dise_publication_validation.csv",
    "outputs/tables/appendix/appendix_b9_dise_publication_validation.tex",
    "outputs/tables/appendix/appendix_c1_full_absorption_ladder.csv",
    "outputs/tables/appendix/appendix_c1_full_absorption_ladder.tex",
    "outputs/figures/appendix/appendix_c2_residual_geography.pdf",
    "outputs/tables/appendix/appendix_c3_control_block_absorption.csv",
    "outputs/tables/appendix/appendix_c3_control_block_absorption.tex",
    "outputs/tables/appendix/appendix_c4_geographic_scale_sensitivity.csv",
    "outputs/tables/appendix/appendix_c4_geographic_scale_sensitivity.tex",
    "outputs/tables/appendix/appendix_c5_alternative_scalar_distances.csv",
    "outputs/tables/appendix/appendix_c5_alternative_scalar_distances.tex",
    "outputs/tables/appendix/appendix_c6_mapping_composition_sensitivity.csv",
    "outputs/tables/appendix/appendix_c6_mapping_composition_sensitivity.tex",
    "outputs/tables/appendix/appendix_c7_historical_balance.csv",
    "outputs/tables/appendix/appendix_c7_historical_balance.tex",
    "outputs/figures/appendix/appendix_c8_historical_pretrends.pdf",
    "outputs/figures/appendix/appendix_c8_historical_pretrends.csv",
    "outputs/tables/appendix/appendix_c9_historical_first_stage.csv",
    "outputs/tables/appendix/appendix_c9_historical_first_stage.tex",
    "outputs/figures/appendix/appendix_c10_monotonicity.pdf",
    "outputs/figures/appendix/appendix_c10_monotonicity.csv",
    "outputs/tables/appendix/appendix_c11_multiple_instruments.csv",
    "outputs/tables/appendix/appendix_c11_multiple_instruments.tex",
    "outputs/figures/appendix/appendix_c12_consumption_iv_dynamics.pdf",
    "outputs/figures/appendix/appendix_c12_consumption_iv_dynamics.csv",
    "outputs/tables/appendix/appendix_c13_robustness_family_census.csv",
    "outputs/tables/appendix/appendix_c13_robustness_family_census.tex",
    "outputs/tables/appendix/appendix_c14_exclusion_sensitivity.csv",
    "outputs/tables/appendix/appendix_c14_exclusion_sensitivity.tex",
    "outputs/tables/appendix/appendix_d1_migration.csv",
    "outputs/tables/appendix/appendix_d1_migration.tex",
    "outputs/tables/appendix/appendix_d2_migration_context.csv",
    "outputs/tables/appendix/appendix_d2_migration_context.tex",
    "outputs/tables/appendix/appendix_d3_housing_assets.csv",
    "outputs/tables/appendix/appendix_d3_housing_assets.tex",
    "outputs/tables/appendix/appendix_d4_economic_census.csv",
    "outputs/tables/appendix/appendix_d4_economic_census.tex",
    "outputs/tables/appendix/appendix_d5_labor.csv",
    "outputs/tables/appendix/appendix_d5_labor.tex",
    "outputs/tables/appendix/appendix_d6_household_capacity.csv",
    "outputs/tables/appendix/appendix_d6_household_capacity.tex",
    "outputs/tables/appendix/appendix_d7_social_heterogeneity.csv",
    "outputs/tables/appendix/appendix_d7_social_heterogeneity.tex",
    "outputs/tables/appendix/appendix_d9_residual_spatial_diagnostics.csv",
    "outputs/tables/appendix/appendix_d9_residual_spatial_diagnostics.tex",
    "outputs/tables/appendix/appendix_e1_selection_sample.csv",
    "outputs/tables/appendix/appendix_e1_selection_sample.tex",
    "outputs/tables/main/probit_mfx.tex",
    "outputs/tables/appendix/appendix_e4_missingness.csv",
    "outputs/tables/appendix/appendix_e4_missingness.tex",
    "outputs/figures/appendix/appendix_e5_missingness_predictability.png",
    "outputs/figures/main/paper_language_schooling_maps.pdf",
    "outputs/figures/main/paper_unequal_schooling_access.pdf",
    "outputs/figures/main/paper_first_stage_absorption.pdf",
    "outputs/figures/main/consumption_iv_dynamics.pdf",
    "outputs/figures/main/fig_ilo_trends.png",
    "outputs/figures/main/district_carveouts_shifts.png",
    "outputs/figures/main/collage_main_maps.png",
    "outputs/figures/main/collage_iv_region_maps.png",
    "outputs/figures/main/poster_emie_expected_values.pdf",
    "posters/2026_predoc_conference/poster.pdf",
    "posters/2026_predoc_conference/RishavRoy-Education.png",
    "outputs/diagnostics/public/spatial_moran_tests.csv",
    "outputs/diagnostics/public/spatial_moran_mc_reference.csv",
    "outputs/diagnostics/public/multicollinearity_diagnostics.csv",
    "outputs/diagnostics/public/anderson_rubin_candidate_designs.csv"
  )
}

missing_or_empty_files <- function(paths) {
  paths[!file.exists(paths) | file.info(paths)$size <= 0]
}


add_failure <- function(...) {
  failures <<- c(failures, paste0(...))
}

is_false_env <- function(name, default = "true") {
  tolower(trimws(Sys.getenv(name, default))) %in% c("0", "false", "no", "off")
}
