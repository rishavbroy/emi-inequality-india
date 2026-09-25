# Shared public-output file contracts for final checks.

public_qmd_sources <- function() {
  c(
    "paper/paper.qmd",
    "posters/2026_predoc_conference/poster.qmd"
  )
}

public_report_value_sources <- function() {
  "paper/paper.qmd"
}


paper_appendix_render_inputs <- function() {
  c(
    "outputs/figures/appendix/appendix_consumption_hces_consistency.pdf",
    "outputs/figures/appendix/appendix_historical_language_persistence.pdf",
    "outputs/figures/appendix/appendix_nss_dise_agreement.pdf",
    "outputs/tables/appendix/appendix_migration_summary.tex",
    "outputs/tables/appendix/appendix_selection_ame.tex",
    "outputs/tables/appendix/appendix_selection_missingness.tex",
    "outputs/tables/appendix/appendix_iv_relevance_summary.tex",
    "outputs/tables/appendix/appendix_iv_weak_inference.tex"
  )
}

poster_render_inputs <- function() {
  c(
    "outputs/figures/main/poster_first_stage_specs.pdf",
    "posters/2026_predoc_conference/generated/poster_second_stage_specs.pdf",
    "assets/uw-logo-horizontal-full-color-print.pdf",
    "assets/repo-qr.svg"
  )
}

required_public_render_inputs <- function(require_poster = FALSE) {
  files <- c(
    "paper/references.bib",
    "outputs/tables/main/paper_core_summary.tex",
    "outputs/tables/main/paper_schooling_market_geography.tex",
    "outputs/tables/main/paper_language_behavior.tex",
    "outputs/tables/main/paper_economic_conversion.tex",
    "outputs/tables/main/paper_local_development.tex",
    "outputs/figures/main/map_emi_share_enrolled.pdf",
    "outputs/figures/main/map_public_emi_exposure.pdf",
    "outputs/figures/main/map_private_emi_exposure.pdf",
    "outputs/figures/main/map_residual_linguistic_distance_state_main.pdf",
    "outputs/figures/main/map_paper_real_consumption_2022_23.pdf",
    "outputs/figures/main/map_paper_real_consumption_change.pdf",
    "outputs/figures/main/paper_unequal_schooling_access.pdf",
    "outputs/figures/main/consumption_iv_dynamics.pdf",
    "outputs/figures/main/poster_first_stage_specs.pdf",
    "outputs/figures/main/map_emi_exposure.pdf",
    "outputs/figures/main/map_linguistic_distance.pdf",
    paper_appendix_render_inputs()
  )
  if (isTRUE(require_poster)) files <- c(files, poster_render_inputs())
  unique(files)
}

application_sample_outputs <- function() {
  source("R/io/utils_data_frame.R", local = TRUE)
  source("R/application_samples/sample_manifest.R", local = TRUE)
  application_sample_expected_outputs()
}


required_final_documents <- function(require_application_samples = TRUE, require_poster = FALSE) {
  files <- "paper/paper.pdf"
  if (isTRUE(require_poster)) {
    files <- c(
      files,
      "posters/2026_predoc_conference/poster.pdf",
      "posters/2026_predoc_conference/RishavRoy-Education.png"
    )
  }
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

required_final_artifacts <- function(require_poster = FALSE) {
  files <- c(
    "paper/references.bib",
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
    "outputs/figures/appendix/appendix_consumption_hces_consistency.pdf",
    "outputs/figures/appendix/appendix_historical_language_persistence.pdf",
    "outputs/figures/appendix/appendix_nss_dise_agreement.pdf",
    "outputs/tables/appendix/appendix_iv_relevance_summary.csv",
    "outputs/tables/appendix/appendix_iv_relevance_summary.tex",
    "outputs/tables/appendix/appendix_iv_weak_inference.csv",
    "outputs/tables/appendix/appendix_iv_weak_inference.tex",
    "outputs/tables/appendix/appendix_migration_summary.csv",
    "outputs/tables/appendix/appendix_migration_summary.tex",
    "outputs/tables/appendix/appendix_selection_ame.csv",
    "outputs/tables/appendix/appendix_selection_ame.tex",
    "outputs/tables/appendix/appendix_selection_missingness.csv",
    "outputs/tables/appendix/appendix_selection_missingness.tex",
    "outputs/figures/main/map_linguistic_distance.pdf",
    "outputs/figures/main/map_emi_exposure.pdf",
    "outputs/figures/main/map_emi_share_enrolled.pdf",
    "outputs/figures/main/map_public_emi_exposure.pdf",
    "outputs/figures/main/map_private_emi_exposure.pdf",
    "outputs/figures/main/map_residual_linguistic_distance_state_main.pdf",
    "outputs/figures/main/map_paper_real_consumption_2022_23.pdf",
    "outputs/figures/main/map_paper_real_consumption_change.pdf",
    "outputs/figures/main/paper_unequal_schooling_access.pdf",
    "outputs/figures/main/consumption_iv_dynamics.pdf",
    "outputs/figures/main/poster_first_stage_specs.pdf",
    "outputs/diagnostics/public/spatial_moran_tests.csv",
    "outputs/diagnostics/public/spatial_moran_mc_reference.csv",
    "outputs/diagnostics/public/multicollinearity_diagnostics.csv",
    "outputs/diagnostics/public/anderson_rubin_candidate_designs.csv"
  )
  if (isTRUE(require_poster)) {
    files <- c(
      files, poster_render_inputs(),
      "posters/2026_predoc_conference/poster.pdf",
      "posters/2026_predoc_conference/RishavRoy-Education.png"
    )
  }
  unique(files)
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
