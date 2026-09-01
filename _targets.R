library(targets)

source("R/config.R")
source("R/paths.R")

tar_source("R/io")
tar_source("R/clean")
tar_source("R/districts")
tar_source("R/measures")
tar_source("R/prices")
tar_source("R/controls")
tar_source("R/selection")
tar_source("R/iv")
tar_source("R/diagnostics")
tar_source("R/benchmarking")
tar_source("R/output")
tar_source("R/application_samples")

source("R/pipeline/core_consumption_targets.R")
source("R/pipeline/core_consumption_outcome_targets.R")
source("R/pipeline/core_consumption_iv_targets.R")
source("R/pipeline/core_measurement_targets.R")
source("R/pipeline/core_lineage_targets.R")
source("R/pipeline/core_panel_targets.R")
source("R/pipeline/core_public_targets.R")
source("R/pipeline/extended_historical_targets.R")
source("R/pipeline/extended_lineage_targets.R")
source("R/pipeline/extended_census_targets.R")
source("R/pipeline/extended_economic_census_targets.R")
source("R/pipeline/extended_labor_targets.R")
source("R/pipeline/extended_dise_targets.R")
source("R/pipeline/extended_iv_targets.R")
source("R/pipeline/extended_diagnostic_targets.R")

tar_option_set(
  packages = character(),
  format = "rds",
  error = "abridge"
)

env_flag_enabled <- function(name, default = FALSE) {
  default_value <- if (isTRUE(default)) "true" else "false"
  value <- tolower(trimws(Sys.getenv(name, default_value)))
  !value %in% c("0", "false", "no", "off")
}

render_application_samples_enabled <- function() {
  env_flag_enabled("EMI_RENDER_APPLICATION_SAMPLES", default = TRUE)
}

extended_diagnostics_enabled <- function() {
  env_flag_enabled("EMI_RUN_EXTENDED_DIAGNOSTICS", default = FALSE)
}

benchmarks_enabled <- function() {
  env_flag_enabled("EMI_RUN_BENCHMARKS", default = FALSE)
}

analysis_notes_enabled <- function() {
  env_flag_enabled("EMI_RENDER_ANALYSIS_NOTES", default = FALSE)
}


core_pipeline_targets <- c(
  list(
  tar_target(config_path, Sys.getenv("EMI_CONFIG", "config/draft.yml"), cue = tar_cue(mode = "always")),
  tar_target(cfg, read_config(config_path)),
  tar_target(paths, build_paths()),
  tar_target(
    census_2001_control_registry_file,
    census_2001_control_registry_path(paths),
    format = "file"
  ),
  tar_target(
    census_2001_control_registry,
    read_census_2001_control_registry(census_2001_control_registry_file)
  ),
  tar_target(raw_manifest, validate_raw_files(paths)),
  tar_target(raw_data_preflight, stop_if_required_files_missing(raw_manifest))
  ),
  core_consumption_target_definitions(),
  core_measurement_target_definitions(),
  core_lineage_target_definitions(),
  core_consumption_outcome_target_definitions(),
  core_panel_target_definitions(),
  core_consumption_iv_target_definitions(),
  core_public_target_definitions()
)

legacy_geography_targets <- list(
  tar_target(raw_boundaries_2020, { raw_data_preflight; read_district_boundaries_2020(paths) }),
  tar_target(raw_district_changes, { raw_data_preflight; read_district_change_sources(paths) }),
  tar_target(boundaries_2020, clean_district_boundaries(raw_boundaries_2020)),
  tar_target(district_keys_2020, build_district_keys_2020(boundaries_2020)),
  tar_target(district_tracker_raw, build_district_tracker(raw_district_changes)),
  tar_target(district_tracker, apply_manual_district_corrections(district_tracker_raw)),
  tar_target(district_harmonization_crosswalk_file, "data/metadata/district_harmonization_crosswalk.csv", format = "file"),
  tar_target(district_harmonization_crosswalk, read_district_harmonization_crosswalk(district_harmonization_crosswalk_file)),
  tar_target(
    district_join_map,
    prepare_district_join_map(district_harmonization_crosswalk)
  )
)

legacy_comparison_targets <- list(
  tar_target(
    district_panel_legacy,
    {
      legacy_cfg <- utils::modifyList(
        cfg,
        list(
          strict_district_panel_validation = FALSE,
          strict_analysis_panel_validation = FALSE
        )
      )
      build_district_panel(
        district_join_map, measures_2007, measures_2017,
        linguistic_distance_iv, boundaries_2020, legacy_cfg
      )
    }
  ),
  tar_target(legacy_mapping_reviews_file, "data/metadata/district_legacy_mapping_reviews.csv", format = "file"),
  tar_target(legacy_mapping_reviews, read_legacy_mapping_reviews(read.csv(legacy_mapping_reviews_file, stringsAsFactors = FALSE, check.names = FALSE))),
  tar_target(
    legacy_crosswalk_comparison,
    build_legacy_crosswalk_comparison(
      district_lineage$conservative_source_crosswalk,
      district_panel_legacy,
      legacy_mapping_reviews
    )
  ),
  tar_target(
    diag_ext_legacy_crosswalk_comparison,
    write_diagnostic_csv(
      legacy_crosswalk_comparison,
      "outputs/diagnostics/extended/district_lineage/legacy_crosswalk_comparison.csv"
    ),
    format = "file"
  )
)

extended_diagnostic_targets <- extended_diagnostic_target_definitions()

benchmark_targets <- list(
  tar_target(bench_ame_methods, run_ame_methods_benchmark(selection_model, cfg)),
  tar_target(
    bench_consumption_distribution_domains,
    run_consumption_distribution_benchmark(
      consumption_households_lineaged_2004_05,
      consumption_welfare_outcomes
    ),
    format = "file"
  ),
  tar_target(bench_fuzzy_matching, run_fuzzy_matching_benchmark(district_tracker, district_join_map, cfg)),
  tar_target(bench_spatial_weights, run_spatial_weights_benchmark(district_panel, cfg)),
  tar_target(bench_spatial_iv_experimental, run_spatial_iv_benchmark(district_panel, spatial_weights, with_diagnostic_enabled(cfg, "spatial_iv_experimental")))
)


analysis_note_targets <- analysis_markdown_target_definitions("analysis")

application_sample_targets <- list(
  tar_target(application_sample_inputs, application_sample_input_files(), format = "file"),
  tar_target(writing_sample_pdfs, { report_values; application_sample_inputs; render_writing_samples(output_files = c(figure_files, table_files)) }, format = "file"),
  tar_target(coding_sample_pdfs, { report_values; application_sample_inputs; render_coding_samples(output_files = c(figure_files, table_files)) }, format = "file")
)

selected_targets <- core_pipeline_targets

if (extended_diagnostics_enabled() || benchmarks_enabled()) {
  selected_targets <- c(selected_targets, legacy_geography_targets)
}

if (extended_diagnostics_enabled()) {
  selected_targets <- c(selected_targets, legacy_comparison_targets, extended_diagnostic_targets)
} else {
  message("EMI_RUN_EXTENDED_DIAGNOSTICS=false: omitting extended diagnostic targets from this targets run.")
}

if (benchmarks_enabled()) {
  selected_targets <- c(selected_targets, benchmark_targets)
} else {
  message("EMI_RUN_BENCHMARKS=false: omitting benchmark targets from this targets run.")
}

if (analysis_notes_enabled()) {
  selected_targets <- c(selected_targets, analysis_note_targets)
} else {
  message("EMI_RENDER_ANALYSIS_NOTES=false: omitting analysis-note render targets from this targets run.")
}

if (render_application_samples_enabled()) {
  selected_targets <- c(selected_targets, application_sample_targets)
} else {
  message("EMI_RENDER_APPLICATION_SAMPLES=false: omitting application-sample targets from this targets run.")
}

selected_targets
