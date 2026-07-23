library(targets)

source("R/config.R")
source("R/paths.R")

tar_source_r <- function(path) {
  tar_source(list.files(path, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE))
}

tar_source_r("R/io")
tar_source_r("R/clean")
tar_source_r("R/districts")
tar_source_r("R/measures")
tar_source_r("R/selection")
tar_source_r("R/iv")
tar_source_r("R/diagnostics")
tar_source_r("R/benchmarking")
tar_source_r("R/output")
tar_source_r("R/application_samples")

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


core_pipeline_targets <- list(
  tar_target(config_path, Sys.getenv("EMI_CONFIG", "config/draft.yml"), cue = tar_cue(mode = "always")),
  tar_target(cfg, read_config(config_path)),
  tar_target(paths, build_paths()),
  tar_target(raw_manifest, validate_raw_files(paths)),
  tar_target(raw_data_preflight, stop_if_required_files_missing(raw_manifest)),

  tar_target(raw_nss_2007_education, { raw_data_preflight; read_nss_2007_education(paths) }),
  tar_target(raw_nss_2007_consumption, { raw_data_preflight; read_nss_2007_consumption(paths) }),
  tar_target(raw_nss_2017_education, { raw_data_preflight; read_nss_2017_education(paths) }),
  tar_target(raw_census_2001, { raw_data_preflight; read_census_2001_mother_tongue(paths) }),
  tar_target(raw_boundaries_2020, { raw_data_preflight; read_district_boundaries_2020(paths) }),
  tar_target(raw_district_changes, { raw_data_preflight; read_district_change_sources(paths) }),
  tar_target(raw_ilo_figures, { raw_data_preflight; list_ilo_figure_paths(paths) }, format = "file"),

  tar_target(nss_2007_education, clean_nss_2007_education(raw_nss_2007_education)),
  tar_target(nss_2007_consumption, clean_nss_2007_consumption(raw_nss_2007_consumption)),
  tar_target(nss_2017_education, clean_nss_2017_education(raw_nss_2017_education)),
  tar_target(census_2001_languages, clean_census_2001_languages(raw_census_2001)),
  tar_target(boundaries_2020, clean_district_boundaries(raw_boundaries_2020)),

  tar_target(district_keys_2001, build_district_keys_2001(census_2001_languages)),
  tar_target(district_keys_2007, build_district_keys_2007(nss_2007_education, nss_2007_consumption)),
  tar_target(district_keys_2017, build_district_keys_2017(nss_2017_education)),
  tar_target(district_keys_2020, build_district_keys_2020(boundaries_2020)),
  tar_target(district_tracker_raw, build_district_tracker(raw_district_changes)),
  tar_target(district_harmonization_crosswalk_file, "data/metadata/district_harmonization_crosswalk.csv", format = "file"),
  tar_target(district_harmonization_crosswalk, read_district_harmonization_crosswalk(district_harmonization_crosswalk_file)),
  tar_target(district_tracker, apply_manual_district_corrections(district_tracker_raw)),
  tar_target(district_join_map, prepare_district_join_map(district_harmonization_crosswalk)),

  tar_target(selection_data, build_selection_data(nss_2007_education, district_keys_2007, cfg)),
  tar_target(selection_model, estimate_selection_probit(selection_data, cfg)),
  tar_target(ame_results, compute_average_marginal_effects(selection_model, selection_data, cfg)),

  tar_target(measures_2007, build_2007_measures(nss_2007_education, nss_2007_consumption, cfg)),
  tar_target(measures_2017, build_2017_measures(nss_2017_education, cfg)),
  tar_target(linguistic_distance_iv, build_linguistic_distance_iv(census_2001_languages, cfg)),
  tar_target(district_panel, build_district_panel(district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg)),
  tar_target(processed_district_panel_file, save_processed_district_panel(district_panel), format = "file"),

  tar_target(iv_formulas, build_iv_formulas(cfg)),
  tar_target(iv_models, estimate_2sls(district_panel, iv_formulas, cfg)),
  tar_target(first_stage_tests, estimate_first_stage(iv_models, district_panel, cfg)),
  tar_target(diag_public_weak_instruments, diagnose_weak_instruments(iv_models, district_panel, cfg)),
  tar_target(diag_public_overidentification, diagnose_overidentification(iv_models, iv_formulas, cfg)),

  tar_target(spatial_weights, build_spatial_weights(district_panel, cfg)),
  tar_target(diag_public_spatial_autocorrelation, diagnose_spatial_autocorrelation(district_panel, iv_models, spatial_weights, cfg)),
  tar_target(diag_public_spatial_autocorrelation_files, save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = "file"),
  tar_target(diag_public_multicollinearity, save_multicollinearity_diagnostics(diagnose_multicollinearity(district_panel, iv_models, cfg)), format = "file"),

  tar_target(figures, make_figures(district_panel, raw_ilo_figures, cfg, boundaries_2020)),
  tar_target(figure_files, save_figures(figures, cfg), format = "file"),
  tar_target(tables, make_tables(selection_data, ame_results, district_panel, iv_models, first_stage_tests, cfg, selection_model)),
  tar_target(diag_public_iv_panel, save_public_iv_panel_diagnostics(district_panel, tables), format = "file"),
  tar_target(table_files, save_tables(tables, cfg), format = "file"),
  tar_target(report_values, { diag_public_spatial_autocorrelation_files; build_report_values(ame_results, first_stage_tests, iv_models, selection_data, district_panel, diag_public_spatial_autocorrelation, cfg) }),
  tar_target(report_qmd, "paper/report.qmd", format = "file"),
  tar_target(district_matching_qmd, "docs/district-matching.qmd", format = "file"),
  tar_target(long_paths_qmd, "docs/long-paths-and-8-3-filenames.qmd", format = "file"),

  tar_target(district_matching_note, render_public_html(district_matching_qmd, dependencies = list(report_values)), format = "file"),
  tar_target(long_paths_note, render_public_html(long_paths_qmd), format = "file"),
  tar_target(report, render_report_pdf(report_qmd, report_values, figure_files, table_files), format = "file")
)

extended_diagnostic_targets <- list(
  tar_target(
    district_lineage_v2_specs,
    district_lineage_v2_input_specs(paths),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    district_lineage_v2_inventory,
    district_lineage_v2_source_inventory(district_lineage_v2_specs)
  ),
  tar_target(
    district_lineage_v2_source_specs,
    split_district_lineage_v2_source_specs(district_lineage_v2_specs),
    iteration = "list"
  ),
  tar_target(
    district_lineage_v2_source_file,
    district_lineage_v2_source_path(district_lineage_v2_source_specs),
    pattern = map(district_lineage_v2_source_specs),
    format = "file"
  ),
  tar_target(
    district_lineage_v2_source,
    read_district_lineage_v2_source(
      district_lineage_v2_source_specs,
      district_lineage_v2_source_file
    ),
    pattern = map(district_lineage_v2_source_specs, district_lineage_v2_source_file),
    iteration = "list"
  ),
  tar_target(
    district_lineage_v2_sources,
    assemble_district_lineage_v2_sources(district_lineage_v2_source)
  ),
  tar_target(
    diag_ext_district_lineage_v2,
    save_district_lineage_v2(build_district_lineage_v2(
      district_lineage_v2_sources,
      district_lineage_v2_inventory,
      district_tracker,
      census_2001_languages,
      measures_2007,
      measures_2017
    ))
  ),
  tar_target(diag_ext_missingness, save_missingness_diagnostics(diagnose_missingness(selection_data, cfg))),
  tar_target(diag_ext_district_tracker_sources, save_tracker_source_diagnostics(diagnose_district_tracker_sources(raw_district_changes, district_tracker, cfg))),
  tar_target(diag_ext_district_matching, save_district_matching_diagnostics(diagnose_district_matching(district_panel, district_join_map, cfg))),
  tar_target(diag_ext_fuzzy_matching, save_fuzzy_matching_diagnostics(diagnose_fuzzy_matching(district_tracker, district_join_map, cfg))),
  tar_target(diag_ext_spatial_weights, save_spatial_weight_diagnostics(diagnose_spatial_weights(district_panel, spatial_weights, cfg))),
  tar_target(diag_ext_instrument_exploration, save_instrument_exploration_diagnostics(diagnose_instrument_exploration(district_panel, cfg)))
)

benchmark_targets <- list(
  tar_target(bench_ame_methods, run_ame_methods_benchmark(selection_model, selection_data, cfg)),
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

if (extended_diagnostics_enabled()) {
  selected_targets <- c(selected_targets, extended_diagnostic_targets)
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
