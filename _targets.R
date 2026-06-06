library(targets)
library(tarchetypes)

source("R/packages.R")
source("R/config.R")
source("R/paths.R")

tar_source("R/io")
tar_source("R/clean")
tar_source("R/districts")
tar_source("R/measures")
tar_source("R/selection")
tar_source("R/iv")
tar_source("R/diagnostics")
tar_source("R/output")
tar_source("R/application_samples")

tar_option_set(
  packages = project_packages(),
  format = "rds",
  error = "abridge"
)

list(
  # Configuration and paths -----------------------------------------------------
  tar_target(config_path, Sys.getenv("EMI_CONFIG", "config/draft.yml"), cue = tar_cue(mode = "always")),
  tar_target(cfg, read_config(config_path)),
  tar_target(paths, build_paths()),
  tar_target(raw_manifest, validate_raw_files(paths)),
  tar_target(raw_data_preflight, stop_if_required_files_missing(raw_manifest)),

  # Raw inputs ------------------------------------------------------------------
  tar_target(raw_nss_2007_education, { raw_data_preflight; read_nss_2007_education(paths) }),
  tar_target(raw_nss_2007_consumption, { raw_data_preflight; read_nss_2007_consumption(paths) }),
  tar_target(raw_nss_2017_education, { raw_data_preflight; read_nss_2017_education(paths) }),
  tar_target(raw_census_2001, { raw_data_preflight; read_census_2001_mother_tongue(paths) }),
  tar_target(raw_boundaries_2020, { raw_data_preflight; read_district_boundaries_2020(paths) }),
  tar_target(raw_district_changes, { raw_data_preflight; read_district_change_sources(paths) }),
  tar_target(raw_ilo_figures, { raw_data_preflight; list_ilo_figure_paths(paths) }, format = "file"),

  # Clean inputs ----------------------------------------------------------------
  tar_target(nss_2007_education, clean_nss_2007_education(raw_nss_2007_education)),
  tar_target(nss_2007_consumption, clean_nss_2007_consumption(raw_nss_2007_consumption)),
  tar_target(nss_2017_education, clean_nss_2017_education(raw_nss_2017_education)),
  tar_target(census_2001_languages, clean_census_2001_languages(raw_census_2001)),
  tar_target(boundaries_2020, clean_district_boundaries(raw_boundaries_2020)),

  # District keys and tracker ---------------------------------------------------
  tar_target(district_keys_2001, build_district_keys_2001(census_2001_languages)),
  tar_target(district_keys_2007, build_district_keys_2007(nss_2007_education, nss_2007_consumption)),
  tar_target(district_keys_2017, build_district_keys_2017(nss_2017_education)),
  tar_target(district_keys_2020, build_district_keys_2020(boundaries_2020)),
  tar_target(district_tracker_raw, build_district_tracker(raw_district_changes)),
  tar_target(district_tracker, apply_manual_district_corrections(district_tracker_raw)),
  tar_target(district_join_map, fuzzy_join_districts(
    district_tracker,
    district_keys_2001,
    district_keys_2007,
    district_keys_2017,
    district_keys_2020,
    cfg
  )),

  # Selection model -------------------------------------------------------------
  tar_target(selection_data, build_selection_data(nss_2007_education, district_keys_2007, cfg)),
  tar_target(missingness_diagnostics, diagnose_missingness(selection_data, cfg)),
  tar_target(selection_model, estimate_selection_probit(selection_data, cfg)),
  tar_target(ame_results, compute_average_marginal_effects(selection_model, selection_data, cfg)),

  # District-level measures -----------------------------------------------------
  tar_target(measures_2007, build_2007_measures(nss_2007_education, nss_2007_consumption, selection_data, ame_results, cfg)),
  tar_target(measures_2017, build_2017_measures(nss_2017_education, cfg)),
  tar_target(linguistic_distance_iv, build_linguistic_distance_iv(census_2001_languages, cfg)),
  tar_target(district_panel, build_district_panel(
    district_tracker,
    district_join_map,
    measures_2007,
    measures_2017,
    linguistic_distance_iv,
    boundaries_2020,
    cfg
  )),
  tar_target(processed_district_tracker_file, save_processed_district_tracker(district_tracker), format = "file"),
  tar_target(processed_district_panel_file, save_processed_district_panel(district_panel), format = "file"),

  # IV/2SLS ---------------------------------------------------------------------
  tar_target(iv_formulas, build_iv_formulas(cfg)),
  tar_target(iv_models, estimate_2sls(district_panel, iv_formulas, cfg)),
  tar_target(first_stage_tests, estimate_first_stage(iv_models, district_panel, cfg)),
  tar_target(weak_iv_diagnostics, diagnose_weak_instruments(iv_models, district_panel, cfg)),
  tar_target(overid_diagnostics, diagnose_overidentification(iv_models, district_panel, cfg)),

  # Diagnostics -----------------------------------------------------------------
  tar_target(spatial_weights, build_spatial_weights(district_panel, cfg)),
  tar_target(diag_district_tracker_sources, diagnose_district_tracker_sources(raw_district_changes, district_tracker, cfg)),
  tar_target(diag_district_matching, diagnose_district_matching(district_panel, district_join_map, cfg)),
  tar_target(diag_fuzzy_matching, diagnose_fuzzy_matching(district_tracker, district_join_map, cfg)),
  tar_target(diag_ame_benchmark, diagnose_ame_benchmark(selection_model, selection_data, cfg)),
  tar_target(diag_spatial_weights, diagnose_spatial_weights(district_panel, spatial_weights, cfg)),
  tar_target(diag_spatial_autocorrelation, diagnose_spatial_autocorrelation(district_panel, iv_models, spatial_weights, cfg)),
  tar_target(diag_multicollinearity, diagnose_multicollinearity(district_panel, iv_models, cfg)),

  # Outputs ---------------------------------------------------------------------
  tar_target(figures, make_figures(district_panel, raw_ilo_figures, cfg)),
  tar_target(figure_files, save_figures(figures, cfg), format = "file"),
  tar_target(tables, make_tables(selection_data, ame_results, district_panel, iv_models, first_stage_tests, cfg)),
  tar_target(table_files, save_tables(tables, cfg), format = "file"),
  tar_target(report_values, build_report_values(
    ame_results,
    first_stage_tests,
    iv_models,
    selection_data,
    district_panel,
    diag_spatial_autocorrelation,
    cfg
  )),

  # Rendered notes, report, and samples ----------------------------------------
  tar_render(district_matching_note, "docs/district-matching.qmd"),
  tar_render(long_paths_note, "docs/long-paths-and-8-3-filenames.qmd"),
  tar_render(district_tracker_source_diagnostics, "analysis/diagnostics/district-tracker-source-diagnostics.qmd"),
  tar_render(ame_benchmark_note, "analysis/diagnostics/ame-benchmark.qmd"),
  tar_render(fuzzy_matching_note, "analysis/diagnostics/fuzzy-matching-diagnostics.qmd"),
  tar_render(spatial_autocorrelation_note, "analysis/diagnostics/spatial-autocorrelation-diagnostics.qmd"),
  tar_render(report, "paper/report.qmd"),
  tar_target(writing_sample_pdfs, render_writing_samples(output_files = c(figure_files, table_files)), format = "file"),
  tar_target(coding_sample_pdfs, render_coding_samples(), format = "file")
)
