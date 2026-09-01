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
source("R/pipeline/extended_historical_targets.R")
source("R/pipeline/extended_lineage_targets.R")
source("R/pipeline/extended_census_targets.R")
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
  list(


  tar_target(raw_nss_2007_education, { raw_data_preflight; read_nss_2007_education(paths) }),
  tar_target(raw_nss_2017_education, { raw_data_preflight; read_nss_2017_education(paths) }),
  tar_target(raw_census_2001, { raw_data_preflight; read_census_2001_mother_tongue(paths) }),
  tar_target(glottolog_5_3, { raw_data_preflight; read_glottolog_5_3(paths) }),
  tar_target(historical_linguistic_sources, { raw_data_preflight; read_historical_linguistic_sources(paths) }),
  tar_target(
    census_glottolog_crosswalk_file,
    path_metadata(paths, "census_language_glottolog_crosswalk.csv"),
    format = "file"
  ),
  tar_target(
    census_glottolog_crosswalk,
    read_census_language_glottolog_crosswalk(census_glottolog_crosswalk_file)
  ),
  tar_target(
    shastry_language_distance_file,
    path_metadata(paths, "shastry_language_distance.csv"),
    format = "file"
  ),
  tar_target(
    shastry_language_distance,
    read_shastry_language_distance(shastry_language_distance_file)
  ),
  tar_target(
    shastry_language_adjudications_file,
    path_metadata(paths, "shastry_language_adjudications.csv"),
    format = "file"
  ),
  tar_target(
    shastry_language_adjudications,
    read_shastry_language_adjudications(shastry_language_adjudications_file)
  ),
  tar_target(
    lexical_language_index_file,
    path_metadata(paths, "lexical_language_index.csv"),
    format = "file"
  ),
  tar_target(lexical_language_index, read_lexical_language_index(lexical_language_index_file)),
  tar_target(raw_ilo_figures, { raw_data_preflight; list_ilo_figure_paths(paths) }, format = "file"),
  tar_target(
    raw_price_sources,
    {
      raw_data_preflight
      read_price_sources(
        price_source_paths(paths),
        cpi_iw_estimation_start = consumption_price_window$start_period[[1L]]
      )
    }
  ),
  tar_target(raw_census_2001_controls, { raw_data_preflight; read_census_2001_control_sources(paths) }),

  tar_target(nss_2007_education, clean_nss_2007_education(raw_nss_2007_education)),
  tar_target(nss_2007_consumption, clean_nss_2007_consumption(raw_nss_2007_consumption)),
  tar_target(nss_2017_education, clean_nss_2017_education(raw_nss_2017_education)),
  tar_target(census_2001_languages, clean_census_2001_languages(raw_census_2001)),
  tar_target(
    consumption_price_window,
    registered_consumption_price_window(consumption_survey_registry)
  ),
  tar_target(
    temporal_price_series,
    build_temporal_price_series(raw_price_sources)
  ),
  tar_target(price_reference_index, build_ruc_reference_index(raw_price_sources)),
  tar_target(
    state_sector_price_deflators,
    validate_consumption_price_window(
      build_state_sector_price_deflators(
        temporal_price_series,
        reference_index = price_reference_index,
        start_period = consumption_price_window$start_period[[1L]],
        end_period = consumption_price_window$end_period[[1L]],
        require_complete_grid = FALSE
      ),
      consumption_price_window
    )
  ),

  tar_target(district_keys_2001, build_district_keys_2001(census_2001_languages)),
  tar_target(district_keys_2007, build_district_keys_2007(nss_2007_education, nss_2007_consumption)),
  tar_target(district_keys_2017, build_district_keys_2017(nss_2017_education)),

  tar_target(selection_data, build_selection_data(nss_2007_education, district_keys_2007, cfg)),
  tar_target(selection_model_data, project_selection_model_data(selection_data)),
  tar_target(selection_model, estimate_selection_probit(selection_model_data, cfg)),
  tar_target(ame_results, compute_average_marginal_effects(selection_model, cfg)),

  tar_target(
    consumption_households_2007,
    prepare_2007_consumption_households(
      nss_2007_education, state_sector_price_deflators, consumption_price_spec_2007_legacy
    )
  ),
  tar_target(
    consumption_households_2017,
    prepare_2017_consumption_households(
      nss_2017_education, state_sector_price_deflators, consumption_price_spec_2017_legacy
    )
  ),
  tar_target(
    measures_2007,
    build_2007_measures(
      nss_2007_education, nss_2007_consumption, cfg,
      consumption_households_2007, selection_data
    )
  ),
  tar_target(
    measures_2017,
    build_2017_measures(nss_2017_education, cfg, consumption_households_2017)
  ),
  tar_target(
    linguistic_distance_iv,
    build_linguistic_distance_iv(
      census_2001_languages,
      cfg,
      glottolog_5_3,
      census_glottolog_crosswalk,
      historical_linguistic_sources,
      shastry_adjudications = shastry_language_adjudications,
      shastry_concordance = shastry_language_distance,
      lexical_index = lexical_language_index
    )
  ),
  tar_target(
    lineage_geometry_2001_file,
    lineage_geometry_2001_path(paths),
    format = "file"
  ),
  tar_target(
    lineage_geometry_2001,
    read_lineage_geometry_2001(lineage_geometry_2001_file)
  ),
  tar_target(
    census_2001_district_totals,
    build_census_2001_district_totals(raw_census_2001_controls, lineage_geometry_2001)
  ),
  tar_target(
    census_2001_controls,
    {
      controls <- build_census_2001_controls(census_2001_district_totals)
      expected_keys <- clean_shrug_pca_2001_district(raw_census_2001_controls$shrug_pca)
      validate_census_2001_controls(
        controls,
        expected_keys = expected_keys[census_2001_keys()]
      )
      controls
    }
  ),
  tar_target(
    census_2001_source_coverage,
    summarise_census_2001_source_coverage(raw_census_2001_controls, lineage_geometry_2001)
  ),
  tar_target(
    district_lineage_specs,
    district_lineage_input_specs(paths),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    district_lineage_inventory,
    district_lineage_source_inventory(district_lineage_specs)
  ),
  tar_target(
    district_lineage_source_specs,
    split_district_lineage_source_specs(district_lineage_specs),
    iteration = "list"
  ),
  tar_target(
    district_lineage_source_file,
    district_lineage_source_path(district_lineage_source_specs),
    pattern = map(district_lineage_source_specs),
    format = "file"
  ),
  tar_target(
    district_lineage_source,
    read_district_lineage_source(
      district_lineage_source_specs,
      district_lineage_source_file
    ),
    pattern = map(district_lineage_source_specs, district_lineage_source_file),
    iteration = "list"
  ),
  tar_target(
    district_lineage_raw_sources,
    assemble_district_lineage_sources(district_lineage_source)
  ),
  tar_target(
    district_lineage_sources,
    attach_lineage_geometry_source(
      district_lineage_raw_sources,
      lineage_geometry_2001
    )
  ),
  tar_target(
    historical_linguistic_geography_1991_2001,
    build_historical_linguistic_geography_1991_2001(district_lineage_sources)
  ),
  tar_target(
    diag_ext_historical_linguistic_geography_1991_2001,
    save_historical_linguistic_geography_1991_2001(
      historical_linguistic_geography_1991_2001
    ),
    format = "file"
  ),
  tar_target(
    historical_vanneman_source_qa,
    {
      raw_data_preflight
      summarize_vanneman_historical_sources(paths)
    }
  ),
  tar_target(
    diag_ext_historical_vanneman_source_qa,
    save_vanneman_historical_source_qa(historical_vanneman_source_qa),
    format = "file"
  ),
  tar_target(
    historical_vanneman_panel4_geography,
    build_vanneman_panel4_geography_inventory(historical_vanneman_source_qa, paths)
  ),
  tar_target(
    diag_ext_historical_vanneman_panel4_geography,
    save_vanneman_panel4_geography_inventory(historical_vanneman_panel4_geography),
    format = "file"
  ),
  tar_target(
    historical_vanneman_panel4_dist91_crosswalk_seed,
    build_vanneman_panel4_dist91_crosswalk(
      historical_vanneman_source_qa,
      historical_vanneman_panel4_geography,
      paths
    )
  ),
  tar_target(
    historical_vanneman_panel4_dist91_adjudications_file,
    "data/metadata/vanneman_panel4_dist91_adjudications.csv",
    format = "file"
  ),
  tar_target(
    historical_vanneman_panel4_dist91_adjudications,
    {
      raw_data_preflight
      validate_vanneman_panel4_dist91_adjudications(
        read_vanneman_panel4_dist91_adjudications(historical_vanneman_panel4_dist91_adjudications_file),
        historical_vanneman_panel4_dist91_crosswalk_seed,
        paths
      )
    }
  ),
  tar_target(
    diag_ext_historical_vanneman_panel4_dist91_adjudication_evidence,
    save_vanneman_panel4_dist91_adjudication_evidence(
      historical_vanneman_panel4_dist91_adjudications
    ),
    format = "file"
  ),
  tar_target(
    historical_vanneman_panel4_dist91_crosswalk,
    apply_vanneman_panel4_dist91_adjudications(
      historical_vanneman_panel4_dist91_crosswalk_seed,
      historical_vanneman_panel4_dist91_adjudications
    )
  ),
  tar_target(
    diag_ext_historical_vanneman_panel4_dist91_crosswalk,
    save_vanneman_panel4_dist91_crosswalk(historical_vanneman_panel4_dist91_crosswalk),
    format = "file"
  ),
  tar_target(
    historical_vanneman_pretrend_geography,
    build_vanneman_pretrend_geography(
      historical_vanneman_panel4_dist91_crosswalk,
      historical_linguistic_geography_1991_2001$source_districts,
      historical_linguistic_geography_1991_2001$transition
    )
  ),
  tar_target(
    diag_ext_historical_vanneman_pretrend_geography,
    save_vanneman_pretrend_geography(historical_vanneman_pretrend_geography),
    format = "file"
  ),
  tar_target(
    historical_vanneman_pretrend_parent_bridge,
    build_vanneman_pretrend_parent_bridge(
      historical_vanneman_panel4_dist91_crosswalk,
      historical_linguistic_geography_1991_2001$source_districts,
      historical_linguistic_geography_1991_2001$transition
    )
  ),
  tar_target(
    diag_ext_historical_vanneman_pretrend_parent_bridge,
    save_vanneman_pretrend_parent_bridge(
      historical_vanneman_pretrend_parent_bridge
    ),
    format = "file"
  ),
  tar_target(
    historical_vanneman_liu_geography_benchmark,
    {
      raw_data_preflight
      build_vanneman_liu_geography_benchmark(historical_vanneman_panel4_geography, paths)
    }
  ),
  tar_target(
    diag_ext_historical_vanneman_liu_geography_benchmark,
    save_vanneman_liu_geography_benchmark(historical_vanneman_liu_geography_benchmark),
    format = "file"
  ),
  tar_target(
    district_lineage,
    build_district_lineage(
      district_lineage_sources,
      district_lineage_inventory,
      census_2001_languages,
      measures_2007,
      measures_2017
    )
  ),
  tar_target(
    district_transition_2001_2011,
    district_lineage$district_transition_2001_2011
  ),
  tar_target(
    historical_linguistic_geography_external_benchmark,
    build_historical_linguistic_geography_external_benchmark(
      historical_linguistic_geography_1991_2001,
      district_lineage_sources$kumar_somanathan_1991_2001,
      district_lineage$admin_units_2001
    )
  ),
  tar_target(
    diag_ext_historical_linguistic_geography_external_benchmark,
    save_historical_linguistic_geography_external_benchmark(
      historical_linguistic_geography_external_benchmark
    ),
    format = "file"
  ),
  tar_target(
    helms_lim_linguistic_distance_file,
    "data/metadata/helms_lim_linguistic_distance_1991.csv",
    format = "file"
  ),
  tar_target(
    helms_lim_linguistic_distance_1991,
    read_helms_lim_linguistic_distance_1991(
      helms_lim_linguistic_distance_file
    )
  ),
  tar_target(
    historical_linguistic_kumar_somanathan_geography,
    build_historical_linguistic_kumar_somanathan_geography(
      district_lineage_sources$kumar_somanathan_1991_2001,
      helms_lim_linguistic_distance_1991,
      district_lineage$admin_units_2001
    )
  ),
  tar_target(
    diag_ext_historical_linguistic_kumar_somanathan_geography,
    save_historical_linguistic_kumar_somanathan_geography(
      historical_linguistic_kumar_somanathan_geography
    ),
    format = "file"
  ),
  tar_target(
    historical_linguistic_exact_transition_comparison,
    build_historical_linguistic_exact_transition_comparison(
      historical_linguistic_geography_1991_2001,
      historical_linguistic_kumar_somanathan_geography
    )
  ),
  tar_target(
    diag_ext_historical_linguistic_exact_transition_comparison,
    save_historical_linguistic_exact_transition_comparison(
      historical_linguistic_exact_transition_comparison
    ),
    format = "file"
  ),
  tar_target(
    geography_allocation_semantics,
    {
      out <- geography_allocation_semantics_registry()
      validate_geography_allocation_semantics(out)
      out
    }
  ),
  tar_target(
    geography_measure_families,
    {
      out <- geography_measure_family_registry()
      validate_geography_measure_families(
        out, geography_allocation_semantics
      )
      out
    }
  ),
  tar_target(
    geography_specifications,
    {
      out <- geography_specification_registry()
      validate_geography_specifications(out)
      out
    }
  ),
  tar_target(
    multivintage_geography_1991_2001_2011,
    build_multivintage_geography_inventory(
      list(
        shrug_1991_2001 =
          historical_linguistic_geography_1991_2001$canonical_transition,
        production_2011_2001 =
          district_lineage$canonical_transition_2001_2011
      ),
      required_vintages = c(1991L, 2001L, 2011L)
    )
  ),
  tar_target(
    diag_ext_geography_harmonization_foundation,
    save_geography_harmonization_foundation(
      multivintage_geography_1991_2001_2011,
      geography_allocation_semantics,
      geography_measure_families,
      geography_specifications
    ),
    format = "file"
  ),
  tar_target(
    exact_multivintage_geography_1991_2001_2011,
    build_exact_multivintage_geography(
      list(
        shrug_1991_2001 =
          historical_linguistic_geography_1991_2001$canonical_transition,
        production_2011_2001 =
          district_lineage$canonical_transition_2001_2011
      ),
      required_vintages = c(1991L, 2001L, 2011L)
    )
  ),
  tar_target(
    diag_ext_exact_multivintage_geography,
    save_exact_multivintage_geography(
      exact_multivintage_geography_1991_2001_2011
    ),
    format = "file"
  ),
  tar_target(
    population_interpolation_geography_1991_2001_2011,
    build_population_interpolation_crosswalk(
      list(
        shrug_1991_2001 =
          historical_linguistic_geography_1991_2001$canonical_transition,
        production_2011_2001 =
          district_lineage$canonical_transition_2001_2011
      ),
      target_vintage = 2001L
    )
  ),
  tar_target(
    diag_ext_population_interpolation_geography,
    save_population_interpolation_geography(
      population_interpolation_geography_1991_2001_2011
    ),
    format = "file"
  ),
  tar_target(
    historical_linguistic_consensus_geography,
    build_historical_linguistic_consensus_geography(
      historical_linguistic_geography_1991_2001,
      historical_linguistic_kumar_somanathan_geography,
      historical_linguistic_exact_transition_comparison
    )
  ),
  tar_target(
    diag_ext_historical_linguistic_consensus_geography,
    save_historical_linguistic_consensus_geography(
      historical_linguistic_consensus_geography
    ),
    format = "file"
  )
  ),
  core_consumption_outcome_target_definitions(),
  list(
  tar_target(
    district_panel_conservative_provisional,
    build_lineage_district_panel(
      district_lineage$conservative_source_crosswalk,
      measures_2007,
      measures_2017,
      linguistic_distance_iv,
      lineage_geometry_2001,
      cfg
    )
  ),
  tar_target(
    conservative_gini_reconstruction,
    reconstruct_lineage_pooled_ginis(
      district_panel_conservative_provisional,
      district_lineage$conservative_source_crosswalk,
      nss_2007_education,
      nss_2017_education
    )
  ),
  tar_target(
    district_panel_primary_provisional,
    build_lineage_district_panel(
      district_lineage$primary_source_crosswalk,
      measures_2007,
      measures_2017,
      linguistic_distance_iv,
      lineage_geometry_2001,
      cfg
    )
  ),
  tar_target(
    primary_gini_reconstruction,
    reconstruct_lineage_pooled_ginis(
      district_panel_primary_provisional,
      district_lineage$primary_source_crosswalk,
      nss_2007_education,
      nss_2017_education
    )
  ),
  tar_target(
    district_panel_primary,
    finalize_analysis_panel(
      primary_gini_reconstruction$panel, census_2001_controls, cfg,
      control_registry = census_2001_control_registry
    )
  ),
  tar_target(
    district_panel_conservative,
    finalize_analysis_panel(
      conservative_gini_reconstruction$panel, census_2001_controls, cfg,
      control_registry = census_2001_control_registry
    )
  ),
  tar_target(district_panel, district_panel_primary),
  tar_target(processed_district_panel_file, save_processed_district_panel(district_panel), format = "file")
  ),
  core_consumption_iv_target_definitions(),
  list(

  tar_target(
    revised_iv_formulas,
    build_revised_iv_formulas(census_2001_control_registry)
  ),
  tar_target(revised_iv_models, estimate_2sls(district_panel, revised_iv_formulas, cfg)),
  tar_target(revised_first_stage_tests, estimate_first_stage(revised_iv_models, district_panel, cfg)),
  tar_target(diag_public_weak_instruments, diagnose_weak_instruments(revised_iv_models, district_panel, cfg)),
  tar_target(
    diag_public_anderson_rubin,
    save_candidate_anderson_rubin(
      diagnose_candidate_anderson_rubin(district_panel)
    ),
    format = "file"
  ),
  tar_target(diag_public_overidentification, diagnose_overidentification(revised_iv_models, revised_iv_formulas, cfg)),

  tar_target(spatial_weights, build_spatial_weights(district_panel, cfg)),
  tar_target(diag_public_spatial_autocorrelation, diagnose_spatial_autocorrelation(district_panel, revised_iv_models, spatial_weights, cfg)),
  tar_target(diag_public_spatial_autocorrelation_files, save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = "file"),
  tar_target(diag_public_multicollinearity, save_multicollinearity_diagnostics(diagnose_multicollinearity(district_panel, revised_iv_models, cfg)), format = "file"),

  tar_target(
    figures,
    make_figures(
      district_panel, raw_ilo_figures, cfg,
      iv_models = revised_iv_models,
      map_geometry = lineage_geometry_2001,
      consumption_iv_dynamics = consumption_iv_dynamics
    )
  ),
  tar_target(figure_files, save_figures(figures, cfg), format = "file"),
  tar_target(tables, make_tables(selection_data, ame_results, district_panel, revised_iv_models, revised_first_stage_tests, cfg, selection_model)),
  tar_target(diag_public_iv_panel, save_public_iv_panel_diagnostics(district_panel, tables), format = "file"),
  tar_target(table_files, save_tables(tables, cfg), format = "file"),
  tar_target(report_values, { diag_public_spatial_autocorrelation_files; build_report_values(ame_results, revised_first_stage_tests, revised_iv_models, selection_data, district_panel, diag_public_spatial_autocorrelation, cfg) }),
  tar_target(report_qmd, "paper/report.qmd", format = "file"),
  tar_target(poster_qmd, "posters/2026_predoc_conference/poster.qmd", format = "file"),
  tar_target(poster_assets, poster_required_assets(), format = "file"),
  tar_target(district_matching_qmd, "docs/district-matching.qmd", format = "file"),
  tar_target(long_paths_qmd, "docs/long-paths-and-8-3-filenames.qmd", format = "file"),

  tar_target(district_matching_note, render_public_html(district_matching_qmd, dependencies = list(report_values)), format = "file"),
  tar_target(long_paths_note, render_public_html(long_paths_qmd), format = "file"),
  tar_target(report, render_report_pdf(report_qmd, report_values, figure_files, table_files), format = "file"),
  tar_target(poster, render_poster_pdf(poster_qmd, figure_files, poster_assets, paths$root), format = "file")
  )
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
