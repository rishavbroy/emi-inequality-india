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
tar_source_r("R/prices")
tar_source_r("R/controls")
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
  tar_target(
    consumption_survey_registry_file,
    consumption_survey_registry_path(paths),
    format = "file"
  ),
  tar_target(
    consumption_survey_registry,
    read_consumption_survey_registry_file(consumption_survey_registry_file)
  ),
  tar_target(
    consumption_price_spec_2007_legacy,
    consumption_survey_spec(consumption_survey_registry, "nss_2007_08_education")
  ),
  tar_target(
    consumption_price_spec_2017_legacy,
    consumption_survey_spec(consumption_survey_registry, "nss_2017_18_education")
  ),
  tar_target(
    consumption_archive_2004_05,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2004_05"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2009_10_type1,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type1"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2009_10_type2,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type2"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2011_12_type2,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2011_12_type2"))
    },
    format = "file"
  ),
  tar_target(
    consumption_households_2004_05,
    read_registered_detailed_consumption(
      consumption_archive_2004_05, consumption_survey_spec(consumption_survey_registry, "nss_2004_05")
    )
  ),
  tar_target(
    consumption_households_2009_10_type1,
    read_registered_detailed_consumption(
      consumption_archive_2009_10_type1, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type1")
    )
  ),
  tar_target(
    consumption_households_2009_10_type2,
    read_registered_detailed_consumption(
      consumption_archive_2009_10_type2, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type2")
    )
  ),
  tar_target(
    consumption_households_2011_12_type2,
    read_registered_detailed_consumption(
      consumption_archive_2011_12_type2, consumption_survey_spec(consumption_survey_registry, "nss_2011_12_type2")
    )
  ),
  tar_target(
    consumption_mpce_benchmarks_file,
    path_project(paths, "data/metadata/consumption_mpce_benchmarks.csv"),
    format = "file"
  ),
  tar_target(
    consumption_mpce_benchmarks,
    read_consumption_mpce_benchmarks(consumption_mpce_benchmarks_file)
  ),
  tar_target(
    consumption_mpce_validation_2004_05,
    validate_consumption_mpce_reconstruction(
      consumption_households_2004_05, consumption_mpce_benchmarks, "nss_2004_05"
    )
  ),
  tar_target(
    consumption_mpce_validation_2009_10_type1,
    validate_consumption_mpce_reconstruction(
      consumption_households_2009_10_type1, consumption_mpce_benchmarks, "nss_2009_10_type1"
    )
  ),
  tar_target(
    consumption_mpce_validation_2009_10_type2,
    validate_consumption_mpce_reconstruction(
      consumption_households_2009_10_type2, consumption_mpce_benchmarks, "nss_2009_10_type2"
    )
  ),
  tar_target(
    consumption_mpce_validation_2011_12_type2,
    validate_consumption_mpce_reconstruction(
      consumption_households_2011_12_type2, consumption_mpce_benchmarks, "nss_2011_12_type2"
    )
  ),
  tar_target(
    consumption_mpce_validation,
    combine_consumption_mpce_validations(
      consumption_mpce_validation_2004_05,
      consumption_mpce_validation_2009_10_type1,
      consumption_mpce_validation_2009_10_type2,
      consumption_mpce_validation_2011_12_type2
    )
  ),
  tar_target(
    consumption_mpce_validation_file,
    save_consumption_mpce_validation(consumption_mpce_validation),
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_2004_05_file,
    {
      raw_data_preflight
      path_project(paths, "data/raw/hces/2004-05/District_code_list_nss61_round.xls")
    },
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_2009_10_file,
    {
      raw_data_preflight
      path_project(paths, "data/raw/hces/2009-10/District code_66.xls")
    },
    format = "file"
  ),
  tar_target(
    consumption_source_geography_special_units_file,
    path_project(paths, "data/metadata/consumption_source_geography_special_units.csv"),
    format = "file"
  ),
  tar_target(
    consumption_source_geography_special_units,
    read_consumption_source_geography_special_units(consumption_source_geography_special_units_file)
  ),
  tar_target(
    consumption_district_codebook_2004_05_base,
    read_consumption_district_codebook_excel(consumption_district_codebook_2004_05_file, "nss_2004_05")
  ),
  tar_target(
    consumption_district_codebook_2004_05,
    merge_consumption_codebook_special_units(
      consumption_district_codebook_2004_05_base,
      consumption_source_geography_special_units,
      "nss_2004_05"
    )
  ),
  tar_target(
    consumption_district_codebook_2009_10,
    read_consumption_district_codebook_excel(consumption_district_codebook_2009_10_file, "nss_2009_10")
  ),
  tar_target(
    consumption_district_codebook_2009_10_anomalies,
    consumption_codebook_name_anomalies(consumption_district_codebook_2009_10)
  ),
  tar_target(
    consumption_district_codebook_2011_12_file,
    {
      raw_data_preflight
      path_project(paths, "data/raw/hces/2011-12/DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012.xml")
    },
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_2011_12,
    read_consumption_district_codebook_ddi(consumption_district_codebook_2011_12_file, "nss_2011_12")
  ),
  tar_target(
    consumption_households_named_2004_05,
    attach_consumption_source_district_identity(consumption_households_2004_05, consumption_district_codebook_2004_05)
  ),
  tar_target(
    consumption_households_named_2009_10_type1,
    attach_consumption_source_district_identity(consumption_households_2009_10_type1, consumption_district_codebook_2009_10)
  ),
  tar_target(
    consumption_households_named_2009_10_type2,
    attach_consumption_source_district_identity(consumption_households_2009_10_type2, consumption_district_codebook_2009_10)
  ),
  tar_target(
    consumption_households_named_2011_12_type2,
    attach_consumption_source_district_identity(consumption_households_2011_12_type2, consumption_district_codebook_2011_12)
  ),

  tar_target(
    consumption_households_real_2004_05,
    {
      consumption_mpce_validation_2004_05
      deflate_detailed_consumption_households(
        consumption_households_named_2004_05, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2004_05")
      )
    }
  ),
  tar_target(
    consumption_households_real_2009_10_type1,
    {
      consumption_mpce_validation_2009_10_type1
      deflate_detailed_consumption_households(
        consumption_households_named_2009_10_type1, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type1")
      )
    }
  ),
  tar_target(
    consumption_households_real_2009_10_type2,
    {
      consumption_mpce_validation_2009_10_type2
      deflate_detailed_consumption_households(
        consumption_households_named_2009_10_type2, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type2")
      )
    }
  ),
  tar_target(
    consumption_households_real_2011_12_type2,
    {
      consumption_mpce_validation_2011_12_type2
      deflate_detailed_consumption_households(
        consumption_households_named_2011_12_type2, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2011_12_type2")
      )
    }
  ),

  tar_target(raw_nss_2007_education, { raw_data_preflight; read_nss_2007_education(paths) }),
  tar_target(raw_nss_2007_consumption, { raw_data_preflight; read_nss_2007_consumption(paths) }),
  tar_target(raw_nss_2017_education, { raw_data_preflight; read_nss_2017_education(paths) }),
  tar_target(raw_census_2001, { raw_data_preflight; read_census_2001_mother_tongue(paths) }),
  tar_target(glottolog_5_3, { raw_data_preflight; read_glottolog_5_3(paths) }),
  tar_target(historical_linguistic_sources, { raw_data_preflight; read_historical_linguistic_sources(paths) }),
  tar_target(census_glottolog_crosswalk, read_census_language_glottolog_crosswalk()),
  tar_target(shastry_language_adjudications, read_shastry_language_adjudications()),
  tar_target(raw_ilo_figures, { raw_data_preflight; list_ilo_figure_paths(paths) }, format = "file"),
  tar_target(raw_price_sources, { raw_data_preflight; read_price_sources(price_source_paths(paths)) }),
  tar_target(raw_census_2001_controls, { raw_data_preflight; read_census_2001_control_sources(paths) }),

  tar_target(nss_2007_education, clean_nss_2007_education(raw_nss_2007_education)),
  tar_target(nss_2007_consumption, clean_nss_2007_consumption(raw_nss_2007_consumption)),
  tar_target(nss_2017_education, clean_nss_2017_education(raw_nss_2017_education)),
  tar_target(census_2001_languages, clean_census_2001_languages(raw_census_2001)),
  tar_target(
    temporal_price_series,
    build_temporal_price_series(
      raw_price_sources,
      pre_switch_start = as.Date("2004-07-01"),
      pre_switch_end = as.Date("2012-12-01")
    )
  ),
  tar_target(price_reference_index, build_ruc_reference_index(raw_price_sources)),
  tar_target(
    state_sector_price_deflators,
    build_state_sector_price_deflators(
      temporal_price_series,
      reference_index = price_reference_index,
      start_period = as.Date("2004-07-01"),
      end_period = as.Date("2018-06-01")
    )
  ),

  tar_target(district_keys_2001, build_district_keys_2001(census_2001_languages)),
  tar_target(district_keys_2007, build_district_keys_2007(nss_2007_education, nss_2007_consumption)),
  tar_target(district_keys_2017, build_district_keys_2017(nss_2017_education)),

  tar_target(selection_data, build_selection_data(nss_2007_education, district_keys_2007, cfg)),
  tar_target(selection_model, estimate_selection_probit(selection_data, cfg)),
  tar_target(ame_results, compute_average_marginal_effects(selection_model, selection_data, cfg)),

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
      shastry_language_adjudications
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
    consumption_lineage_identity_aliases_file,
    "data/metadata/consumption_lineage_identity_aliases.csv",
    format = "file"
  ),
  tar_target(
    consumption_lineage_identity_aliases,
    read_consumption_lineage_identity_aliases(consumption_lineage_identity_aliases_file)
  ),
  tar_target(
    consumption_lineage_reference,
    build_consumption_lineage_reference(
      district_lineage$admin_units_2001,
      district_lineage$nss_source_roster,
      district_lineage$full_reviewed_source_crosswalk,
      consumption_lineage_identity_aliases
    )
  ),
  tar_target(
    consumption_lineage_bridge_2004_05,
    build_consumption_lineage_bridge(
      consumption_households_real_2004_05, consumption_lineage_reference
    )
  ),
  tar_target(
    consumption_lineage_bridge_2009_10_type1,
    build_consumption_lineage_bridge(
      consumption_households_real_2009_10_type1, consumption_lineage_reference
    )
  ),
  tar_target(
    consumption_lineage_bridge_2009_10_type2,
    build_consumption_lineage_bridge(
      consumption_households_real_2009_10_type2, consumption_lineage_reference
    )
  ),
  tar_target(
    consumption_lineage_bridge_2011_12_type2,
    build_consumption_lineage_bridge(
      consumption_households_real_2011_12_type2, consumption_lineage_reference
    )
  ),
  tar_target(
    consumption_households_lineaged_2004_05,
    attach_consumption_lineage(
      consumption_households_real_2004_05, consumption_lineage_bridge_2004_05
    )
  ),
  tar_target(
    consumption_households_lineaged_2009_10_type1,
    attach_consumption_lineage(
      consumption_households_real_2009_10_type1, consumption_lineage_bridge_2009_10_type1
    )
  ),
  tar_target(
    consumption_households_lineaged_2009_10_type2,
    attach_consumption_lineage(
      consumption_households_real_2009_10_type2, consumption_lineage_bridge_2009_10_type2
    )
  ),
  tar_target(
    consumption_households_lineaged_2011_12_type2,
    attach_consumption_lineage(
      consumption_households_real_2011_12_type2, consumption_lineage_bridge_2011_12_type2
    )
  ),
  tar_target(
    consumption_lineage_coverage,
    safe_bind_rows(list(
      summarize_consumption_lineage_coverage(consumption_households_lineaged_2004_05),
      summarize_consumption_lineage_coverage(consumption_households_lineaged_2009_10_type1),
      summarize_consumption_lineage_coverage(consumption_households_lineaged_2009_10_type2),
      summarize_consumption_lineage_coverage(consumption_households_lineaged_2011_12_type2)
    ))
  ),
  tar_target(
    consumption_lineage_coverage_file,
    save_consumption_lineage_coverage(consumption_lineage_coverage),
    format = "file"
  ),
  tar_target(
    consumption_lineage_review_queue,
    safe_bind_rows(list(
      build_consumption_lineage_review_queue(consumption_lineage_bridge_2004_05),
      build_consumption_lineage_review_queue(consumption_lineage_bridge_2009_10_type1),
      build_consumption_lineage_review_queue(consumption_lineage_bridge_2009_10_type2),
      build_consumption_lineage_review_queue(consumption_lineage_bridge_2011_12_type2)
    ))
  ),
  tar_target(
    consumption_lineage_review_queue_file,
    save_consumption_lineage_review_queue(consumption_lineage_review_queue),
    format = "file"
  ),
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
      primary_gini_reconstruction$panel, census_2001_controls, cfg
    )
  ),
  tar_target(
    district_panel_conservative,
    finalize_analysis_panel(
      conservative_gini_reconstruction$panel, census_2001_controls, cfg
    )
  ),
  tar_target(district_panel, district_panel_primary),
  tar_target(processed_district_panel_file, save_processed_district_panel(district_panel), format = "file"),

  tar_target(revised_iv_formulas, build_revised_iv_formulas()),
  tar_target(revised_iv_models, estimate_2sls(district_panel, revised_iv_formulas, cfg)),
  tar_target(revised_first_stage_tests, estimate_first_stage(revised_iv_models, district_panel, cfg)),
  tar_target(diag_public_weak_instruments, diagnose_weak_instruments(revised_iv_models, district_panel, cfg)),
  tar_target(
    diag_public_anderson_rubin,
    save_preferred_anderson_rubin(diagnose_preferred_anderson_rubin(district_panel)),
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
      map_geometry = lineage_geometry_2001
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

extended_diagnostic_targets <- list(
  tar_target(
    diag_ext_district_lineage,
    {
      district_panel_primary
      district_panel_full_reviewed
      revised_iv_models
      iv_models_full_reviewed
      revised_first_stage_tests
      first_stage_tests_full_reviewed
      diag_ext_lineage_panel_variants
      save_district_lineage(district_lineage)
    }
  ),
  tar_target(
    district_panel_full_reviewed_provisional,
    build_lineage_district_panel(
      district_lineage$full_reviewed_source_crosswalk,
      measures_2007,
      measures_2017,
      linguistic_distance_iv,
      lineage_geometry_2001,
      cfg
    )
  ),
  tar_target(
    full_reviewed_gini_reconstruction,
    reconstruct_lineage_pooled_ginis(
      district_panel_full_reviewed_provisional,
      district_lineage$full_reviewed_source_crosswalk,
      nss_2007_education,
      nss_2017_education
    )
  ),
  tar_target(
    district_panel_full_reviewed,
    attach_census_2001_controls(
      full_reviewed_gini_reconstruction$panel,
      census_2001_controls
    )
  ),
  tar_target(
    iv_models_conservative,
    estimate_2sls(district_panel_conservative, revised_iv_formulas, cfg)
  ),
  tar_target(
    first_stage_tests_conservative,
    estimate_first_stage(iv_models_conservative, district_panel_conservative, cfg)
  ),
  tar_target(
    iv_models_full_reviewed,
    estimate_2sls(district_panel_full_reviewed, revised_iv_formulas, cfg)
  ),
  tar_target(
    first_stage_tests_full_reviewed,
    estimate_first_stage(
      iv_models_full_reviewed,
      district_panel_full_reviewed,
      cfg
    )
  ),
  tar_target(
    lineage_panel_variant_review,
    build_lineage_panel_variant_review(
      panels = list(
        conservative = district_panel_conservative,
        primary = district_panel_primary,
        full_reviewed = district_panel_full_reviewed
      ),
      models = list(
        conservative = iv_models_conservative,
        primary = revised_iv_models,
        full_reviewed = iv_models_full_reviewed
      ),
      first_stage_tests = list(
        conservative = first_stage_tests_conservative,
        primary = revised_first_stage_tests,
        full_reviewed = first_stage_tests_full_reviewed
      ),
      gini_audits = list(
        conservative = conservative_gini_reconstruction$audit,
        primary = primary_gini_reconstruction$audit,
        full_reviewed = full_reviewed_gini_reconstruction$audit
      )
    )
  ),
  tar_target(
    diag_ext_lineage_panel_variants,
    save_lineage_panel_variant_review(lineage_panel_variant_review)
  ),
  tar_target(legacy_iv_formulas, build_legacy_iv_formulas()),
  tar_target(
    iv_models_legacy,
    estimate_2sls(district_panel_legacy, legacy_iv_formulas, cfg)
  ),
  tar_target(
    iv_models_conservative_legacy_spec,
    estimate_2sls(district_panel_conservative, legacy_iv_formulas, cfg)
  ),
  tar_target(
    first_stage_tests_legacy,
    estimate_first_stage(iv_models_legacy, district_panel_legacy, cfg)
  ),
  tar_target(
    first_stage_tests_conservative_legacy_spec,
    estimate_first_stage(
      iv_models_conservative_legacy_spec,
      district_panel_conservative,
      cfg
    )
  ),
  tar_target(
    lineage_shared_support,
    build_lineage_shared_support(
      district_panel_legacy,
      district_panel_conservative
    )
  ),
  tar_target(
    iv_models_legacy_shared,
    estimate_2sls(
      lineage_shared_support$legacy,
      legacy_iv_formulas,
      cfg
    )
  ),
  tar_target(
    first_stage_tests_legacy_shared,
    estimate_first_stage(
      iv_models_legacy_shared,
      lineage_shared_support$legacy,
      cfg
    )
  ),
  tar_target(
    iv_models_lineage_shared,
    estimate_2sls(
      lineage_shared_support$lineage,
      legacy_iv_formulas,
      cfg
    )
  ),
  tar_target(
    first_stage_tests_lineage_shared,
    estimate_first_stage(
      iv_models_lineage_shared,
      lineage_shared_support$lineage,
      cfg
    )
  ),
  tar_target(
    lineage_downstream_review,
    build_lineage_downstream_review(
      district_panel_legacy,
      district_panel_conservative,
      iv_models_legacy,
      iv_models_conservative_legacy_spec,
      first_stage_tests_legacy,
      first_stage_tests_conservative_legacy_spec,
      district_lineage$full_reviewed_source_crosswalk,
      district_lineage$conservative_mapping_eligibility,
      iv_models_legacy_shared,
      iv_models_lineage_shared,
      first_stage_tests_legacy_shared,
      first_stage_tests_lineage_shared,
      lineage_shared_support$legacy,
      lineage_shared_support$lineage,
      district_lineage$admin_units_2001,
      district_lineage$adjudicated_allocation_weights,
      conservative_gini_reconstruction$audit
    )
  ),
  tar_target(
    diag_ext_lineage_downstream,
    save_lineage_downstream_review(lineage_downstream_review)
  ),
  tar_target(diag_ext_missingness, save_missingness_diagnostics(diagnose_missingness(selection_data, cfg))),
  tar_target(diag_ext_district_tracker_sources, save_tracker_source_diagnostics(diagnose_district_tracker_sources(raw_district_changes, district_tracker, cfg))),
  tar_target(diag_ext_district_matching, save_district_matching_diagnostics(diagnose_district_matching(district_panel, district_join_map, cfg))),
  tar_target(diag_ext_fuzzy_matching, save_fuzzy_matching_diagnostics(diagnose_fuzzy_matching(district_tracker, district_join_map, cfg))),
  tar_target(diag_ext_spatial_weights, save_spatial_weight_diagnostics(diagnose_spatial_weights(district_panel, spatial_weights, cfg))),
  tar_target(diag_ext_instrument_exploration, save_instrument_exploration_diagnostics(diagnose_instrument_exploration(district_panel, cfg))),
  tar_target(first_stage_absorption_diagnostics, diagnose_first_stage_absorption(district_panel)),
  tar_target(diag_ext_first_stage_absorption, save_first_stage_absorption_diagnostics(first_stage_absorption_diagnostics)),
  tar_target(
    glottolog_cldf_5_3,
    read_glottolog_cldf_5_3(glottolog_5_3$cldf_zip)
  ),
  tar_target(
    census_glottolog_match_candidates,
    build_census_glottolog_match_candidates(
      census_2001_languages, glottolog_5_3, glottolog_cldf_5_3,
      reviews = census_glottolog_crosswalk
    )
  ),
  tar_target(
    glottolog_language_distances,
    glottolog_language_distance_table(census_glottolog_crosswalk, glottolog_5_3)
  ),
  tar_target(
    shastry_extension_candidates,
    build_shastry_extension_candidates(
      census_glottolog_crosswalk,
      glottolog_5_3,
      historical_linguistic_sources,
      adjudications = shastry_language_adjudications
    )
  ),
  tar_target(
    diag_ext_dyen_hindi_cognates,
    save_dyen_hindi_cognates(historical_linguistic_sources$dyen_hindi),
    format = "file"
  ),
  tar_target(
    diag_ext_asjp_review_anchor_distances,
    save_asjp_review_anchor_distances(
      historical_linguistic_sources$asjp_review_anchor_distances
    ),
    format = "file"
  ),
  tar_target(
    diag_ext_asjp_review_summary,
    save_asjp_review_summary(historical_linguistic_sources$asjp_review_summary),
    format = "file"
  ),
  tar_target(
    diag_ext_shastry_extension_candidates,
    save_shastry_extension_candidates(shastry_extension_candidates),
    format = "file"
  ),
  tar_target(
    diag_ext_glottolog_language_distances,
    save_glottolog_language_distance_table(glottolog_language_distances),
    format = "file"
  ),
  tar_target(
    diag_ext_census_glottolog_match_candidates,
    save_census_glottolog_match_candidates(census_glottolog_match_candidates),
    format = "file"
  ),
  tar_target(
    census_2001_c13_manifest_file,
    path_metadata(paths, "census_2001_download_manifest.tsv"),
    format = "file"
  ),
  tar_target(
    census_2011_c13_manifest_file,
    path_metadata(paths, "census_2011_download_manifest.tsv"),
    format = "file"
  ),
  tar_target(
    census_2001_c13_files,
    census_c13_manifest_files(paths, 2001, census_2001_c13_manifest_file),
    format = "file"
  ),
  tar_target(
    census_2011_c13_files,
    census_c13_manifest_files(paths, 2011, census_2011_c13_manifest_file),
    format = "file"
  ),
  tar_target(
    census_age_6_13_2001,
    read_census_c13_age_6_13(census_2001_c13_files, 2001)
  ),
  tar_target(
    census_age_6_13_2011,
    read_census_c13_age_6_13(census_2011_c13_files, 2011)
  ),
  tar_target(
    dise_archive_registry_file,
    path_metadata(paths, "dise_archive_registry.csv"),
    format = "file"
  ),
  tar_target(
    dise_medium_slot_crosswalk_file,
    path_metadata(paths, "dise_medium_slot_crosswalk.csv"),
    format = "file"
  ),
  tar_target(
    dise_publication_checks_file,
    path_metadata(paths, "dise_publication_checks.csv"),
    format = "file"
  ),
  tar_target(
    dise_report_language_enrollment_file,
    path_metadata(paths, "dise_report_language_enrollment.csv"),
    format = "file"
  ),
  tar_target(
    dise_report_total_enrollment_2010_file,
    path_metadata(paths, "dise_report_total_enrollment_2010_11.csv"),
    format = "file"
  ),
  tar_target(
    dise_report_school_quality_file,
    path_metadata(paths, "dise_report_school_quality_2011_15.csv"),
    format = "file"
  ),
  tar_target(
    dise_archive_registry,
    read_dise_archive_registry(paths, dise_archive_registry_file)
  ),
  tar_target(
    dise_medium_slot_crosswalk,
    read_dise_medium_slot_crosswalk(paths, dise_medium_slot_crosswalk_file)
  ),
  tar_target(
    dise_publication_checks,
    read_dise_publication_checks(paths, dise_publication_checks_file)
  ),
  tar_target(
    dise_report_language_enrollment,
    read_dise_report_language_enrollment(paths, dise_report_language_enrollment_file)
  ),
  tar_target(
    dise_report_total_enrollment_2010,
    read_dise_report_total_enrollment_2010(
      paths,
      dise_report_total_enrollment_2010_file
    )
  ),
  tar_target(
    dise_report_school_quality,
    read_dise_report_school_quality(
      paths,
      dise_report_school_quality_file
    )
  ),
  tar_target(raw_dise_baseline, read_dise_baseline_archive(paths, dise_archive_registry)),
  tar_target(
    raw_dise_baseline_teachers,
    read_dise_baseline_teacher_archive(paths, dise_archive_registry)
  ),
  tar_target(
    raw_dise_dynamic,
    read_dise_dynamic_archive(
      paths,
      dise_archive_registry,
      dise_report_language_enrollment,
      dise_report_total_enrollment_2010
    )
  ),
  tar_target(
    dise_baseline_district_year,
    {
      baseline <- attach_dise_medium_identities(raw_dise_baseline, dise_medium_slot_crosswalk)
      baseline <- merge(
        baseline,
        raw_dise_baseline_teachers,
        by = c("academic_year", "district_code_dise"),
        all.x = TRUE,
        sort = FALSE
      )
      finalize_dise_school_quality_measures(baseline)
    }
  ),
  tar_target(
    dise_all_district_year,
    safe_bind_rows(list(dise_baseline_district_year, raw_dise_dynamic))
  ),
  tar_target(
    dise_lineage_bridge,
    build_dise_deterministic_lineage_bridge(
      dise_all_district_year,
      district_lineage$nss_source_roster,
      district_lineage$full_reviewed_source_crosswalk,
      district_lineage$admin_units_2001
    )
  ),
  tar_target(
    dise_report_school_quality_2001,
    harmonize_dise_report_school_quality_to_2001(
      dise_report_school_quality,
      dise_lineage_bridge
    )
  ),
  tar_target(
    census_age_6_13_anchors,
    build_census_age_6_13_anchors(
      census_age_6_13_2001,
      census_age_6_13_2011,
      district_lineage$district_transition_2001_2011
    )
  ),
  tar_target(
    census_age_6_13_population,
    project_census_age_6_13_population(
      census_age_6_13_anchors,
      unique(dise_all_district_year$academic_year)
    )
  ),
  tar_target(
    dise_baseline_district_year_2001,
    attach_dise_age_6_13_exposure(
      harmonize_dise_counts_to_2001(dise_baseline_district_year, dise_lineage_bridge),
      census_age_6_13_population
    )
  ),
  tar_target(
    dise_baseline_treatments,
    build_dise_baseline_treatments_2001(dise_baseline_district_year_2001)
  ),
  tar_target(
    district_panel_with_dise,
    attach_dise_treatments_to_panel_2001(district_panel, dise_baseline_treatments)
  ),
  tar_target(
    dise_dynamic_panel,
    attach_dise_age_6_13_exposure(
      build_dise_longitudinal_panel(
        dise_baseline_district_year,
        raw_dise_dynamic,
        district_lineage$nss_source_roster,
        district_lineage$full_reviewed_source_crosswalk,
        district_lineage$admin_units_2001,
        district_panel
      ),
      census_age_6_13_population
    )
  ),
  tar_target(
    dise_dynamic_relevance,
    diagnose_dise_dynamic_relevance(dise_dynamic_panel)
  ),
  tar_target(
    dise_elementary_age_dynamic_relevance,
    diagnose_dise_dynamic_relevance(
      dise_dynamic_panel,
      outcome = "dise_emi_gross_enrollment_ratio_age_6_13"
    )
  ),
  tar_target(
    dise_school_quality_mechanisms,
    diagnose_dise_school_quality_mechanisms(
      dise_dynamic_panel,
      dise_report_school_quality_2001
    )
  ),
  tar_target(
    dise_archive_diagnostics,
    diagnose_dise_archive(
      dise_baseline_district_year, dise_baseline_treatments, dise_publication_checks
    )
  ),
  tar_target(dise_iv_construct_registry, dise_construct_registry(), iteration = "list"),
  tar_target(
    dise_iv_analysis_panel,
    prepare_dise_iv_diagnostic_panel(
      district_panel_with_dise,
      dise_iv_construct_registry
    )
  ),
  tar_target(
    dise_iv_construct,
    split(dise_iv_construct_registry, seq_len(nrow(dise_iv_construct_registry))),
    iteration = "list"
  ),
  tar_target(
    dise_iv_construct_diagnostic,
    diagnose_dise_iv_construct(dise_iv_analysis_panel, dise_iv_construct),
    pattern = map(dise_iv_construct),
    iteration = "list"
  ),
  tar_target(dise_iv_nss_validation, diagnose_dise_nss_validation(dise_iv_analysis_panel)),
  tar_target(
    dise_iv_permutations,
    assemble_dise_iv_permutations(
      dise_iv_construct_registry,
      dise_iv_nss_validation,
      dise_iv_construct_diagnostic
    )
  ),
  tar_target(
    diag_ext_dise,
    save_dise_diagnostics(
      dise_archive_diagnostics,
      dise_iv_permutations,
      dise_baseline_district_year,
      dise_baseline_treatments,
      lineage_bridge = dise_lineage_bridge,
      harmonized_district_year = dise_baseline_district_year_2001,
      dynamic_panel = dise_dynamic_panel,
      dynamic_relevance = dise_dynamic_relevance,
      school_quality = dise_school_quality_mechanisms,
      age_exposure = list(
        anchors = census_age_6_13_anchors,
        population = census_age_6_13_population,
        dynamic_relevance = dise_elementary_age_dynamic_relevance
      )
    )
  ),

  tar_target(
    alternative_distance_analysis_panel,
    prepare_alternative_distance_panel(district_panel)
  ),
  tar_target(
    alternative_distance_augmentation_panel,
    project_alternative_distance_panel(
      district_panel,
      retain = "real_log_consumption_change"
    )
  ),
  tar_target(
    alternative_distance_spec_registry,
    alternative_distance_registry(),
    iteration = "list"
  ),
  tar_target(
    alternative_distance_specification,
    split(
      alternative_distance_spec_registry,
      seq_len(nrow(alternative_distance_spec_registry))
    ),
    iteration = "list"
  ),
  tar_target(
    alternative_distance_spec_diagnostic,
    diagnose_alternative_distance_specification(
      alternative_distance_analysis_panel,
      alternative_distance_specification
    ),
    pattern = map(alternative_distance_specification),
    iteration = "list"
  ),
  tar_target(
    alternative_distance_first_stage_base,
    assemble_alternative_distance_first_stages(
      alternative_distance_analysis_panel,
      alternative_distance_spec_registry,
      alternative_distance_spec_diagnostic
    )
  ),
  tar_target(
    alternative_distance_first_stages,
    augment_alternative_distance_diagnostics(
      alternative_distance_first_stage_base,
      alternative_distance_augmentation_panel,
      census_2001_languages,
      glottolog = glottolog_5_3,
      glottolog_crosswalk = census_glottolog_crosswalk
    )
  ),
  tar_target(diag_ext_alternative_distance_first_stages, save_alternative_distance_first_stages(alternative_distance_first_stages)),
  tar_target(
    census_2001_control_diagnostics,
    diagnose_census_2001_controls(
      district_panel, revised_iv_models, revised_first_stage_tests, census_2001_source_coverage
    )
  ),
  tar_target(
    diag_ext_census_2001_controls,
    save_census_2001_control_diagnostics(census_2001_control_diagnostics),
    format = "file"
  ),
  tar_target(
    consumption_outcome_comparison,
    compare_consumption_outcomes(district_panel, cfg)
  ),
  tar_target(
    diag_ext_consumption_prices,
    save_consumption_price_diagnostics(
      consumption_outcome_comparison,
      consumption_households_2007,
      consumption_households_2017,
      district_panel
    ),
    format = "file"
  )
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
