# Historical validation targets required by final-paper Appendices B/C.
#
# This factory owns only the bounded historical construction, source-validation,
# persistence, first-stage, and 1991 balance objects consumed by publication
# exhibits. The strict Vanneman pretrend design is publication evidence; parent-
# bridge/geography sensitivities and other forensic variants remain extended.

core_historical_validation_target_definitions <- function() {
  list(
    tar_target(
      historical_linguistic_geography_1991_2001,
      build_historical_linguistic_geography_1991_2001(district_lineage_sources)
    ),
    tar_target(
      historical_vanneman_source_qa,
      {
        raw_data_preflight
        summarize_vanneman_historical_sources(paths)
      }
    ),
    tar_target(
      historical_vanneman_1991_control_statistics,
      {
        raw_data_preflight
        build_vanneman_1991_control_sufficient_statistics(
          vanneman_historical_paths(paths)[["dist91"]]
        )
      }
    ),
    tar_target(
      census_1991_download_manifest_file,
      path_metadata(paths, "census_1991_download_manifest.tsv"),
      format = "file"
    ),
    tar_target(
      census_1991_b01s_files,
      census_1991_validation_manifest_files(
        paths, "B01S", census_1991_download_manifest_file
      ),
      format = "file"
    ),
    tar_target(
      census_1991_c02t_files,
      census_1991_validation_manifest_files(
        paths, "C02T", census_1991_download_manifest_file
      ),
      format = "file"
    ),
    tar_target(
      census_1991_c02u_files,
      census_1991_validation_manifest_files(
        paths, "C02U", census_1991_download_manifest_file
      ),
      format = "file"
    ),
    tar_target(
      census_1991_c06t_files,
      census_1991_validation_manifest_files(
        paths, "C06T", census_1991_download_manifest_file
      ),
      format = "file"
    ),
    tar_target(
      census_1991_c09t_files,
      census_1991_validation_manifest_files(
        paths, "C09T", census_1991_download_manifest_file
      ),
      format = "file"
    ),
    tar_target(census_1991_b01s, read_census_1991_b01s(census_1991_b01s_files)),
    tar_target(census_1991_c02t, read_census_1991_c02t(census_1991_c02t_files)),
    tar_target(census_1991_c02u, read_census_1991_c02u(census_1991_c02u_files)),
    tar_target(census_1991_c06t, read_census_1991_c06t(census_1991_c06t_files)),
    tar_target(census_1991_c09t, read_census_1991_c09t(census_1991_c09t_files)),
    tar_target(
      census_1991_primary_validation,
      build_census_1991_primary_validation(
        census_1991_b01s,
        census_1991_c02t,
        census_1991_c02u,
        census_1991_c06t,
        census_1991_c09t,
        historical_vanneman_1991_control_statistics
      )
    ),
    tar_target(
      historical_vanneman_panel4_geography,
      build_vanneman_panel4_geography_inventory(historical_vanneman_source_qa, paths)
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
      historical_vanneman_panel4_dist91_crosswalk,
      apply_vanneman_panel4_dist91_adjudications(
        historical_vanneman_panel4_dist91_crosswalk_seed,
        historical_vanneman_panel4_dist91_adjudications
      )
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
      shrug_1991_baseline_files,
      shrug_1991_baseline_source_paths(paths),
      format = "file"
    ),
    tar_target(
      raw_shrug_1991_baseline,
      read_shrug_1991_baseline_sources(shrug_1991_baseline_files)
    ),
    tar_target(
      historical_baseline_1991,
      build_shrug_1991_baseline_controls(raw_shrug_1991_baseline)
    ),
    tar_target(
      language_atlas_1991_accepted_source_file,
      "data/metadata/language_atlas_1991_accepted_source.csv",
      format = "file"
    ),
    tar_target(
      language_atlas_1991_accepted_source,
      read_language_atlas_1991_accepted_source(language_atlas_1991_accepted_source_file)
    ),
    tar_target(
      historical_linguistic_distance_validation,
      build_historical_linguistic_distance_validation(
        language_atlas_1991_accepted_source,
        historical_linguistic_geography_1991_2001
      )
    ),
    tar_target(
      helms_lim_linguistic_distance_benchmark,
      build_helms_lim_linguistic_distance_benchmark(
        helms_lim_linguistic_distance_1991,
        historical_linguistic_distance_validation$preferred_distance,
        historical_linguistic_geography_1991_2001$source_districts,
        historical_vanneman_panel4_dist91_crosswalk
      )
    ),
    tar_target(
      historical_linguistic_persistence_validation,
      build_historical_linguistic_persistence_validation(
        historical_linguistic_distance_validation$preferred_distance,
        linguistic_distance_iv,
        historical_linguistic_geography_1991_2001
      )
    ),
    tar_target(
      historical_linguistic_first_stage_robustness,
      build_historical_linguistic_first_stage_robustness(
        historical_linguistic_distance_validation$preferred_distance,
        linguistic_distance_iv,
        historical_linguistic_geography_1991_2001,
        district_panel,
        baseline_1991 = historical_baseline_1991
      )
    ),
    tar_target(
      historical_baseline_balance_1991,
      build_historical_baseline_balance_1991(
        historical_baseline_1991,
        historical_linguistic_geography_1991_2001,
        district_panel,
        historical_distance = historical_linguistic_distance_validation$preferred_distance,
        external_historical_distance = helms_lim_linguistic_distance_1991
      )
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
      historical_baseline_g2_sensitivity,
      build_historical_baseline_g2_sensitivity(
        raw_shrug_1991_baseline$pca,
        population_interpolation_geography_1991_2001_2011$crosswalk,
        district_panel,
        coverage_thresholds = c(.90, .95, .99)
      )
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
      historical_vanneman_pretrend_levels,
      build_vanneman_pretrend_levels_from_sources(
        historical_vanneman_source_qa,
        historical_vanneman_pretrend_geography,
        paths
      )
    ),
    tar_target(
      historical_vanneman_pretrend_validation,
      build_vanneman_pretrend_validation(
        historical_vanneman_pretrend_levels,
        district_panel,
        historical_distance = historical_linguistic_distance_validation$preferred_distance,
        external_historical_distance = helms_lim_linguistic_distance_1991
      )
    )
  )
}
