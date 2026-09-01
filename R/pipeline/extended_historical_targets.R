# Historical extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_historical_target_definitions <- function() {
  list(
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
      diag_ext_helms_lim_linguistic_distance_benchmark,
      save_helms_lim_linguistic_distance_benchmark(helms_lim_linguistic_distance_benchmark),
      format = "file"
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
      diag_ext_historical_baseline_balance_1991,
      save_historical_baseline_balance_1991(historical_baseline_balance_1991),
      format = "file"
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
      diag_ext_historical_baseline_g2_sensitivity,
      save_historical_baseline_g2_sensitivity(
        historical_baseline_g2_sensitivity
      ),
      format = "file"
    ),
    tar_target(
      historical_baseline_geography_comparison,
      build_historical_baseline_geography_comparison(
        historical_baseline_balance_1991,
        historical_baseline_g2_sensitivity
      )
    ),
    tar_target(
      diag_ext_historical_baseline_geography_comparison,
      save_historical_baseline_geography_comparison(
        historical_baseline_geography_comparison
      ),
      format = "file"
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
    ),
    tar_target(
      diag_ext_historical_vanneman_pretrend_validation,
      save_vanneman_pretrend_validation(historical_vanneman_pretrend_validation),
      format = "file"
    ),
    tar_target(
      historical_vanneman_parent_pretrend_validation,
      build_vanneman_parent_pretrend_validation(
        historical_vanneman_pretrend_levels,
        historical_vanneman_pretrend_parent_bridge,
        district_panel,
        historical_distance = historical_linguistic_distance_validation$preferred_distance,
        external_historical_distance = helms_lim_linguistic_distance_1991
      )
    ),
    tar_target(
      historical_vanneman_harmonized_membership,
      build_vanneman_harmonized_membership(
        historical_vanneman_panel4_dist91_crosswalk,
        historical_linguistic_geography_1991_2001$harmonized_crosswalk
      )
    ),
    tar_target(
      historical_vanneman_amalgamation_feasibility,
      build_vanneman_amalgamation_feasibility(
        historical_vanneman_harmonized_membership
      )
    ),
    tar_target(
      diag_ext_historical_vanneman_amalgamation_feasibility,
      save_vanneman_amalgamation_feasibility(
        historical_vanneman_amalgamation_feasibility
      ),
      format = "file"
    ),
    tar_target(
      historical_vanneman_kumar_somanathan_membership,
      build_vanneman_harmonized_membership(
        historical_vanneman_panel4_dist91_crosswalk,
        historical_linguistic_kumar_somanathan_geography$harmonized_crosswalk
      )
    ),
    tar_target(
      historical_vanneman_kumar_somanathan_amalgamation_feasibility,
      build_vanneman_amalgamation_feasibility(
        historical_vanneman_kumar_somanathan_membership
      )
    ),
    tar_target(
      diag_ext_historical_vanneman_kumar_somanathan_amalgamation_feasibility,
      save_vanneman_amalgamation_feasibility(
        historical_vanneman_kumar_somanathan_amalgamation_feasibility,
        prefix = "vanneman_kumar_somanathan_amalgamation_feasibility"
      ),
      format = "file"
    ),
    tar_target(
      historical_vanneman_kumar_somanathan_pretrend_validation,
      build_vanneman_amalgamated_pretrend_validation(
        historical_vanneman_pretrend_levels,
        historical_vanneman_kumar_somanathan_membership,
        district_panel,
        geography_status = "kumar_somanathan_exact_amalgamation"
      )
    ),
    tar_target(
      diag_ext_historical_vanneman_kumar_somanathan_pretrend_validation,
      save_vanneman_pretrend_validation(
        historical_vanneman_kumar_somanathan_pretrend_validation,
        prefix = "vanneman_kumar_somanathan_pretrend"
      ),
      format = "file"
    ),
    tar_target(
      historical_vanneman_consensus_membership,
      build_vanneman_harmonized_membership(
        historical_vanneman_panel4_dist91_crosswalk,
        historical_linguistic_consensus_geography$harmonized_crosswalk
      )
    ),
    tar_target(
      historical_vanneman_consensus_amalgamation_feasibility,
      build_vanneman_amalgamation_feasibility(
        historical_vanneman_consensus_membership
      )
    ),
    tar_target(
      diag_ext_historical_vanneman_consensus_amalgamation_feasibility,
      save_vanneman_amalgamation_feasibility(
        historical_vanneman_consensus_amalgamation_feasibility,
        prefix = "vanneman_consensus_amalgamation_feasibility"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_vanneman_parent_pretrend_validation,
      save_vanneman_pretrend_validation(
        historical_vanneman_parent_pretrend_validation,
        prefix = "vanneman_parent_pretrend",
        persist_inputs = FALSE
      ),
      format = "file"
    ),
    tar_target(
      historical_vanneman_pretrend_geography_comparison,
      build_vanneman_pretrend_geography_comparison(list(
        strict_one_to_one = historical_vanneman_pretrend_validation,
        historical_parent = historical_vanneman_parent_pretrend_validation,
        kumar_somanathan_exact_amalgamation =
          historical_vanneman_kumar_somanathan_pretrend_validation
      ))
    ),
    tar_target(
      historical_vanneman_pretrend_support_comparison,
      historical_vanneman_pretrend_geography_comparison$support
    ),
    tar_target(
      diag_ext_historical_vanneman_pretrend_support_comparison,
      save_vanneman_pretrend_support_comparison(
        historical_vanneman_pretrend_support_comparison
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_vanneman_pretrend_geography_comparison,
      save_vanneman_pretrend_geography_comparison(
        historical_vanneman_pretrend_geography_comparison
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_linguistic_inference_validation,
      save_historical_linguistic_inference_validation(
        historical_linguistic_distance_validation,
        historical_linguistic_persistence_validation,
        historical_linguistic_first_stage_robustness
      ),
      format = "file"
    )
  )
}
