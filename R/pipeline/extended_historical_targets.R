# Historical extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_historical_target_definitions <- function() {
  list(
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
      diag_ext_census_1991_primary_validation,
      save_census_1991_primary_validation(census_1991_primary_validation),
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
