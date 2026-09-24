# Historical extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_historical_target_definitions <- function() {
  list(
    tar_target(
      diag_ext_historical_linguistic_geography_1991_2001,
      save_historical_linguistic_geography_1991_2001(
        historical_linguistic_geography_1991_2001
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_vanneman_source_qa,
      save_vanneman_historical_source_qa(historical_vanneman_source_qa),
      format = "file"
    ),
    tar_target(
      census_1991_st16_source_files,
      census_1991_st16_files(paths, census_1991_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_1991_st17_district_source_files,
      census_1991_st17_district_files(paths, census_1991_download_manifest_file),
      format = "file"
    ),
    tar_target(census_1991_st16, read_census_1991_st16(census_1991_st16_source_files)),
    tar_target(
      census_1991_st17_districts,
      read_census_1991_st17_districts(census_1991_st17_district_source_files)
    ),
    tar_target(
      census_1991_st_language_diagnostic,
      build_census_1991_st_language_diagnostic(census_1991_st17_districts, census_1991_st16)
    ),
    tar_target(
      diag_ext_census_1991_st_language,
      save_census_1991_st_language_diagnostic(census_1991_st_language_diagnostic),
      format = "file"
    ),
    tar_target(
      diag_ext_census_1991_primary_validation,
      save_census_1991_primary_validation(census_1991_primary_validation),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_vanneman_panel4_geography,
      save_vanneman_panel4_geography_inventory(historical_vanneman_panel4_geography),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_vanneman_panel4_dist91_adjudication_evidence,
      save_vanneman_panel4_dist91_adjudication_evidence(
        historical_vanneman_panel4_dist91_adjudications
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_vanneman_panel4_dist91_crosswalk,
      save_vanneman_panel4_dist91_crosswalk(historical_vanneman_panel4_dist91_crosswalk),
      format = "file"
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
      diag_ext_helms_lim_linguistic_distance_benchmark,
      save_helms_lim_linguistic_distance_benchmark(helms_lim_linguistic_distance_benchmark),
      format = "file"
    ),
    tar_target(
      diag_ext_historical_baseline_balance_1991,
      save_historical_baseline_balance_1991(historical_baseline_balance_1991),
      format = "file"
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
