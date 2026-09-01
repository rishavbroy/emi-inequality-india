core_consumption_outcome_target_definitions <- function() {
  list(
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
        consumption_lineage_identity_aliases,
        district_lineage$reference_units,
        district_lineage$adjudicated_admin_events,
        district_lineage$admin_units_2011,
        district_transition_2001_2011
      )
    ),
    tar_target(
      consumption_lineage_bridge_2000_01,
      build_consumption_lineage_bridge(
        consumption_households_real_2000_01, consumption_lineage_reference
      )
    ),
    tar_target(
      consumption_lineage_bridge_2001_02,
      build_consumption_lineage_bridge(
        consumption_households_real_2001_02, consumption_lineage_reference
      )
    ),
    tar_target(
      consumption_lineage_bridge_2004_05,
      build_consumption_lineage_bridge(
        consumption_households_real_2004_05, consumption_lineage_reference
      )
    ),
    tar_target(
      consumption_lineage_bridge_2007_08,
      build_consumption_lineage_bridge(
        consumption_households_real_2007_08, consumption_lineage_reference
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
      consumption_lineage_bridge_hces_2022_23,
      build_consumption_lineage_bridge(
        consumption_households_real_hces_2022_23, consumption_lineage_reference
      )
    ),
    tar_target(
      consumption_lineage_bridge_hces_2023_24,
      build_consumption_lineage_bridge(
        consumption_households_real_hces_2023_24, consumption_lineage_reference
      )
    ),
    tar_target(
      consumption_households_lineaged_2000_01,
      attach_consumption_lineage(
        consumption_households_real_2000_01, consumption_lineage_bridge_2000_01
      )
    ),
    tar_target(
      consumption_households_lineaged_2001_02,
      attach_consumption_lineage(
        consumption_households_real_2001_02, consumption_lineage_bridge_2001_02
      )
    ),
    tar_target(
      consumption_households_lineaged_2004_05,
      attach_consumption_lineage(
        consumption_households_real_2004_05, consumption_lineage_bridge_2004_05
      )
    ),
    tar_target(
      consumption_households_lineaged_2007_08,
      attach_consumption_lineage(
        consumption_households_real_2007_08, consumption_lineage_bridge_2007_08
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
      consumption_households_lineaged_hces_2022_23,
      attach_consumption_lineage(
        consumption_households_real_hces_2022_23, consumption_lineage_bridge_hces_2022_23
      )
    ),
    tar_target(
      consumption_households_lineaged_hces_2023_24,
      attach_consumption_lineage(
        consumption_households_real_hces_2023_24, consumption_lineage_bridge_hces_2023_24
      )
    ),
    tar_target(
      consumption_lineage_coverage,
      safe_bind_rows(list(
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2000_01),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2001_02),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2004_05),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2007_08),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2009_10_type1),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2009_10_type2),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_2011_12_type2),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_hces_2022_23),
        summarize_consumption_lineage_coverage(consumption_households_lineaged_hces_2023_24)
      ))
    ),
    tar_target(
      consumption_lineage_coverage_file,
      save_consumption_lineage_coverage(consumption_lineage_coverage),
      format = "file"
    ),
    tar_target(
      consumption_lineage_status_coverage,
      safe_bind_rows(list(
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2000_01),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2001_02),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2004_05),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2007_08),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2009_10_type1),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2009_10_type2),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_2011_12_type2),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_hces_2022_23),
        summarize_consumption_lineage_status_coverage(consumption_households_lineaged_hces_2023_24)
      ))
    ),
    tar_target(
      consumption_lineage_status_coverage_file,
      save_consumption_lineage_status_coverage(consumption_lineage_status_coverage),
      format = "file"
    ),
    tar_target(
      consumption_lineage_review_queue,
      safe_bind_rows(list(
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2000_01),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2001_02),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2004_05),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2007_08),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2009_10_type1),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2009_10_type2),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_2011_12_type2),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_hces_2022_23),
        build_consumption_lineage_review_queue(consumption_lineage_bridge_hces_2023_24)
      ))
    ),
    tar_target(
      consumption_lineage_review_queue_file,
      save_consumption_lineage_review_queue(consumption_lineage_review_queue),
      format = "file"
    ),
    tar_target(
      consumption_welfare_outcomes_file,
      "data/metadata/consumption_welfare_outcomes.csv",
      format = "file"
    ),
    tar_target(
      consumption_welfare_outcomes,
      read_consumption_welfare_outcomes(consumption_welfare_outcomes_file)
    ),
    tar_target(
      consumption_district_welfare_core_2000_01,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2000_01, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2000_01,
      consumption_district_welfare_core_2000_01
    ),
    tar_target(
      consumption_district_welfare_core_2001_02,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2001_02, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2001_02,
      consumption_district_welfare_core_2001_02
    ),
    tar_target(
      consumption_district_welfare_core_2004_05,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2004_05, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_2004_05,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_2004_05, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2004_05,
      safe_bind_rows(list(
        consumption_district_welfare_core_2004_05,
        consumption_district_welfare_distributional_2004_05
      ))
    ),
    tar_target(
      consumption_district_welfare_core_2007_08,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2007_08, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_2007_08,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_2007_08, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2007_08,
      safe_bind_rows(list(
        consumption_district_welfare_core_2007_08,
        consumption_district_welfare_distributional_2007_08
      ))
    ),
    tar_target(
      consumption_district_welfare_core_2009_10_type1,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2009_10_type1, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_2009_10_type1,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_2009_10_type1, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2009_10_type1,
      safe_bind_rows(list(
        consumption_district_welfare_core_2009_10_type1,
        consumption_district_welfare_distributional_2009_10_type1
      ))
    ),
    tar_target(
      consumption_district_welfare_core_2009_10_type2,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2009_10_type2, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_2009_10_type2,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_2009_10_type2, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2009_10_type2,
      safe_bind_rows(list(
        consumption_district_welfare_core_2009_10_type2,
        consumption_district_welfare_distributional_2009_10_type2
      ))
    ),
    tar_target(
      consumption_district_welfare_core_2011_12_type2,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_2011_12_type2, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_2011_12_type2,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_2011_12_type2, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_2011_12_type2,
      safe_bind_rows(list(
        consumption_district_welfare_core_2011_12_type2,
        consumption_district_welfare_distributional_2011_12_type2
      ))
    ),
    tar_target(
      consumption_district_welfare_core_hces_2022_23,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_hces_2022_23, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_hces_2022_23,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_hces_2022_23, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_hces_2022_23,
      safe_bind_rows(list(
        consumption_district_welfare_core_hces_2022_23,
        consumption_district_welfare_distributional_hces_2022_23
      ))
    ),
    tar_target(
      consumption_district_welfare_core_hces_2023_24,
      estimate_consumption_district_welfare_core(
        consumption_households_lineaged_hces_2023_24, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_distributional_hces_2023_24,
      estimate_consumption_district_welfare_distributional(
        consumption_households_lineaged_hces_2023_24, consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_district_welfare_hces_2023_24,
      safe_bind_rows(list(
        consumption_district_welfare_core_hces_2023_24,
        consumption_district_welfare_distributional_hces_2023_24
      ))
    ),
    tar_target(
      consumption_district_welfare,
      safe_bind_rows(list(
        consumption_district_welfare_2000_01,
        consumption_district_welfare_2001_02,
        consumption_district_welfare_2004_05,
        consumption_district_welfare_2007_08,
        consumption_district_welfare_2009_10_type1,
        consumption_district_welfare_2009_10_type2,
        consumption_district_welfare_2011_12_type2,
        consumption_district_welfare_hces_2022_23,
        consumption_district_welfare_hces_2023_24
      ))
    ),
    tar_target(
      consumption_district_welfare_file,
      save_consumption_district_welfare(consumption_district_welfare),
      format = "file"
    ),
    tar_target(
      consumption_welfare_comparisons_file,
      "data/metadata/consumption_welfare_comparisons.csv",
      format = "file"
    ),
    tar_target(
      consumption_welfare_comparisons,
      read_consumption_welfare_comparisons(consumption_welfare_comparisons_file)
    ),
    tar_target(
      consumption_welfare_changes,
      build_consumption_welfare_changes(
        consumption_district_welfare,
        consumption_welfare_outcomes,
        consumption_welfare_comparisons
      )
    ),
    tar_target(
      consumption_welfare_changes_file,
      save_consumption_welfare_changes(consumption_welfare_changes),
      format = "file"
    ),
    tar_target(
      consumption_welfare_comparability,
      compare_consumption_welfare(
        consumption_district_welfare,
        consumption_welfare_outcomes,
        consumption_welfare_comparisons
      )
    ),
    tar_target(
      consumption_welfare_comparability_file,
      save_consumption_welfare_comparability(consumption_welfare_comparability),
      format = "file"
    ),
    tar_target(
      consumption_iv_outcome_registry_file,
      "data/metadata/consumption_iv_outcomes.csv",
      format = "file"
    ),
    tar_target(
      consumption_iv_outcome_registry,
      read_consumption_iv_outcome_registry(consumption_iv_outcome_registry_file)
    ),
    tar_target(
      consumption_iv_specifications,
      compile_consumption_iv_specifications(
        consumption_iv_outcome_registry, census_2001_control_registry
      )
    )
  )
}
