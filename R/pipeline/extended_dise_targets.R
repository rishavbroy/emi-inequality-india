# DISE extended-diagnostic target declarations.
#
# Publication-required baseline DISE ingestion, lineage, treatment construction,
# NSS validation, and schooling-market estimates live in core_dise_targets.R.
# This factory retains only diagnostics not consumed by the main paper.

extended_dise_target_definitions <- function() {
  list(
    tar_target(
      dise_report_school_quality_file,
      path_metadata(paths, "dise_report_school_quality_2011_15.csv"),
      format = "file"
    ),
    tar_target(
      dise_report_school_quality,
      read_dise_report_school_quality(paths, dise_report_school_quality_file)
    ),
    tar_target(
      census_age_6_13_anchors,
      build_census_age_6_13_anchors(
        census_age_6_13_2001,
        census_age_6_13_2011,
        district_transition_2001_2011
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
      dise_baseline_district_year_2001_with_age,
      attach_dise_age_6_13_exposure(
        dise_baseline_district_year_2001,
        census_age_6_13_population
      )
    ),
    tar_target(
      dise_baseline_treatments_all_constructs,
      build_dise_baseline_treatments_2001(
        dise_baseline_district_year_2001_with_age
      )
    ),
    tar_target(
      district_panel_with_dise_all_constructs,
      attach_dise_treatments_to_panel_2001(
        district_panel, dise_baseline_treatments_all_constructs
      )
    ),
    tar_target(
      dise_report_school_quality_2001,
      harmonize_dise_report_school_quality_to_2001(
        dise_report_school_quality,
        dise_lineage_bridge
      )
    ),
    # ST-concentration heterogeneity is core-owned for Appendix D7.
    # Extended mode persists the shared object below.
    tar_target(
      diag_ext_english_opportunity_st_heterogeneity_files,
      save_english_opportunity_st_heterogeneity(
        english_opportunity_st_heterogeneity
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_english_opportunity_measure_registry,
      save_english_opportunity_measure_registry(
        english_opportunity_measure_registry
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_english_opportunity_district_mechanism_files,
      save_english_opportunity_district_mechanisms(
        english_opportunity_district_mechanisms
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_english_opportunity_mechanism_table_files,
      save_english_opportunity_mechanism_table(
        census_2001_c17_mechanism,
        english_opportunity_district_mechanisms,
        english_opportunity_measure_registry
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_english_opportunity_mechanism_figure_files,
      save_english_opportunity_mechanism_figure(
        census_2001_c17_mechanism,
        english_opportunity_district_mechanisms,
        english_opportunity_measure_registry
      ),
      format = "file"
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
        dise_baseline_district_year,
        dise_baseline_treatments_all_constructs,
        dise_publication_checks
      )
    ),
    tar_target(dise_iv_construct_registry, dise_construct_registry(), iteration = "list"),
    tar_target(
      dise_iv_analysis_panel,
      prepare_dise_iv_diagnostic_panel(
        district_panel_with_dise_all_constructs,
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
        dise_baseline_treatments_all_constructs,
        lineage_bridge = dise_lineage_bridge,
        harmonized_district_year = dise_baseline_district_year_2001_with_age,
        dynamic_panel = dise_dynamic_panel,
        dynamic_relevance = dise_dynamic_relevance,
        school_quality = dise_school_quality_mechanisms,
        age_exposure = list(
          anchors = census_age_6_13_anchors,
          population = census_age_6_13_population,
          dynamic_relevance = dise_elementary_age_dynamic_relevance
        )
      )$path,
      format = "file"
    )
  )
}
