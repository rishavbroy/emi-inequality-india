# Dise extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_dise_target_definitions <- function() {
  list(
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
      english_opportunity_district_mechanisms,
      diagnose_english_opportunity_district_mechanisms(
        district_panel_with_dise, english_opportunity_measure_registry,
        control_registry = census_2001_control_registry
      )
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
    )
  )
}
