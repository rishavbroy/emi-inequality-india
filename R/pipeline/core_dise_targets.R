# Publication DISE target declarations.
#
# These targets are required by the main-paper schooling-market evidence.
# Keep only the baseline/lineage objects needed by publication exhibits here;
# age-denominator, longitudinal, school-quality, and weak-IV permutation work
# remains in extended_dise_target_definitions().

core_dise_target_definitions <- function() {
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
      dise_archive_registry,
      read_dise_archive_registry(paths, dise_archive_registry_file)
    ),
    tar_target(
      dise_medium_slot_crosswalk,
      read_dise_medium_slot_crosswalk(paths, dise_medium_slot_crosswalk_file)
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
      raw_dise_baseline,
      {
        raw_data_preflight
        read_dise_baseline_archive(paths, dise_archive_registry)
      }
    ),
    tar_target(
      raw_dise_baseline_teachers,
      {
        raw_data_preflight
        read_dise_baseline_teacher_archive(paths, dise_archive_registry)
      }
    ),
    tar_target(
      raw_dise_dynamic,
      {
        raw_data_preflight
        read_dise_dynamic_archive(
          paths,
          dise_archive_registry,
          dise_report_language_enrollment,
          dise_report_total_enrollment_2010
        )
      }
    ),
    tar_target(
      dise_baseline_district_year,
      {
        baseline <- attach_dise_medium_identities(
          raw_dise_baseline, dise_medium_slot_crosswalk
        )
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
      dise_baseline_district_year_2001,
      harmonize_dise_counts_to_2001(
        dise_baseline_district_year, dise_lineage_bridge
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
      english_opportunity_measure_registry_file,
      english_opportunity_measure_registry_path(paths),
      format = "file"
    ),
    tar_target(
      english_opportunity_measure_registry,
      read_english_opportunity_measure_registry(
        english_opportunity_measure_registry_file
      )
    ),
    tar_target(
      english_opportunity_district_mechanisms,
      diagnose_english_opportunity_district_mechanisms(
        district_panel_with_dise,
        english_opportunity_measure_registry,
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      dise_iv_nss_validation,
      diagnose_dise_nss_validation(district_panel_with_dise)
    )
  )
}
