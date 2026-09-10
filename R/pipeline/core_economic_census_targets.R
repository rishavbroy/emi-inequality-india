# Publication Economic Census target declarations.
#
# The main paper uses only the predetermined 2005 IT-employment environment.
# Keep this minimal baseline chain core; longitudinal EC05-EC13 construction and
# mechanism diagnostics remain extended until a publication exhibit consumes them.

core_economic_census_target_definitions <- function() {
  list(
    tar_target(
      economic_census_ec05_raw_archive,
      manifest_file_by_id(
        paths,
        "economic_census_raw",
        "ec05_raw_archive",
        "Fifth Economic Census raw archive"
      ),
      format = "file"
    ),
    tar_target(
      economic_census_2005_it_source,
      read_economic_census_2005_it_baseline(economic_census_ec05_raw_archive)
    ),
    tar_target(
      economic_census_2005_it_baseline,
      build_economic_census_2005_it_baseline(
        economic_census_2005_it_source,
        district_lineage$admin_units_2001,
        district_lineage$admin_units_2011,
        district_transition_2001_2011
      )
    ),
    tar_target(
      economic_census_it_opportunity,
      diagnose_economic_census_it_opportunity(
        consumption_iv_panel,
        economic_census_2005_it_baseline,
        consumption_iv_outcome_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      shrug_ec05_archive,
      manifest_file_by_id(paths, "shrug_economic_census", "shrug_ec05_csv_archive", "SHRUG EC05 archive"),
      format = "file"
    ),
    tar_target(
      shrug_ec13_archive,
      manifest_file_by_id(paths, "shrug_economic_census", "shrug_ec13_csv_archive", "SHRUG EC13 archive"),
      format = "file"
    ),
    tar_target(
      shrug_ec05_district_source,
      read_shrug_ec05_district(shrug_ec05_archive)
    ),
    tar_target(
      shrug_ec13_district_source,
      read_shrug_ec13_district(shrug_ec13_archive)
    ),
    tar_target(
      economic_census_2005_district_measures,
      build_economic_census_2005_measures(
        shrug_ec05_district_source,
        district_lineage$admin_units_2001
      )
    ),
    tar_target(
      economic_census_2013_district_measures,
      build_economic_census_2013_measures(
        shrug_ec13_district_source,
        district_lineage$admin_units_2011,
        district_lineage$admin_units_2001,
        district_transition_2001_2011
      )
    ),
    tar_target(
      economic_census_2005_2013_changes,
      build_economic_census_change_measures(
        economic_census_2005_district_measures,
        economic_census_2013_district_measures
      )
    ),
    tar_target(
      economic_census_diagnostics,
      build_economic_census_diagnostics(
        economic_census_2005_district_measures,
        economic_census_2005_it_baseline,
        economic_census_2013_district_measures,
        economic_census_2005_2013_changes,
        mechanism_panel = district_panel,
        welfare_panel = consumption_iv_panel,
        consumption_registry = consumption_iv_outcome_registry,
        cfg = cfg,
        control_registry = census_2001_control_registry,
        it_opportunity = economic_census_it_opportunity
      )
    )
  )
}
