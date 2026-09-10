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
    )
  )
}
