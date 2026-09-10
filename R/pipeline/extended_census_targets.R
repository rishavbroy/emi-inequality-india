# Census extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_census_target_definitions <- function() {
  list(
    tar_target(
      diag_ext_census_2001_c17_mechanism_files,
      save_census_c17_mechanism_diagnostics(census_2001_c17_mechanism),
      format = "file"
    ),
    tar_target(
      census_2001_c13_files,
      census_c13_manifest_files(paths, 2001, census_2001_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_c13_files,
      census_c13_manifest_files(paths, 2011, census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_age_6_13_2001,
      read_census_c13_age_6_13(census_2001_c13_files, 2001)
    ),
    tar_target(
      census_age_6_13_2011,
      read_census_c13_age_6_13(census_2011_c13_files, 2011)
    ),
    tar_target(
      diag_ext_census_migration,
      save_census_migration_diagnostics(census_migration_diagnostics),
      format = "file"
    ),
    tar_target(
      diag_ext_census_workers,
      save_census_worker_diagnostics(census_worker_diagnostics),
      format = "file"
    ),
    tar_target(
      diag_ext_census_housing,
      save_census_housing_diagnostics(census_housing_diagnostics),
      format = "file"
    ),
    tar_target(
      diag_ext_census_household_capacity,
      save_census_household_capacity(census_household_capacity),
      format = "file"
    ),
    tar_target(
      diag_ext_census_households,
      save_census_household_diagnostics(census_household_diagnostics),
      format = "file"
    )
  )
}
