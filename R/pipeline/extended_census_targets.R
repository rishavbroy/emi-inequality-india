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
      census_2001_b04_files,
      census_worker_manifest_files(
        paths, "B04", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_2001_b25_files,
      census_worker_manifest_files(
        paths, "B25", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_2001_b26_files,
      census_worker_manifest_files(
        paths, "B26", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_workers_b04_2001_source,
      read_census_b04_2001_district(census_2001_b04_files)
    ),
    tar_target(
      census_workers_b25_2001_source,
      read_census_b25_2001_district(census_2001_b25_files)
    ),
    tar_target(
      census_workers_b26_2001_source,
      read_census_b26_2001_district(census_2001_b26_files)
    ),
    tar_target(
      census_workers_industry_2001,
      build_census_2001_industry_measures(census_workers_b04_2001_source)
    ),
    tar_target(
      census_workers_occupation_2001,
      build_census_2001_occupation_measures(census_workers_b26_2001_source)
    ),
    tar_target(
      census_2011_b04_files,
      census_worker_manifest_files(paths, "B04", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_b06_files,
      census_worker_manifest_files(paths, "B06", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_b25a_files,
      census_worker_manifest_files(paths, "B25A", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_b25b_files,
      census_worker_manifest_files(paths, "B25B", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_workers_b04_2011_source,
      read_census_b04_2011_district(census_2011_b04_files)
    ),
    tar_target(
      census_workers_b06_2011_source,
      read_census_b06_2011_district(census_2011_b06_files)
    ),
    tar_target(
      census_workers_b25a_2011_source,
      read_census_b25a_2011_district(census_2011_b25a_files)
    ),
    tar_target(
      census_workers_b25b_2011_source,
      read_census_b25b_2011_district(census_2011_b25b_files)
    ),
    tar_target(
      census_workers_industry_2011,
      build_census_2011_industry_measures(
        census_workers_b04_2011_source,
        census_workers_b06_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_workers_occupation_2011,
      build_census_2011_occupation_measures(
        census_workers_b25a_2011_source,
        census_workers_b25b_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_worker_diagnostics,
      build_census_worker_diagnostics(
        census_workers_b04_2001_source,
        census_workers_b25_2001_source,
        census_workers_b26_2001_source,
        census_workers_industry_2001,
        census_workers_occupation_2001,
        census_workers_b04_2011_source,
        census_workers_b06_2011_source,
        census_workers_b25a_2011_source,
        census_workers_b25b_2011_source,
        census_workers_industry_2011,
        census_workers_occupation_2011,
        district_panel,
        control_registry = census_2001_control_registry
      )
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
      census_household_diagnostics,
      build_census_household_diagnostics(
        census_household_hh09_2001_source,
        census_household_hh13_2001_source,
        census_household_hh15_2001_source,
        census_household_hh15a_2001_source,
        census_household_2001,
        census_household_hh08_2011_source,
        census_household_hh10_2011_source,
        census_household_hh11_2011_source,
        census_household_2011,
        census_household_change_2011_2001
      )
    ),
    tar_target(
      diag_ext_census_households,
      save_census_household_diagnostics(census_household_diagnostics),
      format = "file"
    )
  )
}
