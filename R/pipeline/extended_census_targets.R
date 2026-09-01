# Census extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_census_target_definitions <- function() {
  list(
    tar_target(
      census_2001_download_manifest_file,
      path_metadata(paths, "census_2001_download_manifest.tsv"),
      format = "file"
    ),
    tar_target(
      census_2011_download_manifest_file,
      path_metadata(paths, "census_2011_download_manifest.tsv"),
      format = "file"
    ),
    tar_target(
      census_2001_c17_files,
      census_c17_manifest_files(paths, census_2001_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2001_c16_state_language_totals,
      census_2001_state_language_totals(raw_census_2001)
    ),
    tar_target(
      census_2001_c17_state_languages,
      read_census_c17_state_languages(
        census_2001_c17_files, census_2001_c16_state_language_totals
      )
    ),
    tar_target(
      census_2001_c17_mechanism,
      diagnose_census_c17_mechanism(
        census_2001_c17_state_languages,
        glottolog_5_3,
        census_glottolog_crosswalk,
        historical_linguistic_sources,
        shastry_concordance = shastry_language_distance,
        lexical_index = lexical_language_index
      )
    ),
    tar_target(
      english_opportunity_measure_registry_file,
      english_opportunity_measure_registry_path(paths),
      format = "file"
    ),
    tar_target(
      english_opportunity_measure_registry,
      read_english_opportunity_measure_registry(english_opportunity_measure_registry_file)
    ),
    tar_target(
      analysis_design_registry,
      compile_analysis_design_registry(
        consumption_iv_specifications,
        english_opportunity_measure_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      diag_ext_english_opportunity_measure_registry,
      save_english_opportunity_measure_registry(english_opportunity_measure_registry),
      format = "file"
    ),
    tar_target(
      diag_ext_census_2001_c17_mechanism_files,
      save_census_c17_mechanism_diagnostics(census_2001_c17_mechanism),
      format = "file"
    ),
    tar_target(
      census_2011_pca_population_file,
      path_project(paths, "data", "raw", "shrug", "census_2011", "shrug-pca11-csv.zip"),
      format = "file"
    ),
    tar_target(
      census_2011_population_source,
      read_census_2011_district_population(census_2011_pca_population_file)
    ),
    tar_target(
      census_2011_population,
      build_census_2011_population_measures(
        census_2011_population_source,
        district_transition_2001_2011
      )
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
      census_2001_d02_files,
      census_migration_manifest_files(paths, 2001, "D02", census_2001_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_d02_files,
      census_migration_manifest_files(paths, 2011, "D02", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_d03_files,
      census_migration_manifest_files(paths, 2011, "D03", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_d04_files,
      census_migration_manifest_files(paths, 2011, "D04", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_d05_files,
      census_migration_manifest_files(paths, 2011, "D05", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_d06_files,
      census_migration_manifest_files(paths, 2011, "D06", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_d07_files,
      census_migration_manifest_files(paths, 2011, "D07", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_migration_d02_2001_source,
      read_census_d02_district(census_2001_d02_files, 2001)
    ),
    tar_target(
      census_migration_d02_2011_source,
      read_census_d02_district(census_2011_d02_files, 2011)
    ),
    tar_target(
      census_migration_d03_2011_source,
      read_census_d03_2011_district(census_2011_d03_files)
    ),
    tar_target(
      census_migration_d04_2011_source,
      read_census_d04_2011_district(census_2011_d04_files)
    ),
    tar_target(
      census_migration_d05_2011_source,
      read_census_d05_2011_district(census_2011_d05_files)
    ),
    tar_target(
      census_migration_d06_2011_source,
      read_census_d06_2011_district(census_2011_d06_files)
    ),
    tar_target(
      census_migration_d07_2011_source,
      read_census_d07_2011_district(census_2011_d07_files)
    ),
    tar_target(
      census_migration_d02_2001,
      build_census_d02_2001_measures(
        census_migration_d02_2001_source,
        census_2001_district_totals
      )
    ),
    tar_target(
      census_migration_d02_2011,
      build_census_d02_2011_measures(
        census_migration_d02_2011_source,
        district_transition_2001_2011,
        census_2011_population
      )
    ),
    tar_target(
      census_migration_d02_population_change,
      build_census_d02_population_change_measures(
        census_migration_d02_2001,
        census_migration_d02_2011
      )
    ),
    tar_target(
      census_migration_d03_2011,
      build_census_d03_2011_measures(
        census_migration_d03_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_migration_d04_2011,
      build_census_d04_2011_measures(
        census_migration_d04_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_migration_d05_2011,
      build_census_d05_2011_measures(
        census_migration_d05_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_migration_d06_2011,
      build_census_d06_2011_measures(
        census_migration_d06_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_migration_d07_2011,
      build_census_d07_2011_measures(
        census_migration_d07_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_migration_diagnostics,
      build_census_migration_diagnostics(
        census_migration_d02_2001,
        census_migration_d02_2011_source,
        census_migration_d03_2011_source,
        census_migration_d04_2011_source,
        census_migration_d05_2011_source,
        census_migration_d06_2011_source,
        census_migration_d07_2011_source,
        census_2011_population_source,
        census_2011_population,
        census_migration_d02_2011,
        census_migration_d02_population_change,
        census_migration_d03_2011,
        census_migration_d04_2011,
        census_migration_d05_2011,
        census_migration_d06_2011,
        census_migration_d07_2011,
        district_panel,
        cfg,
        control_registry = census_2001_control_registry
      )
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
      census_2001_h05_files,
      census_housing_manifest_files(
        paths, "H05", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_2001_h08_files,
      census_housing_manifest_files(
        paths, "H08", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_2001_h09_files,
      census_housing_manifest_files(
        paths, "H09", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_2001_h12_files,
      census_housing_manifest_files(
        paths, "H12", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(
      census_2001_h13_files,
      census_housing_manifest_files(
        paths, "H13", census_2001_download_manifest_file, census_year = 2001L
      ),
      format = "file"
    ),
    tar_target(census_housing_h05_2001_source, read_census_h05_2001_district(census_2001_h05_files)),
    tar_target(census_housing_h08_2001_source, read_census_h08_2001_district(census_2001_h08_files)),
    tar_target(census_housing_h09_2001_source, read_census_h09_2001_district(census_2001_h09_files)),
    tar_target(census_housing_h12_2001_source, read_census_h12_2001_district(census_2001_h12_files)),
    tar_target(census_housing_h13_2001_source, read_census_h13_2001_district(census_2001_h13_files)),
    tar_target(
      census_housing_2001,
      build_census_2001_housing_measures(
        census_housing_h09_2001_source,
        census_housing_h12_2001_source,
        census_housing_h13_2001_source,
        h05 = census_housing_h05_2001_source,
        h08 = census_housing_h08_2001_source
      )
    ),
    tar_target(
      census_2011_hl04_files,
      census_housing_manifest_files(paths, "HL04", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_hl06_files,
      census_housing_manifest_files(paths, "HL06", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_hl07_files,
      census_housing_manifest_files(paths, "HL07", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_hl11_files,
      census_housing_manifest_files(paths, "HL11", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_hl12_files,
      census_housing_manifest_files(paths, "HL12", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(census_housing_hl04_2011_source, read_census_hl04_2011_district(census_2011_hl04_files)),
    tar_target(census_housing_hl06_2011_source, read_census_hl06_2011_district(census_2011_hl06_files)),
    tar_target(census_housing_hl07_2011_source, read_census_hl07_2011_district(census_2011_hl07_files)),
    tar_target(census_housing_hl11_2011_source, read_census_hl11_2011_district(census_2011_hl11_files)),
    tar_target(census_housing_hl12_2011_source, read_census_hl12_2011_district(census_2011_hl12_files)),
    tar_target(
      census_housing_2011,
      build_census_2011_housing_measures(
        census_housing_hl07_2011_source,
        census_housing_hl11_2011_source,
        census_housing_hl12_2011_source,
        district_transition_2001_2011,
        hl04 = census_housing_hl04_2011_source,
        hl06 = census_housing_hl06_2011_source
      )
    ),
    tar_target(
      census_housing_change_2011_2001,
      build_census_housing_change_measures(census_housing_2001, census_housing_2011)
    ),
    tar_target(
      census_housing_diagnostics,
      build_census_housing_diagnostics(
        census_housing_h05_2001_source,
        census_housing_h08_2001_source,
        census_housing_h09_2001_source,
        census_housing_h12_2001_source,
        census_housing_h13_2001_source,
        census_housing_2001,
        census_housing_hl04_2011_source,
        census_housing_hl06_2011_source,
        census_housing_hl07_2011_source,
        census_housing_hl11_2011_source,
        census_housing_hl12_2011_source,
        census_housing_2011,
        census_housing_change_2011_2001,
        district_panel,
        cfg,
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      diag_ext_census_housing,
      save_census_housing_diagnostics(census_housing_diagnostics),
      format = "file"
    ),
    tar_target(
      census_2011_hh08_files,
      census_household_manifest_files(paths, "HH08", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_hh10_files,
      census_household_manifest_files(paths, "HH10", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(
      census_2011_hh11_files,
      census_household_manifest_files(paths, "HH11", census_2011_download_manifest_file),
      format = "file"
    ),
    tar_target(census_household_hh08_2011_source, read_census_hh08_2011_district(census_2011_hh08_files)),
    tar_target(census_household_hh10_2011_source, read_census_hh10_2011_district(census_2011_hh10_files)),
    tar_target(census_household_hh11_2011_source, read_census_hh11_2011_district(census_2011_hh11_files)),
    tar_target(
      census_household_2011,
      build_census_2011_household_measures(
        census_household_hh08_2011_source,
        census_household_hh10_2011_source,
        census_household_hh11_2011_source,
        district_transition_2001_2011
      )
    ),
    tar_target(
      census_household_diagnostics,
      build_census_household_diagnostics(
        census_household_hh08_2011_source,
        census_household_hh10_2011_source,
        census_household_hh11_2011_source,
        census_household_2011
      )
    ),
    tar_target(
      diag_ext_census_households,
      save_census_household_diagnostics(census_household_diagnostics),
      format = "file"
    )
  )
}
