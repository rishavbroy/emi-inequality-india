core_consumption_target_definitions <- function() {
  list(
  tar_target(
    consumption_survey_registry_file,
    consumption_survey_registry_path(paths),
    format = "file"
  ),
  tar_target(
    consumption_survey_registry,
    read_consumption_survey_registry_file(consumption_survey_registry_file)
  ),
  tar_target(
    raw_nss_2007_consumption,
    {
      raw_data_preflight
      read_nss_2007_consumption(paths)
    }
  ),
  tar_target(
    consumption_households_2007_08,
    read_registered_detailed_consumption_frames(
      raw_nss_2007_consumption,
      consumption_survey_spec(consumption_survey_registry, "nss_2007_08_consumption")
    )
  ),
  tar_target(
    consumption_price_spec_2007_legacy,
    consumption_survey_spec(consumption_survey_registry, "nss_2007_08_education")
  ),
  tar_target(
    consumption_price_spec_2017_legacy,
    consumption_survey_spec(consumption_survey_registry, "nss_2017_18_education")
  ),
  tar_target(
    hces_summary_items_file,
    hces_summary_items_path(paths),
    format = "file"
  ),
  tar_target(
    hces_summary_items,
    read_hces_summary_items_file(hces_summary_items_file)
  ),
  tar_target(
    consumption_archive_2000_01,
    {
      raw_data_preflight
      discover_consumption_csv_archive(
        paths, consumption_survey_spec(consumption_survey_registry, "nss_2000_01")
      )
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2001_02,
    {
      raw_data_preflight
      discover_consumption_csv_archive(
        paths, consumption_survey_spec(consumption_survey_registry, "nss_2001_02")
      )
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2004_05,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2004_05"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2009_10_type1,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type1"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2009_10_type2,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type2"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_2011_12_type2,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "nss_2011_12_type2"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_hces_2022_23,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "hces_2022_23"))
    },
    format = "file"
  ),
  tar_target(
    consumption_archive_hces_2023_24,
    {
      raw_data_preflight
      discover_consumption_csv_archive(paths, consumption_survey_spec(consumption_survey_registry, "hces_2023_24"))
    },
    format = "file"
  ),
  tar_target(
    consumption_households_2000_01,
    read_registered_detailed_consumption(
      consumption_archive_2000_01,
      consumption_survey_spec(consumption_survey_registry, "nss_2000_01")
    )
  ),
  tar_target(
    consumption_households_2001_02,
    read_registered_detailed_consumption(
      consumption_archive_2001_02,
      consumption_survey_spec(consumption_survey_registry, "nss_2001_02")
    )
  ),
  tar_target(
    consumption_households_2004_05,
    read_registered_detailed_consumption(
      consumption_archive_2004_05, consumption_survey_spec(consumption_survey_registry, "nss_2004_05")
    )
  ),
  tar_target(
    consumption_households_2009_10_type1,
    read_registered_detailed_consumption(
      consumption_archive_2009_10_type1, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type1")
    )
  ),
  tar_target(
    consumption_households_2009_10_type2,
    read_registered_detailed_consumption(
      consumption_archive_2009_10_type2, consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type2")
    )
  ),
  tar_target(
    consumption_households_2011_12_type2,
    read_registered_detailed_consumption(
      consumption_archive_2011_12_type2, consumption_survey_spec(consumption_survey_registry, "nss_2011_12_type2")
    )
  ),
  tar_target(
    consumption_hces_bundle_2022_23,
    read_registered_hces_bundle(
      consumption_archive_hces_2022_23,
      consumption_survey_spec(consumption_survey_registry, "hces_2022_23"),
      hces_summary_items
    )
  ),
  tar_target(
    consumption_hces_bundle_2023_24,
    read_registered_hces_bundle(
      consumption_archive_hces_2023_24,
      consumption_survey_spec(consumption_survey_registry, "hces_2023_24"),
      hces_summary_items
    )
  ),
  tar_target(
    consumption_households_hces_2022_23,
    consumption_hces_bundle_2022_23$households
  ),
  tar_target(
    consumption_households_hces_2023_24,
    consumption_hces_bundle_2023_24$households
  ),
  tar_target(
    consumption_mpce_benchmarks_file,
    path_project(paths, "data/metadata/consumption_mpce_benchmarks.csv"),
    format = "file"
  ),
  tar_target(
    consumption_mpce_benchmarks,
    read_consumption_mpce_benchmarks(consumption_mpce_benchmarks_file)
  ),
  tar_target(
    consumption_mpce_validation_2000_01,
    validate_consumption_mpce_reconstruction(
      consumption_households_2000_01, consumption_mpce_benchmarks, "nss_2000_01"
    )
  ),
  tar_target(
    consumption_mpce_validation_2001_02,
    validate_consumption_mpce_reconstruction(
      consumption_households_2001_02, consumption_mpce_benchmarks, "nss_2001_02"
    )
  ),
  tar_target(
    consumption_mpce_validation_2004_05,
    validate_consumption_mpce_reconstruction(
      consumption_households_2004_05, consumption_mpce_benchmarks, "nss_2004_05"
    )
  ),
  tar_target(
    consumption_mpce_validation_2007_08,
    validate_consumption_mpce_reconstruction(
      consumption_households_2007_08,
      consumption_mpce_benchmarks,
      "nss_2007_08_consumption"
    )
  ),
  tar_target(
    consumption_mpce_validation_2009_10_type1,
    validate_consumption_mpce_reconstruction(
      consumption_households_2009_10_type1, consumption_mpce_benchmarks, "nss_2009_10_type1"
    )
  ),
  tar_target(
    consumption_mpce_validation_2009_10_type2,
    validate_consumption_mpce_reconstruction(
      consumption_households_2009_10_type2, consumption_mpce_benchmarks, "nss_2009_10_type2"
    )
  ),
  tar_target(
    consumption_mpce_validation_2011_12_type2,
    validate_consumption_mpce_reconstruction(
      consumption_households_2011_12_type2, consumption_mpce_benchmarks, "nss_2011_12_type2"
    )
  ),
  tar_target(
    consumption_mpce_validation_hces_2022_23,
    validate_consumption_mpce_reconstruction(
      consumption_households_hces_2022_23, consumption_mpce_benchmarks, "hces_2022_23"
    )
  ),
  tar_target(
    consumption_mpce_validation_hces_2023_24,
    validate_consumption_mpce_reconstruction(
      consumption_households_hces_2023_24, consumption_mpce_benchmarks, "hces_2023_24"
    )
  ),
  tar_target(
    consumption_mpce_validation,
    combine_consumption_mpce_validations(
      consumption_mpce_validation_2000_01,
      consumption_mpce_validation_2001_02,
      consumption_mpce_validation_2004_05,
      consumption_mpce_validation_2007_08,
      consumption_mpce_validation_2009_10_type1,
      consumption_mpce_validation_2009_10_type2,
      consumption_mpce_validation_2011_12_type2,
      consumption_mpce_validation_hces_2022_23,
      consumption_mpce_validation_hces_2023_24
    )
  ),
  tar_target(
    consumption_mpce_validation_file,
    save_consumption_mpce_validation(consumption_mpce_validation),
    format = "file"
  ),
  tar_target(
    hces_summary_coverage_2022_23,
    consumption_hces_bundle_2022_23$summary_coverage
  ),
  tar_target(
    hces_summary_coverage_2023_24,
    consumption_hces_bundle_2023_24$summary_coverage
  ),
  tar_target(
    hces_summary_coverage,
    safe_bind_rows(list(hces_summary_coverage_2022_23, hces_summary_coverage_2023_24))
  ),
  tar_target(
    hces_summary_coverage_file,
    save_hces_summary_coverage(hces_summary_coverage),
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_2004_05_file,
    {
      raw_data_preflight
      path_project(paths, "data/raw/hces/2004-05/District_code_list_nss61_round.xls")
    },
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_2009_10_file,
    {
      raw_data_preflight
      path_project(paths, "data/raw/hces/2009-10/District code_66.xls")
    },
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_hces_file,
    path_project(paths, "data/metadata/hces_2022_24_district_codebook.csv"),
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_hces,
    read_consumption_district_codebook_csv(
      consumption_district_codebook_hces_file, "hces_2022_24"
    )
  ),
  tar_target(
    consumption_state_code_crosswalk_file,
    path_project(paths, "data/metadata/consumption_state_code_crosswalk.csv"),
    format = "file"
  ),
  tar_target(
    consumption_state_code_crosswalk,
    read_consumption_state_code_crosswalk(consumption_state_code_crosswalk_file)
  ),
  tar_target(
    consumption_source_geography_special_units_file,
    path_project(paths, "data/metadata/consumption_source_geography_special_units.csv"),
    format = "file"
  ),
  tar_target(
    consumption_source_geography_special_units,
    read_consumption_source_geography_special_units(consumption_source_geography_special_units_file)
  ),
  tar_target(
    consumption_district_codebook_2000_01,
    build_consumption_census2001_codebook(
      consumption_households_2000_01,
      district_lineage$admin_units_2001,
      consumption_survey_spec(consumption_survey_registry, "nss_2000_01"),
      consumption_state_code_crosswalk
    )
  ),
  tar_target(
    consumption_district_codebook_2001_02,
    build_consumption_census2001_codebook(
      consumption_households_2001_02,
      district_lineage$admin_units_2001,
      consumption_survey_spec(consumption_survey_registry, "nss_2001_02"),
      consumption_state_code_crosswalk
    )
  ),
  tar_target(
    consumption_district_codebook_2004_05_base,
    read_consumption_district_codebook_excel(consumption_district_codebook_2004_05_file, "nss_2004_05")
  ),
  tar_target(
    consumption_district_codebook_2004_05,
    merge_consumption_codebook_special_units(
      consumption_district_codebook_2004_05_base,
      consumption_source_geography_special_units,
      "nss_2004_05"
    )
  ),
  tar_target(
    consumption_district_codebook_2009_10,
    read_consumption_district_codebook_excel(consumption_district_codebook_2009_10_file, "nss_2009_10")
  ),
  tar_target(
    consumption_district_codebook_2009_10_anomalies,
    consumption_codebook_name_anomalies(consumption_district_codebook_2009_10)
  ),
  tar_target(
    consumption_district_codebook_2011_12_file,
    {
      raw_data_preflight
      path_project(paths, "data/raw/hces/2011-12/DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012.xml")
    },
    format = "file"
  ),
  tar_target(
    consumption_district_codebook_2011_12,
    read_consumption_district_codebook_ddi(consumption_district_codebook_2011_12_file, "nss_2011_12")
  ),
  tar_target(
    consumption_district_codebook_2007_08,
    build_consumption_district_codebook_from_labels(
      raw_nss_2007_consumption,
      consumption_survey_spec(consumption_survey_registry, "nss_2007_08_consumption")
    )
  ),
  tar_target(
    consumption_households_named_2000_01,
    attach_consumption_source_district_identity(
      consumption_households_2000_01, consumption_district_codebook_2000_01
    )
  ),
  tar_target(
    consumption_households_named_2001_02,
    attach_consumption_source_district_identity(
      consumption_households_2001_02, consumption_district_codebook_2001_02
    )
  ),
  tar_target(
    consumption_households_named_2004_05,
    attach_consumption_source_district_identity(consumption_households_2004_05, consumption_district_codebook_2004_05)
  ),
  tar_target(
    consumption_households_named_2007_08,
    attach_consumption_source_district_identity(
      consumption_households_2007_08, consumption_district_codebook_2007_08
    )
  ),
  tar_target(
    consumption_households_named_2009_10_type1,
    attach_consumption_source_district_identity(consumption_households_2009_10_type1, consumption_district_codebook_2009_10)
  ),
  tar_target(
    consumption_households_named_2009_10_type2,
    attach_consumption_source_district_identity(consumption_households_2009_10_type2, consumption_district_codebook_2009_10)
  ),
  tar_target(
    consumption_households_named_2011_12_type2,
    attach_consumption_source_district_identity(consumption_households_2011_12_type2, consumption_district_codebook_2011_12)
  ),
  tar_target(
    consumption_households_named_hces_2022_23,
    attach_consumption_source_district_identity(
      consumption_households_hces_2022_23, consumption_district_codebook_hces
    )
  ),
  tar_target(
    consumption_households_named_hces_2023_24,
    attach_consumption_source_district_identity(
      consumption_households_hces_2023_24, consumption_district_codebook_hces
    )
  ),

  tar_target(
    consumption_households_real_2000_01,
    {
      consumption_mpce_validation_2000_01
      deflate_detailed_consumption_households(
        consumption_households_named_2000_01, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2000_01")
      )
    }
  ),
  tar_target(
    consumption_households_real_2001_02,
    {
      consumption_mpce_validation_2001_02
      deflate_detailed_consumption_households(
        consumption_households_named_2001_02, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2001_02")
      )
    }
  ),
  tar_target(
    consumption_households_real_2004_05,
    {
      consumption_mpce_validation_2004_05
      deflate_detailed_consumption_households(
        consumption_households_named_2004_05, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2004_05")
      )
    }
  ),
  tar_target(
    consumption_households_real_2007_08,
    {
      consumption_mpce_validation_2007_08
      deflate_detailed_consumption_households(
        consumption_households_named_2007_08, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2007_08_consumption")
      )
    }
  ),
  tar_target(
    consumption_households_real_2009_10_type1,
    {
      consumption_mpce_validation_2009_10_type1
      deflate_detailed_consumption_households(
        consumption_households_named_2009_10_type1, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type1")
      )
    }
  ),
  tar_target(
    consumption_households_real_2009_10_type2,
    {
      consumption_mpce_validation_2009_10_type2
      deflate_detailed_consumption_households(
        consumption_households_named_2009_10_type2, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2009_10_type2")
      )
    }
  ),
  tar_target(
    consumption_households_real_2011_12_type2,
    {
      consumption_mpce_validation_2011_12_type2
      deflate_detailed_consumption_households(
        consumption_households_named_2011_12_type2, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "nss_2011_12_type2")
      )
    }
  ),
  tar_target(
    consumption_households_real_hces_2022_23,
    {
      consumption_mpce_validation_hces_2022_23
      deflate_detailed_consumption_households(
        consumption_households_named_hces_2022_23, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "hces_2022_23")
      )
    }
  ),
  tar_target(
    consumption_households_real_hces_2023_24,
    {
      consumption_mpce_validation_hces_2023_24
      deflate_detailed_consumption_households(
        consumption_households_named_hces_2023_24, state_sector_price_deflators,
        consumption_survey_spec(consumption_survey_registry, "hces_2023_24")
      )
    }
  )
  )
}
