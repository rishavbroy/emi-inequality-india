# Extended Economic Census source and measurement targets.

manifest_file_by_id <- function(paths, source_id, file_id, label) {
  rows <- require_manifest_files(paths, source_id = source_id, required_only = FALSE)
  row <- rows[rows$file_id == file_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Expected one ", label, " manifest row.", call. = FALSE)
  row$absolute_path[[1L]]
}

extended_economic_census_target_definitions <- function() {
  list(
    tar_target(
      economic_census_ec13_ddi_file,
      manifest_file_by_id(paths, "economic_census_raw", "ec13_ddi_xml", "Sixth Economic Census DDI"),
      format = "file"
    ),
    tar_target(
      economic_census_ec13_raw_contract,
      read_economic_census_ddi_contract(economic_census_ec13_ddi_file)
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
      diag_ext_economic_census_2005_files,
      save_economic_census_2005_diagnostics(economic_census_2005_district_measures),
      format = "file"
    ),
    tar_target(
      diag_ext_economic_census_2013_files,
      save_economic_census_2013_diagnostics(economic_census_2013_district_measures),
      format = "file"
    ),
    tar_target(
      diag_ext_economic_census_change_files,
      save_economic_census_change_diagnostics(economic_census_2005_2013_changes),
      format = "file"
    )
  )
}
