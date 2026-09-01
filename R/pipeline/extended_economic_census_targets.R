# Extended Economic Census source and measurement targets.

extended_economic_census_target_definitions <- function() {
  list(
    tar_target(
      economic_census_ec13_ddi_file,
      {
        rows <- require_manifest_files(
          paths,
          source_id = "economic_census_raw",
          required_only = FALSE
        )
        row <- rows[rows$file_id == "ec13_ddi_xml", , drop = FALSE]
        if (nrow(row) != 1L) {
          stop("Expected one Sixth Economic Census DDI manifest row.", call. = FALSE)
        }
        row$absolute_path[[1L]]
      },
      format = "file"
    ),
    tar_target(
      economic_census_ec13_raw_contract,
      read_economic_census_ddi_contract(economic_census_ec13_ddi_file)
    ),
    tar_target(
      shrug_ec05_archive,
      {
        rows <- require_manifest_files(
          paths,
          source_id = "shrug_economic_census",
          required_only = FALSE
        )
        row <- rows[rows$file_id == "shrug_ec05_csv_archive", , drop = FALSE]
        if (nrow(row) != 1L) {
          stop("Expected one SHRUG EC05 archive manifest row.", call. = FALSE)
        }
        row$absolute_path[[1L]]
      },
      format = "file"
    ),
    tar_target(
      shrug_ec05_district_source,
      read_shrug_ec05_district(shrug_ec05_archive)
    ),
    tar_target(
      economic_census_2005_district_measures,
      build_economic_census_2005_measures(
        shrug_ec05_district_source,
        district_lineage$admin_units_2001
      )
    ),
    tar_target(
      diag_ext_economic_census_2005_files,
      save_economic_census_2005_diagnostics(economic_census_2005_district_measures),
      format = "file"
    )
  )
}
