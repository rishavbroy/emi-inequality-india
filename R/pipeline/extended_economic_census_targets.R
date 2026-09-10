# Extended Economic Census source and measurement targets.

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
      diag_ext_economic_census_files,
      save_economic_census_diagnostics(economic_census_diagnostics),
      format = "file"
    )
  )
}
