# Extended labor-market source and reviewed-lineage targets.

extended_labor_target_definitions <- function() {
  list(
    tar_target(
      nss64_eum_ddi_file,
      manifest_file_by_id(paths, "nss_2007_08_employment_migration", "nss64_eum_ddi", "NSS64 EUM DDI"),
      format = "file"
    ),
    tar_target(
      nss64_eum_block4_file,
      manifest_file_by_id(paths, "nss_2007_08_employment_migration", "nss64_eum_block4", "NSS64 Block 4"),
      format = "file"
    ),
    tar_target(
      nss64_eum_block6_file,
      manifest_file_by_id(paths, "nss_2007_08_employment_migration", "nss64_eum_block6", "NSS64 Block 6"),
      format = "file"
    ),
    tar_target(nss64_eum_ddi_contract, read_nss64_eum_ddi_contract(nss64_eum_ddi_file)),
    tar_target(nss64_usual_activity_source, read_nss64_usual_activity(nss64_eum_block4_file)),
    tar_target(nss64_migration_source, read_nss64_migration(nss64_eum_block6_file)),
    tar_target(
      nss64_lineaged_usual_activity,
      attach_nss64_reviewed_lineage(
        nss64_usual_activity_source,
        district_lineage$full_reviewed_source_crosswalk
      )
    ),
    tar_target(
      nss64_diagnostics,
      build_nss64_source_diagnostics(
        nss64_usual_activity_source,
        nss64_migration_source,
        nss64_eum_ddi_contract,
        nss64_lineaged_usual_activity
      )
    ),
    tar_target(
      diag_ext_nss64_files,
      save_nss64_diagnostics(nss64_diagnostics),
      format = "file"
    )
  )
}
