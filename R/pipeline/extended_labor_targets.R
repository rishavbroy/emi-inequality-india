# Extended labor-market source and reviewed-lineage targets.

extended_labor_target_definitions <- function() {
  list(
    tar_target(
      plfs_labor_contract_file,
      path_project(paths, "data/metadata/plfs_labor_contracts.csv"),
      format = "file"
    ),
    tar_target(plfs_labor_contracts, read_plfs_labor_contracts(plfs_labor_contract_file)),
    tar_target(
      plfs_2017_18_source_package,
      inspect_plfs_2017_18_source_package(paths),
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      plfs_2017_18_conversion_contract,
      read_nesstar_conversion_contract("plfs_2017_18")
    ),
    tar_target(
      plfs_2017_18_materialization,
      inspect_nesstar_materialization(plfs_2017_18_conversion_contract),
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      plfs_2017_18_materialization_diagnostics,
      build_nesstar_materialization_diagnostics(plfs_2017_18_materialization)
    ),
    tar_target(
      diag_ext_plfs_2017_18_source_package_file,
      save_plfs_source_package_diagnostics(plfs_2017_18_source_package),
      format = "file"
    ),
    tar_target(
      diag_ext_plfs_2017_18_materialization_file,
      save_nesstar_materialization_diagnostics(
        plfs_2017_18_materialization_diagnostics, "plfs_2017_18_materialization.csv"
      ),
      format = "file"
    ),
    tar_target(
      nss66_eus_ddi_file,
      manifest_file_by_id(paths, "nss_2009_10_employment", "nss66_eus_ddi", "NSS66 EUS DDI"),
      format = "file"
    ),
    tar_target(
      nss66_eus_ddi_contract,
      read_nss66_eus_ddi_contract(nss66_eus_ddi_file)
    ),
    tar_target(
      nss66_conversion_contract,
      read_nss66_conversion_contract()
    ),
    tar_target(
      nss66_materialization,
      inspect_nesstar_materialization(nss66_conversion_contract),
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      nss66_usual_activity_source,
      if (isTRUE(nss66_materialization$ready)) {
        read_nss66_materialized_usual_activity(
          nss66_materialization, nss66_eus_ddi_contract, nss66_conversion_contract
        )
      } else {
        NULL
      }
    ),
    tar_target(
      nss66_lineaged_usual_activity,
      if (is.null(nss66_usual_activity_source)) {
        NULL
      } else {
        attach_nss66_reviewed_lineage(
          nss66_usual_activity_source, consumption_lineage_bridge_2009_10_type2
        )
      }
    ),
    tar_target(
      nss66_materialization_diagnostics,
      build_nesstar_materialization_diagnostics(nss66_materialization)
    ),
    tar_target(
      diag_ext_nss66_materialization_files,
      save_nesstar_materialization_diagnostics(
        nss66_materialization_diagnostics, "nss66_materialization.csv"
      ),
      format = "file"
    ),
    tar_target(
      nss66_diagnostics,
      if (is.null(nss66_usual_activity_source)) {
        NULL
      } else {
        build_nss66_diagnostics(nss66_usual_activity_source, nss66_lineaged_usual_activity)
      }
    ),
    tar_target(
      nss66_district_outcomes,
      if (is.null(nss66_diagnostics)) {
        NULL
      } else {
        estimate_nss_labor_district_outcomes(
          nss66_lineaged_usual_activity,
          nss66_diagnostics$target_support,
          nss66_outcome_registry(),
          label = "NSS66 labor"
        )
      }
    ),
    tar_target(
      diag_ext_nss66_files,
      save_nss66_diagnostics(
        nss66_diagnostics, nss66_district_outcomes,
        fallback_path = diag_ext_nss66_materialization_files
      ),
      format = "file"
    ),
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
      nss64_district_outcomes,
      estimate_nss64_district_outcomes(
        nss64_lineaged_usual_activity,
        nss64_migration_source,
        nss64_diagnostics$target_support
      )
    ),
    tar_target(
      diag_ext_nss64_files,
      save_nss64_diagnostics(nss64_diagnostics, nss64_district_outcomes),
      format = "file"
    )
  )
}
