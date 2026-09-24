# Extended labor-market source and reviewed-lineage targets.

extended_labor_target_definitions <- function() {
  list(
    # Conservative-lineage PLFS analysis is core-owned analytical output.
    # Extended mode persists the shared analytical object below.
    tar_target(
      diag_ext_plfs_2017_18_labor_mechanism_files,
      save_labor_mechanism_inference(plfs_2017_18_labor_mechanism, "plfs_2017_18"),
      format = "file"
    ),
    # Outcome and mechanism registries define estimands, not lineage support.
    # Persist them once with the primary PLFS variant; conservative outputs
    # retain only the variant-specific samples and estimates.
    tar_target(
      diag_ext_plfs_2017_18_conservative_labor_mechanism_files,
      save_labor_mechanism_inference(
        plfs_2017_18_conservative_labor_mechanism, "plfs_2017_18_conservative",
        include_registry = FALSE
      ),
      format = "file"
    ),
    tar_target(
      plfs_2017_18_variant_comparison,
      build_labor_variant_comparison(
        plfs_2017_18_district_outcomes$estimates,
        plfs_2017_18_conservative_district_outcomes$estimates
      )
    ),
    tar_target(
      diag_ext_plfs_2017_18_source_validation_files,
      c(
        save_plfs_2017_18_diagnostics(
          plfs_2017_18_diagnostics, plfs_2017_18_district_outcomes
        ),
        save_nss_labor_diagnostics(
          plfs_2017_18_conservative_diagnostics,
          "plfs_2017_18_conservative",
          plfs_2017_18_conservative_district_outcomes,
          include_registry = FALSE
        )
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_plfs_2017_18_variant_comparison_file,
      save_labor_variant_comparison(
        plfs_2017_18_variant_comparison,
        "plfs_2017_18_variant_comparison.csv"
      )
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
      diag_ext_nss66_materialization_files,
      save_nesstar_materialization_diagnostics(
        nss66_materialization_diagnostics, "nss66_materialization.csv"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_nss66_labor_mechanism_files,
      save_labor_mechanism_inference(nss66_labor_mechanism, "nss66"),
      format = "file"
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
