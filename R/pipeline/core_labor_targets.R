# Publication-required NSS66 and PLFS labor targets.
# Conservative variants and diagnostic persistence remain extended.
core_labor_target_definitions <- function() {
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
      plfs_2017_18_usual_activity_source,
      read_plfs_2017_18_materialized_persons(
        plfs_2017_18_materialization,
        plfs_labor_contracts[plfs_labor_contracts$wave_id == "plfs_2017_18", , drop = FALSE]
      )
    ),
    tar_target(
      plfs_2017_18_lineaged_primary,
      attach_plfs_2017_18_reviewed_lineage(
        plfs_2017_18_usual_activity_source,
        district_lineage$primary_source_crosswalk,
        variant = "primary"
      )
    ),
    tar_target(
      plfs_2017_18_diagnostics,
      build_plfs_2017_18_diagnostics(
        plfs_2017_18_usual_activity_source,
        plfs_2017_18_lineaged_primary,
        "primary"
      )
    ),
    tar_target(
      plfs_2017_18_district_outcomes,
      estimate_nss_labor_district_outcomes(
        plfs_2017_18_lineaged_primary,
        plfs_2017_18_diagnostics$target_support,
        plfs_2017_18_outcome_registry(),
        label = "PLFS 2017-18 labor"
      )
    ),
    tar_target(
      plfs_2017_18_labor_mechanism,
      build_labor_mechanism_inference(
        district_panel,
        plfs_2017_18_district_outcomes$estimates,
        wave_id = "plfs_2017_18",
        cfg = cfg,
        control_registry = census_2001_control_registry
      )
    ),
    # Conservative reviewed-lineage PLFS is a final-paper Appendix-D sensitivity,
    # so analytical ownership is core; extended mode only persists its diagnostics.
    tar_target(
      plfs_2017_18_lineaged_conservative,
      attach_plfs_2017_18_reviewed_lineage(
        plfs_2017_18_usual_activity_source,
        district_lineage$conservative_source_crosswalk,
        variant = "deterministic"
      )
    ),
    tar_target(
      plfs_2017_18_conservative_diagnostics,
      build_plfs_2017_18_diagnostics(
        plfs_2017_18_usual_activity_source,
        plfs_2017_18_lineaged_conservative,
        "conservative"
      )
    ),
    tar_target(
      plfs_2017_18_conservative_district_outcomes,
      estimate_nss_labor_district_outcomes(
        plfs_2017_18_lineaged_conservative,
        plfs_2017_18_conservative_diagnostics$target_support,
        plfs_2017_18_outcome_registry(),
        label = "PLFS 2017-18 conservative labor"
      )
    ),
    tar_target(
      plfs_2017_18_conservative_labor_mechanism,
      build_labor_mechanism_inference(
        district_panel,
        plfs_2017_18_conservative_district_outcomes$estimates,
        wave_id = "plfs_2017_18",
        cfg = cfg,
        control_registry = census_2001_control_registry,
        sample_suffix = "conservative"
      )
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
      read_nss66_materialized_usual_activity(
        nss66_materialization, nss66_eus_ddi_contract, nss66_conversion_contract
      )
    ),
    tar_target(
      nss66_lineaged_usual_activity,
      attach_nss66_reviewed_lineage(
        nss66_usual_activity_source, consumption_lineage_bridge_2009_10_type2
      )
    ),
    tar_target(
      nss66_materialization_diagnostics,
      build_nesstar_materialization_diagnostics(nss66_materialization)
    ),
    tar_target(
      nss66_diagnostics,
      build_nss66_diagnostics(nss66_usual_activity_source, nss66_lineaged_usual_activity)
    ),
    tar_target(
      nss66_district_outcomes,
      estimate_nss_labor_district_outcomes(
        nss66_lineaged_usual_activity,
        nss66_diagnostics$target_support,
        nss66_outcome_registry(),
        label = "NSS66 labor"
      )
    ),
    tar_target(
      nss66_labor_mechanism,
      build_labor_mechanism_inference(
        district_panel,
        nss66_district_outcomes$estimates,
        wave_id = "nss66",
        cfg = cfg,
        control_registry = census_2001_control_registry
      )
    )
  )
}
