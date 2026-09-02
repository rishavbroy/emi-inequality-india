# Cross-family post-treatment mechanism evidence targets.
# Source-specific modules remain authoritative for measurement and estimation;
# this factory only summarizes their common weak-IV inference contract.

extended_mechanism_target_definitions <- function() {
  list(
    tar_target(
      posttreatment_mechanism_evidence,
      build_posttreatment_mechanism_evidence(list(
        census_migration = list(
          result = census_migration_diagnostics,
          temporal_role = "long_run_post",
          analysis_role = "causal_mechanism"
        ),
        census_housing = list(
          result = census_housing_diagnostics,
          temporal_role = "long_run_post",
          analysis_role = "causal_mechanism"
        ),
        economic_census = list(
          result = economic_census_diagnostics,
          temporal_role = "post_treatment_change_2005_2013",
          analysis_role = "causal_mechanism"
        ),
        nss66_labor = list(
          result = nss66_labor_mechanism,
          temporal_role = "early_post",
          analysis_role = "causal_mechanism"
        ),
        plfs_2017_18_labor = list(
          result = plfs_2017_18_labor_mechanism,
          temporal_role = "long_run_post",
          analysis_role = "causal_mechanism"
        ),
        plfs_2017_18_conservative_labor = list(
          result = plfs_2017_18_conservative_labor_mechanism,
          temporal_role = "long_run_post",
          analysis_role = "geography_robustness"
        )
      ))
    ),
    tar_target(
      diag_ext_posttreatment_mechanism_evidence_files,
      save_posttreatment_mechanism_evidence(posttreatment_mechanism_evidence),
      format = "file"
    )
  )
}
