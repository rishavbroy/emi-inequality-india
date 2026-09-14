# Identification results required by the final paper.
#
# Keep this factory narrow: it owns the alternative linguistic-distance first
# stages used by the main identification table and the compact IV appendix.
# Richer measurement and multi-instrument calculations augment the same
# registered specifications so publication summaries do not duplicate models.
core_identification_target_definitions <- function() {
  list(
    tar_target(
      alternative_distance_analysis_panel,
      prepare_alternative_distance_panel(
        district_panel, control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      alternative_distance_spec_registry,
      alternative_distance_registry(
        control_registry = census_2001_control_registry
      ),
      iteration = "list"
    ),
    tar_target(
      alternative_distance_specification,
      split(
        alternative_distance_spec_registry,
        seq_len(nrow(alternative_distance_spec_registry))
      ),
      iteration = "list"
    ),
    tar_target(
      alternative_distance_spec_diagnostic,
      diagnose_alternative_distance_specification(
        alternative_distance_analysis_panel,
        alternative_distance_specification
      ),
      pattern = map(alternative_distance_specification),
      iteration = "list"
    ),
    tar_target(
      alternative_distance_first_stage_base,
      assemble_alternative_distance_first_stages(
        alternative_distance_analysis_panel,
        alternative_distance_spec_registry,
        alternative_distance_spec_diagnostic
      )
    ),
    tar_target(
      alternative_distance_measurement_diagnostics,
      augment_alternative_distance_measurement_diagnostics(
        alternative_distance_first_stage_base,
        district_panel,
        census_2001_languages,
        glottolog = glottolog_5_3,
        glottolog_crosswalk = census_glottolog_crosswalk
      )
    ),
    tar_target(
      alternative_distance_augmentation_panel,
      project_alternative_distance_panel(
        district_panel,
        retain = "real_log_consumption_change",
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      alternative_distance_first_stages,
      augment_alternative_distance_inference_diagnostics(
        alternative_distance_measurement_diagnostics,
        alternative_distance_augmentation_panel
      )
    )
  )
}
