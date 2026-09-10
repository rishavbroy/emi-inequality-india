# Publication-required identification diagnostics.
#
# Keep this factory narrow: it owns identification evidence consumed by the main
# paper or final Appendix C, including bounded geography/scale and linguistic-
# measurement diagnostics. Weak-IV outcome grids, monotonicity, FAS, and other
# forensic inference remain extended and augment this publication base.
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
      hindi_belt_first_stage_diagnostics,
      diagnose_hindi_belt_first_stage(
        district_panel, control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      child_population_first_stage_diagnostics,
      diagnose_child_population_first_stage(
        district_panel, control_registry = census_2001_control_registry
      )
    )
  )
}
