# Production target declarations for this domain.
# Statistical and data-construction logic remains in the domain modules.
core_lineage_target_definitions <- function() {
  list(
    tar_target(
      district_lineage_specs,
      district_lineage_input_specs(paths),
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      district_lineage_inventory,
      district_lineage_source_inventory(district_lineage_specs)
    ),
    tar_target(
      district_lineage_source_specs,
      split_district_lineage_source_specs(district_lineage_specs),
      iteration = "list"
    ),
    tar_target(
      district_lineage_source_file,
      district_lineage_source_path(district_lineage_source_specs),
      pattern = map(district_lineage_source_specs),
      format = "file"
    ),
    tar_target(
      district_lineage_source,
      read_district_lineage_source(
        district_lineage_source_specs,
        district_lineage_source_file
      ),
      pattern = map(district_lineage_source_specs, district_lineage_source_file),
      iteration = "list"
    ),
    tar_target(
      district_lineage_raw_sources,
      assemble_district_lineage_sources(district_lineage_source)
    ),
    tar_target(
      district_lineage_sources,
      attach_lineage_geometry_source(
        district_lineage_raw_sources,
        lineage_geometry_2001
      )
    ),
    tar_target(
      district_lineage,
      build_district_lineage(
        district_lineage_sources,
        district_lineage_inventory,
        census_2001_languages,
        measures_2007,
        measures_2017
      )
    ),
    tar_target(
      district_transition_2001_2011,
      district_lineage$district_transition_2001_2011
    ),
    tar_target(
      district_panel_full_reviewed_provisional,
      build_lineage_district_panel(
        district_lineage$full_reviewed_source_crosswalk,
        measures_2007,
        measures_2017,
        linguistic_distance_iv,
        lineage_geometry_2001,
        cfg
      )
    ),
    tar_target(
      full_reviewed_gini_reconstruction,
      reconstruct_lineage_pooled_ginis(
        district_panel_full_reviewed_provisional,
        district_lineage$full_reviewed_source_crosswalk,
        nss_2007_education,
        nss_2017_education
      )
    ),
    tar_target(
      district_panel_full_reviewed,
      attach_census_2001_controls(
        full_reviewed_gini_reconstruction$panel,
        census_2001_controls
      )
    ),
    tar_target(
      iv_models_conservative,
      estimate_2sls(district_panel_conservative, revised_iv_formulas, cfg)
    ),
    tar_target(
      first_stage_tests_conservative,
      estimate_first_stage(iv_models_conservative, district_panel_conservative, cfg)
    ),
    tar_target(
      iv_models_full_reviewed,
      estimate_2sls(district_panel_full_reviewed, revised_iv_formulas, cfg)
    ),
    tar_target(
      first_stage_tests_full_reviewed,
      estimate_first_stage(
        iv_models_full_reviewed,
        district_panel_full_reviewed,
        cfg
      )
    ),
    tar_target(
      lineage_panel_variant_review,
      build_lineage_panel_variant_review(
        panels = list(
          conservative = district_panel_conservative,
          primary = district_panel_primary,
          full_reviewed = district_panel_full_reviewed
        ),
        models = list(
          conservative = iv_models_conservative,
          primary = revised_iv_models,
          full_reviewed = iv_models_full_reviewed
        ),
        first_stage_tests = list(
          conservative = first_stage_tests_conservative,
          primary = revised_first_stage_tests,
          full_reviewed = first_stage_tests_full_reviewed
        ),
        gini_audits = list(
          conservative = conservative_gini_reconstruction$audit,
          primary = primary_gini_reconstruction$audit,
          full_reviewed = full_reviewed_gini_reconstruction$audit
        )
      )
    )
  )
}
