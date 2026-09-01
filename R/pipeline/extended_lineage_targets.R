# Lineage extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_lineage_target_definitions <- function() {
  list(
    tar_target(
      diag_ext_district_lineage,
      {
        district_panel_primary
        district_panel_full_reviewed
        revised_iv_models
        iv_models_full_reviewed
        revised_first_stage_tests
        first_stage_tests_full_reviewed
        diag_ext_lineage_panel_variants
        save_district_lineage(district_lineage)
      }
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
    ),
    tar_target(
      diag_ext_lineage_panel_variants,
      save_lineage_panel_variant_review(lineage_panel_variant_review)
    ),
    tar_target(legacy_iv_formulas, build_legacy_iv_formulas()),
    tar_target(
      iv_models_legacy,
      estimate_2sls(district_panel_legacy, legacy_iv_formulas, cfg)
    ),
    tar_target(
      iv_models_conservative_legacy_spec,
      estimate_2sls(district_panel_conservative, legacy_iv_formulas, cfg)
    ),
    tar_target(
      first_stage_tests_legacy,
      estimate_first_stage(iv_models_legacy, district_panel_legacy, cfg)
    ),
    tar_target(
      first_stage_tests_conservative_legacy_spec,
      estimate_first_stage(
        iv_models_conservative_legacy_spec,
        district_panel_conservative,
        cfg
      )
    ),
    tar_target(
      lineage_shared_support,
      build_lineage_shared_support(
        district_panel_legacy,
        district_panel_conservative
      )
    ),
    tar_target(
      iv_models_legacy_shared,
      estimate_2sls(
        lineage_shared_support$legacy,
        legacy_iv_formulas,
        cfg
      )
    ),
    tar_target(
      first_stage_tests_legacy_shared,
      estimate_first_stage(
        iv_models_legacy_shared,
        lineage_shared_support$legacy,
        cfg
      )
    ),
    tar_target(
      iv_models_lineage_shared,
      estimate_2sls(
        lineage_shared_support$lineage,
        legacy_iv_formulas,
        cfg
      )
    ),
    tar_target(
      first_stage_tests_lineage_shared,
      estimate_first_stage(
        iv_models_lineage_shared,
        lineage_shared_support$lineage,
        cfg
      )
    ),
    tar_target(
      lineage_downstream_review,
      build_lineage_downstream_review(
        district_panel_legacy,
        district_panel_conservative,
        iv_models_legacy,
        iv_models_conservative_legacy_spec,
        first_stage_tests_legacy,
        first_stage_tests_conservative_legacy_spec,
        district_lineage$full_reviewed_source_crosswalk,
        district_lineage$conservative_mapping_eligibility,
        iv_models_legacy_shared,
        iv_models_lineage_shared,
        first_stage_tests_legacy_shared,
        first_stage_tests_lineage_shared,
        lineage_shared_support$legacy,
        lineage_shared_support$lineage,
        district_lineage$admin_units_2001,
        district_lineage$adjudicated_allocation_weights,
        conservative_gini_reconstruction$audit
      )
    ),
    tar_target(
      diag_ext_lineage_downstream,
      save_lineage_downstream_review(lineage_downstream_review)
    ),
    tar_target(diag_ext_missingness, save_missingness_diagnostics(diagnose_missingness(selection_data, cfg))),
    tar_target(diag_ext_district_tracker_sources, save_tracker_source_diagnostics(diagnose_district_tracker_sources(raw_district_changes, district_tracker, cfg))),
    tar_target(diag_ext_district_matching, save_district_matching_diagnostics(diagnose_district_matching(district_panel, district_join_map, cfg))),
    tar_target(diag_ext_fuzzy_matching, save_fuzzy_matching_diagnostics(diagnose_fuzzy_matching(district_tracker, district_join_map, cfg))),
    tar_target(diag_ext_spatial_weights, save_spatial_weight_diagnostics(diagnose_spatial_weights(district_panel, spatial_weights, cfg))),
    tar_target(diag_ext_instrument_exploration, save_instrument_exploration_diagnostics(diagnose_instrument_exploration(district_panel, cfg))),
    tar_target(
      first_stage_absorption_diagnostics,
      diagnose_first_stage_absorption(
        district_panel, control_registry = census_2001_control_registry
      )
    ),
    tar_target(diag_ext_first_stage_absorption, save_first_stage_absorption_diagnostics(first_stage_absorption_diagnostics)),
    tar_target(
      glottolog_cldf_5_3,
      read_glottolog_cldf_5_3(glottolog_5_3$cldf_zip)
    ),
    tar_target(
      census_glottolog_match_candidates,
      build_census_glottolog_match_candidates(
        census_2001_languages, glottolog_5_3, glottolog_cldf_5_3,
        reviews = census_glottolog_crosswalk
      )
    ),
    tar_target(
      glottolog_language_distances,
      glottolog_language_distance_table(census_glottolog_crosswalk, glottolog_5_3)
    ),
    tar_target(
      shastry_extension_candidates,
      build_shastry_extension_candidates(
        census_glottolog_crosswalk,
        glottolog_5_3,
        historical_linguistic_sources,
        concordance = shastry_language_distance,
        lexical_index = lexical_language_index,
        adjudications = shastry_language_adjudications
      )
    ),
    tar_target(
      diag_ext_dyen_hindi_cognates,
      save_dyen_hindi_cognates(historical_linguistic_sources$dyen_hindi),
      format = "file"
    ),
    tar_target(
      diag_ext_asjp_review_anchor_distances,
      save_asjp_review_anchor_distances(
        historical_linguistic_sources$asjp_review_anchor_distances
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_asjp_review_summary,
      save_asjp_review_summary(historical_linguistic_sources$asjp_review_summary),
      format = "file"
    ),
    tar_target(
      diag_ext_shastry_extension_candidates,
      save_shastry_extension_candidates(shastry_extension_candidates),
      format = "file"
    ),
    tar_target(
      diag_ext_glottolog_language_distances,
      save_glottolog_language_distance_table(glottolog_language_distances),
      format = "file"
    ),
    tar_target(
      diag_ext_census_glottolog_match_candidates,
      save_census_glottolog_match_candidates(census_glottolog_match_candidates),
      format = "file"
    )
  )
}
