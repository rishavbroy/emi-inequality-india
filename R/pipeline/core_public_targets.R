# Production target declarations for this domain.
# Statistical and data-construction logic remains in the domain modules.
core_public_target_definitions <- function() {
  list(
    tar_target(
      public_iv_specifications,
      public_iv_specification_registry(census_2001_control_registry)
    ),
    tar_target(
      revised_iv_formulas,
      iv_specification_formulas(public_iv_specifications)
    ),
    tar_target(revised_iv_models, estimate_2sls(district_panel, revised_iv_formulas, cfg)),
    tar_target(revised_first_stage_tests, estimate_first_stage(revised_iv_models, district_panel, cfg)),
    tar_target(diag_public_weak_instruments, diagnose_weak_instruments(revised_iv_models, district_panel, cfg)),
    tar_target(
      diag_public_anderson_rubin,
      save_candidate_anderson_rubin(
        diagnose_candidate_anderson_rubin(district_panel)
      ),
      format = "file"
    ),
    tar_target(diag_public_overidentification, diagnose_overidentification(revised_iv_models, public_iv_specifications, cfg)),
    tar_target(
      first_stage_absorption_diagnostics,
      diagnose_first_stage_absorption(
        district_panel, control_registry = census_2001_control_registry
      )
    ),

    tar_target(spatial_weights, build_spatial_weights(district_panel, cfg)),
    tar_target(diag_public_spatial_autocorrelation, diagnose_spatial_autocorrelation(district_panel, revised_iv_models, spatial_weights, cfg)),
    tar_target(diag_public_spatial_autocorrelation_files, save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = "file"),
    tar_target(diag_public_multicollinearity, save_multicollinearity_diagnostics(diagnose_multicollinearity(district_panel, revised_iv_models, cfg)), format = "file"),

    tar_target(
      nss64_schooling_social_group_margins,
      build_education_exposure_2007_by_social_group(selection_data)
    ),
    tar_target(
      nss64_schooling_social_group_crosscut_margins,
      build_education_exposure_2007_by_social_group_crosscut(selection_data)
    ),
    tar_target(
      nss64_schooling_social_group_diagnostic,
      build_nss64_schooling_social_group_diagnostic(
        nss64_schooling_social_group_margins,
        district_panel,
        census_2001_control_registry,
        nss64_schooling_social_group_crosscut_margins
      )
    ),

    tar_target(
      figures,
      make_figures(
        district_panel, raw_ilo_figures, cfg,
        iv_models = revised_iv_models,
        map_geometry = lineage_geometry_2001,
        consumption_iv_dynamics = consumption_iv_dynamics,
        schooling_access = nss64_schooling_social_group_diagnostic,
        first_stage_absorption = first_stage_absorption_diagnostics
      )
    ),
    tar_target(figure_files, save_figures(figures, cfg), format = "file"),
    tar_target(
      paper_schooling_market_geography,
      make_paper_schooling_market_geography_table(
        english_opportunity_district_mechanisms,
        district_panel_with_dise
      )
    ),
    tar_target(
      paper_language_behavior,
      make_paper_language_behavior_table(census_2001_c17_mechanism)
    ),
    tar_target(
      paper_economic_conversion,
      make_paper_economic_conversion_table(
        schooling_consumption_bridge,
        schooling_consumption_conversion
      )
    ),
    tar_target(
      paper_local_development,
      make_paper_local_development_table(
        census_household_capacity,
        census_migration_diagnostics,
        census_housing_diagnostics,
        economic_census_diagnostics,
        nss66_labor_mechanism,
        plfs_2017_18_labor_mechanism
      )
    ),
    tar_target(
      paper_identification_boundary,
      make_paper_identification_boundary_table(
        alternative_distance_first_stage_base,
        consumption_iv_dynamics
      )
    ),
    tar_target(
      appendix_data_construction_exhibits,
      make_appendix_data_construction_exhibits(district_lineage, consumption_district_welfare)
    ),
    tar_target(
      appendix_data_construction_files,
      save_appendix_data_construction_exhibits(appendix_data_construction_exhibits, cfg),
      format = "file"
    ),
    tar_target(
      appendix_validation_identification_exhibits,
      make_appendix_validation_identification_exhibits(
        district_lineage, census_1991_primary_validation, historical_linguistic_persistence_validation,
        helms_lim_linguistic_distance_benchmark, district_panel, district_panel_with_dise,
        dise_iv_nss_validation, lineage_panel_variant_review,
        census_migration_diagnostics, census_housing_diagnostics, census_household_diagnostics,
        census_worker_diagnostics, historical_baseline_balance_1991, historical_linguistic_first_stage_robustness
      )
    ),
    tar_target(
      appendix_validation_identification_files,
      save_appendix_validation_identification_exhibits(
        appendix_validation_identification_exhibits, cfg
      ),
      format = "file"
    ),
    tar_target(
      appendix_identification_exhibits,
      make_appendix_identification_exhibits(
        first_stage_absorption_diagnostics,
        district_panel,
        hindi_belt_first_stage_diagnostics,
        child_population_first_stage_diagnostics,
        alternative_distance_first_stage_base,
        alternative_distance_measurement_diagnostics,
        alternative_distance_first_stages,
        consumption_iv_dynamics,
        historical_vanneman_pretrend_validation,
        consumption_robustness_evidence,
        consumption_exclusion_sensitivity,
        census_2001_control_registry
      )
    ),
    tar_target(
      appendix_identification_files,
      save_appendix_identification_exhibits(appendix_identification_exhibits, cfg),
      format = "file"
    ),
    tar_target(
      appendix_local_development_exhibits,
      make_appendix_local_development_exhibits(
        census_migration_diagnostics, census_housing_diagnostics, economic_census_diagnostics,
        nss66_labor_mechanism, plfs_2017_18_labor_mechanism,
        plfs_2017_18_conservative_labor_mechanism, census_household_capacity,
        nss64_schooling_social_group_diagnostic, english_opportunity_st_heterogeneity,
        diag_public_spatial_autocorrelation
      )
    ),
    tar_target(
      appendix_local_development_files,
      save_appendix_local_development_exhibits(appendix_local_development_exhibits, figure_files, cfg),
      format = "file"
    ),
    tar_target(
      appendix_selection_exhibits,
      make_appendix_selection_exhibits(
        selection_data, selection_missingness_diagnostics
      )
    ),
    tar_target(
      appendix_selection_files,
      save_appendix_selection_exhibits(appendix_selection_exhibits, cfg),
      format = "file"
    ),
    tar_target(
      tables,
      {
        out <- make_tables(
          selection_data, ame_results, district_panel, revised_iv_models,
          revised_first_stage_tests, cfg, selection_model, consumption_district_welfare,
          schooling_consumption_bridge
        )
        out$paper_schooling_market_geography <- paper_schooling_market_geography
        out$paper_language_behavior <- paper_language_behavior
        out$paper_economic_conversion <- paper_economic_conversion
        out$paper_local_development <- paper_local_development
        out$paper_identification_boundary <- paper_identification_boundary
        out
      }
    ),
    tar_target(diag_public_iv_panel, save_public_iv_panel_diagnostics(district_panel, tables), format = "file"),
    tar_target(table_files, save_tables(tables, cfg), format = "file"),
    tar_target(report_values, { diag_public_spatial_autocorrelation_files; build_report_values(ame_results, revised_first_stage_tests, revised_iv_models, selection_data, district_panel, diag_public_spatial_autocorrelation, cfg) }),
    tar_target(paper_qmd, "paper/paper.qmd", format = "file"),
    tar_target(paper_new_qmd, "paper/paper-new.qmd", format = "file"),
    tar_target(poster_qmd, "posters/2026_predoc_conference/poster.qmd", format = "file"),
    tar_target(poster_assets, poster_required_assets(), format = "file"),

    tar_target(paper, render_paper_pdf(paper_qmd, report_values, figure_files, table_files), format = "file"),
    tar_target(
      paper_new,
      render_public_pdf(
        paper_new_qmd,
        dependencies = list(
          report_values, table_files, figure_files, dise_publication_validation,
          appendix_data_construction_files, appendix_validation_identification_files,
          appendix_identification_files, appendix_local_development_files,
          appendix_selection_files
        )
      ),
      format = "file"
    ),
    tar_target(poster, render_poster_pdf(poster_qmd, figure_files, poster_assets, paths$root), format = "file")
  )
}
