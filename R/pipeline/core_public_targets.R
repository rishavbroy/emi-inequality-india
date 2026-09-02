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

    tar_target(spatial_weights, build_spatial_weights(district_panel, cfg)),
    tar_target(diag_public_spatial_autocorrelation, diagnose_spatial_autocorrelation(district_panel, revised_iv_models, spatial_weights, cfg)),
    tar_target(diag_public_spatial_autocorrelation_files, save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = "file"),
    tar_target(diag_public_multicollinearity, save_multicollinearity_diagnostics(diagnose_multicollinearity(district_panel, revised_iv_models, cfg)), format = "file"),

    tar_target(
      figures,
      make_figures(
        district_panel, raw_ilo_figures, cfg,
        iv_models = revised_iv_models,
        map_geometry = lineage_geometry_2001,
        consumption_iv_dynamics = consumption_iv_dynamics
      )
    ),
    tar_target(figure_files, save_figures(figures, cfg), format = "file"),
    tar_target(tables, make_tables(selection_data, ame_results, district_panel, revised_iv_models, revised_first_stage_tests, cfg, selection_model)),
    tar_target(diag_public_iv_panel, save_public_iv_panel_diagnostics(district_panel, tables), format = "file"),
    tar_target(table_files, save_tables(tables, cfg), format = "file"),
    tar_target(report_values, { diag_public_spatial_autocorrelation_files; build_report_values(ame_results, revised_first_stage_tests, revised_iv_models, selection_data, district_panel, diag_public_spatial_autocorrelation, cfg) }),
    tar_target(report_qmd, "paper/report.qmd", format = "file"),
    tar_target(poster_qmd, "posters/2026_predoc_conference/poster.qmd", format = "file"),
    tar_target(poster_assets, poster_required_assets(), format = "file"),
    tar_target(district_matching_qmd, "docs/district-matching.qmd", format = "file"),
    tar_target(long_paths_qmd, "docs/long-paths-and-8-3-filenames.qmd", format = "file"),

    tar_target(district_matching_note, render_public_html(district_matching_qmd, dependencies = list(report_values)), format = "file"),
    tar_target(long_paths_note, render_public_html(long_paths_qmd), format = "file"),
    tar_target(report, render_report_pdf(report_qmd, report_values, figure_files, table_files), format = "file"),
    tar_target(poster, render_poster_pdf(poster_qmd, figure_files, poster_assets, paths$root), format = "file")
  )
}
