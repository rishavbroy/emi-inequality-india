# Production target declarations for this domain.
# Statistical and data-construction logic remains in the domain modules.
core_measurement_target_definitions <- function() {
  list(
    tar_target(raw_nss_2007_education, { raw_data_preflight; read_nss_2007_education(paths) }),
    tar_target(raw_nss_2017_education, { raw_data_preflight; read_nss_2017_education(paths) }),
    tar_target(raw_census_2001, { raw_data_preflight; read_census_2001_mother_tongue(paths) }),
    tar_target(glottolog_5_3, { raw_data_preflight; read_glottolog_5_3(paths) }),
    tar_target(historical_linguistic_sources, { raw_data_preflight; read_historical_linguistic_sources(paths) }),
    tar_target(
      census_glottolog_crosswalk_file,
      path_metadata(paths, "census_language_glottolog_crosswalk.csv"),
      format = "file"
    ),
    tar_target(
      census_glottolog_crosswalk,
      read_census_language_glottolog_crosswalk(census_glottolog_crosswalk_file)
    ),
    tar_target(
      shastry_language_distance_file,
      path_metadata(paths, "shastry_language_distance.csv"),
      format = "file"
    ),
    tar_target(
      shastry_language_distance,
      read_shastry_language_distance(shastry_language_distance_file)
    ),
    tar_target(
      shastry_language_adjudications_file,
      path_metadata(paths, "shastry_language_adjudications.csv"),
      format = "file"
    ),
    tar_target(
      shastry_language_adjudications,
      read_shastry_language_adjudications(shastry_language_adjudications_file)
    ),
    tar_target(
      lexical_language_index_file,
      path_metadata(paths, "lexical_language_index.csv"),
      format = "file"
    ),
    tar_target(lexical_language_index, read_lexical_language_index(lexical_language_index_file)),
    tar_target(raw_ilo_figures, { raw_data_preflight; list_ilo_figure_paths(paths) }, format = "file"),
    tar_target(
      raw_price_sources,
      {
        raw_data_preflight
        read_price_sources(
          price_source_paths(paths),
          cpi_iw_estimation_start = consumption_price_window$start_period[[1L]]
        )
      }
    ),
    tar_target(raw_census_2001_controls, { raw_data_preflight; read_census_2001_control_sources(paths) }),

    tar_target(nss_2007_education, clean_nss_2007_education(raw_nss_2007_education)),
    tar_target(nss_2007_consumption, clean_nss_2007_consumption(raw_nss_2007_consumption)),
    tar_target(nss_2017_education, clean_nss_2017_education(raw_nss_2017_education)),
    tar_target(census_2001_languages, clean_census_2001_languages(raw_census_2001)),
    tar_target(
      consumption_price_window,
      registered_consumption_price_window(consumption_survey_registry)
    ),
    tar_target(
      temporal_price_series,
      build_temporal_price_series(raw_price_sources)
    ),
    tar_target(price_reference_index, build_ruc_reference_index(raw_price_sources)),
    tar_target(
      state_sector_price_deflators,
      validate_consumption_price_window(
        build_state_sector_price_deflators(
          temporal_price_series,
          reference_index = price_reference_index,
          start_period = consumption_price_window$start_period[[1L]],
          end_period = consumption_price_window$end_period[[1L]],
          require_complete_grid = FALSE
        ),
        consumption_price_window
      )
    ),

    tar_target(district_keys_2001, build_district_keys_2001(census_2001_languages)),
    tar_target(district_keys_2007, build_district_keys_2007(nss_2007_education, nss_2007_consumption)),
    tar_target(district_keys_2017, build_district_keys_2017(nss_2017_education)),

    tar_target(selection_data, build_selection_data(nss_2007_education, district_keys_2007, cfg)),
    tar_target(selection_model_data, project_selection_model_data(selection_data)),
    tar_target(selection_model, estimate_selection_probit(selection_model_data, cfg)),
    tar_target(ame_results, compute_average_marginal_effects(selection_model, cfg)),

    tar_target(
      consumption_households_2007,
      prepare_2007_consumption_households(
        nss_2007_education, state_sector_price_deflators, consumption_price_spec_2007_legacy
      )
    ),
    tar_target(
      consumption_households_2017,
      prepare_2017_consumption_households(
        nss_2017_education, state_sector_price_deflators, consumption_price_spec_2017_legacy
      )
    ),
    tar_target(
      measures_2007,
      build_2007_measures(
        nss_2007_education, nss_2007_consumption, cfg,
        consumption_households_2007, selection_data
      )
    ),
    tar_target(
      measures_2017,
      build_2017_measures(nss_2017_education, cfg, consumption_households_2017)
    ),
    tar_target(
      linguistic_distance_iv,
      build_linguistic_distance_iv(
        census_2001_languages,
        cfg,
        glottolog_5_3,
        census_glottolog_crosswalk,
        historical_linguistic_sources,
        shastry_adjudications = shastry_language_adjudications,
        shastry_concordance = shastry_language_distance,
        lexical_index = lexical_language_index
      )
    ),
    tar_target(
      lineage_geometry_2001_file,
      lineage_geometry_2001_path(paths),
      format = "file"
    ),
    tar_target(
      lineage_geometry_2001,
      read_lineage_geometry_2001(lineage_geometry_2001_file)
    ),
    tar_target(
      census_2001_district_totals,
      build_census_2001_district_totals(raw_census_2001_controls, lineage_geometry_2001)
    ),
    tar_target(
      census_2001_controls,
      {
        controls <- build_census_2001_controls(census_2001_district_totals)
        expected_keys <- clean_shrug_pca_2001_district(raw_census_2001_controls$shrug_pca)
        validate_census_2001_controls(
          controls,
          expected_keys = expected_keys[census_2001_keys()]
        )
        controls
      }
    ),
    tar_target(
      census_2001_source_coverage,
      summarise_census_2001_source_coverage(raw_census_2001_controls, lineage_geometry_2001)
    )
  )
}
