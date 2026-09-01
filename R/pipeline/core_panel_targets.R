# Production target declarations for this domain.
# Statistical and data-construction logic remains in the domain modules.
core_panel_target_definitions <- function() {
  list(
    tar_target(
      district_panel_conservative_provisional,
      build_lineage_district_panel(
        district_lineage$conservative_source_crosswalk,
        measures_2007,
        measures_2017,
        linguistic_distance_iv,
        lineage_geometry_2001,
        cfg
      )
    ),
    tar_target(
      conservative_gini_reconstruction,
      reconstruct_lineage_pooled_ginis(
        district_panel_conservative_provisional,
        district_lineage$conservative_source_crosswalk,
        nss_2007_education,
        nss_2017_education
      )
    ),
    tar_target(
      district_panel_primary_provisional,
      build_lineage_district_panel(
        district_lineage$primary_source_crosswalk,
        measures_2007,
        measures_2017,
        linguistic_distance_iv,
        lineage_geometry_2001,
        cfg
      )
    ),
    tar_target(
      primary_gini_reconstruction,
      reconstruct_lineage_pooled_ginis(
        district_panel_primary_provisional,
        district_lineage$primary_source_crosswalk,
        nss_2007_education,
        nss_2017_education
      )
    ),
    tar_target(
      district_panel_primary,
      finalize_analysis_panel(
        primary_gini_reconstruction$panel, census_2001_controls, cfg,
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      district_panel_conservative,
      finalize_analysis_panel(
        conservative_gini_reconstruction$panel, census_2001_controls, cfg,
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(district_panel, district_panel_primary),
    tar_target(processed_district_panel_file, save_processed_district_panel(district_panel), format = "file")
  )
}
