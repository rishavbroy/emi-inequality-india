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
    )
  )
}
