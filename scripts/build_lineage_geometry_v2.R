# Build a compact Census 2001 district geometry from the local SHRID archive.
#
# This is intentionally separate from routine extended diagnostics because the
# raw polygon archive is large. The resulting compact GeoPackage is registered
# as a derived input on the next extended-diagnostic run.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make init-renv`.", call. = FALSE)
}
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required. Run `make restore`.", call. = FALSE)
}

targets::tar_source(
  list.files(
    "R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE
  )
)

sources <- targets::tar_read(district_lineage_v2_sources)
specs <- targets::tar_read(district_lineage_v2_specs)
census_2001_languages <- targets::tar_read(census_2001_languages)

geometry_row <- specs[specs$source_id == "shrug_shrid_geometry_zip", , drop = FALSE]
if (nrow(geometry_row) != 1L || !geometry_row$exists[[1]]) {
  stop("The local SHRID geometry ZIP is not available.", call. = FALSE)
}

bridge <- build_shrug_district_bridge(
  sources$shrug_pc01r, sources$shrug_pc01u,
  sources$shrug_pc11r, sources$shrug_pc11u,
  sources$shrug_pc01dist, sources$shrug_pc11dist
)
admin_2001 <- build_admin_registry_2001(census_2001_languages)
shrid_geometry <- read_zipped_gpkg_v2(geometry_row$absolute_path[[1]])
geometry_2001 <- dissolve_shrid_geometry_2001_v2(shrid_geometry, bridge)
paths <- save_lineage_geometry_2001_v2(geometry_2001, admin_2001)

message("Wrote Census 2001 geometry outputs:")
message(paste0("- ", paths, collapse = "\n"))
