# Build the canonical Census-2001 district geometry from DataMeet boundaries.
#
# The raw shapefile is converted to the compact GeoPackage consumed by maps,
# spatial weights, and Moran diagnostics. SHRUG remains a lineage-transition
# source; it is no longer used to draw production district boundaries.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run `make restore`.", call. = FALSE)
}
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required. Run `make restore`.", call. = FALSE)
}

targets::tar_source(
  list.files(
    "R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE
  )
)

census_2001_languages <- targets::tar_read(census_2001_languages)
admin_2001 <- build_admin_registry_2001(census_2001_languages)
geometry_path <- datameet_census_2001_geometry_path()
geometry_2001 <- read_datameet_census_2001_geometry(geometry_path, admin_2001)
paths <- save_lineage_geometry_2001(geometry_2001, admin_2001)

message("Wrote Census 2001 geometry outputs:")
message(paste0("- ", paths, collapse = "\n"))
