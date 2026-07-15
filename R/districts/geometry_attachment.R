# Geometry attachment helpers for district panels.
# Keep spatial joins separate from measure and source-attachment code.

attach_panel_geometry <- function(panel, boundaries_2020) {
  if (!inherits(boundaries_2020, "sf")) return(panel)
  geom_col <- attr(boundaries_2020, "sf_column")
  b <- boundaries_2020
  if ("state_20" %in% names(panel) && "district_20" %in% names(panel) && all(c("state_20", "district_20") %in% names(b))) {
    panel$.geometry_state_key <- canon(panel$state_20)
    panel$.geometry_district_key <- canon(panel$district_20)
    b$.geometry_state_key <- canon(b$state_20)
    b$.geometry_district_key <- canon(b$district_20)
    b <- b[!duplicated(as.data.frame(b[c(".geometry_state_key", ".geometry_district_key")])), ]
    out <- merge(panel, b[c(".geometry_state_key", ".geometry_district_key", geom_col)], by = c(".geometry_state_key", ".geometry_district_key"), all.x = TRUE)
    out$.geometry_state_key <- NULL
    out$.geometry_district_key <- NULL
    return(sf::st_as_sf(out, sf_column_name = geom_col))
  }
  if (!all(c("state_std", "district_std") %in% names(panel)) || !all(c("state_std", "district_std") %in% names(b))) return(panel)
  boundary_keys <- b[c("state_std", "district_std", geom_col)]
  boundary_keys <- boundary_keys[!duplicated(as.data.frame(boundary_keys[c("state_std", "district_std")])), ]
  panel_key <- paste(normalize_panel_geometry_key(panel$state_std), normalize_panel_geometry_key(panel$district_std), sep = "\r")
  boundary_key <- paste(normalize_panel_geometry_key(boundary_keys$state_std), normalize_panel_geometry_key(boundary_keys$district_std), sep = "\r")
  idx <- match(panel_key, boundary_key)
  if (all(is.na(idx))) return(panel)
  panel[[geom_col]] <- sf::st_geometry(boundary_keys)[idx]
  sf::st_as_sf(panel, sf_column_name = geom_col)
}

normalize_panel_geometry_key <- function(x) canon(x)
