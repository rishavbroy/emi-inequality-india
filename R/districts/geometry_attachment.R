# Geometry attachment helpers for district panels.
# Keep spatial joins separate from measure and source-attachment code.

attach_panel_geometry <- function(panel, boundaries_2020) {
  if (!inherits(boundaries_2020, "sf")) return(panel)

  if (all(c("state_20", "district_20") %in% names(panel)) &&
      all(c("state_20", "district_20") %in% names(boundaries_2020))) {
    return(attach_geometry_by_keys(
      panel,
      boundaries_2020,
      panel_state = "state_20",
      panel_district = "district_20",
      boundary_state = "state_20",
      boundary_district = "district_20"
    ))
  }

  if (all(c("state_std", "district_std") %in% names(panel)) &&
      all(c("state_std", "district_std") %in% names(boundaries_2020))) {
    return(attach_geometry_by_keys(
      panel,
      boundaries_2020,
      panel_state = "state_std",
      panel_district = "district_std",
      boundary_state = "state_std",
      boundary_district = "district_std"
    ))
  }

  panel
}

attach_geometry_by_keys <- function(panel, boundaries, panel_state, panel_district,
                                    boundary_state, boundary_district) {
  geometry <- sf::st_geometry(boundaries)
  if (!inherits(geometry, "sfc")) {
    stop("Boundary input does not contain a valid simple-features geometry column.", call. = FALSE)
  }

  panel_key <- district_geometry_key(panel[[panel_state]], panel[[panel_district]])
  boundary_key <- district_geometry_key(
    boundaries[[boundary_state]],
    boundaries[[boundary_district]]
  )

  keep <- !is.na(boundary_key) & !duplicated(boundary_key)
  boundary_key <- boundary_key[keep]
  geometry <- geometry[keep]
  matched <- match(panel_key, boundary_key)

  # Subsetting an sfc vector with unmatched (NA) indices yields typed empty
  # geometries. Assign through sf's documented geometry setter so the result
  # remains an sf object even when no panel row matches a boundary key.
  sf::st_set_geometry(panel, geometry[matched])
}

district_geometry_key <- function(state, district) {
  state_key <- normalize_panel_geometry_key(state)
  district_key <- normalize_panel_geometry_key(district)
  complete <- !is.na(state_key) & nzchar(state_key) &
    !is.na(district_key) & nzchar(district_key)
  out <- rep(NA_character_, length(state_key))
  out[complete] <- paste(state_key[complete], district_key[complete], sep = "\r")
  out
}

normalize_panel_geometry_key <- function(x) canon(x)
