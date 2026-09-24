# Display-only disputed-area masks for manuscript maps.
# Census-2001 DataMeet geometry remains the analytical authority. Natural Earth
# polygons classify the five registered disputed/no-estimate areas plus one
# administered Jammu-and-Kashmir reference used only to reconcile display
# coverage where the two source geometries disagree.

natural_earth_map_reference_spec <- function() {
  c(disputed_areas = "ne_10m_admin_0_disputed_areas")
}

natural_earth_map_reference_paths <- function(paths = build_paths()) {
  base <- path_project(paths, "data/raw/natural-earth/10m")
  stems <- unname(natural_earth_map_reference_spec())
  extensions <- c("shp", "dbf", "shx", "prj", "cpg")
  as.vector(outer(
    stems,
    extensions,
    function(stem, ext) file.path(base, paste0(stem, ".", ext))
  ))
}

map_disputed_area_registry_path <- function(paths = build_paths()) {
  path_project(paths, "data/metadata/map_disputed_areas.csv")
}

read_map_disputed_area_registry <- function(path = map_disputed_area_registry_path()) {
  if (!file.exists(path)) {
    stop("Missing disputed-area registry: ", path, call. = FALSE)
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("area_id", "natural_earth_brk_name", "display_label", "role", "include")
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop(
      "Disputed-area registry lacks column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  include <- tolower(trimws(as.character(out$include))) %in% c("true", "1", "yes")
  out <- out[include, , drop = FALSE]
  out$area_id <- trimws(as.character(out$area_id))
  out$natural_earth_brk_name <- trimws(as.character(out$natural_earth_brk_name))
  out$display_label <- trimws(as.character(out$display_label))
  out$role <- trimws(as.character(out$role))
  if (!nrow(out) || any(!nzchar(out$area_id)) || any(!nzchar(out$natural_earth_brk_name))) {
    stop("Disputed-area registry must contain non-empty included area IDs and Natural Earth names.", call. = FALSE)
  }
  if (anyDuplicated(out$area_id) || anyDuplicated(out$natural_earth_brk_name)) {
    stop("Included disputed-area registry rows must have unique IDs and Natural Earth names.", call. = FALSE)
  }
  allowed_roles <- c("display_disputed", "administered_reference")
  if (any(!out$role %in% allowed_roles)) {
    stop(
      "Disputed-area registry contains unsupported role(s): ",
      paste(unique(out$role[!out$role %in% allowed_roles]), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (sum(out$role == "administered_reference") != 1L) {
    stop("Disputed-area registry must contain exactly one administered_reference row.", call. = FALSE)
  }
  if (!any(out$role == "display_disputed")) {
    stop("Disputed-area registry must contain at least one display_disputed row.", call. = FALSE)
  }
  out
}

natural_earth_reference_shapefiles <- function(files) {
  files <- unname(as.character(files))
  spec <- natural_earth_map_reference_spec()
  wanted <- paste0(unname(spec), ".shp")
  index <- match(wanted, basename(files))
  if (anyNA(index)) {
    missing <- names(spec)[is.na(index)]
    stop(
      "Natural Earth boundary reference is missing layer(s): ",
      paste(missing, collapse = ", "),
      ". Run `make prepare-data` to download them.",
      call. = FALSE
    )
  }
  out <- files[index]
  names(out) <- names(spec)
  out
}

select_registered_disputed_areas <- function(x, registry) {
  if (!"BRK_NAME" %in% names(x)) {
    stop("Natural Earth disputed-area layer lacks BRK_NAME.", call. = FALSE)
  }
  source_names <- trimws(as.character(x$BRK_NAME))
  counts <- vapply(
    registry$natural_earth_brk_name,
    function(name) sum(!is.na(source_names) & source_names == name),
    integer(1)
  )
  if (any(counts == 0L)) {
    stop(
      "Natural Earth disputed-area layer is missing registered polygon(s): ",
      paste(registry$natural_earth_brk_name[counts == 0L], collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(counts > 1L)) {
    stop(
      "Registered Natural Earth disputed-area name(s) are not unique: ",
      paste(registry$natural_earth_brk_name[counts > 1L], collapse = ", "), ".",
      call. = FALSE
    )
  }
  index <- match(registry$natural_earth_brk_name, source_names)
  out <- x[index, , drop = FALSE]
  out$area_id <- registry$area_id
  out$display_label <- registry$display_label
  out$role <- registry$role
  out
}

read_natural_earth_map_reference <- function(files, registry) {
  need_pkg("sf", "Natural Earth disputed-area reference")
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop(
      "Missing Natural Earth boundary files: ",
      paste(basename(missing), collapse = ", "),
      ". Run `make prepare-data` to download them.",
      call. = FALSE
    )
  }
  layers <- natural_earth_reference_shapefiles(files)
  source <- sf::st_read(
    layers[["disputed_areas"]], quiet = TRUE, stringsAsFactors = FALSE
  )
  selected <- sf::st_make_valid(select_registered_disputed_areas(source, registry))
  display <- selected[selected$role == "display_disputed", , drop = FALSE]
  administered <- selected[selected$role == "administered_reference", , drop = FALSE]
  if (!nrow(display) || nrow(administered) != 1L) {
    stop("Natural Earth map reference roles did not resolve to the required polygons.", call. = FALSE)
  }
  list(
    disputed_areas = display,
    administered_reference = administered
  )
}

public_map_state_code_2001 <- function(unit_id) {
  unit_id <- plain_chr(unit_id)
  valid <- !is.na(unit_id) & grepl("^pc2001__[0-9]{2}__[0-9]{2}$", unit_id)
  if (any(!valid)) {
    stop(
      "Public map geometry contains invalid canonical Census-2001 district IDs.",
      call. = FALSE
    )
  }
  sub("^pc2001__([0-9]{2})__[0-9]{2}$", "\\1", unit_id)
}

# Classify DataMeet J&K pieces left unresolved after Natural Earth partitions the
# state into an administered reference and the registered disputed polygons.
# Pieces that share a one-dimensional boundary with another DataMeet state stay
# ordinary Census geography; the remainder join the disputed/no-estimate class.
public_map_jk_disputed_residual <- function(canonical_geometry, administered_reference, disputed_areas) {
  if (!inherits(canonical_geometry, "sf") || !nrow(canonical_geometry)) {
    stop("Public map reconciliation requires complete canonical Census-2001 geometry.", call. = FALSE)
  }
  key <- if ("unit_id" %in% names(canonical_geometry)) "unit_id" else if ("target_unit_2001" %in% names(canonical_geometry)) "target_unit_2001" else NA_character_
  if (is.na(key) || anyDuplicated(canonical_geometry[[key]])) {
    stop("Canonical public map geometry must contain one unique Census-2001 district ID per feature.", call. = FALSE)
  }
  if (!inherits(administered_reference, "sf") || nrow(administered_reference) != 1L ||
      !inherits(disputed_areas, "sf") || !nrow(disputed_areas)) {
    stop("Jammu-and-Kashmir display reconciliation requires one administered reference and registered disputed polygons.", call. = FALSE)
  }

  state_code <- public_map_state_code_2001(canonical_geometry[[key]])
  jk <- canonical_geometry[state_code == "01", , drop = FALSE]
  other_states <- canonical_geometry[state_code != "01", , drop = FALSE]
  if (!nrow(jk) || !nrow(other_states)) {
    stop("Canonical public map geometry lacks Jammu and Kashmir or comparison-state coverage.", call. = FALSE)
  }
  if (is.na(sf::st_crs(jk)) || is.na(sf::st_crs(administered_reference)) || is.na(sf::st_crs(disputed_areas))) {
    stop("Public map reconciliation geometries must have defined coordinate reference systems.", call. = FALSE)
  }

  administered_reference <- sf::st_transform(administered_reference, sf::st_crs(jk))
  disputed_areas <- sf::st_transform(disputed_areas, sf::st_crs(jk))
  classified <- sf::st_union(c(
    sf::st_geometry(administered_reference),
    sf::st_geometry(disputed_areas)
  ))
  residual <- sf::st_difference(
    sf::st_union(sf::st_geometry(jk)),
    classified
  )
  residual <- sf::st_make_valid(residual)
  residual <- residual[!sf::st_is_empty(residual)]
  if (!length(residual)) return(sf::st_sfc(crs = sf::st_crs(jk)))

  components <- suppressWarnings(sf::st_cast(residual, "POLYGON"))
  components <- components[!sf::st_is_empty(components)]
  if (!length(components)) return(sf::st_sfc(crs = sf::st_crs(jk)))

  # DE-9IM side adjacency is a topological question. Use a projected copy so
  # st_relate() follows its documented planar semantics without relying on an
  # arbitrary distance or snapping tolerance. EPSG:6933 is a global projected
  # CRS; only the relation result, not the transformed geometry, is retained.
  components_planar <- sf::st_transform(components, 6933)
  others_planar <- sf::st_transform(
    sf::st_union(sf::st_geometry(other_states)),
    6933
  )
  shares_state_side <- lengths(sf::st_relate(
    components_planar,
    others_planar,
    pattern = "****1****"
  )) > 0L

  disputed_residual <- components[!shares_state_side]
  if (!length(disputed_residual)) return(sf::st_sfc(crs = sf::st_crs(jk)))
  sf::st_union(disputed_residual)
}

# Compose the display-only disputed/no-estimate mask once.
#
# Natural Earth explicitly classifies five disputed areas and supplies one
# administered-J&K reference. DataMeet contributes the canonical 593-district
# geometry plus its noncanonical 99/99 scaffold. Residual DataMeet J&K pieces
# not covered by either Natural Earth class are resolved topologically: pieces
# sharing a side with another DataMeet state stay ordinary geography; the rest
# join the disputed/no-estimate mask. Analytical geometry is never modified.
build_public_map_boundary_reference <- function(
  natural_earth_reference,
  datameet_scaffold,
  canonical_geometry
) {
  disputed <- natural_earth_reference$disputed_areas
  administered <- natural_earth_reference$administered_reference
  if (!inherits(disputed, "sf") || !nrow(disputed) ||
      !inherits(administered, "sf") || nrow(administered) != 1L) {
    stop("Public map reference requires registered Natural Earth display and administered-reference polygons.", call. = FALSE)
  }
  if (!inherits(datameet_scaffold, "sf") || nrow(datameet_scaffold) != 1L) {
    stop("Public map reference requires the one-feature DataMeet map scaffold.", call. = FALSE)
  }
  if (!inherits(canonical_geometry, "sf") || !nrow(canonical_geometry)) {
    stop("Public map reference requires complete canonical Census-2001 geometry.", call. = FALSE)
  }
  if (is.na(sf::st_crs(disputed)) || is.na(sf::st_crs(datameet_scaffold)) || is.na(sf::st_crs(canonical_geometry))) {
    stop("Public map reference geometries must have defined coordinate reference systems.", call. = FALSE)
  }

  disputed <- sf::st_transform(disputed, sf::st_crs(canonical_geometry))
  administered <- sf::st_transform(administered, sf::st_crs(canonical_geometry))
  datameet_scaffold <- sf::st_transform(datameet_scaffold, sf::st_crs(canonical_geometry))
  residual <- public_map_jk_disputed_residual(
    canonical_geometry,
    administered,
    disputed
  )

  geometry <- sf::st_union(c(
    sf::st_geometry(datameet_scaffold),
    sf::st_geometry(disputed),
    residual
  ))
  geometry <- sf::st_make_valid(geometry)
  if (length(geometry) == 0L || all(sf::st_is_empty(geometry))) {
    stop("Public map disputed/no-estimate mask is empty.", call. = FALSE)
  }

  list(
    disputed_display = sf::st_sf(
      area_id = "registered_disputed_no_estimate",
      geometry = geometry
    )
  )
}
