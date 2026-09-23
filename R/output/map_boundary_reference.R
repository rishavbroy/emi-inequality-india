# Display-only disputed-area masks for manuscript maps.
# Census-2001 DataMeet geometry remains the analytical authority. Natural Earth
# polygons are used only to separate five registered disputed areas for which
# the project has no district estimate from ordinary district-level missingness.

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
  required <- c("area_id", "natural_earth_brk_name", "display_label", "include")
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
  if (!nrow(out) || any(!nzchar(out$area_id)) || any(!nzchar(out$natural_earth_brk_name))) {
    stop("Disputed-area registry must contain non-empty included area IDs and Natural Earth names.", call. = FALSE)
  }
  if (anyDuplicated(out$area_id) || anyDuplicated(out$natural_earth_brk_name)) {
    stop("Included disputed-area registry rows must have unique IDs and Natural Earth names.", call. = FALSE)
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
  disputed_areas <- sf::st_read(
    layers[["disputed_areas"]], quiet = TRUE, stringsAsFactors = FALSE
  )
  disputed_areas <- select_registered_disputed_areas(disputed_areas, registry)
  list(disputed_areas = sf::st_make_valid(disputed_areas))
}

# Compose the display-only disputed/no-estimate mask once.
#
# Natural Earth classifies the five registered disputed areas; the DataMeet
# 99/99 scaffold supplies source-native J&K coverage where canonical districts
# are unavailable. The union is a cartographic class only: analytical district
# geometry, joins, samples, and spatial weights remain unchanged.
build_public_map_boundary_reference <- function(natural_earth_reference, datameet_scaffold) {
  disputed <- natural_earth_reference$disputed_areas
  if (!inherits(disputed, "sf") || !nrow(disputed)) {
    stop("Public map reference requires registered Natural Earth disputed polygons.", call. = FALSE)
  }
  if (!inherits(datameet_scaffold, "sf") || nrow(datameet_scaffold) != 1L) {
    stop("Public map reference requires the one-feature DataMeet map scaffold.", call. = FALSE)
  }
  if (is.na(sf::st_crs(disputed)) || is.na(sf::st_crs(datameet_scaffold))) {
    stop("Public map reference geometries must have defined coordinate reference systems.", call. = FALSE)
  }

  disputed <- sf::st_transform(disputed, sf::st_crs(datameet_scaffold))
  geometry <- sf::st_union(c(
    sf::st_geometry(datameet_scaffold),
    sf::st_geometry(disputed)
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
