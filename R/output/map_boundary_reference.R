# Display-only political boundary reference for manuscript maps.
# Empirical district geometry remains unchanged and continues to determine
# district identities, spatial weights, and all estimation samples.

natural_earth_map_reference_spec <- function() {
  c(
    countries = "ne_10m_admin_0_countries",
    disputed_areas = "ne_10m_admin_0_disputed_areas",
    disputed_lines = "ne_10m_admin_0_boundary_lines_disputed_areas"
  )
}

natural_earth_map_reference_paths <- function(paths = build_paths()) {
  base <- path_project(paths, "data/raw/natural-earth/10m")
  stems <- unname(natural_earth_map_reference_spec())
  extensions <- c("shp", "dbf", "shx", "prj")
  as.vector(outer(
    stems,
    extensions,
    function(stem, ext) file.path(base, paste0(stem, ".", ext))
  ))
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

natural_earth_india_row <- function(countries) {
  candidates <- list(
    ADM0_A3 = "IND",
    SOV_A3 = "IND",
    ADMIN = "India",
    NAME = "India"
  )
  keep <- rep(FALSE, nrow(countries))
  for (nm in names(candidates)) {
    if (nm %in% names(countries)) {
      keep <- keep | toupper(trimws(as.character(countries[[nm]]))) == toupper(candidates[[nm]])
    }
  }
  out <- countries[keep, , drop = FALSE]
  if (nrow(out) != 1L) {
    stop("Natural Earth countries layer must contain exactly one de facto India feature.", call. = FALSE)
  }
  out
}

near_india_reference_features <- function(x, india, distance_m = 500000) {
  if (!nrow(x)) return(x)
  projected <- 3857
  india_buffer <- sf::st_buffer(sf::st_transform(india, projected), dist = distance_m)
  buffer_native <- sf::st_transform(india_buffer, sf::st_crs(x))
  hits <- lengths(sf::st_intersects(x, buffer_native)) > 0L
  x[hits, , drop = FALSE]
}

read_natural_earth_map_reference <- function(files) {
  need_pkg("sf", "Natural Earth boundary reference")
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
  countries <- sf::st_read(layers[["countries"]], quiet = TRUE, stringsAsFactors = FALSE)
  disputed_areas <- sf::st_read(layers[["disputed_areas"]], quiet = TRUE, stringsAsFactors = FALSE)
  disputed_lines <- sf::st_read(layers[["disputed_lines"]], quiet = TRUE, stringsAsFactors = FALSE)
  india <- natural_earth_india_row(countries)
  disputed_areas <- near_india_reference_features(disputed_areas, india)
  disputed_lines <- near_india_reference_features(disputed_lines, india)
  list(
    india = sf::st_make_valid(india),
    disputed_areas = sf::st_make_valid(disputed_areas),
    disputed_lines = sf::st_make_valid(disputed_lines)
  )
}

clip_public_map_to_de_facto_india <- function(plot_data, boundary_reference) {
  if (!inherits(plot_data, "sf") || is.null(boundary_reference$india)) return(plot_data)
  india <- boundary_reference$india
  if (sf::st_crs(india) != sf::st_crs(plot_data)) {
    india <- sf::st_transform(india, sf::st_crs(plot_data))
  }
  suppressWarnings(sf::st_intersection(plot_data, sf::st_union(india)))
}
