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
  extensions <- c("shp", "dbf", "shx", "prj", "cpg")
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

natural_earth_india_disputed_areas <- function(x) {
  if (!nrow(x)) return(x)
  required <- c("NOTE_BRK", "BRK_A3")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Natural Earth disputed-area layer lacks field(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  note <- as.character(x$NOTE_BRK)
  x[!is.na(note) & grepl("India", note, ignore.case = TRUE), , drop = FALSE]
}

natural_earth_india_disputed_lines <- function(x, disputed_areas) {
  if (!nrow(x)) return(x)
  codes <- character()
  if ("BRK_A3" %in% names(disputed_areas)) {
    codes <- unique(trimws(as.character(disputed_areas$BRK_A3)))
    codes <- codes[!is.na(codes) & nzchar(codes) & codes != "-99"]
  }

  by_code <- rep(FALSE, nrow(x))
  if (length(codes) && "BRK_A3" %in% names(x)) {
    by_code <- trimws(as.character(x$BRK_A3)) %in% codes
  }

  required <- c("BRK_A3", "FEATURECLA")
  missing <- setdiff(required, names(x))
  actor_fields <- intersect(
    c("ADM0_A3_L", "ADM0_A3_R", "SOV_A3_L", "SOV_A3_R"),
    names(x)
  )
  if (length(missing) || !length(actor_fields)) {
    missing <- c(missing, if (!length(actor_fields)) "India actor fields" else character())
    stop(
      "Natural Earth disputed-line layer lacks field(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  by_actor <- rep(FALSE, nrow(x))
  for (field in actor_fields) {
    by_actor <- by_actor | trimws(as.character(x[[field]])) == "IND"
  }
  by_actor <- by_actor & tolower(trimws(as.character(x$FEATURECLA))) == "claim boundary"

  x[by_code | by_actor, , drop = FALSE]
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
  disputed_areas <- natural_earth_india_disputed_areas(disputed_areas)
  disputed_lines <- natural_earth_india_disputed_lines(disputed_lines, disputed_areas)
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
