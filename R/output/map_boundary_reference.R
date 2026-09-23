# Display-only political boundary annotations for manuscript maps.
# Census-2001 DataMeet geometry is the sole authority for district fills,
# ordinary state boundaries, and the empirical map exterior.

natural_earth_map_reference_spec <- function() {
  c(disputed_lines = "ne_10m_admin_0_boundary_lines_disputed_areas")
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

natural_earth_india_disputed_lines <- function(x) {
  if (!nrow(x)) return(x)
  required <- c("FEATURECLA", "NOTE")
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

  feature <- trimws(as.character(x$FEATURECLA))
  note <- as.character(x$NOTE)
  claim <- !is.na(feature) & tolower(feature) == "claim boundary"
  india <- !is.na(note) & grepl("India", note, ignore.case = TRUE)
  for (field in actor_fields) {
    actor <- trimws(as.character(x[[field]]))
    india <- india | (!is.na(actor) & actor == "IND")
  }
  x[claim & india, , drop = FALSE]
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
  disputed_lines <- sf::st_read(
    layers[["disputed_lines"]], quiet = TRUE, stringsAsFactors = FALSE
  )
  disputed_lines <- natural_earth_india_disputed_lines(disputed_lines)
  list(disputed_lines = sf::st_make_valid(disputed_lines))
}
