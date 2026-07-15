# Processed district artifact writers.
# These are target output helpers, separate from panel construction.

#' Save processed district tracker
#'
#' Write the public processed district tracker artifact expected by `_targets.R`.
#' Geometry/list columns are flattened so the file is a portable CSV.
save_processed_district_tracker <- function(district_tracker, legacy_district_tracker = NULL, path = "data/processed/district_tracker_2001_2007_2017_2020.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- legacy_tracker_frame(district_tracker, legacy_district_tracker)
  if (!nrow(x)) x <- district_tracker
  x <- flatten_processed_output(x)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  path
}

#' Save processed district panel
#'
#' Write the public processed district-level analysis panel expected by `_targets.R`.
#' If the panel is an sf object, geometry is dropped before CSV export.
save_processed_district_panel <- function(district_panel, path = "data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- flatten_processed_output(district_panel)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  path
}

flatten_processed_output <- function(x) {
  if (inherits(x, "sf") && requireNamespace("sf", quietly = TRUE)) {
    x <- sf::st_drop_geometry(x)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!nrow(x) && !length(names(x))) return(data.frame())
  for (nm in names(x)) {
    if (inherits(x[[nm]], "POSIXt")) x[[nm]] <- format(x[[nm]], usetz = TRUE)
    if (inherits(x[[nm]], "Date")) x[[nm]] <- as.character(x[[nm]])
    if (is.factor(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
    if (is.list(x[[nm]])) {
      x[[nm]] <- vapply(x[[nm]], function(value) {
        if (length(value) == 0L || all(is.na(value))) return(NA_character_)
        paste(as.character(value), collapse = "; ")
      }, character(1))
    }
  }
  x
}
