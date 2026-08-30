# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-census-geospatial-import

#' read nss 2007 education
#'
read_nss_2007_education <- function(paths) {
  read_manifest_group(paths, "nss_2007_education")
}

#' read nss 2007 consumption
#'
read_nss_2007_consumption <- function(paths) {
  read_manifest_group(paths, "nss_2007_consumption")
}

#' read nss 2017 education
#'
read_nss_2017_education <- function(paths) {
  read_manifest_group(paths, "nss_2017_education")
}

#' read census 2001 mother tongue
#'
read_census_2001_mother_tongue <- function(paths) {
  read_manifest_group(paths, "census_2001_mother_tongue")
}

#' read district boundaries 2020
#'
read_district_boundaries_2020 <- function(paths) {
  rows <- require_manifest_files(paths, "district_boundaries_2020")
  shp <- rows[tolower(rows$file_type) == "shp", , drop = FALSE]
  if (!nrow(shp)) stop("The district-boundary manifest includes shapefile sidecars but no .shp row.", call. = FALSE)
  need_pkg("sf", "district boundaries")
  sf::st_read(shp$absolute_path[[1]], quiet = TRUE)
}

#' read district change sources
#'
read_district_change_sources <- function(paths) {
  read_manifest_group(paths, "district_changes")
}

#' list ilo figure paths
#'
list_ilo_figure_paths <- function(paths) {
  rows <- require_manifest_files(paths, "ilo_figures")
  stats::setNames(rows$absolute_path, rows$file_id)
}

#' Join one label split by table line wrapping
join_district_carveout_wrapped_label <- function(previous, continuation) {
  previous <- trimws(as.character(previous))
  continuation <- trimws(as.character(continuation))
  if (!nzchar(previous) || !nzchar(continuation)) {
    stop("District carve-out wrapped labels must be nonblank.", call. = FALSE)
  }
  if (!grepl("-$", previous)) return(paste(previous, continuation))
  if (grepl("^[[:lower:]]", continuation)) {
    return(paste0(sub("-$", "", previous), continuation))
  }
  paste0(previous, continuation)
}

#' Read the headerless 1961-2001 district carve-out source
#'
#' The source has five data columns and no header row. Reading it with the
#' ordinary CSV default would consume the first Anantapur observation as names.
repair_district_carveout_wrapped_rows <- function(x) {
  out <- safe_df(x)
  if (!nrow(out)) return(out)
  blank <- function(value) is.na(value) || !nzchar(trimws(as.character(value)))
  drop <- rep(FALSE, nrow(out))
  for (i in seq_len(nrow(out))) {
    if (i == 1L || !blank(out$pop_1991[[i]])) next
    previous <- i - 1L
    source_continuation <- !blank(out$district_1991[[i]])
    target_continuation <- !blank(out$district_2001[[i]])
    has_transfer <- !blank(out$pct_01in91[[i]]) || !blank(out$pct_91in01[[i]])

    if (source_continuation) {
      previous_name <- plain_chr(out$district_1991[[previous]])
      if (!length(previous_name) || !grepl("-$", trimws(previous_name[[1L]]))) {
        stop("Unexpected wrapped row in the district carve-out source.", call. = FALSE)
      }
      continuation <- plain_chr(out$district_1991[[i]])[[1L]]
      combined <- join_district_carveout_wrapped_label(previous_name[[1L]], continuation)
      out$district_1991[[previous]] <- combined
      if (has_transfer) {
        out$district_1991[[i]] <- combined
        out$pop_1991[[i]] <- out$pop_1991[[previous]]
      }
    }

    if (!has_transfer && (source_continuation || target_continuation)) {
      if (target_continuation) {
        if (blank(out$district_2001[[previous]])) {
          stop("Unexpected target-only continuation in the district carve-out source.", call. = FALSE)
        }
        previous_target <- trimws(as.character(out$district_2001[[previous]]))
        continuation <- trimws(as.character(out$district_2001[[i]]))
        out$district_2001[[previous]] <- join_district_carveout_wrapped_label(
          previous_target, continuation
        )
      }
      drop[[i]] <- TRUE
    }
  }
  out[!drop, , drop = FALSE]
}

district_carveout_rounding_tolerance_pp <- function() 0.05

validate_district_carveout_shares <- function(
    x,
    rounding_tolerance_pp = district_carveout_rounding_tolerance_pp()) {
  out <- safe_df(x)
  if (!nrow(out)) return(invisible(TRUE))
  share_cols <- c("pct_01in91", "pct_91in01")
  for (field in share_cols) {
    values <- num(out[[field]])
    bad <- is.finite(values) & (values < 0 | values > 100)
    if (any(bad)) {
      stop("District carve-out transfer shares must lie in [0, 100].", call. = FALSE)
    }
  }
  source_share <- num(out$pct_01in91)
  if (any(!is.finite(source_share))) {
    stop("District carve-out source shares must be complete.", call. = FALSE)
  }
  source_key <- paste(out$district_1991, out$pop_1991, sep = "__")
  source_sum <- vapply(split(source_share, source_key), sum, numeric(1))
  if (any(abs(source_sum - 100) > rounding_tolerance_pp)) {
    stop("District carve-out source shares must sum to 100 within rounding tolerance.", call. = FALSE)
  }
  invisible(TRUE)
}

read_district_carveouts <- function(path) {
  out <- utils::read.csv(
    path,
    header = FALSE,
    col.names = c("district_1991", "pop_1991", "district_2001", "pct_01in91", "pct_91in01"),
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE
  )
  out <- repair_district_carveout_wrapped_rows(out)
  fill_down <- function(x) {
    if (!length(x)) return(x)
    for (i in seq_along(x)) {
      missing <- is.na(x[[i]]) || (is.character(x) && !nzchar(trimws(x[[i]])))
      if (missing && i > 1L) x[[i]] <- x[[i - 1L]]
    }
    x
  }
  out$district_1991 <- fill_down(out$district_1991)
  out$pop_1991 <- num(gsub(",", "", fill_down(out$pop_1991), fixed = TRUE))
  out$pct_01in91 <- num(out$pct_01in91)
  out$pct_91in01 <- num(out$pct_91in01)
  validate_district_carveout_shares(out)
  out
}

#' read one manifest row
#'
#' @return Reader output for raw data files, or a validated path for sidecars/assets.
read_by_manifest_row <- function(row) {
  path <- row$absolute_path[[1]]
  reader <- as.character(row$reader_function[[1]] %||% "")
  if (identical(reader, "read_district_carveouts")) {
    return(read_district_carveouts(path))
  }
  file_type <- tolower(row$file_type[[1]])
  switch(
    file_type,
    sav = read_sav_short(path),
    csv = read_csv_short(path),
    xls = read_excel_short(path),
    xlsx = read_excel_short(path),
    ods = read_ods_short(path),
    shp = {
      need_pkg("sf", "shapefiles")
      sf::st_read(path, quiet = TRUE)
    },
    shx = path,
    dbf = path,
    prj = path,
    png = path,
    stop("No reader implemented for ", file_type, call. = FALSE)
  )
}

#' read all manifest rows for a source
#'
#' @return Named list of reader outputs keyed by manifest file_id.
read_manifest_group <- function(paths, source_id) {
  rows <- require_manifest_files(paths, source_id)
  stats::setNames(lapply(seq_len(nrow(rows)), function(i) read_by_manifest_row(rows[i, ])), rows$file_id)
}

# sample-end: code-census-geospatial-import
