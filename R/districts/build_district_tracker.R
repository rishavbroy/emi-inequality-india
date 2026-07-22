# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-district-crosswalk-qa

#' build district tracker
#'
build_district_tracker <- function(raw_district_changes) {
  source_aliases <- list(
    alluvial = c("alluvial", "district_changes_alluvial"),
    carveouts_renamings = c("carveouts_renamings", "district_changes_carveouts"),
    new_districts_created = c("new_districts_created", "district_changes_new_districts"),
    name_changes = c("name_changes", "district_changes_name_changes"),
    district_splits = c("district_splits", "district_changes_splits"),
    india_district_tracker = c("india_district_tracker", "tracker", "district_changes_tracker")
  )
  source_value <- function(id) {
    hit <- source_aliases[[id]][source_aliases[[id]] %in% names(raw_district_changes)]
    if (length(hit)) raw_district_changes[[hit[[1]]]] else data.frame()
  }
  parsed <- list(
    alluvial = parse_alluvial_district_changes(source_value("alluvial")),
    carveouts_renamings = parse_carveouts_renamings(source_value("carveouts_renamings")),
    new_districts_created = parse_new_districts_created(source_value("new_districts_created")),
    name_changes = parse_name_changes(source_value("name_changes")),
    district_splits = parse_district_splits(source_value("district_splits")),
    india_district_tracker = parse_india_district_tracker(source_value("india_district_tracker"))
  )
  consumed <- unique(unlist(source_aliases, use.names = FALSE))
  extras <- setdiff(names(raw_district_changes), consumed)
  for (nm in extras) parsed[[nm]] <- parse_district_change_source(raw_district_changes[[nm]], source_type = nm)
  standardize_tracker_names(standardize_tracker_years(combine_district_tracker_sources(parsed)))
}

#' parse alluvial district changes
#'
parse_alluvial_district_changes <- function(x) {
  x <- safe_df(x)
  year_state <- grep("^[0-9]{4}.*state", canon(names(x)), value = FALSE)
  years <- suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", canon(names(x)[year_state]))))
  years <- sort(unique(years[is.finite(years)]))
  if (length(years) < 2L) return(parse_district_change_source(x, source_type = "alluvial"))
  parse_wide_year_lineages(x, years, source_type = "alluvial")
}

#' parse india district tracker
#'
parse_india_district_tracker <- function(x) {
  x <- safe_df(x)
  years <- suppressWarnings(as.integer(names(x)[grepl("^[0-9]{4}$", names(x))]))
  years <- sort(unique(years[is.finite(years)]))
  if (length(years) < 2L) return(parse_district_change_source(x, source_type = "india_district_tracker"))
  parse_tracker_year_triplets(x, years)
}

#' parse carveouts renamings
#'
parse_carveouts_renamings <- function(x) {
  x <- safe_df(x)
  required <- c("district_1991", "pop_1991", "district_2001", "pct_01in91", "pct_91in01")
  if (!all(required %in% names(x))) {
    return(parse_district_change_source(x, source_type = "carveouts_renamings"))
  }
  data.frame(
    source_type = "carveouts_renamings",
    source_state_raw = NA_character_,
    source_district_raw = plain_chr(x$district_1991),
    target_state_raw = NA_character_,
    target_district_raw = plain_chr(x$district_2001),
    source_year_raw = 1991L,
    target_year_raw = 2001L,
    change_type = "population_transfer_1991_2001",
    event_year = 2001L,
    source_population = num(gsub(",", "", plain_chr(x$pop_1991), fixed = TRUE)),
    source_share_to_target = num(x$pct_01in91) / 100,
    target_share_from_source = num(x$pct_91in01) / 100,
    stringsAsFactors = FALSE
  )
}

#' parse new districts created
#'
parse_new_districts_created <- function(x) {
  x <- safe_df(x)
  if (!all(c("State/UT", "Old districts", "New District") %in% names(x))) {
    return(parse_district_change_source(x, source_type = "new_districts_created"))
  }
  data.frame(
    source_type = "new_districts_created",
    source_state_raw = plain_chr(x[["State/UT"]]),
    source_district_raw = plain_chr(x[["Old districts"]]),
    target_state_raw = plain_chr(x[["State/UT"]]),
    target_district_raw = plain_chr(x[["New District"]]),
    source_year_raw = suppressWarnings(as.integer(x$Year)) - 1L,
    target_year_raw = suppressWarnings(as.integer(x$Year)),
    change_type = "new_district",
    event_year = suppressWarnings(as.integer(x$Year)),
    decade = plain_chr(x$Decade),
    stringsAsFactors = FALSE
  )
}

#' parse name changes
#'
parse_name_changes <- function(x) {
  x <- safe_df(x)
  if (!all(c("State/UT", "Old Name", "New Name") %in% names(x))) {
    return(parse_district_change_source(x, source_type = "name_changes"))
  }
  decade_end <- suppressWarnings(as.integer(sub(".*-", "", plain_chr(x$Decade))))
  data.frame(
    source_type = "name_changes",
    source_state_raw = plain_chr(x[["State/UT"]]),
    source_district_raw = plain_chr(x[["Old Name"]]),
    target_state_raw = plain_chr(x[["State/UT"]]),
    target_district_raw = plain_chr(x[["New Name"]]),
    source_year_raw = NA_integer_,
    target_year_raw = decade_end,
    change_type = paste0("name_change:", canon(x$Type)),
    event_year = NA_integer_,
    decade = plain_chr(x$Decade),
    stringsAsFactors = FALSE
  )
}

#' parse district splits
#'
parse_district_splits <- function(x) {
  x <- safe_df(x)
  if (!all(c("State/UT", "District-Before", "District-After") %in% names(x))) {
    return(parse_district_change_source(x, source_type = "district_splits"))
  }
  year <- suppressWarnings(as.integer(x$Year))
  data.frame(
    source_type = "district_splits",
    source_state_raw = plain_chr(x[["State/UT"]]),
    source_district_raw = plain_chr(x[["District-Before"]]),
    target_state_raw = plain_chr(x[["State/UT"]]),
    target_district_raw = plain_chr(x[["District-After"]]),
    source_year_raw = year - 1L,
    target_year_raw = year,
    change_type = "split_or_carveout",
    event_year = year,
    decade = plain_chr(x$Decade),
    stringsAsFactors = FALSE
  )
}


find_year_field <- function(x, year, kind) {
  keys <- canon(names(x))
  pattern <- paste0("^", year, " ", kind)
  hit <- which(grepl(pattern, keys))
  if (length(hit)) names(x)[hit[[1]]] else NULL
}

parse_wide_year_lineages <- function(x, years, source_type) {
  rows <- lapply(seq_len(length(years) - 1L), function(i) {
    source_year <- years[[i]]
    target_year <- years[[i + 1L]]
    ss <- find_year_field(x, source_year, "state")
    sd <- find_year_field(x, source_year, "district")
    ts <- find_year_field(x, target_year, "state")
    td <- find_year_field(x, target_year, "district")
    if (any(vapply(list(ss, sd, ts, td), is.null, logical(1)))) return(data.frame())
    out <- data.frame(
      source_type = source_type,
      source_state_raw = plain_chr(x[[ss]]),
      source_district_raw = plain_chr(x[[sd]]),
      target_state_raw = plain_chr(x[[ts]]),
      target_district_raw = plain_chr(x[[td]]),
      source_year_raw = source_year,
      target_year_raw = target_year,
      stringsAsFactors = FALSE
    )
    source_pair <- paste(canonicalize_state_name(out$source_state_raw), canon(out$source_district_raw), sep = "__")
    target_pair <- paste(canonicalize_state_name(out$target_state_raw), canon(out$target_district_raw), sep = "__")
    out$change_type <- ifelse(source_pair == target_pair, "continuity", "lineage_candidate")
    out$.source_wide_row <- seq_len(nrow(x))
    keep <- !is.na(out$source_district_raw) & nzchar(trimws(out$source_district_raw)) &
      !is.na(out$target_district_raw) & nzchar(trimws(out$target_district_raw))
    out[keep, , drop = FALSE]
  })
  safe_bind_rows(rows)
}

parse_tracker_year_triplets <- function(x, years) {
  # The Jaacks sheet has state, district, and code columns in repeating
  # three-column year blocks, with the first data row containing field labels.
  if (nrow(x) && any(canon(unlist(x[1, ], use.names = FALSE)) == "statename")) x <- x[-1, , drop = FALSE]
  rows <- lapply(seq_len(length(years) - 1L), function(i) {
    source_year <- years[[i]]
    target_year <- years[[i + 1L]]
    source_col <- match(as.character(source_year), names(x))
    target_col <- match(as.character(target_year), names(x))
    if (!is.finite(source_col) || !is.finite(target_col)) return(data.frame())
    out <- data.frame(
      source_type = "india_district_tracker",
      source_state_raw = plain_chr(x[[source_col]]),
      source_district_raw = plain_chr(x[[source_col + 1L]]),
      source_code_raw = plain_chr(x[[source_col + 2L]]),
      target_state_raw = plain_chr(x[[target_col]]),
      target_district_raw = plain_chr(x[[target_col + 1L]]),
      target_code_raw = plain_chr(x[[target_col + 2L]]),
      source_year_raw = source_year,
      target_year_raw = target_year,
      stringsAsFactors = FALSE
    )
    source_pair <- paste(canonicalize_state_name(out$source_state_raw), canon(out$source_district_raw), sep = "__")
    target_pair <- paste(canonicalize_state_name(out$target_state_raw), canon(out$target_district_raw), sep = "__")
    out$change_type <- ifelse(source_pair == target_pair, "continuity", "annual_lineage_candidate")
    out$.source_wide_row <- seq_len(nrow(x))
    keep <- !is.na(out$source_district_raw) & nzchar(trimws(out$source_district_raw)) &
      !is.na(out$target_district_raw) & nzchar(trimws(out$target_district_raw))
    out[keep, , drop = FALSE]
  })
  safe_bind_rows(rows)
}

parse_district_change_source <- function(x, source_type) {
  x <- safe_df(x)
  if (!nrow(x)) return(data.frame(source_type = character(), stringsAsFactors = FALSE))
  x$source_type <- source_type
  x$source_state_raw <- first_present_value(x, c("state", "State", "state_raw", "state_from", "state_01", "state_07", "state_08", "state_17", "state_18", "state_20"))
  x$source_district_raw <- first_present_value(x, c("district", "District", "district_raw", "district_from", "district_01", "district_07", "district_08", "district_17", "district_18", "district_20", "district_name"))
  x$target_state_raw <- first_present_value(x, c("state_to", "new_state", "target_state", "state_20", "state_18", "state_17", "state_08", "state_07", "state"))
  x$target_district_raw <- first_present_value(x, c("district_to", "new_district", "target_district", "district_20", "district_18", "district_17", "district_08", "district_07", "district"))
  x$source_year_raw <- first_present_value(x, c("source_year", "year_from", "from_year", "start_year", "year"))
  x$target_year_raw <- first_present_value(x, c("target_year", "year_to", "to_year", "end_year", "year"))
  x$change_type <- first_present_value(x, c("change_type", "type", "event_type", "status"))
  x
}

first_present_value <- function(df, candidates) {
  hit <- first_col(df, candidates)
  if (is.null(hit)) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit]])
}

#' combine district tracker sources
#'
combine_district_tracker_sources <- function(raw_district_changes) {
  out <- safe_bind_rows(lapply(names(raw_district_changes), function(name) {
    x <- safe_df(raw_district_changes[[name]])
    if (!nrow(x)) return(data.frame())
    x$source_file_id <- if ("source_file_id" %in% names(x)) x$source_file_id else name
    if (!"source_type" %in% names(x)) x$source_type <- name
    x
  }))
  if (nrow(out)) out$.row_in_source <- ave(seq_len(nrow(out)), out$source_file_id, FUN = seq_along)
  out
}

#' standardize tracker years
#'
standardize_tracker_years <- function(tracker) {
  tracker <- safe_df(tracker)
  if (!nrow(tracker)) return(tracker)
  if ("source_year_raw" %in% names(tracker) && !"source_year" %in% names(tracker)) tracker$source_year <- suppressWarnings(as.integer(tracker$source_year_raw))
  if ("target_year_raw" %in% names(tracker) && !"target_year" %in% names(tracker)) tracker$target_year <- suppressWarnings(as.integer(tracker$target_year_raw))
  tracker
}

#' standardize tracker names
#'
standardize_tracker_names <- function(tracker) {
  tracker <- safe_df(tracker)
  if (!nrow(tracker)) return(tracker)
  if ("source_state_raw" %in% names(tracker)) tracker$source_state_key <- canonicalize_state_name(tracker$source_state_raw)
  if ("source_district_raw" %in% names(tracker)) tracker$source_district_key <- canon(tracker$source_district_raw)
  if ("target_state_raw" %in% names(tracker)) tracker$target_state_key <- canonicalize_state_name(tracker$target_state_raw)
  if ("target_district_raw" %in% names(tracker)) tracker$target_district_key <- canon(tracker$target_district_raw)
  tracker
}

# sample-end: code-district-crosswalk-qa
