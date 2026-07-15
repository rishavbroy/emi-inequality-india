# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' build district panel
#'
#' @return A district panel; an sf object when validated boundary geometry joins.
build_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg) {
  tracker <- legacy_tracker_frame(district_tracker)
  if (nrow(tracker) && legacy_named_measures_available(measures_2007, measures_2017, linguistic_distance_iv)) {
    out <- build_tracker_based_district_panel(tracker, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020)
    if (nrow(out)) return(validate_legacy_district_panel(out, cfg))
  }

  # Fallback for tests and draft diagnostics when named tracker inputs are not
  # available.  This preserves the previous key merge but still emits legacy
  # aliases expected by the report/table code.
  out <- safe_df(measures_2007)
  if (!nrow(out)) return(empty_panel())
  if (all(c("state_std", "district_std") %in% names(measures_2017))) {
    out <- merge(out, measures_2017, by = c("state_std", "district_std"), all.x = TRUE, suffixes = c("_2007", "_2017"))
  }
  if (all(c("state_std", "district_std") %in% names(linguistic_distance_iv))) {
    out <- merge(out, linguistic_distance_iv, by = c("state_std", "district_std"), all.x = TRUE)
  }
  out <- add_legacy_panel_aliases(out)
  out <- add_legacy_regions(out)
  if (!"district_panel_id" %in% names(out)) out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  out <- compute_consumption_growth_pct(out)
  out <- compute_log_consumption_difference(out)
  out <- compute_gini_change(out)
  validate_legacy_district_panel(attach_panel_geometry(out, boundaries_2020), cfg)
}

legacy_tracker_frame <- function(district_tracker) {
  tracker <- safe_df(district_tracker)
  if (all(c("state_01", "district_01", "state_07", "district_07", "state_17", "district_17", "state_20", "district_20") %in% names(tracker))) {
    return(tracker)
  }
  path <- "data/processed/district_tracker_legacy.csv"
  if (file.exists(path)) return(utils::read.csv(path, stringsAsFactors = FALSE))
  data.frame()
}

legacy_named_measures_available <- function(...) {
  dfs <- list(...)
  any(vapply(dfs, function(df) any(grepl("^state_(01|07|08|17|18)$", names(safe_df(df)))), logical(1)))
}

build_tracker_based_district_panel <- function(tracker, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020) {
  tracker$.tracker_row <- seq_len(nrow(tracker))
  out <- tracker
  out <- legacy_attach_source(out, safe_df(linguistic_distance_iv), legacy_suffix_chain("01"), source_label = "2001")
  out <- legacy_attach_source_one_to_one(out, safe_df(measures_2007), legacy_suffix_chain("08"), source_label = "2007")
  out <- legacy_attach_source(out, safe_df(measures_2017), legacy_suffix_chain("18"), source_label = "2017")

  # Some rebuilt measure targets only expose numeric standardized district keys
  # (state_std/district_std) and no longer carry the legacy name columns used by
  # the tracker.  Attach those sources after the 2007 join has supplied the
  # panel's standardized district keys; otherwise the panel silently lacks the
  # 2017 outcome and 2001 instrument required by maps and IV models.
  out <- legacy_attach_source_by_standard_keys(out, safe_df(linguistic_distance_iv), source_label = "2001")
  out <- legacy_attach_source_by_standard_keys(out, safe_df(measures_2017), source_label = "2017")

  out <- add_legacy_panel_aliases(out)
  out <- add_legacy_regions(out)
  out <- compute_consumption_growth_pct(out)
  out <- compute_log_consumption_difference(out)
  out <- compute_gini_change(out)
  if (!"district_panel_id" %in% names(out)) out$district_panel_id <- paste0("legacy_tracker_", out$.tracker_row)
  out <- attach_panel_geometry(out, boundaries_2020)
  out <- out[legacy_panel_has_analysis_core(out), , drop = FALSE]
  rownames(out) <- NULL
  out
}


legacy_suffix_chain <- function(source_suffix, years_of_interest = c("2001", "2005", "2006", "2007", "2008", "2011", "2017", "2018", "2019", "2020")) {
  suffixes <- substr(as.character(years_of_interest), 3, 4)
  source_num <- suppressWarnings(as.integer(source_suffix))
  suffix_nums <- suppressWarnings(as.integer(suffixes))
  if (!is.finite(source_num)) return(suffixes)
  suffixes[order(abs(suffix_nums - source_num), suffix_nums)]
}

legacy_panel_has_analysis_core <- function(out) {
  required <- c("EMIE", "wavg_ling_degrees", "consumption_0708", "consumption_1718")
  present <- intersect(required, names(out))
  if (!length(present)) return(rep(TRUE, nrow(out)))
  vals <- lapply(out[present], function(x) {
    if (is.list(x)) {
      vapply(x, function(value) {
        if (length(value) == 0L || all(is.na(value))) NA_character_ else paste(as.character(value), collapse = "; ")
      }, character(1))
    } else {
      x
    }
  })
  stats::complete.cases(as.data.frame(vals, stringsAsFactors = FALSE))
}

legacy_attach_source <- function(tracker, source, suffixes, source_label) {
  if (!nrow(source)) return(tracker)
  source <- add_legacy_join_keys(source, suffixes)
  if (!all(c(".legacy_state_key", ".legacy_district_key") %in% names(source))) return(tracker)
  source <- source[!duplicated(source[c(".legacy_state_key", ".legacy_district_key")]), , drop = FALSE]

  for (suffix in suffixes) {
    s_col <- paste0("state_", suffix)
    d_col <- paste0("district_", suffix)
    if (!all(c(s_col, d_col) %in% names(tracker))) next
    tracker$.legacy_state_key <- canonicalize_state_name(tracker[[s_col]])
    tracker$.legacy_district_key <- canon(tracker[[d_col]])
    already <- paste0(".matched_", source_label)
    if (!already %in% names(tracker)) tracker[[already]] <- FALSE
    to_join <- tracker[!tracker[[already]], c(".tracker_row", ".legacy_state_key", ".legacy_district_key"), drop = FALSE]
    if (!nrow(to_join)) next
    joined <- merge(to_join, source, by = c(".legacy_state_key", ".legacy_district_key"), all.x = TRUE, sort = FALSE)
    source_cols <- setdiff(names(source), c(".legacy_state_key", ".legacy_district_key"))
    has_value <- vapply(seq_len(nrow(joined)), function(i) {
      any(vapply(joined[i, source_cols, drop = FALSE], function(col) {
        value <- col[[1]]
        length(value) > 0L && !all(is.na(value))
      }, logical(1)))
    }, logical(1))
    hits <- joined[!is.na(joined$.tracker_row) & has_value, , drop = FALSE]
    if (!nrow(hits)) next
    rows <- match(hits$.tracker_row, tracker$.tracker_row)
    for (nm in setdiff(names(hits), c(".legacy_state_key", ".legacy_district_key", ".tracker_row"))) {
      if (!nm %in% names(tracker)) tracker[[nm]] <- NA
      fill <- !vapply(tracker[[nm]][rows], scalar_has_value, logical(1)) &
        vapply(hits[[nm]], scalar_has_value, logical(1))
      if (any(fill)) tracker[[nm]][rows[fill]] <- hits[[nm]][fill]
    }
    tracker[[already]][rows] <- TRUE
  }
  tracker$.legacy_state_key <- NULL
  tracker$.legacy_district_key <- NULL
  tracker
}

legacy_attach_source_one_to_one <- function(tracker, source, suffixes, source_label, max_dist = 2L) {
  if (!nrow(source)) return(tracker)
  source <- add_legacy_join_keys(source, suffixes)
  if (!all(c(".legacy_state_key", ".legacy_district_key") %in% names(source))) return(tracker)
  source$.source_row <- seq_len(nrow(source))
  source$.source_used <- FALSE

  matched <- paste0(".matched_", source_label)
  if (!matched %in% names(tracker)) tracker[[matched]] <- FALSE

  for (suffix in suffixes) {
    s_col <- paste0("state_", suffix)
    d_col <- paste0("district_", suffix)
    if (!all(c(s_col, d_col) %in% names(tracker))) next

    tracker$.legacy_state_key <- canonicalize_state_name(tracker[[s_col]])
    tracker$.legacy_district_key <- canon(tracker[[d_col]])
    tracker_open <- tracker[
      !tracker[[matched]] &
        !is.na(tracker$.legacy_state_key) &
        nzchar(tracker$.legacy_state_key) &
        !is.na(tracker$.legacy_district_key) &
        nzchar(tracker$.legacy_district_key),
      c(".tracker_row", ".legacy_state_key", ".legacy_district_key"),
      drop = FALSE
    ]
    source_open <- source[
      !source$.source_used &
        !is.na(source$.legacy_state_key) &
        nzchar(source$.legacy_state_key) &
        !is.na(source$.legacy_district_key) &
        nzchar(source$.legacy_district_key),
      c(".source_row", ".legacy_state_key", ".legacy_district_key"),
      drop = FALSE
    ]
    if (!nrow(tracker_open) || !nrow(source_open)) next

    chosen <- legacy_select_source_tracker_matches(source_open, tracker_open)
    if (!nrow(chosen)) next

    hits <- merge(chosen, source, by = ".source_row", sort = FALSE)
    rows <- match(hits$.tracker_row, tracker$.tracker_row)
    source_cols <- setdiff(
      names(hits),
      c(
        ".source_row", ".source_used", ".tracker_row",
        ".legacy_state_key", ".legacy_district_key",
        ".legacy_match_method", ".legacy_match_distance"
      )
    )
    for (nm in source_cols) {
      if (!nm %in% names(tracker)) tracker[[nm]] <- NA
      fill <- !vapply(tracker[[nm]][rows], scalar_has_value, logical(1)) &
        vapply(hits[[nm]], scalar_has_value, logical(1))
      if (any(fill)) tracker[[nm]][rows[fill]] <- hits[[nm]][fill]
    }
    tracker[[matched]][rows] <- TRUE
    source$.source_used[match(hits$.source_row, source$.source_row)] <- TRUE
    if (all(source$.source_used)) break
  }

  tracker$.legacy_state_key <- NULL
  tracker$.legacy_district_key <- NULL
  tracker
}


legacy_source_match_methods <- function() c("soundex", "qgram", "jw", "dl", "osa")

legacy_source_match_thresholds <- function() c(0, 0, 0.15, 2, 1)

legacy_select_source_tracker_matches <- function(
  source_open, tracker_open,
  methods = legacy_source_match_methods(),
  thresholds = legacy_source_match_thresholds()
) {
  if (!nrow(source_open) || !nrow(tracker_open)) {
    return(data.frame(
      .source_row = integer(),
      .tracker_row = integer(),
      .legacy_match_method = character(),
      .legacy_match_distance = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  if (length(methods) != length(thresholds)) {
    stop("Legacy district match methods and thresholds must have the same length.", call. = FALSE)
  }
  need_pkg("stringdist", "legacy district-source fuzzy matching")

  remaining_source <- source_open
  remaining_tracker <- tracker_open
  chosen <- data.frame(
    .source_row = integer(),
    .tracker_row = integer(),
    .legacy_match_method = character(),
    .legacy_match_distance = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(methods)) {
    candidates <- merge(
      remaining_source,
      remaining_tracker,
      by = ".legacy_state_key",
      suffixes = c("_source", "_tracker"),
      sort = FALSE
    )
    if (!nrow(candidates)) next

    candidates$.legacy_match_distance <- stringdist::stringdist(
      candidates$.legacy_district_key_source,
      candidates$.legacy_district_key_tracker,
      method = methods[[i]]
    )
    candidates <- candidates[
      is.finite(candidates$.legacy_match_distance) &
        candidates$.legacy_match_distance <= thresholds[[i]],
      ,
      drop = FALSE
    ]
    if (!nrow(candidates)) next
    candidates <- candidates[
      order(candidates$.legacy_match_distance, candidates$.source_row, candidates$.tracker_row),
      ,
      drop = FALSE
    ]

    picked <- data.frame(
      .source_row = integer(),
      .tracker_row = integer(),
      .legacy_match_method = character(),
      .legacy_match_distance = numeric(),
      stringsAsFactors = FALSE
    )
    used_source <- integer()
    used_tracker <- integer()
    for (j in seq_len(nrow(candidates))) {
      source_j <- candidates$.source_row[[j]]
      tracker_j <- candidates$.tracker_row[[j]]
      if (source_j %in% used_source || tracker_j %in% used_tracker) next
      picked <- rbind(
        picked,
        data.frame(
          .source_row = source_j,
          .tracker_row = tracker_j,
          .legacy_match_method = methods[[i]],
          .legacy_match_distance = candidates$.legacy_match_distance[[j]],
          stringsAsFactors = FALSE
        )
      )
      used_source <- c(used_source, source_j)
      used_tracker <- c(used_tracker, tracker_j)
    }
    if (!nrow(picked)) next

    chosen <- rbind(chosen, picked)
    remaining_source <- remaining_source[!remaining_source$.source_row %in% picked$.source_row, , drop = FALSE]
    remaining_tracker <- remaining_tracker[!remaining_tracker$.tracker_row %in% picked$.tracker_row, , drop = FALSE]
    if (!nrow(remaining_source) || !nrow(remaining_tracker)) break
  }

  rownames(chosen) <- NULL
  chosen
}

add_legacy_join_keys <- function(source, suffixes) {
  candidates <- unlist(lapply(suffixes, function(sfx) list(
    c(paste0("state_", sfx), paste0("district_", sfx)),
    c(paste0("state_", sfx), paste0("district_", sfx, "_name"))
  )), recursive = FALSE)
  candidates <- c(candidates, list(c("state_0708", "district_0708"), c("state_1718", "district_1718"), c("state", "district"), c("state", "district_name")))
  for (pair in candidates) {
    if (all(pair %in% names(source))) {
      source$.legacy_state_key <- canonicalize_state_name(source[[pair[[1]]]])
      source$.legacy_district_key <- canon(source[[pair[[2]]]])
      return(source)
    }
  }
  source
}

legacy_attach_source_by_standard_keys <- function(panel, source, source_label = NULL) {
  if (!nrow(panel) || !nrow(source)) return(panel)
  if (!all(c("state_std", "district_std") %in% names(panel))) return(panel)
  if (!all(c("state_std", "district_std") %in% names(source))) return(panel)

  source <- source[!is.na(source$state_std) & !is.na(source$district_std), , drop = FALSE]
  if (!nrow(source)) return(panel)
  source$.std_state_key <- canon(source$state_std)
  source$.std_district_key <- canon(source$district_std)
  source <- source[!duplicated(source[c(".std_state_key", ".std_district_key")]), , drop = FALSE]

  panel$.std_state_key <- canon(panel$state_std)
  panel$.std_district_key <- canon(panel$district_std)
  joined <- merge(
    panel[c(".tracker_row", ".std_state_key", ".std_district_key")],
    source,
    by = c(".std_state_key", ".std_district_key"),
    all.x = TRUE,
    sort = FALSE
  )

  source_cols <- setdiff(
    names(joined),
    c(".std_state_key", ".std_district_key", ".tracker_row", "state_std", "district_std")
  )
  if (!length(source_cols)) {
    panel$.std_state_key <- NULL
    panel$.std_district_key <- NULL
    return(panel)
  }

  has_value <- vapply(seq_len(nrow(joined)), function(i) {
    any(vapply(joined[i, source_cols, drop = FALSE], scalar_has_value, logical(1)))
  }, logical(1))
  hits <- joined[!is.na(joined$.tracker_row) & has_value, c(".tracker_row", source_cols), drop = FALSE]
  if (!nrow(hits)) {
    panel$.std_state_key <- NULL
    panel$.std_district_key <- NULL
    return(panel)
  }

  rows <- match(hits$.tracker_row, panel$.tracker_row)
  for (nm in source_cols) {
    if (!nm %in% names(panel)) panel[[nm]] <- NA
    fill <- !vapply(panel[[nm]][rows], scalar_has_value, logical(1)) &
      vapply(hits[[nm]], scalar_has_value, logical(1))
    if (any(fill)) panel[[nm]][rows[fill]] <- hits[[nm]][fill]
  }

  if (!is.null(source_label)) {
    matched <- paste0(".matched_", source_label)
    if (!matched %in% names(panel)) panel[[matched]] <- FALSE
    panel[[matched]][rows] <- TRUE
  }

  panel$.std_state_key <- NULL
  panel$.std_district_key <- NULL
  panel
}

scalar_has_value <- function(x) {
  if (is.data.frame(x)) return(nrow(x) > 0L)
  if (is.list(x)) {
    return(any(vapply(x, scalar_has_value, logical(1))))
  }
  length(x) > 0L && !all(is.na(x))
}

add_legacy_panel_aliases <- function(out) {
  alias <- function(new, old) if (old %in% names(out) && !new %in% names(out)) out[[new]] <<- out[[old]]
  alias("EMIE", "emie_2007")
  alias("emie_2007", "EMIE")
  alias("consumption_0708", "consumption_2007")
  alias("consumption_2007", "consumption_0708")
  alias("gini_cons_0708", "gini_consumption_2007")
  alias("gini_consumption_2007", "gini_cons_0708")
  alias("consumption_1718", "consumption_2017")
  alias("consumption_2017", "consumption_1718")
  alias("gini_cons_1718", "gini_consumption_2017")
  alias("gini_consumption_2017", "gini_cons_1718")
  alias("pucca_share_2007", "pct_pucca")
  alias("pct_pucca", "pucca_share_2007")
  alias("head_secondary_plus_2007", "pct_head_secondary_plus")
  alias("npeople_0708", "n_2007")
  alias("n_2007", "npeople_0708")
  alias("npeople_1718", "n_2017")
  alias("n_2017", "npeople_1718")
  if (!"nhouses_0708" %in% names(out) && "n_households_2007" %in% names(out)) out$nhouses_0708 <- out$n_households_2007
  if (!"nhouses_1718" %in% names(out) && "n_households_2017" %in% names(out)) out$nhouses_1718 <- out$n_households_2017
  if (!"state_std" %in% names(out) && "state_20" %in% names(out)) out$state_std <- canonicalize_state_name(out$state_20)
  if (!"district_std" %in% names(out) && "district_20" %in% names(out)) out$district_std <- canonicalize_district_name(out$district_20)
  out
}

add_legacy_regions <- function(out) {
  valid_regions <- c("North", "Central", "East", "West", "South")
  if ("region" %in% names(out)) {
    current <- as.character(out$region)
    current_nonmissing <- stats::na.omit(current)
    if (length(current_nonmissing) && all(current_nonmissing %in% valid_regions)) return(out)
  }

  state <- NULL
  for (nm in c("state_20", "state_17", "state_07", "state_01", "state_std")) {
    if (nm %in% names(out)) { state <- canon(out[[nm]]); break }
  }
  if (is.null(state)) return(out)
  north <- canon(c("Jammu & Kashmir", "Himachal Pradesh", "Punjab", "Chandigarh", "Uttarakhand", "Haryana", "Delhi", "Rajasthan"))
  central <- canon(c("Uttar Pradesh", "Chhattisgarh", "Madhya Pradesh"))
  east <- canon(c("Bihar", "Sikkim", "Arunachal Pradesh", "Nagaland", "Manipur", "Mizoram", "Tripura", "Meghalaya", "Assam", "West Bengal", "Jharkhand", "Odisha"))
  west <- canon(c("Gujarat", "Daman & Diu", "Dadra & Nagar Haveli", "Maharashtra", "Goa"))
  south <- canon(c("Andhra Pradesh", "Karnataka", "Lakshadweep", "Kerala", "Tamil Nadu", "Puducherry", "Andaman & Nicobar Islands", "Telangana"))
  out$region <- ifelse(state %in% north, "North",
    ifelse(state %in% central, "Central",
      ifelse(state %in% east, "East",
        ifelse(state %in% west, "West",
          ifelse(state %in% south, "South", NA_character_)))))
  out$region <- factor(out$region, levels = valid_regions)
  out
}

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

compute_consumption_growth_pct <- function(panel) {
  if (all(c("consumption_1718", "consumption_0708") %in% names(panel))) {
    growth <- (num(panel$consumption_1718) - num(panel$consumption_0708)) / num(panel$consumption_0708) * 100
    panel$consumption_pct_change <- growth
    panel$consumption_growth_pct <- growth
  } else if (all(c("consumption_2017", "consumption_2007") %in% names(panel))) {
    growth <- (num(panel$consumption_2017) - num(panel$consumption_2007)) / num(panel$consumption_2007) * 100
    panel$consumption_growth_pct <- growth
    panel$consumption_pct_change <- growth
  }
  panel
}

compute_log_consumption_difference <- function(panel) {
  if (all(c("consumption_1718", "consumption_0708") %in% names(panel))) {
    panel$log_consumption_difference <- log(num(panel$consumption_1718)) - log(num(panel$consumption_0708))
  } else if (all(c("consumption_2017", "consumption_2007") %in% names(panel))) {
    panel$log_consumption_difference <- log(num(panel$consumption_2017)) - log(num(panel$consumption_2007))
  }
  panel
}

compute_gini_change <- function(panel) {
  if (all(c("gini_cons_1718", "gini_cons_0708") %in% names(panel))) {
    panel$gini_change <- num(panel$gini_cons_1718) - num(panel$gini_cons_0708)
  } else if (all(c("gini_consumption_2017", "gini_consumption_2007") %in% names(panel))) {
    panel$gini_change <- num(panel$gini_consumption_2017) - num(panel$gini_consumption_2007)
  }
  panel
}

#' Save processed district tracker
#'
#' Write the public processed district tracker artifact expected by `_targets.R`.
#' Geometry/list columns are flattened so the file is a portable CSV.
save_processed_district_tracker <- function(district_tracker, path = "data/processed/district_tracker_2001_2007_2017_2020.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- legacy_tracker_frame(district_tracker)
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

legacy_panel_validation_failures <- function(out) {
  df <- as.data.frame(out)
  failures <- character()
  add <- function(...) failures <<- c(failures, paste0(...))

  required <- c("EMIE", "wavg_ling_degrees", "npeople_0708", "consumption_0708", "gini_cons_0708", "consumption_1718", "gini_cons_1718", "consumption_pct_change", "gini_change")
  missing <- setdiff(required, names(df))
  if (length(missing)) add("district_panel is missing required legacy columns: ", paste(missing, collapse = ", "))

  present_required <- intersect(required, names(df))
  if (length(present_required)) {
    incomplete <- !stats::complete.cases(df[present_required])
    if (any(incomplete)) add("district_panel has ", sum(incomplete), " rows with missing core IV analysis values.")
  }

  if ("district_panel_id" %in% names(df) && anyDuplicated(df$district_panel_id)) {
    add("district_panel_id is not unique after tracker/source matching.")
  }

  for (flag in c(".matched_2001", ".matched_2007", ".matched_2017")) {
    if (flag %in% names(df) && any(!isTRUEish(df[[flag]]), na.rm = TRUE)) {
      add("district_panel contains rows without ", flag, " after final core filtering.")
    }
  }

  if ("EMIE" %in% names(df)) {
    emie <- num(df$EMIE)
    if (mean(emie, na.rm = TRUE) < 10 || max(emie, na.rm = TRUE) < 90) add("EMIE scale is inconsistent with legacy 0-100 percentage scale.")
  }
  if ("npeople_0708" %in% names(df) && mean(num(df$npeople_0708), na.rm = TRUE) < 10000) {
    add("npeople_0708 looks like a sample count rather than weighted population.")
  }
  if ("dependency_ratio" %in% names(df) && mean(num(df$dependency_ratio), na.rm = TRUE) > 80) {
    add("dependency_ratio mean is implausibly high relative to the legacy table; verify weighted numerator/denominator construction.")
  }
  failures
}

isTRUEish <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true", "1")
}

validate_legacy_district_panel <- function(out, cfg = list(), strict = isTRUE(cfg$strict_legacy_panel_validation)) {
  out <- validate_district_panel(out, strict = isTRUE(cfg$strict_district_panel_validation))
  if (!identical(cfg$mode, "final")) return(out)
  failures <- legacy_panel_validation_failures(out)
  attr(out, "legacy_panel_validation_failures") <- failures
  if (length(failures) && isTRUE(strict)) {
    stop(paste(failures, collapse = "\n"), call. = FALSE)
  }
  out
}
