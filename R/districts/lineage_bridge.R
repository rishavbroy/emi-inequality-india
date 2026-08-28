# Census registries and stable-locality transition matrices.

pad_admin_code <- function(x, width) {
  x <- trimws(plain_chr(x))
  x <- sub("\\.0+$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[!nzchar(x)] <- NA_character_
  out <- ifelse(is.na(x), NA_character_, sprintf(paste0("%0", width, "d"), suppressWarnings(as.integer(x))))
  out[grepl("^0+$", out)] <- NA_character_
  out
}

shrug_census_code_widths <- function(year) {
  year <- as.integer(year)
  widths <- switch(
    as.character(year),
    `1991` = c(district = 2L, subdistrict = 4L),
    `2001` = c(district = 2L, subdistrict = 4L),
    `2011` = c(district = 3L, subdistrict = 5L),
    NULL
  )
  if (is.null(widths)) {
    stop("Unsupported SHRUG Population Census year: ", year, call. = FALSE)
  }
  widths
}

first_matching_column <- function(df, patterns, exclude = character()) {
  nms <- names(df)
  keys <- canon(nms)
  keep <- !keys %in% canon(exclude)
  for (pattern in patterns) {
    hit <- which(keep & grepl(pattern, keys))
    if (length(hit)) return(nms[hit[[1]]])
  }
  NULL
}

standardize_shrug_locality_key <- function(x, year, sector) {
  x <- safe_df(x)
  year2 <- substr(as.character(year), 3, 4)
  shrid <- first_col(x, c("shrid2", "shrid"))
  state <- first_matching_column(x, c(paste0("pc", year2, " state id"), "state id", "state code"))
  district <- first_matching_column(x, c(paste0("pc", year2, " district id"), "district id", "district code"))
  subdistrict <- first_matching_column(x, c(paste0("pc", year2, " subdistrict id"), "subdistrict id", "sub district id"))
  locality <- first_matching_column(x, c(paste0("pc", year2, " town village id"), "town village id", "village id", "town id"))
  population <- first_matching_column(x, c("pca tot p", "population", "pop total", "total population", "pop"))
  area <- first_matching_column(x, c("land area", "area"))
  if (is.null(shrid)) stop("SHRUG locality key is missing shrid2.", call. = FALSE)

  widths <- shrug_census_code_widths(year)
  district_width <- widths[["district"]]
  subdistrict_width <- widths[["subdistrict"]]
  n <- nrow(x)
  data.frame(
    shrid2 = plain_chr(x[[shrid]]),
    census_year = rep(as.integer(year), n),
    sector = rep(sector, n),
    state_code = if (!is.null(state)) pad_admin_code(x[[state]], 2L) else rep(NA_character_, n),
    district_code = if (!is.null(district)) pad_admin_code(x[[district]], district_width) else rep(NA_character_, n),
    subdistrict_code = if (!is.null(subdistrict)) pad_admin_code(x[[subdistrict]], subdistrict_width) else rep(NA_character_, n),
    locality_code = if (!is.null(locality)) plain_chr(x[[locality]]) else rep(NA_character_, n),
    population = if (!is.null(population)) num(x[[population]]) else rep(NA_real_, n),
    area = if (!is.null(area)) num(x[[area]]) else rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
}

standardize_shrug_district_key <- function(x, year) {
  x <- safe_df(x)
  year2 <- substr(as.character(year), 3, 4)
  shrid <- first_col(x, c("shrid2", "shrid"))
  state <- first_matching_column(x, c(paste0("pc", year2, " state id"), "state id", "state code"))
  district <- first_matching_column(x, c(paste0("pc", year2, " district id"), "district id", "district code"))
  if (is.null(shrid) || is.null(district)) {
    stop("SHRUG district key must contain shrid2 and a district identifier.", call. = FALSE)
  }
  district_width <- shrug_census_code_widths(year)[["district"]]
  data.frame(
    shrid2 = plain_chr(x[[shrid]]),
    census_year = as.integer(year),
    state_code = if (!is.null(state)) pad_admin_code(x[[state]], 2L) else NA_character_,
    district_code = pad_admin_code(x[[district]], district_width),
    stringsAsFactors = FALSE
  )
}

sum_finite_or_na <- function(x) {
  x <- num(x)
  finite <- is.finite(x)
  if (!any(finite)) return(NA_real_)
  sum(x[finite])
}

duplicate_ids <- function(x) {
  unique(x[duplicated(x) | duplicated(x, fromLast = TRUE)])
}

aggregate_shrid_weights <- function(locality_keys) {
  locality_keys <- safe_df(locality_keys)
  if (!nrow(locality_keys)) {
    return(data.frame(shrid2 = character(), population = numeric(), area = numeric(), stringsAsFactors = FALSE))
  }
  locality_keys <- locality_keys[
    !is.na(locality_keys$shrid2) & nzchar(locality_keys$shrid2),
    c("shrid2", "population", "area"),
    drop = FALSE
  ]
  duplicate <- duplicate_ids(locality_keys$shrid2)
  single <- locality_keys[!locality_keys$shrid2 %in% duplicate, , drop = FALSE]
  if (!length(duplicate)) return(single)

  repeated <- locality_keys[locality_keys$shrid2 %in% duplicate, , drop = FALSE]
  groups <- split(seq_len(nrow(repeated)), repeated$shrid2)
  combined <- safe_bind_rows(lapply(groups, function(i) {
    data.frame(
      shrid2 = repeated$shrid2[[i[[1]]]],
      population = sum_finite_or_na(repeated$population[i]),
      area = sum_finite_or_na(repeated$area[i]),
      stringsAsFactors = FALSE
    )
  }))
  safe_bind_rows(list(single, combined))
}

unique_shrid_district_membership <- function(key, suffix) {
  key <- unique(safe_df(key)[c("shrid2", "state_code", "district_code")])
  key <- key[!is.na(key$shrid2) & nzchar(key$shrid2), , drop = FALSE]
  duplicate <- duplicate_ids(key$shrid2)
  single <- key[!key$shrid2 %in% duplicate, , drop = FALSE]
  single$n_state_memberships <- as.integer(!is.na(single$state_code) & nzchar(single$state_code))
  single$n_district_memberships <- as.integer(!is.na(single$district_code) & nzchar(single$district_code))
  single$deterministic <- single$n_state_memberships == 1L & single$n_district_memberships == 1L

  repeated <- key[key$shrid2 %in% duplicate, , drop = FALSE]
  groups <- split(seq_len(nrow(repeated)), repeated$shrid2)
  combined <- safe_bind_rows(lapply(groups, function(i) {
    states <- unique(repeated$state_code[i][!is.na(repeated$state_code[i]) & nzchar(repeated$state_code[i])])
    districts <- unique(repeated$district_code[i][!is.na(repeated$district_code[i]) & nzchar(repeated$district_code[i])])
    data.frame(
      shrid2 = repeated$shrid2[[i[[1]]]],
      state_code = if (length(states) == 1L) states else NA_character_,
      district_code = if (length(districts) == 1L) districts else NA_character_,
      n_state_memberships = length(states),
      n_district_memberships = length(districts),
      deterministic = length(states) == 1L && length(districts) == 1L,
      stringsAsFactors = FALSE
    )
  }))
  out <- safe_bind_rows(list(single, combined))
  stats::setNames(out, c(
    "shrid2", paste0("state_code_", suffix), paste0("district_code_", suffix),
    paste0("n_state_memberships_", suffix), paste0("n_district_memberships_", suffix),
    paste0("deterministic_", suffix)
  ))
}

shrid_bridge_status <- function(bridge, years = c(2001L, 2011L)) {
  bridge <- safe_df(bridge)
  years <- as.integer(years)
  if (length(years) != 2L || anyDuplicated(years)) {
    stop("SHRUG bridge status requires exactly two distinct Census years.", call. = FALSE)
  }
  n <- nrow(bridge)
  status <- rep("missing_census_membership", n)
  crosses <- rep(FALSE, n)
  missing_locality <- rep(FALSE, n)
  for (year in years) {
    crosses <- crosses |
      (num(bridge[[paste0("n_state_memberships_", year)]]) > 1L) %in% TRUE |
      (num(bridge[[paste0("n_district_memberships_", year)]]) > 1L) %in% TRUE
    missing_locality <- missing_locality |
      !(bridge[[paste0("has_locality_key_", year)]] %in% TRUE)
  }
  deterministic <- bridge$deterministic %in% TRUE

  status[crosses] <- "crosses_district_boundary"
  status[!crosses & missing_locality] <- "missing_census_locality_key"
  status[deterministic] <- "deterministic_one_district_each_year"
  status
}

build_shrug_district_bridge_between_years <- function(
    source_r, source_u, target_r, target_u, source_dist, target_dist,
    source_year, target_year) {
  source_year <- as.integer(source_year)
  target_year <- as.integer(target_year)
  if (identical(source_year, target_year)) {
    stop("SHRUG district bridge requires distinct source and target Census years.", call. = FALSE)
  }
  shrug_census_code_widths(source_year)
  shrug_census_code_widths(target_year)

  source_loc <- safe_bind_rows(list(
    standardize_shrug_locality_key(source_r, source_year, "rural"),
    standardize_shrug_locality_key(source_u, source_year, "urban")
  ))
  target_loc <- safe_bind_rows(list(
    standardize_shrug_locality_key(target_r, target_year, "rural"),
    standardize_shrug_locality_key(target_u, target_year, "urban")
  ))
  source_membership <- unique_shrid_district_membership(
    standardize_shrug_district_key(source_dist, source_year), as.character(source_year)
  )
  target_membership <- unique_shrid_district_membership(
    standardize_shrug_district_key(target_dist, target_year), as.character(target_year)
  )
  weights <- aggregate_shrid_weights(source_loc)

  bridge <- merge(source_membership, target_membership, by = "shrid2", all = TRUE, sort = FALSE)
  bridge <- merge(bridge, weights, by = "shrid2", all.x = TRUE, sort = FALSE)
  bridge[[paste0("has_locality_key_", source_year)]] <- bridge$shrid2 %in% unique(source_loc$shrid2)
  bridge[[paste0("has_locality_key_", target_year)]] <- bridge$shrid2 %in% unique(target_loc$shrid2)
  bridge$deterministic <-
    bridge[[paste0("deterministic_", source_year)]] %in% TRUE &
    bridge[[paste0("deterministic_", target_year)]] %in% TRUE &
    bridge[[paste0("has_locality_key_", source_year)]] &
    bridge[[paste0("has_locality_key_", target_year)]]
  bridge$bridge_status <- shrid_bridge_status(bridge, c(source_year, target_year))
  attr(bridge, paste0("locality_keys_", source_year)) <- source_loc
  attr(bridge, paste0("locality_keys_", target_year)) <- target_loc
  attr(bridge, "source_year") <- source_year
  attr(bridge, "target_year") <- target_year
  bridge
}

#' Build the production Census-2011 to Census-2001 SHRUG district bridge
build_shrug_district_bridge <- function(pc01r, pc01u, pc11r, pc11u, pc01dist, pc11dist) {
  build_shrug_district_bridge_between_years(
    source_r = pc11r, source_u = pc11u,
    target_r = pc01r, target_u = pc01u,
    source_dist = pc11dist, target_dist = pc01dist,
    source_year = 2011L, target_year = 2001L
  )
}

#' Build the historical Census-1991 to Census-2001 SHRUG district bridge
build_shrug_district_bridge_1991_2001 <- function(pc91r, pc91u, pc01r, pc01u, pc91dist, pc01dist) {
  build_shrug_district_bridge_between_years(
    source_r = pc91r, source_u = pc91u,
    target_r = pc01r, target_u = pc01u,
    source_dist = pc91dist, target_dist = pc01dist,
    source_year = 1991L, target_year = 2001L
  )
}

weighted_share <- function(x, group_total) {
  x <- num(x)
  group_total <- num(group_total)
  ifelse(is.finite(x) & is.finite(group_total) & group_total > 0, x / group_total, NA_real_)
}


#' Build an official Census-2011 to Census-2001 district-code bridge
#'
#' LGD's historical district modification report carries both Census codes for
#' districts existing in the 2001--2011 interval. Rows without a Census-2001
#' code are newly created districts and are intentionally left to the reviewed
#' event/allocation machinery.
build_lgd_district_transition_2001_2011 <- function(lgd_mod_districts) {
  x <- safe_df(lgd_mod_districts)
  required <- c("state_lgd_code", "census2001_code", "census2011_code")
  missing <- setdiff(required, names(x))
  if (length(missing)) return(data.frame())
  out <- unique(data.frame(
    state_code_2011 = pad_admin_code(x$state_lgd_code, 2L),
    district_code_2011 = pad_admin_code(x$census2011_code, 3L),
    state_code_2001 = pad_admin_code(x$state_lgd_code, 2L),
    district_code_2001 = pad_admin_code(x$census2001_code, 2L),
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    source_id = "lgd_mod_districts_2001_2011",
    stringsAsFactors = FALSE
  ))
  keep <- !is.na(out$state_code_2011) & nzchar(out$state_code_2011) &
    !is.na(out$district_code_2011) & nzchar(out$district_code_2011) &
    !is.na(out$district_code_2001) & nzchar(out$district_code_2001)
  out <- out[keep, , drop = FALSE]
  key <- paste(out$state_code_2011, out$district_code_2011, sep = "__")
  conflicting <- key %in% key[duplicated(key) | duplicated(key, fromLast = TRUE)]
  if (any(conflicting)) {
    target <- paste(out$state_code_2001, out$district_code_2001, sep = "__")
    bad <- vapply(split(target[conflicting], key[conflicting]), function(z) length(unique(z)) > 1L, logical(1))
    if (any(bad)) stop("LGD Census-code bridge has conflicting targets.", call. = FALSE)
    out <- out[!duplicated(key), , drop = FALSE]
  }
  out
}

build_reviewed_ancestry_transition_2001_2011 <- function(
  admin_events, admin_2001, admin_2011
) {
  events <- safe_df(admin_events)
  if (!nrow(events)) return(data.frame())
  required <- c("from_unit", "to_unit", "source_id", "status")
  missing <- setdiff(required, names(events))
  if (length(missing)) {
    stop(
      "Reviewed administrative events lack transition fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  events <- events[
    events$status %in% "accepted" &
      grepl("^pc2001__[0-9]{2}__[0-9]{2}$", events$from_unit) &
      grepl("^pc2011__[0-9]{2}__[0-9]{3}$", events$to_unit),
    required,
    drop = FALSE
  ]
  if (!nrow(events)) return(data.frame())

  child_counts <- table(events$to_unit)
  ambiguous <- names(child_counts[child_counts > 1L])
  events <- events[!events$to_unit %in% ambiguous, , drop = FALSE]
  if (!nrow(events)) return(data.frame())

  source_units <- unique(plain_chr(safe_df(admin_2011)$unit_id %||% character()))
  target_units <- unique(plain_chr(safe_df(admin_2001)$unit_id %||% character()))
  unknown_source <- !events$to_unit %in% source_units
  unknown_target <- !events$from_unit %in% target_units
  if (any(unknown_source)) {
    stop(
      "Reviewed ancestry events reference unknown Census-2011 districts: ",
      paste(unique(events$to_unit[unknown_source]), collapse = ", "),
      call. = FALSE
    )
  }
  if (any(unknown_target)) {
    stop(
      "Reviewed ancestry events reference unknown Census-2001 districts: ",
      paste(unique(events$from_unit[unknown_target]), collapse = ", "),
      call. = FALSE
    )
  }

  source_parts <- strsplit(events$to_unit, "__", fixed = TRUE)
  target_parts <- strsplit(events$from_unit, "__", fixed = TRUE)
  out <- data.frame(
    state_code_2011 = vapply(source_parts, `[[`, character(1), 2L),
    district_code_2011 = vapply(source_parts, `[[`, character(1), 3L),
    state_code_2001 = vapply(target_parts, `[[`, character(1), 2L),
    district_code_2001 = vapply(target_parts, `[[`, character(1), 3L),
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "reviewed_single_parent_ancestry",
    source_id = plain_chr(events$source_id),
    stringsAsFactors = FALSE
  )
  key <- transition_source_key(out)
  if (anyDuplicated(key)) {
    stop("Reviewed ancestry transition must contain one target per 2011 district.", call. = FALSE)
  }
  out
}

transition_source_key <- function(x) {
  paste(
    pad_admin_code(x$state_code_2011, 2L),
    pad_admin_code(x$district_code_2011, 3L),
    sep = "__"
  )
}

deterministic_transition_mapping_classes <- function() {
  c(
    "deterministic_containment",
    "official_lgd_census_code_bridge",
    "reviewed_single_parent_ancestry"
  )
}

#' Combine district transitions by evidence priority
#'
#' Valid LGD Census-code links retain highest priority. Reviewed one-parent
#' ancestry is next: it fills sources with no LGD code bridge and replaces an
#' LGD row only when that row points outside the authoritative Census-2001
#' registry. Locality-derived SHRUG containment is the fallback. Invalid LGD
#' targets without reviewed ancestry remain in the combined table so the final
#' registry-validity gate fails loudly.
combine_district_transitions_2001_2011 <- function(
  shrug_transition, lgd_transition, reviewed_transition = data.frame(),
  admin_2001 = data.frame()
) {
  shrug <- safe_df(shrug_transition)
  lgd <- safe_df(lgd_transition)
  reviewed <- safe_df(reviewed_transition)

  lgd_keys <- if (nrow(lgd)) transition_source_key(lgd) else character()
  reviewed_keys <- if (nrow(reviewed)) transition_source_key(reviewed) else character()

  reviewed_use <- reviewed[
    if (nrow(reviewed)) !reviewed_keys %in% lgd_keys else logical(),
    , drop = FALSE
  ]

  if (nrow(lgd) && nrow(reviewed) && nrow(safe_df(admin_2001))) {
    valid_targets <- unique(plain_chr(safe_df(admin_2001)$unit_id %||% character()))
    invalid_lgd <- !transition_target_unit_2001(lgd) %in% valid_targets
    invalid_keys <- lgd_keys[invalid_lgd]
    reviewed_replacements <- reviewed[
      reviewed_keys %in% invalid_keys,
      , drop = FALSE
    ]
    replacement_keys <- transition_source_key(reviewed_replacements)
    if (length(replacement_keys)) {
      lgd <- lgd[
        !(invalid_lgd & lgd_keys %in% replacement_keys),
        , drop = FALSE
      ]
      reviewed_use <- safe_bind_rows(list(reviewed_use, reviewed_replacements))
    }
  }

  preferred_keys <- unique(c(
    if (nrow(lgd)) transition_source_key(lgd) else character(),
    if (nrow(reviewed_use)) transition_source_key(reviewed_use) else character()
  ))
  if (nrow(shrug) && length(preferred_keys)) {
    shrug <- shrug[!transition_source_key(shrug) %in% preferred_keys, , drop = FALSE]
  }
  safe_bind_rows(list(reviewed_use, lgd, shrug))
}

transition_target_unit_2001 <- function(transition) {
  x <- safe_df(transition)
  paste0(
    "pc2001__",
    pad_admin_code(x$state_code_2001, 2L), "__",
    pad_admin_code(x$district_code_2001, 2L)
  )
}

validate_district_transition_targets <- function(transition, admin_2001) {
  x <- safe_df(transition)
  if (!nrow(x)) return(invisible(TRUE))
  target <- transition_target_unit_2001(x)
  valid <- unique(plain_chr(safe_df(admin_2001)$unit_id %||% character()))
  unknown <- !target %in% valid
  if (any(unknown)) {
    source <- paste0(
      "pc2011__",
      pad_admin_code(x$state_code_2011[unknown], 2L), "__",
      pad_admin_code(x$district_code_2011[unknown], 3L)
    )
    pairs <- paste(source, "->", target[unknown])
    stop(
      "District transition references unknown Census-2001 target units: ",
      paste(unique(pairs), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Aggregate deterministic SHRID mappings to district transition weights
#'
#' Shares use the full source-year district denominator. Consequently, excluded
#' cross-boundary or missing-membership SHRID units make the weights sum to less
#' than one instead of being silently renormalized away.
build_district_transition_between_years <- function(shrid_bridge, source_year, target_year) {
  all_rows <- safe_df(shrid_bridge)
  source_year <- as.integer(source_year)
  target_year <- as.integer(target_year)
  source_state <- paste0("state_code_", source_year)
  source_district <- paste0("district_code_", source_year)
  target_state <- paste0("state_code_", target_year)
  target_district <- paste0("district_code_", target_year)
  required <- c(
    "shrid2", "deterministic", "population", "area",
    source_state, source_district, target_state, target_district
  )
  missing <- setdiff(required, names(all_rows))
  if (length(missing)) {
    stop("SHRUG bridge lacks transition fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  source_rows <- all_rows[
    !is.na(all_rows[[source_state]]) & !is.na(all_rows[[source_district]]),
    , drop = FALSE
  ]
  mapped <- source_rows[
    source_rows$deterministic %in% TRUE &
      !is.na(source_rows[[target_state]]) & !is.na(source_rows[[target_district]]),
    , drop = FALSE
  ]
  if (!nrow(mapped)) return(data.frame())

  source_key_all <- interaction(source_rows[[source_state]], source_rows[[source_district]], drop = TRUE)
  source_totals <- safe_bind_rows(lapply(split(seq_len(nrow(source_rows)), source_key_all), function(i) {
    out <- data.frame(
      n_shrid_total = length(unique(source_rows$shrid2[i])),
      population_total = sum_finite_or_na(source_rows$population[i]),
      area_total = sum_finite_or_na(source_rows$area[i]),
      stringsAsFactors = FALSE
    )
    out[[source_state]] <- source_rows[[source_state]][[i[[1]]]]
    out[[source_district]] <- source_rows[[source_district]][[i[[1]]]]
    out[c(source_state, source_district, "n_shrid_total", "population_total", "area_total")]
  }))

  transition_key <- interaction(
    mapped[[source_state]], mapped[[source_district]],
    mapped[[target_state]], mapped[[target_district]],
    drop = TRUE
  )
  rows <- safe_bind_rows(lapply(split(seq_len(nrow(mapped)), transition_key), function(i) {
    out <- data.frame(
      n_shrid_mapped = length(unique(mapped$shrid2[i])),
      population_mapped = sum_finite_or_na(mapped$population[i]),
      area_mapped = sum_finite_or_na(mapped$area[i]),
      stringsAsFactors = FALSE
    )
    out[[source_state]] <- mapped[[source_state]][[i[[1]]]]
    out[[source_district]] <- mapped[[source_district]][[i[[1]]]]
    out[[target_state]] <- mapped[[target_state]][[i[[1]]]]
    out[[target_district]] <- mapped[[target_district]][[i[[1]]]]
    out[c(
      source_state, source_district, target_state, target_district,
      "n_shrid_mapped", "population_mapped", "area_mapped"
    )]
  }))
  rows <- merge(
    rows, source_totals,
    by = c(source_state, source_district),
    all.x = TRUE, sort = FALSE
  )

  population_share <- paste0("population_share_to_", target_year)
  area_share <- paste0("area_share_to_", target_year)
  n_targets <- paste0("n_target_", target_year, "_districts")
  rows[[paste0("population_", source_year, "_mapped")]] <- rows$population_mapped
  rows[[paste0("area_", source_year, "_mapped")]] <- rows$area_mapped
  rows[[paste0("population_", source_year, "_total")]] <- rows$population_total
  rows[[paste0("area_", source_year, "_total")]] <- rows$area_total
  rows[[population_share]] <- weighted_share(rows$population_mapped, rows$population_total)
  rows[[area_share]] <- weighted_share(rows$area_mapped, rows$area_total)

  source_summary <- summarize_shrug_source_district_mapping(
    all_rows, source_year, target_year, min_population_coverage = 1
  )
  source_summary <- source_summary[c(
    source_state, source_district,
    "n_target_districts", "shrid_coverage", "exact_one_to_one"
  )]
  names(source_summary)[names(source_summary) == "n_target_districts"] <- n_targets
  rows <- merge(
    rows, source_summary,
    by = c(source_state, source_district),
    all.x = TRUE, sort = FALSE
  )
  rows$mapping_class <- ifelse(
    rows$exact_one_to_one %in% TRUE,
    "deterministic_containment",
    "non_nested_or_incomplete"
  )
  rows$exact_one_to_one <- NULL
  rows$population_mapped <- NULL
  rows$area_mapped <- NULL
  rows$population_total <- NULL
  rows$area_total <- NULL
  ordered <- c(
    source_state, source_district, target_state, target_district,
    "n_shrid_mapped",
    paste0("population_", source_year, "_mapped"),
    paste0("area_", source_year, "_mapped"),
    "n_shrid_total",
    paste0("population_", source_year, "_total"),
    paste0("area_", source_year, "_total"),
    population_share, area_share, n_targets, "shrid_coverage", "mapping_class"
  )
  rows <- rows[ordered]
  rows[order(rows[[source_state]], rows[[source_district]], -rows[[population_share]]), , drop = FALSE]
}


#' Summarize deterministic source-district coverage across Census years
#'
#' This is the district-level coverage contract shared by historical validation
#' and production lineage diagnostics. Preferred single-target mappings may
#' tolerate incomplete locality coverage only when the represented source-year
#' population meets the caller's explicit threshold; exact one-to-one remains
#' reserved for complete SHRID coverage.
summarize_shrug_source_district_mapping <- function(
    shrid_bridge, source_year, target_year, min_population_coverage = 1) {
  bridge <- safe_df(shrid_bridge)
  source_year <- as.integer(source_year)
  target_year <- as.integer(target_year)
  if (!is.numeric(min_population_coverage) || length(min_population_coverage) != 1L ||
      !is.finite(min_population_coverage) || min_population_coverage <= 0 ||
      min_population_coverage > 1) {
    stop("District mapping population coverage must be in (0, 1].", call. = FALSE)
  }
  source_state <- paste0("state_code_", source_year)
  source_district <- paste0("district_code_", source_year)
  target_state <- paste0("state_code_", target_year)
  target_district <- paste0("district_code_", target_year)
  required <- c(
    "shrid2", "deterministic", "population",
    source_state, source_district, target_state, target_district
  )
  missing <- setdiff(required, names(bridge))
  if (length(missing)) {
    stop(
      "SHRUG source-district mapping lacks fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  source <- bridge[
    !is.na(bridge[[source_state]]) & !is.na(bridge[[source_district]]),
    , drop = FALSE
  ]
  if (!nrow(source)) return(data.frame())

  key <- interaction(source[[source_state]], source[[source_district]], drop = TRUE)
  out <- safe_bind_rows(lapply(split(seq_len(nrow(source)), key), function(i) {
    part <- source[i, , drop = FALSE]
    deterministic <- part$deterministic %in% TRUE &
      !is.na(part[[target_state]]) & !is.na(part[[target_district]])
    targets <- unique(paste(
      part[[target_state]][deterministic],
      part[[target_district]][deterministic],
      sep = "__"
    ))
    n_total <- length(unique(part$shrid2))
    n_mapped <- length(unique(part$shrid2[deterministic]))
    population_total <- sum_finite_or_na(part$population)
    population_mapped <- sum_finite_or_na(part$population[deterministic])
    shrid_coverage <- if (n_total > 0L) n_mapped / n_total else NA_real_
    population_coverage <- if (is.finite(population_total) && population_total > 0) {
      population_mapped / population_total
    } else {
      NA_real_
    }
    complete <- is.finite(shrid_coverage) && abs(shrid_coverage - 1) <= 1e-12
    one_target <- length(targets) == 1L
    high_population_coverage <- is.finite(population_coverage) &&
      population_coverage >= min_population_coverage
    mapping_class <- if (!length(targets)) {
      "no_deterministic_target"
    } else if (length(targets) > 1L) {
      "splits_across_target_districts"
    } else if (complete) {
      "deterministic_one_to_one"
    } else if (high_population_coverage) {
      "high_population_coverage_single_target"
    } else {
      "incomplete_population_coverage_single_target"
    }

    row <- data.frame(
      n_shrid_total = n_total,
      n_shrid_deterministic = n_mapped,
      shrid_coverage = shrid_coverage,
      population_total = population_total,
      population_deterministic = population_mapped,
      population_coverage = population_coverage,
      n_target_districts = length(targets),
      mapping_class = mapping_class,
      exact_one_to_one = identical(mapping_class, "deterministic_one_to_one"),
      preferred_single_target = one_target && high_population_coverage,
      preferred_population_coverage_threshold = min_population_coverage,
      stringsAsFactors = FALSE
    )
    row[[source_state]] <- part[[source_state]][[1L]]
    row[[source_district]] <- part[[source_district]][[1L]]
    row[c(
      source_state, source_district,
      "n_shrid_total", "n_shrid_deterministic", "shrid_coverage",
      "population_total", "population_deterministic", "population_coverage",
      "n_target_districts", "mapping_class", "exact_one_to_one",
      "preferred_single_target", "preferred_population_coverage_threshold"
    )]
  }))
  out[order(out[[source_state]], out[[source_district]]), , drop = FALSE]
}

#' Aggregate deterministic SHRID mappings to 2011-to-2001 district weights
build_district_transition_2001_2011 <- function(shrid_bridge) {
  build_district_transition_between_years(shrid_bridge, 2011L, 2001L)
}

#' Aggregate deterministic SHRID mappings to 1991-to-2001 district weights
build_district_transition_1991_2001 <- function(shrid_bridge) {
  build_district_transition_between_years(shrid_bridge, 1991L, 2001L)
}

#' Canonical Census 2001 district registry
build_admin_registry_2001 <- function(census_2001_languages) {
  x <- safe_df(census_2001_languages)
  required <- c("state_code", "district_code", "district_name")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Census 2001 registry is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- unique(data.frame(
    state_code = pad_admin_code(x$state_code, 2L),
    district_code = pad_admin_code(x$district_code, 2L),
    district_std = canonicalize_district_name(x$district_name),
    stringsAsFactors = FALSE
  ))
  out <- out[
    !is.na(out$state_code) & nzchar(out$state_code) &
      !is.na(out$district_code) & nzchar(out$district_code) &
      !is.na(out$district_std) & nzchar(out$district_std),
    , drop = FALSE
  ]
  if (!nrow(out)) return(empty_admin_registry_2001())

  out$state_std <- canonicalize_state_name(census_2001_state_name(out$state_code))
  missing_states <- sort(unique(out$state_code[is.na(out$state_std) | !nzchar(out$state_std)]))
  if (length(missing_states)) {
    stop(
      "Unknown Census 2001 state codes: ",
      paste(missing_states, collapse = ", "),
      call. = FALSE
    )
  }

  out$unit_id <- paste("pc2001", out$state_code, out$district_code, sep = "__")
  out$level <- "district"
  out$valid_from <- "2001-03-01"
  out$valid_to <- NA_character_
  out$source_id <- "census2001_c16"
  out[c("unit_id", "level", "state_code", "district_code", "state_std", "district_std", "valid_from", "valid_to", "source_id")]
}

empty_admin_registry_2001 <- function() {
  data.frame(
    unit_id = character(), level = character(), state_code = character(), district_code = character(),
    state_std = character(), district_std = character(), valid_from = character(), valid_to = character(),
    source_id = character(), stringsAsFactors = FALSE
  )
}

#' Canonical Census 2011 district registry from SHRUG geometry
build_admin_registry_2011 <- function(pc11_district_geometry) {
  x <- safe_df(sf::st_drop_geometry(pc11_district_geometry))
  state <- first_col(x, c("pc11_state_id", "state_code"))
  district <- first_col(x, c("pc11_district_id", "district_code"))
  name <- first_col(x, c("district_name", "district"))
  if (is.null(state) || is.null(district) || is.null(name)) {
    stop("PC11 district geometry lacks state, district, or district-name fields.", call. = FALSE)
  }
  out <- data.frame(
    state_code = pad_admin_code(x[[state]], 2L),
    district_code = pad_admin_code(x[[district]], 3L),
    district_std = canonicalize_district_name(x[[name]]),
    stringsAsFactors = FALSE
  )
  out <- unique(out[!is.na(out$district_code) & nzchar(out$district_std), , drop = FALSE])
  if (!nrow(out)) {
    return(data.frame(
      unit_id = character(), level = character(), state_code = character(), district_code = character(),
      district_std = character(), valid_from = character(), valid_to = character(), source_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  out$unit_id <- paste("pc2011", out$state_code, out$district_code, sep = "__")
  out$level <- "district"
  out$valid_from <- "2011-03-01"
  out$valid_to <- NA_character_
  out$source_id <- "shrug_pc11_district_geometry"
  out[c("unit_id", "level", "state_code", "district_code", "district_std", "valid_from", "valid_to", "source_id")]
}

canonical_allocation_source_key <- function(x) {
  if (!length(x)) return(character())
  if (!is.character(x)) {
    stop(
      "Allocation source identifiers must be read as character values.",
      call. = FALSE
    )
  }

  value <- trimws(x)
  out <- rep(NA_character_, length(value))

  canonical <- grepl(
    "^pc2011__[0-9]{1,2}__[0-9]{1,3}$",
    value
  )
  if (any(canonical)) {
    parts <- strsplit(value[canonical], "__", fixed = TRUE)
    out[canonical] <- vapply(parts, function(part) {
      paste(
        "pc2011",
        pad_admin_code(part[[2]], 2L),
        pad_admin_code(part[[3]], 3L),
        sep = "__"
      )
    }, character(1))
  }

  legacy <- !canonical & grepl(
    "^[0-9]{1,2}[.][0-9]{1,3}$",
    value
  )
  if (any(legacy)) {
    parts <- strsplit(value[legacy], ".", fixed = TRUE)
    out[legacy] <- vapply(parts, function(part) {
      paste(
        "pc2011",
        pad_admin_code(part[[1]], 2L),
        pad_admin_code(part[[2]], 3L),
        sep = "__"
      )
    }, character(1))
  }

  out
}

allocation_source_key <- function(state_code, district_code) {
  paste(
    "pc2011",
    pad_admin_code(state_code, 2L),
    pad_admin_code(district_code, 3L),
    sep = "__"
  )
}

validate_allocation_weights <- function(weights, source_cols = c("state_code_2011", "district_code_2011"), weight_col = "population_share_to_2001", tolerance = 1e-8) {
  weights <- safe_df(weights)
  if (!nrow(weights)) return(data.frame())
  missing <- setdiff(c(source_cols, weight_col), names(weights))
  if (length(missing)) stop("Allocation-weight table is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  key <- if (identical(source_cols, c("state_code_2011", "district_code_2011"))) {
    allocation_source_key(
      weights$state_code_2011,
      weights$district_code_2011
    )
  } else if (identical(source_cols, "source_unit")) {
    canonical_allocation_source_key(weights$source_unit)
  } else {
    do.call(interaction, c(weights[source_cols], list(drop = TRUE)))
  }
  if (anyNA(key) || any(!nzchar(key))) {
    stop(
      "Allocation source keys could not be canonicalized.",
      call. = FALSE
    )
  }
  groups <- split(seq_len(nrow(weights)), key)
  safe_bind_rows(lapply(groups, function(i) {
    value <- num(weights[[weight_col]][i])
    data.frame(
      source_key = as.character(key[[i[[1]]]]),
      n_targets = length(i),
      n_missing_weights = sum(!is.finite(value)),
      n_negative_weights = sum(is.finite(value) & value < 0),
      weight_sum = if (all(is.finite(value))) sum(value) else NA_real_,
      unmapped_share = if (all(is.finite(value))) max(0, 1 - sum(value)) else NA_real_,
      weights_well_formed =
        all(is.finite(value)) && all(value >= 0) && sum(value) <= 1 + tolerance,
      coverage_complete =
        all(is.finite(value)) && all(value >= 0) &&
          abs(sum(value) - 1) <= tolerance,
      stringsAsFactors = FALSE
    )
  }))
}

allocation_decision_status <- function(weights) {
  weights <- safe_df(weights)
  required <- c("source_unit", "status")
  if (!nrow(weights)) {
    return(data.frame(
      source_key = character(),
      decision_status = character(),
      decision_complete = logical(),
      stringsAsFactors = FALSE
    ))
  }
  missing <- setdiff(required, names(weights))
  if (length(missing)) {
    stop(
      "Allocation decisions are missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  source_key <- canonical_allocation_source_key(
    plain_chr(weights$source_unit)
  )
  if (anyNA(source_key)) {
    stop(
      "Allocation decisions contain noncanonical source identifiers.",
      call. = FALSE
    )
  }
  status <- plain_chr(weights$status)
  groups <- split(seq_len(nrow(weights)), source_key)
  safe_bind_rows(lapply(names(groups), function(key) {
    values <- unique(status[groups[[key]]])
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) != 1L) {
      stop(
        "Each allocation source must have exactly one decision status: ",
        key,
        call. = FALSE
      )
    }
    data.frame(
      source_key = key,
      decision_status = values,
      decision_complete = values %in% c("accepted", "rejected"),
      stringsAsFactors = FALSE
    )
  }))
}

allocation_coverage_status <- function(
  generated_validation, adjudicated_validation,
  adjudicated_decisions = data.frame()
) {
  generated <- safe_df(generated_validation)
  adjudicated <- safe_df(adjudicated_validation)
  decisions <- safe_df(adjudicated_decisions)

  require_coverage_columns <- function(x, label) {
    if (!nrow(x)) return(invisible())
    missing <- setdiff(c("source_key", "coverage_complete"), names(x))
    if (length(missing)) {
      stop(structure(
        list(
          message = paste0(
            label, " is missing required columns: ",
            paste(missing, collapse = ", ")
          ),
          call = NULL
        ),
        class = c(
          "lineage_allocation_validation_error",
          "error",
          "condition"
        )
      ))
    }
    invisible()
  }
  require_coverage_columns(generated, "Generated allocation validation")
  require_coverage_columns(adjudicated, "Reviewed allocation validation")

  generated_keys <- unique(canonical_allocation_source_key(
    generated$source_key
  ))
  incomplete_keys <- unique(canonical_allocation_source_key(
    generated$source_key[!(generated$coverage_complete %in% TRUE)]
  ))
  accepted_keys <- unique(canonical_allocation_source_key(
    adjudicated$source_key[adjudicated$coverage_complete %in% TRUE]
  ))
  rejected_keys <- if (
    nrow(decisions) &&
      all(c("source_key", "decision_status", "decision_complete") %in%
          names(decisions))
  ) {
    unique(canonical_allocation_source_key(
      decisions$source_key[
        decisions$decision_complete %in% TRUE &
          decisions$decision_status %in% "rejected"
      ]
    ))
  } else {
    character()
  }
  reviewed_keys <- unique(c(accepted_keys, rejected_keys))
  if (anyNA(c(
    generated_keys, incomplete_keys, accepted_keys, rejected_keys
  ))) {
    stop(
      "Allocation coverage contains noncanonical source keys.",
      call. = FALSE
    )
  }
  unresolved_keys <- setdiff(incomplete_keys, reviewed_keys)

  data.frame(
    n_generated_sources = length(generated_keys),
    n_generated_complete = sum(generated$coverage_complete %in% TRUE),
    n_reviewed_accepted = length(intersect(incomplete_keys, accepted_keys)),
    n_reviewed_rejected = length(intersect(incomplete_keys, rejected_keys)),
    n_reviewed_complete = length(intersect(incomplete_keys, reviewed_keys)),
    n_unresolved = length(unresolved_keys),
    coverage_resolved =
      length(generated_keys) > 0L && !length(unresolved_keys),
    stringsAsFactors = FALSE
  )
}

read_adjudicated_allocation_weights <- function(x, admin_2001 = data.frame()) {
  x <- safe_df(x)
  required <- c(
    "source_unit", "target_2001", "weight", "basis",
    "reference_year", "source_id", "status", "note"
  )
  for (nm in setdiff(required, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
  source_unit_raw <- trimws(plain_chr(x$source_unit))
  supplied <- !is.na(source_unit_raw) & nzchar(source_unit_raw)
  source_unit <- canonical_allocation_source_key(source_unit_raw)
  if (any(supplied & is.na(source_unit))) {
    stop(
      "District-allocation metadata contains invalid Census-2011 source IDs: ",
      paste(unique(source_unit_raw[supplied & is.na(source_unit)]), collapse = ", "),
      call. = FALSE
    )
  }
  x$source_unit <- source_unit
  x <- x[supplied, required, drop = FALSE]
  if (!nrow(x)) {
    x$weight <- numeric()
    return(x)
  }
  x$weight <- num(x$weight)
  allowed <- c("accepted", "rejected", "needs_review")
  invalid_status <- unique(x$status[!is.na(x$status) & nzchar(x$status) & !x$status %in% allowed])
  if (length(invalid_status)) {
    stop("Unknown district-allocation status: ", paste(invalid_status, collapse = ", "), call. = FALSE)
  }
  accepted <- x$status %in% "accepted"
  rejected <- x$status %in% "rejected"
  rejected_payload <- rejected & (
    (!is.na(x$target_2001) & nzchar(x$target_2001)) |
      is.finite(x$weight)
  )
  if (any(rejected_payload)) {
    stop(
      "Rejected district-allocation rows must not carry targets or weights.",
      call. = FALSE
    )
  }
  incomplete <- accepted & (
    is.na(x$target_2001) | !nzchar(x$target_2001) |
      !is.finite(x$weight) | x$weight < 0
  )
  if (any(incomplete)) {
    stop("Accepted district-allocation rows require a target and a nonnegative finite weight.", call. = FALSE)
  }
  target_units <- unique(plain_chr(safe_df(admin_2001)$unit_id %||% character()))
  unknown <- accepted & length(target_units) > 0L & !x$target_2001 %in% target_units
  if (any(unknown)) {
    stop(
      "Accepted district-allocation rows reference unknown 2001 units: ",
      paste(unique(x$target_2001[unknown]), collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(x[c("source_unit", "target_2001", "status")])) {
    stop("District-allocation metadata contains duplicate source-target-status rows.", call. = FALSE)
  }
  x
}

validate_adjudicated_allocation_weights <- function(weights, tolerance = 1e-8) {
  weights <- safe_df(weights)
  accepted <- weights[weights$status %in% "accepted", , drop = FALSE]
  if (!nrow(accepted)) {
    return(data.frame(
      source_key = character(), n_targets = integer(), n_missing_weights = integer(),
      n_negative_weights = integer(), weight_sum = numeric(), unmapped_share = numeric(),
      weights_well_formed = logical(), coverage_complete = logical(),
      stringsAsFactors = FALSE
    ))
  }
  validate_allocation_weights(
    accepted,
    source_cols = "source_unit",
    weight_col = "weight",
    tolerance = tolerance
  )
}
