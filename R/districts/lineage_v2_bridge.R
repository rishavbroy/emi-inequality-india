# Census 2001/2011 registries and stable-locality transition matrices.

pad_admin_code <- function(x, width) {
  x <- trimws(plain_chr(x))
  x <- sub("\\.0+$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[!nzchar(x)] <- NA_character_
  out <- ifelse(is.na(x), NA_character_, sprintf(paste0("%0", width, "d"), suppressWarnings(as.integer(x))))
  out[grepl("^0+$", out)] <- NA_character_
  out
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

  district_width <- if (as.integer(year) == 2001L) 2L else 3L
  subdistrict_width <- if (as.integer(year) == 2001L) 4L else 5L
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
  district_width <- if (as.integer(year) == 2001L) 2L else 3L
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

shrid_bridge_status <- function(bridge) {
  bridge <- safe_df(bridge)
  n <- nrow(bridge)
  status <- rep("missing_census_membership", n)
  crosses <-
    (num(bridge$n_state_memberships_2001) > 1L) %in% TRUE |
    (num(bridge$n_district_memberships_2001) > 1L) %in% TRUE |
    (num(bridge$n_state_memberships_2011) > 1L) %in% TRUE |
    (num(bridge$n_district_memberships_2011) > 1L) %in% TRUE
  missing_locality <-
    !(bridge$has_locality_key_2001 %in% TRUE) |
    !(bridge$has_locality_key_2011 %in% TRUE)
  deterministic <- bridge$deterministic %in% TRUE

  status[crosses] <- "crosses_district_boundary"
  status[!crosses & missing_locality] <- "missing_census_locality_key"
  status[deterministic] <- "deterministic_one_district_each_year"
  status
}

#' Build a deterministic SHRUG district bridge
#'
#' SHRID units that cross district boundaries in either Census year are retained
#' in the QA table but excluded from deterministic transition weights. This
#' avoids inventing fragment shares not supplied by the district keys.
build_shrug_district_bridge <- function(pc01r, pc01u, pc11r, pc11u, pc01dist, pc11dist) {
  loc01 <- safe_bind_rows(list(
    standardize_shrug_locality_key(pc01r, 2001L, "rural"),
    standardize_shrug_locality_key(pc01u, 2001L, "urban")
  ))
  loc11 <- safe_bind_rows(list(
    standardize_shrug_locality_key(pc11r, 2011L, "rural"),
    standardize_shrug_locality_key(pc11u, 2011L, "urban")
  ))
  d01 <- unique_shrid_district_membership(standardize_shrug_district_key(pc01dist, 2001L), "2001")
  d11 <- unique_shrid_district_membership(standardize_shrug_district_key(pc11dist, 2011L), "2011")
  weights <- aggregate_shrid_weights(loc11)

  bridge <- merge(d01, d11, by = "shrid2", all = TRUE, sort = FALSE)
  bridge <- merge(bridge, weights, by = "shrid2", all.x = TRUE, sort = FALSE)
  bridge$has_locality_key_2001 <- bridge$shrid2 %in% unique(loc01$shrid2)
  bridge$has_locality_key_2011 <- bridge$shrid2 %in% unique(loc11$shrid2)
  bridge$deterministic <- bridge$deterministic_2001 %in% TRUE &
    bridge$deterministic_2011 %in% TRUE &
    bridge$has_locality_key_2001 & bridge$has_locality_key_2011
  bridge$bridge_status <- shrid_bridge_status(bridge)
  attr(bridge, "locality_keys_2001") <- loc01
  attr(bridge, "locality_keys_2011") <- loc11
  bridge
}

weighted_share <- function(x, group_total) {
  x <- num(x)
  group_total <- num(group_total)
  ifelse(is.finite(x) & is.finite(group_total) & group_total > 0, x / group_total, NA_real_)
}

#' Aggregate deterministic SHRID mappings to district transition weights
#'
#' Shares use the full 2011 source-district denominator. Consequently, excluded
#' cross-boundary or missing-membership SHRID units make the weights sum to less
#' than one instead of being silently renormalized away.
build_district_transition_2001_2011 <- function(shrid_bridge) {
  all_rows <- safe_df(shrid_bridge)
  source_rows <- all_rows[
    !is.na(all_rows$state_code_2011) & !is.na(all_rows$district_code_2011),
    , drop = FALSE
  ]
  mapped <- source_rows[
    source_rows$deterministic %in% TRUE &
      !is.na(source_rows$state_code_2001) & !is.na(source_rows$district_code_2001),
    , drop = FALSE
  ]
  if (!nrow(mapped)) return(data.frame())

  source_key_all <- interaction(source_rows$state_code_2011, source_rows$district_code_2011, drop = TRUE)
  source_totals <- safe_bind_rows(lapply(split(seq_len(nrow(source_rows)), source_key_all), function(i) {
    data.frame(
      state_code_2011 = source_rows$state_code_2011[[i[[1]]]],
      district_code_2011 = source_rows$district_code_2011[[i[[1]]]],
      n_shrid_total = length(unique(source_rows$shrid2[i])),
      population_2011_total = sum_finite_or_na(source_rows$population[i]),
      area_2011_total = sum_finite_or_na(source_rows$area[i]),
      stringsAsFactors = FALSE
    )
  }))

  transition_key <- interaction(
    mapped$state_code_2011, mapped$district_code_2011,
    mapped$state_code_2001, mapped$district_code_2001,
    drop = TRUE
  )
  rows <- safe_bind_rows(lapply(split(seq_len(nrow(mapped)), transition_key), function(i) {
    data.frame(
      state_code_2011 = mapped$state_code_2011[[i[[1]]]],
      district_code_2011 = mapped$district_code_2011[[i[[1]]]],
      state_code_2001 = mapped$state_code_2001[[i[[1]]]],
      district_code_2001 = mapped$district_code_2001[[i[[1]]]],
      n_shrid_mapped = length(unique(mapped$shrid2[i])),
      population_2011_mapped = sum_finite_or_na(mapped$population[i]),
      area_2011_mapped = sum_finite_or_na(mapped$area[i]),
      stringsAsFactors = FALSE
    )
  }))
  rows <- merge(
    rows, source_totals,
    by = c("state_code_2011", "district_code_2011"),
    all.x = TRUE, sort = FALSE
  )
  rows$population_share_to_2001 <- weighted_share(rows$population_2011_mapped, rows$population_2011_total)
  rows$area_share_to_2001 <- weighted_share(rows$area_2011_mapped, rows$area_2011_total)

  source_key <- interaction(rows$state_code_2011, rows$district_code_2011, drop = TRUE)
  rows$n_target_2001_districts <- as.integer(ave(rep(1L, nrow(rows)), source_key, FUN = length))
  mapped_shrid <- ave(rows$n_shrid_mapped, source_key, FUN = sum)
  rows$shrid_coverage <- weighted_share(mapped_shrid, rows$n_shrid_total)
  complete <- is.finite(rows$shrid_coverage) & abs(rows$shrid_coverage - 1) <= 1e-12
  rows$mapping_class <- ifelse(
    rows$n_target_2001_districts == 1L & complete,
    "deterministic_containment",
    "non_nested_or_incomplete"
  )
  rows[order(rows$state_code_2011, rows$district_code_2011, -rows$population_share_to_2001), , drop = FALSE]
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

validate_allocation_weights <- function(weights, source_cols = c("state_code_2011", "district_code_2011"), weight_col = "population_share_to_2001", tolerance = 1e-8) {
  weights <- safe_df(weights)
  if (!nrow(weights)) return(data.frame())
  missing <- setdiff(c(source_cols, weight_col), names(weights))
  if (length(missing)) stop("Allocation-weight table is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  key <- do.call(interaction, c(weights[source_cols], list(drop = TRUE)))
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

allocation_coverage_status_v2 <- function(
  generated_validation, adjudicated_validation
) {
  generated <- safe_df(generated_validation)
  adjudicated <- safe_df(adjudicated_validation)

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

  generated_keys <- unique(plain_chr(generated$source_key))
  incomplete_keys <- unique(plain_chr(generated$source_key[
    !(generated$coverage_complete %in% TRUE)
  ]))
  reviewed_keys <- unique(plain_chr(adjudicated$source_key[
    adjudicated$coverage_complete %in% TRUE
  ]))
  unresolved_keys <- setdiff(incomplete_keys, reviewed_keys)

  data.frame(
    n_generated_sources = length(generated_keys),
    n_generated_complete = sum(generated$coverage_complete %in% TRUE),
    n_reviewed_complete = length(intersect(incomplete_keys, reviewed_keys)),
    n_unresolved = length(unresolved_keys),
    coverage_resolved =
      length(generated_keys) > 0L && !length(unresolved_keys),
    stringsAsFactors = FALSE
  )
}

read_adjudicated_allocation_weights_v2 <- function(x, admin_2001 = data.frame()) {
  x <- safe_df(x)
  required <- c(
    "source_unit", "target_2001", "weight", "basis",
    "reference_year", "source_id", "status", "note"
  )
  for (nm in setdiff(required, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
  x <- x[!is.na(x$source_unit) & nzchar(x$source_unit), required, drop = FALSE]
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

validate_adjudicated_allocation_weights_v2 <- function(weights, tolerance = 1e-8) {
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
