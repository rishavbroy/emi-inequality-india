# District migration constructs from Census D-series tables.

census_d02_count_columns <- function() {
  c(
    "migrants_total", "migrants_recent_0_9", "migrants_within_state_outside_place",
    "migrants_within_district", "migrants_other_district_same_state",
    "migrants_interstate", "migrants_outside_india"
  )
}

census_d03_reason_count_columns <- function() {
  c(
    "migrants_total", "work_employment", "business", "education", "marriage",
    "moved_after_birth", "moved_with_household", "other_reason"
  )
}

census_2011_harmonized_count_schema <- function(count_cols) {
  count_cols <- unique(plain_chr(count_cols))
  count_cols <- count_cols[!is.na(count_cols) & nzchar(count_cols)]
  if (!length(count_cols)) {
    stop("Harmonized Census-2011 counts require at least one count column.", call. = FALSE)
  }
  out <- data.frame(
    target_unit_2001 = character(),
    census_2011_source_district_count = integer(),
    census_2011_source_districts = character(),
    census_2011_parent_reconstruction_complete = logical(),
    stringsAsFactors = FALSE
  )
  for (column in count_cols) out[[column]] <- numeric()
  out
}

harmonize_census_2011_counts_to_2001 <- function(
    x, district_transition_2001_2011, count_cols) {
  x <- safe_df(x)
  required <- c("state_code", "district_code", "district_name", count_cols)
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Census-2011 count frame lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census-2011 count frame is not unique by source district.", call. = FALSE)
  }
  x$source_unit_2011 <- paste0(
    "pc2011__", normalize_census_code(x$state_code, 2L), "__",
    normalize_census_code(x$district_code, 3L)
  )
  bridge <- build_complete_deterministic_transition_2011_to_2001(
    district_transition_2001_2011
  )
  x <- merge(x, bridge, by = "source_unit_2011", all.x = TRUE, sort = FALSE)
  mapped <- x[!is.na(x$target_unit_2001) & nzchar(x$target_unit_2001), , drop = FALSE]
  if (!nrow(mapped)) return(census_2011_harmonized_count_schema(count_cols))

  groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- mapped[index, , drop = FALSE]
    values <- vapply(count_cols, function(column) {
      value <- num(part[[column]])
      if (!length(value) || any(!is.finite(value)) || any(value < 0)) return(NA_real_)
      sum(value)
    }, numeric(1))
    if (any(!is.finite(values))) {
      stop(
        "Census-2011 deterministic migration pool contains invalid counts for ",
        part$target_unit_2001[[1L]], ".",
        call. = FALSE
      )
    }
    row <- data.frame(
      target_unit_2001 = part$target_unit_2001[[1L]],
      census_2011_source_district_count = length(unique(part$source_unit_2011)),
      census_2011_source_districts = paste(sort(unique(part$district_name)), collapse = ";"),
      census_2011_parent_reconstruction_complete = all(
        part$census_2011_parent_reconstruction_complete %in% TRUE
      ),
      stringsAsFactors = FALSE
    )
    for (column in count_cols) row[[column]] <- values[[column]]
    row
  }))
  if (anyDuplicated(out$target_unit_2001)) {
    stop("Harmonized Census-2011 counts are not unique by Census-2001 target.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

safe_count_share <- function(numerator, denominator) {
  numerator <- num(numerator)
  denominator <- num(denominator)
  ifelse(
    is.finite(numerator) & numerator >= 0 & is.finite(denominator) & denominator > 0 &
      numerator <= denominator,
    numerator / denominator,
    NA_real_
  )
}

add_census_d02_migration_shares <- function(x) {
  x <- safe_df(x)
  x$recent_0_9_share_among_migrants <- safe_count_share(x$migrants_recent_0_9, x$migrants_total)
  x$within_district_share_among_migrants <- safe_count_share(x$migrants_within_district, x$migrants_total)
  x$other_district_same_state_share_among_migrants <- safe_count_share(
    x$migrants_other_district_same_state, x$migrants_total
  )
  x$interstate_share_among_migrants <- safe_count_share(x$migrants_interstate, x$migrants_total)
  x$outside_india_share_among_migrants <- safe_count_share(x$migrants_outside_india, x$migrants_total)
  x
}

add_census_d03_reason_shares <- function(x) {
  x <- safe_df(x)
  for (column in setdiff(census_d03_reason_count_columns(), "migrants_total")) {
    x[[paste0(column, "_share_among_migrants")]] <- safe_count_share(
      x[[column]], x$migrants_total
    )
  }
  x
}

validate_census_2011_migration_totals <- function(d02_2011, d03_2011) {
  d02 <- safe_df(d02_2011)
  d03 <- safe_df(d03_2011)
  keys <- c("state_code", "district_code")
  for (x in list(d02 = d02, d03 = d03)) {
    missing <- setdiff(c(keys, "migrants_total"), names(x))
    if (length(missing)) {
      stop("Census 2011 migration source lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    if (anyDuplicated(x[keys])) {
      stop("Census 2011 migration source is not unique by district.", call. = FALSE)
    }
  }
  joined <- merge(
    d02[c(keys, "migrants_total")],
    d03[c(keys, "migrants_total")],
    by = keys, all = TRUE, sort = TRUE,
    suffixes = c("_d02", "_d03")
  )
  complete <- is.finite(num(joined$migrants_total_d02)) & is.finite(num(joined$migrants_total_d03))
  same_total <- complete & num(joined$migrants_total_d02) == num(joined$migrants_total_d03)
  if (!nrow(joined) || any(!complete) || any(!same_total)) {
    bad <- joined[!complete | !same_total, , drop = FALSE]
    detail <- if (nrow(bad)) {
      paste0(bad$state_code[[1L]], "/", bad$district_code[[1L]])
    } else {
      "no shared districts"
    }
    stop(
      "Census 2011 D02/D03 all-duration migrant totals disagree or district coverage differs; first mismatch: ",
      detail, ".",
      call. = FALSE
    )
  }
  data.frame(
    n_districts = nrow(joined),
    max_abs_total_difference = max(abs(
      num(joined$migrants_total_d02) - num(joined$migrants_total_d03)
    )),
    stringsAsFactors = FALSE
  )
}

build_census_d02_2001_measures <- function(d02_2001, census_2001_district_totals) {
  x <- safe_df(d02_2001)
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census-2001 D02 measures require unique district rows.", call. = FALSE)
  }
  population <- safe_df(census_2001_district_totals)
  required_population <- c("state_code_2001", "district_code_2001", "population_total")
  missing <- setdiff(required_population, names(population))
  if (length(missing)) {
    stop("Census-2001 population denominator lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  population <- population[required_population]
  names(population)[1:2] <- c("state_code", "district_code")
  population$state_code <- normalize_census_code(population$state_code, 2L)
  population$district_code <- normalize_census_code(population$district_code, 2L)
  if (anyDuplicated(population[c("state_code", "district_code")])) {
    stop("Census-2001 population denominator is not unique by district.", call. = FALSE)
  }
  district_key <- function(frame) paste(frame$state_code, frame$district_code, sep = "/")
  if (!setequal(district_key(x), district_key(population))) {
    stop("Census-2001 D02 and population denominator district coverage differ.", call. = FALSE)
  }
  x <- merge(x, population, by = c("state_code", "district_code"), all.x = TRUE, sort = FALSE)
  if (any(!is.finite(num(x$population_total)) | num(x$population_total) <= 0)) {
    stop("Census-2001 D02 migration rows lack positive district population denominators.", call. = FALSE)
  }
  x$target_unit_2001 <- paste0(
    "pc2001__", normalize_census_code(x$state_code, 2L), "__",
    normalize_census_code(x$district_code, 2L)
  )
  x$migrant_stock_share_population <- safe_count_share(x$migrants_total, x$population_total)
  x$recent_0_9_migrant_share_population <- safe_count_share(x$migrants_recent_0_9, x$population_total)
  x$interstate_migrant_share_population <- safe_count_share(x$migrants_interstate, x$population_total)
  population_share_cols <- c(
    "migrant_stock_share_population", "recent_0_9_migrant_share_population",
    "interstate_migrant_share_population"
  )
  if (any(!is.finite(as.matrix(x[population_share_cols])))) {
    stop("Census-2001 D02 counts are incompatible with district population denominators.", call. = FALSE)
  }
  add_census_d02_migration_shares(x)
}

build_census_d02_2011_measures <- function(d02_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d02_2011, district_transition_2001_2011, census_d02_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d02_migration_shares(pooled)
}

build_census_d03_2011_measures <- function(d03_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d03_2011, district_transition_2001_2011, census_d03_reason_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d03_reason_shares(pooled)
}

summarise_census_migration_coverage <- function(d02_2001, d02_2011, d03_2011) {
  frames <- list(
    d02_2001 = safe_df(d02_2001),
    d02_2011_harmonized = safe_df(d02_2011),
    d03_2011_harmonized = safe_df(d03_2011)
  )
  safe_bind_rows(lapply(names(frames), function(name) {
    x <- frames[[name]]
    data.frame(
      dataset = name,
      n_districts = nrow(x),
      n_positive_migrant_denominators = if ("migrants_total" %in% names(x)) {
        sum(is.finite(num(x$migrants_total)) & num(x$migrants_total) > 0)
      } else {
        NA_integer_
      },
      stringsAsFactors = FALSE
    )
  }))
}
