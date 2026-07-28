# Predetermined district controls from Census 2001.

census_2001_main_controls <- function() {
  c(
    "log_population_2001", "urban_share_2001",
    "adult_secondary_plus_share_2001", "sc_share_2001", "st_share_2001",
    "muslim_share_2001", "agricultural_worker_share_2001",
    "dependency_ratio_2001", "electricity_access_share_2001",
    "log_population_density_2001"
  )
}

census_2001_appendix_controls <- function() {
  c(
    "literacy_share_2001", "worker_share_2001", "hindu_share_2001",
    "banking_access_share_2001", "television_ownership_share_2001",
    "telephone_ownership_share_2001", "primary_schools_per_1000_children_2001"
  )
}

safe_share <- function(numerator, denominator, scale = 100) {
  nume <- num(numerator)
  deno <- num(denominator)
  out <- rep(NA_real_, length(nume))
  keep <- is.finite(nume) & is.finite(deno) & deno > 0
  out[keep] <- scale * nume[keep] / deno[keep]
  out
}

build_census_2001_controls <- function(district_totals) {
  x <- safe_df(district_totals)
  required <- c(
    "state_code_2001", "district_code_2001", "population_total", "population_urban",
    "population_age_7_plus", "adult_secondary_plus", "sc_population",
    "st_population", "muslim_population", "workers_total", "cultivators",
    "agricultural_labourers", "population_age_0_14", "population_age_15_64",
    "population_age_65_plus", "households_total", "households_electricity",
    "area_sq_km"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Census 2001 control input is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- data.frame(
    state_code_2001 = pad_census_code(x$state_code_2001, 2L),
    district_code_2001 = pad_census_code(x$district_code_2001, 2L),
    stringsAsFactors = FALSE
  )
  out$log_population_2001 <- log(num(x$population_total))
  out$urban_share_2001 <- safe_share(x$population_urban, x$population_total)
  out$adult_secondary_plus_share_2001 <- safe_share(x$adult_secondary_plus, x$population_age_7_plus)
  out$sc_share_2001 <- safe_share(x$sc_population, x$population_total)
  out$st_share_2001 <- safe_share(x$st_population, x$population_total)
  out$muslim_share_2001 <- safe_share(x$muslim_population, x$population_total)
  out$agricultural_worker_share_2001 <- safe_share(
    num(x$cultivators) + num(x$agricultural_labourers), x$workers_total
  )
  out$dependency_ratio_2001 <- safe_share(
    num(x$population_age_0_14) + num(x$population_age_65_plus), x$population_age_15_64
  )
  out$electricity_access_share_2001 <- safe_share(x$households_electricity, x$households_total)
  density <- num(x$population_total) / num(x$area_sq_km)
  out$log_population_density_2001 <- ifelse(positive_finite(density), log(density), NA_real_)
  if (anyDuplicated(out[c("state_code_2001", "district_code_2001")])) {
    stop("Census 2001 controls contain duplicate state-district keys.", call. = FALSE)
  }
  out
}


aggregate_census_2001_counts <- function(data, count_columns, keys = c("state_code_2001", "district_code_2001")) {
  x <- safe_df(data)
  missing <- setdiff(c(keys, count_columns), names(x))
  if (length(missing)) stop("Census count source is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  for (key in keys) x[[key]] <- plain_chr(x[[key]])
  if (any(vapply(keys, function(key) any(!nzchar(trimws(x[[key]]))), logical(1)))) stop("Census count source contains blank keys.", call. = FALSE)
  split_i <- split(seq_len(nrow(x)), interaction(x[keys], drop = TRUE, lex.order = TRUE))
  out <- safe_bind_rows(lapply(split_i, function(i) {
    row <- x[i[[1]], keys, drop = FALSE]
    for (nm in count_columns) {
      value <- num(x[[nm]][i])
      row[[nm]] <- if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE)
    }
    row
  }))
  if (anyDuplicated(out[keys])) stop("Aggregated Census counts are not unique by key.", call. = FALSE)
  out
}

combine_census_2001_count_sources <- function(sources, keys = c("state_code_2001", "district_code_2001")) {
  sources <- Filter(function(x) !is.null(x) && nrow(safe_df(x)) > 0L, sources)
  if (!length(sources)) stop("No Census 2001 count sources were supplied.", call. = FALSE)
  sources <- lapply(sources, safe_df)
  for (x in sources) {
    if (!all(keys %in% names(x))) stop("Census source is missing standardized keys.", call. = FALSE)
    if (anyDuplicated(x[keys])) stop("Census source is not unique by standardized keys.", call. = FALSE)
  }
  nonkeys <- lapply(sources, function(x) setdiff(names(x), keys))
  duplicated_columns <- unique(unlist(nonkeys, use.names = FALSE)[duplicated(unlist(nonkeys, use.names = FALSE))])
  if (length(duplicated_columns)) stop("Census sources contain overlapping columns: ", paste(duplicated_columns, collapse = ", "), call. = FALSE)
  Reduce(function(a, b) merge(a, b, by = keys, all = TRUE, sort = FALSE), sources)
}

validate_census_2001_controls <- function(controls, expected_keys = NULL) {
  x <- safe_df(controls)
  keys <- c("state_code_2001", "district_code_2001")
  missing <- setdiff(c(keys, census_2001_main_controls()), names(x))
  if (length(missing)) stop("Census controls are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(x[keys])) stop("Census controls are not unique by state-district.", call. = FALSE)
  if (!is.null(expected_keys)) {
    expected <- unique(do.call(paste, c(safe_df(expected_keys)[keys], sep = "__")))
    observed <- unique(do.call(paste, c(x[keys], sep = "__")))
    if (length(setdiff(expected, observed)) || length(setdiff(observed, expected))) stop("Census control coverage differs from the expected district registry.", call. = FALSE)
  }
  invisible(TRUE)
}

attach_census_2001_controls <- function(panel, controls) {
  p <- safe_df(panel)
  keys <- c("state_code_2001", "district_code_2001")
  if (!all(keys %in% names(p))) stop("District panel lacks standardized Census keys.", call. = FALSE)
  validate_census_2001_controls(controls)
  before <- do.call(paste, c(p[keys], sep = "__"))
  out <- merge(p, controls, by = keys, all.x = TRUE, sort = FALSE)
  out <- out[match(before, do.call(paste, c(out[keys], sep = "__"))), , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) != nrow(p) || !identical(do.call(paste, c(out[keys], sep = "__")), before)) stop("Attaching Census controls changed panel rows or ordering.", call. = FALSE)
  out
}

summarise_census_2001_control_coverage <- function(panel) {
  x <- safe_df(panel)
  safe_bind_rows(lapply(census_2001_main_controls(), function(variable) {
    present <- variable %in% names(x)
    missing <- if (present) sum(!is.finite(num(x[[variable]]))) else nrow(x)
    data.frame(variable = variable, present = present, n = nrow(x), missing = missing, missing_pct = 100 * missing / max(1, nrow(x)), stringsAsFactors = FALSE)
  }))
}
