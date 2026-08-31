# Predetermined district controls from Census 2001.

census_2001_keys <- function() c("state_code_2001", "district_code_2001")

census_2001_control_registry_path <- function(paths = build_paths()) {
  path_metadata(paths, "census_2001_control_registry.csv")
}

parse_registry_flag <- function(x, field, label) {
  value <- tolower(trimws(plain_chr(x)))
  if (any(!value %in% c("true", "false"))) {
    stop(label, " `", field, "` values must be TRUE or FALSE.", call. = FALSE)
  }
  value == "true"
}

read_census_2001_control_registry <- function(path = census_2001_control_registry_path()) {
  if (!file.exists(path)) {
    stop("Census 2001 control registry is missing: ", path, call. = FALSE)
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "variable", "label", "description", "control_block", "block_sequence",
    "parameterization", "alternative_to", "source", "denominator",
    "main_paper", "absorption_control", "appendix_control", "sequence"
  )
  if (!identical(names(out), required)) {
    stop("Census 2001 control registry has an invalid schema.", call. = FALSE)
  }

  text_fields <- c(
    "variable", "label", "description", "control_block", "parameterization",
    "alternative_to", "source", "denominator"
  )
  for (field in text_fields) out[[field]] <- trimws(plain_chr(out[[field]]))
  required_text <- setdiff(text_fields, "alternative_to")
  if (!nrow(out) || anyDuplicated(out$variable) ||
      any(vapply(out[required_text], function(x) any(is.na(x) | !nzchar(x)), logical(1)))) {
    stop("Census 2001 control registry contains empty or duplicate identifiers.", call. = FALSE)
  }

  out$block_sequence <- suppressWarnings(as.integer(out$block_sequence))
  out$sequence <- suppressWarnings(as.integer(out$sequence))
  if (any(!is.finite(out$block_sequence)) || any(!is.finite(out$sequence)) ||
      anyDuplicated(out$sequence)) {
    stop("Census 2001 control registry has invalid ordering metadata.", call. = FALSE)
  }
  for (field in c("main_paper", "absorption_control", "appendix_control")) {
    out[[field]] <- parse_registry_flag(out[[field]], field, "Census 2001 control registry")
  }

  allowed_parameterizations <- c(
    "preferred", "preferred_compact", "alternative_measure",
    "alternative_parameterization", "appendix_context"
  )
  invalid <- setdiff(unique(out$parameterization), allowed_parameterizations)
  if (length(invalid)) {
    stop(
      "Census 2001 control registry contains unknown parameterization values: ",
      paste(invalid, collapse = ", "), call. = FALSE
    )
  }
  referenced <- out$alternative_to[nzchar(out$alternative_to)]
  missing_reference <- setdiff(referenced, out$variable)
  if (length(missing_reference)) {
    stop(
      "Census 2001 control registry alternative_to references are unknown: ",
      paste(missing_reference, collapse = ", "), call. = FALSE
    )
  }
  out <- out[order(out$sequence), , drop = FALSE]
  rownames(out) <- NULL
  out
}

resolve_census_2001_control_registry <- function(registry = NULL) {
  if (is.null(registry)) read_census_2001_control_registry() else safe_df(registry)
}

census_2001_control_set <- function(field, registry = NULL) {
  registry <- resolve_census_2001_control_registry(registry)
  if (!field %in% names(registry) || !is.logical(registry[[field]])) {
    stop("Unknown Census 2001 control-set field: ", field, call. = FALSE)
  }
  registry$variable[registry[[field]] %in% TRUE]
}

census_2001_main_controls <- function(registry = NULL) {
  census_2001_control_set("main_paper", registry)
}

census_2001_absorption_controls <- function(registry = NULL) {
  census_2001_control_set("absorption_control", registry)
}

census_2001_diagnostic_controls <- function(registry = NULL) {
  unique(c(
    census_2001_main_controls(registry),
    census_2001_absorption_controls(registry)
  ))
}

census_2001_appendix_controls <- function(registry = NULL) {
  census_2001_control_set("appendix_control", registry)
}

census_2001_control_metadata <- function(registry = NULL) {
  registry <- resolve_census_2001_control_registry(registry)
  registry[
    registry$main_paper %in% TRUE,
    c("variable", "label", "description"),
    drop = FALSE
  ]
}

census_2001_control_identity_groups <- function() {
  list(
    agricultural_worker_composition = c(
      "agricultural_worker_share_2001",
      "cultivator_share_workers_2001",
      "agricultural_labourer_share_workers_2001"
    )
  )
}

census_2001_balance_linked_controls <- function(variable) {
  groups <- census_2001_control_identity_groups()
  linked <- unlist(
    groups[vapply(groups, function(group) variable %in% group, logical(1))],
    use.names = FALSE
  )
  setdiff(unique(linked), variable)
}

census_2001_joint_balance_controls <- function(
  variables = census_2001_diagnostic_controls()
) {
  out <- unique(variables)
  for (group in census_2001_control_identity_groups()) {
    if (all(group %in% out)) out <- setdiff(out, group[[1]])
  }
  out
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
    census_2001_keys(), "population_total", "population_urban",
    "population_age_7_plus", "adult_secondary_plus", "literate_population", "sc_population",
    "st_population", "religion_population_total", "muslim_population", "workers_total", "cultivators",
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
  out$muslim_share_2001 <- safe_share(x$muslim_population, x$religion_population_total)
  out$agricultural_worker_share_2001 <- safe_share(
    num(x$cultivators) + num(x$agricultural_labourers), x$workers_total
  )
  out$literacy_share_2001 <- safe_share(x$literate_population, x$population_age_7_plus)
  out$worker_share_2001 <- safe_share(x$workers_total, x$population_total)
  out$cultivator_share_workers_2001 <- safe_share(x$cultivators, x$workers_total)
  out$agricultural_labourer_share_workers_2001 <- safe_share(
    x$agricultural_labourers, x$workers_total
  )
  out$dependency_ratio_2001 <- safe_share(
    num(x$population_age_0_14) + num(x$population_age_65_plus), x$population_age_15_64
  )
  out$electricity_access_share_2001 <- safe_share(x$households_electricity, x$households_total)
  density <- num(x$population_total) / num(x$area_sq_km)
  out$log_population_density_2001 <- ifelse(positive_finite(density), log(density), NA_real_)

  bounded_shares <- c(
    "urban_share_2001", "adult_secondary_plus_share_2001",
    "sc_share_2001", "st_share_2001", "muslim_share_2001",
    "agricultural_worker_share_2001", "literacy_share_2001",
    "worker_share_2001", "cultivator_share_workers_2001",
    "agricultural_labourer_share_workers_2001", "electricity_access_share_2001"
  )
  invalid_share <- vapply(bounded_shares, function(variable) {
    value <- num(out[[variable]])
    any(is.finite(value) & (value < 0 | value > 100))
  }, logical(1))
  if (any(invalid_share)) {
    stop(
      "Census 2001 controls contain bounded shares outside [0, 100]: ",
      paste(bounded_shares[invalid_share], collapse = ", "),
      call. = FALSE
    )
  }

  keys <- census_2001_keys()
  if (any(!stats::complete.cases(out[keys]))) {
    stop("Census 2001 controls contain missing state-district keys.", call. = FALSE)
  }
  if (anyDuplicated(out[keys])) {
    stop("Census 2001 controls contain duplicate state-district keys.", call. = FALSE)
  }
  out
}


aggregate_census_2001_counts <- function(data, count_columns, keys = census_2001_keys()) {
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

combine_census_2001_count_sources <- function(sources, keys = census_2001_keys(), require_same_keys = FALSE) {
  sources <- Filter(function(x) !is.null(x) && nrow(safe_df(x)) > 0L, sources)
  if (!length(sources)) stop("No Census 2001 count sources were supplied.", call. = FALSE)
  sources <- lapply(sources, safe_df)
  for (x in sources) {
    if (!all(keys %in% names(x))) stop("Census source is missing standardized keys.", call. = FALSE)
    if (any(!stats::complete.cases(x[keys]))) stop("Census source contains missing standardized keys.", call. = FALSE)
    if (anyDuplicated(x[keys])) stop("Census source is not unique by standardized keys.", call. = FALSE)
  }
  if (isTRUE(require_same_keys)) {
    key_sets <- lapply(sources, function(x) sort(do.call(paste, c(x[keys], sep = "__"))))
    if (!all(vapply(key_sets[-1L], identical, logical(1), key_sets[[1L]]))) {
      stop("Census count sources do not cover the same state-district universe.", call. = FALSE)
    }
  }
  nonkeys <- lapply(sources, function(x) setdiff(names(x), keys))
  duplicated_columns <- unique(unlist(nonkeys, use.names = FALSE)[duplicated(unlist(nonkeys, use.names = FALSE))])
  if (length(duplicated_columns)) stop("Census sources contain overlapping columns: ", paste(duplicated_columns, collapse = ", "), call. = FALSE)
  Reduce(function(a, b) merge(a, b, by = keys, all = TRUE, sort = FALSE), sources)
}

validate_census_2001_controls <- function(controls, expected_keys = NULL) {
  x <- safe_df(controls)
  keys <- census_2001_keys()
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
  is_spatial <- inherits(panel, "sf")
  if (is_spatial) {
    need_pkg("sf", "Census control attachment")
    geometry <- sf::st_geometry(panel)
    geometry_column <- attr(panel, "sf_column") %||% "geometry"
    panel_crs <- sf::st_crs(panel)
    p <- as.data.frame(sf::st_drop_geometry(panel), stringsAsFactors = FALSE)
  } else {
    p <- safe_df(panel)
  }

  keys <- census_2001_keys()
  if (!all(keys %in% names(p))) stop("District panel lacks standardized Census keys.", call. = FALSE)
  validate_census_2001_controls(controls)
  before <- do.call(paste, c(p[keys], sep = "__"))
  out <- merge(p, controls, by = keys, all.x = TRUE, sort = FALSE)
  out <- out[match(before, do.call(paste, c(out[keys], sep = "__"))), , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) != nrow(p) || !identical(do.call(paste, c(out[keys], sep = "__")), before)) {
    stop("Attaching Census controls changed panel rows or ordering.", call. = FALSE)
  }

  if (is_spatial) {
    out[[geometry_column]] <- geometry
    out <- sf::st_as_sf(out, sf_column_name = geometry_column, crs = panel_crs)
  }
  out
}

summarise_census_2001_control_coverage <- function(panel) {
  x <- safe_df(panel)
  safe_bind_rows(lapply(census_2001_diagnostic_controls(), function(variable) {
    present <- variable %in% names(x)
    missing <- if (present) sum(!is.finite(num(x[[variable]]))) else nrow(x)
    data.frame(variable = variable, present = present, n = nrow(x), missing = missing, missing_pct = 100 * missing / max(1, nrow(x)), stringsAsFactors = FALSE)
  }))
}
