# Pre-treatment baselines from the Vanneman-Barnes stable district panel.
#
# The archived panel4.sas is the parsing contract: columns 1-2 state ID,
# 3-4 stable district ID, 5-7 record ID, 8-9 year, 10 version, and
# total/rural/male/rural-male counts in columns 11-46. This module reads only
# a narrow longitudinal family and then applies the already-reviewed geography.

vanneman_pretrend_record_registry <- function() {
  data.frame(
    record_id = c("100", "111", "112", "140", "151", "153"),
    record_name = c(
      "total_population", "main_workers", "farm_workers_main", "literates",
      "primary_school_or_higher", "matriculates_or_higher"
    ),
    stringsAsFactors = FALSE
  )
}

vanneman_pretrend_measure_registry <- function() {
  data.frame(
    measure_id = c(
      "log_population", "urban_share", "main_worker_share",
      "nonfarm_worker_share_main_workers", "literate_share_population",
      "primary_plus_share_population", "matriculate_plus_share_population"
    ),
    domain = c("demography", "demography", "labor", "labor", "education", "education", "education"),
    label = c(
      "Log population", "Urban share", "Main-worker share of population",
      "Non-farm share of main workers", "Literate share of population",
      "Primary-school-or-higher share of population",
      "Matriculate-or-higher share of population"
    ),
    estimated_1961_source = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

vanneman_pretrend_period_registry <- function() {
  data.frame(
    period_id = c("1961_1971", "1971_1981", "1981_1991", "1961_1981", "1961_1991"),
    start_year = c(1961L, 1971L, 1981L, 1961L, 1961L),
    end_year = c(1971L, 1981L, 1991L, 1981L, 1991L),
    stringsAsFactors = FALSE
  )
}

vanneman_count_value <- function(x) {
  out <- suppressWarnings(as.numeric(trimws(x)))
  out[is.finite(out) & out < 0] <- NA_real_
  out
}

validate_vanneman_pretrend_sas_contract <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman panel4 SAS reader: ", path, call. = FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  squashed <- gsub("[[:space:]]+", " ", text)

  checks <- c(
    population = "Record ID # 100 - Total population",
    main_workers = "Record ID # 111 - Total main workers",
    farm_workers = "Record ID # 112 - Farm workers (main)",
    literates = "Record ID # 140 - Literates (ages 5+)",
    primary_plus = "Record ID # 151 - Primary School or higher",
    matriculates = "Record ID # 153 - Matriculates or higher"
  )
  passed <- vapply(checks, function(pattern) {
    grepl(pattern, squashed, fixed = TRUE)
  }, logical(1))
  labor_estimates <- grepl("WORK6 11-19.*estimated", squashed, ignore.case = TRUE, perl = TRUE) &&
    grepl("FARM6 11-19.*estimated", squashed, ignore.case = TRUE, perl = TRUE)

  if (!all(passed) || !labor_estimates) {
    stop("Vanneman panel4 SAS reader does not satisfy the registered pretrend semantic contract.", call. = FALSE)
  }
  invisible(TRUE)
}

read_vanneman_panel4_pretrend_counts <- function(
    path, record_registry = vanneman_pretrend_record_registry()) {
  if (!file.exists(path)) stop("Missing Vanneman panel4 data: ", path, call. = FALSE)
  records <- safe_df(record_registry)
  if (anyDuplicated(records$record_id) || any(nchar(records$record_id) != 3L)) {
    stop("Vanneman pretrend registry must contain unique three-digit record IDs.", call. = FALSE)
  }

  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  if (!length(lines)) stop("Vanneman panel4 pretrend source is empty.", call. = FALSE)

  record_id <- ifelse(nchar(lines) >= 7L, substr(lines, 5L, 7L), "")
  keep <- record_id %in% records$record_id
  selected <- lines[keep]
  if (!length(selected) || any(nchar(selected) < 46L)) {
    stop("Vanneman panel4 registered pretrend records are malformed.", call. = FALSE)
  }
  out <- data.frame(
    vanneman_state_id = substr(selected, 1L, 2L),
    vanneman_district_id = substr(selected, 3L, 4L),
    record_id = substr(selected, 5L, 7L),
    year = 1900L + suppressWarnings(as.integer(substr(selected, 8L, 9L))),
    version = suppressWarnings(as.integer(substr(selected, 10L, 10L))),
    total = vanneman_count_value(substr(selected, 11L, 19L)),
    rural = vanneman_count_value(substr(selected, 20L, 28L)),
    male = vanneman_count_value(substr(selected, 29L, 37L)),
    rural_male = vanneman_count_value(substr(selected, 38L, 46L)),
    stringsAsFactors = FALSE
  )
  out$panel_unit_id <- paste0(out$vanneman_state_id, out$vanneman_district_id)
  expected_years <- c(1961L, 1971L, 1981L, 1991L)
  if (any(!out$year %in% expected_years) || any(!is.finite(out$version)) ||
      any(out$version != 5L)) {
    stop("Vanneman pretrend records violate the stable-panel year/version-5 contract.", call. = FALSE)
  }
  key <- paste(out$panel_unit_id, out$year, out$record_id, sep = "__")
  if (anyDuplicated(key)) stop("Vanneman panel4 has duplicate registered pretrend records.", call. = FALSE)
  expected_n <- length(unique(out$panel_unit_id)) * length(expected_years) * nrow(records)
  if (nrow(out) != expected_n) {
    stop("Vanneman panel4 does not contain the complete registered pretrend record grid.", call. = FALSE)
  }
  out[order(out$panel_unit_id, out$year, out$record_id), , drop = FALSE]
}

vanneman_pretrend_record_field <- function(counts, record_id, field, keys) {
  x <- counts[counts$record_id == record_id, , drop = FALSE]
  idx <- match(keys, paste(x$panel_unit_id, x$year, sep = "__"))
  if (anyNA(idx)) stop("Vanneman pretrend record grid is incomplete for record ", record_id, ".", call. = FALSE)
  x[[field]][idx]
}

build_vanneman_pretrend_levels <- function(counts, geography) {
  x <- safe_df(counts)
  geography <- safe_df(geography)
  base <- x[x$record_id == "100",
            c("panel_unit_id", "vanneman_state_id", "vanneman_district_id", "year", "version"),
            drop = FALSE]
  keys <- paste(base$panel_unit_id, base$year, sep = "__")
  get_total <- function(id) vanneman_pretrend_record_field(x, id, "total", keys)
  population <- get_total("100")
  rural_population <- vanneman_pretrend_record_field(x, "100", "rural", keys)
  main_workers <- get_total("111")
  farm_workers <- get_total("112")
  literates <- get_total("140")
  primary_plus <- get_total("151")
  matriculate_plus <- get_total("153")

  invalid <- (is.finite(population) & population <= 0) |
    (is.finite(rural_population) & is.finite(population) & rural_population > population) |
    (is.finite(main_workers) & is.finite(population) & main_workers > population) |
    (is.finite(farm_workers) & is.finite(main_workers) & farm_workers > main_workers) |
    (is.finite(literates) & is.finite(population) & literates > population) |
    (is.finite(primary_plus) & is.finite(population) & primary_plus > population) |
    (is.finite(matriculate_plus) & is.finite(population) & matriculate_plus > population)
  invalid[is.na(invalid)] <- FALSE
  if (any(invalid)) stop("Vanneman pretrend counts violate basic population accounting identities.", call. = FALSE)

  base$population <- population
  base$log_population <- ifelse(population > 0, log(population), NA_real_)
  base$urban_share <- ifelse(population > 0, 1 - rural_population / population, NA_real_)
  base$main_worker_share <- ifelse(population > 0, main_workers / population, NA_real_)
  base$nonfarm_worker_share_main_workers <- ifelse(main_workers > 0, 1 - farm_workers / main_workers, NA_real_)
  base$literate_share_population <- ifelse(population > 0, literates / population, NA_real_)
  base$primary_plus_share_population <- ifelse(population > 0, primary_plus / population, NA_real_)
  base$matriculate_plus_share_population <- ifelse(population > 0, matriculate_plus / population, NA_real_)

  for (variable in setdiff(vanneman_pretrend_measure_registry()$measure_id, "log_population")) {
    bad <- is.finite(base[[variable]]) & (base[[variable]] < 0 | base[[variable]] > 1)
    if (any(bad)) stop("Vanneman pretrend share falls outside [0, 1]: ", variable, call. = FALSE)
  }

  required_geo <- c(
    "panel_unit_id", "dist91_state_id", "dist91_district_id",
    "state_code_2001", "district_code_2001",
    "pretrend_geography_status", "preferred_vanneman_pretrend_eligible"
  )
  missing <- setdiff(required_geo, names(geography))
  if (length(missing)) stop("Vanneman pretrend geography lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(geography$panel_unit_id)) {
    stop("Vanneman pretrend geography must contain unique stable panel IDs.", call. = FALSE)
  }
  if (!setequal(unique(x$panel_unit_id), geography$panel_unit_id)) {
    stop("Vanneman pretrend count and reviewed-geography stable-ID universes differ.", call. = FALSE)
  }
  out <- merge(base, geography[required_geo], by = "panel_unit_id", all.x = TRUE, sort = FALSE)
  if (anyNA(out$pretrend_geography_status)) {
    stop("Vanneman pretrend levels contain stable IDs absent from reviewed geography.", call. = FALSE)
  }
  out[order(out$panel_unit_id, out$year), , drop = FALSE]
}

build_vanneman_pretrend_levels_from_sources <- function(
    source_qa, geography, paths = build_paths()) {
  qa <- safe_df(source_qa)
  panel <- qa[qa$source_id == "panel4", , drop = FALSE]
  if (nrow(panel) != 1L || !isTRUE(panel$eligible_for_baseline_values[[1L]])) {
    stop("Vanneman panel4 source contract must be verified before constructing pretrend levels.", call. = FALSE)
  }
  files <- vanneman_historical_paths(paths)
  validate_vanneman_pretrend_sas_contract(files[["panel4_sas"]])
  build_vanneman_pretrend_levels(
    read_vanneman_panel4_pretrend_counts(files[["panel4"]]),
    geography
  )
}

build_vanneman_pretrend_changes <- function(levels) {
  x <- safe_df(levels)
  measures <- vanneman_pretrend_measure_registry()
  periods <- vanneman_pretrend_period_registry()
  pop61 <- x[x$year == 1961L, c("panel_unit_id", "population"), drop = FALSE]
  if (anyDuplicated(pop61$panel_unit_id)) stop("Vanneman 1961 population must be unique by stable panel ID.", call. = FALSE)

  safe_bind_rows(lapply(seq_len(nrow(periods)), function(i) {
    start <- x[x$year == periods$start_year[[i]], , drop = FALSE]
    end <- x[x$year == periods$end_year[[i]], , drop = FALSE]
    idx <- match(start$panel_unit_id, end$panel_unit_id)
    if (anyNA(idx)) stop("Vanneman pretrend levels do not share one stable unit universe.", call. = FALSE)
    safe_bind_rows(lapply(seq_len(nrow(measures)), function(j) {
      variable <- measures$measure_id[[j]]
      data.frame(
        panel_unit_id = start$panel_unit_id,
        dist91_state_id = start$dist91_state_id,
        dist91_district_id = start$dist91_district_id,
        state_code_2001 = start$state_code_2001,
        district_code_2001 = start$district_code_2001,
        preferred_vanneman_pretrend_eligible = start$preferred_vanneman_pretrend_eligible,
        pretrend_geography_status = start$pretrend_geography_status,
        period_id = periods$period_id[[i]],
        start_year = periods$start_year[[i]],
        end_year = periods$end_year[[i]],
        measure_id = variable,
        domain = measures$domain[[j]],
        label = measures$label[[j]],
        contains_estimated_source =
          isTRUE(measures$estimated_1961_source[[j]]) &&
          periods$start_year[[i]] == 1961L,
        start_value = num(start[[variable]]),
        end_value = num(end[[variable]][idx]),
        change = num(end[[variable]][idx]) - num(start[[variable]]),
        population_1961 = num(pop61$population[match(start$panel_unit_id, pop61$panel_unit_id)]),
        stringsAsFactors = FALSE
      )
    }))
  }))
}

attach_vanneman_pretrend_predictors <- function(
    changes, predictor_data, historical_distance = NULL) {
  out <- safe_df(changes)
  predictors <- safe_df(predictor_data)
  required <- c(
    "panel_unit_id", "state_code_2001", "district_code_2001",
    "pretrend_analysis_eligible", "pretrend_analysis_geography_status",
    "emie_exposure"
  )
  missing <- setdiff(required, names(predictors))
  if (length(missing)) {
    stop("Vanneman pretrend predictor data lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(predictors$panel_unit_id)) {
    stop("Vanneman pretrend predictor data must contain unique stable panel IDs.", call. = FALSE)
  }

  replace_cols <- c(
    "state_code_2001", "district_code_2001",
    "preferred_vanneman_pretrend_eligible", "pretrend_geography_status"
  )
  out <- out[setdiff(names(out), replace_cols)]
  out <- merge(out, predictors, by = "panel_unit_id", all.x = TRUE, sort = FALSE)
  out$preferred_vanneman_pretrend_eligible <- out$pretrend_analysis_eligible %in% TRUE
  out$pretrend_geography_status <- out$pretrend_analysis_geography_status

  if (!is.null(historical_distance)) {
    distance <- safe_df(historical_distance)
    required <- c(
      "state_code_1991", "district_code_1991", "historical_language_status",
      "ling_distance_nonzero_mean_1991"
    )
    missing <- setdiff(required, names(distance))
    if (length(missing)) {
      stop("Vanneman pretrend historical distance lacks: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    distance$state_code_1991 <- pad_admin_code(distance$state_code_1991, 2L)
    distance$district_code_1991 <- pad_admin_code(distance$district_code_1991, 2L)
    if (anyDuplicated(distance[c("state_code_1991", "district_code_1991")])) {
      stop("Vanneman pretrend historical distance has duplicate Census-1991 keys.", call. = FALSE)
    }
    names(distance)[names(distance) == "state_code_1991"] <- "dist91_state_id"
    names(distance)[names(distance) == "district_code_1991"] <- "dist91_district_id"
    out <- merge(
      out,
      distance[c(
        "dist91_state_id", "dist91_district_id",
        "historical_language_status", "ling_distance_nonzero_mean_1991"
      )],
      by = c("dist91_state_id", "dist91_district_id"), all.x = TRUE, sort = FALSE
    )
  }
  out$historical_ld_eligible <- if ("historical_language_status" %in% names(out)) {
    out$historical_language_status %in% "eligible" &
      is.finite(num(out$ling_distance_nonzero_mean_1991))
  } else FALSE
  out
}

build_vanneman_strict_pretrend_predictors <- function(
    changes, district_panel,
    treatment = preferred_iv_variables()$treatment) {
  x <- safe_df(changes)
  panel <- if (inherits(district_panel, "sf")) {
    sf::st_drop_geometry(district_panel)
  } else {
    safe_df(district_panel)
  }
  geography <- unique(x[c(
    "panel_unit_id", "state_code_2001", "district_code_2001",
    "preferred_vanneman_pretrend_eligible", "pretrend_geography_status"
  )])
  if (anyDuplicated(geography$panel_unit_id)) {
    stop("Strict Vanneman pretrend geography must contain unique stable panel IDs.", call. = FALSE)
  }

  required_panel <- c("state_code_2001", "district_code_2001", treatment)
  if ("ling_distance_nonzero_mean" %in% names(panel)) {
    required_panel <- c(required_panel, "ling_distance_nonzero_mean")
  }
  missing <- setdiff(required_panel, names(panel))
  if (length(missing)) {
    stop("Vanneman pretrend treatment panel lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(panel$district_code_2001, 2L)
  if (anyDuplicated(panel[c("state_code_2001", "district_code_2001")])) {
    stop("Vanneman pretrend treatment panel has duplicate Census-2001 district keys.", call. = FALSE)
  }

  out <- merge(
    geography,
    panel[required_panel],
    by = c("state_code_2001", "district_code_2001"),
    all.x = TRUE, sort = FALSE
  )
  names(out)[names(out) == treatment] <- "emie_exposure"
  if ("ling_distance_nonzero_mean" %in% names(out)) {
    names(out)[names(out) == "ling_distance_nonzero_mean"] <-
      "ling_distance_nonzero_mean_2001"
  }
  out$pretrend_analysis_eligible <- out$preferred_vanneman_pretrend_eligible %in% TRUE
  out$pretrend_analysis_geography_status <- out$pretrend_geography_status
  out
}

vanneman_parent_linguistic_distance_2001 <- function(x) {
  total <- num(x$ling_total_speakers)
  shares <- lapply(1:5, function(degree) {
    num(x[[paste0("ling_share_distance_", degree)]])
  })
  count_by_degree <- lapply(shares, function(share) total * share / 100)
  denominator <- sum(vapply(
    count_by_degree, function(v) sum(v, na.rm = TRUE), numeric(1)
  ))
  numerator <- sum(vapply(seq_along(count_by_degree), function(i) {
    i * sum(count_by_degree[[i]], na.rm = TRUE)
  }, numeric(1)))
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  numerator / denominator
}

aggregate_vanneman_parent_predictors <- function(
    parent_bridge, district_panel,
    treatment = preferred_iv_variables()$treatment) {
  bridge <- safe_df(parent_bridge)
  panel <- if (inherits(district_panel, "sf")) {
    sf::st_drop_geometry(district_panel)
  } else {
    safe_df(district_panel)
  }
  required_bridge <- c(
    "panel_unit_id", "dist91_state_id", "dist91_district_id",
    "state_code_2001", "district_code_2001",
    "parent_bridge_status", "preferred_vanneman_parent_eligible"
  )
  missing <- setdiff(required_bridge, names(bridge))
  if (length(missing)) {
    stop("Vanneman historical-parent bridge lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  distance_share_cols <- paste0("ling_share_distance_", 1:5)
  required_panel <- c(
    "state_code_2001", "district_code_2001",
    "eligible_child_weight_0708", "emi_enrolled_child_weight_0708",
    treatment, "ling_total_speakers", "ling_distance_nonzero_mean",
    distance_share_cols
  )
  missing <- setdiff(required_panel, names(panel))
  if (length(missing)) {
    stop("Historical-parent predictor aggregation lacks district-panel fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(panel$district_code_2001, 2L)
  if (anyDuplicated(panel[c("state_code_2001", "district_code_2001")])) {
    stop("Historical-parent predictor aggregation requires unique Census-2001 districts.", call. = FALSE)
  }

  eligible_bridge <- bridge[bridge$preferred_vanneman_parent_eligible %in% TRUE, , drop = FALSE]
  joined <- merge(
    eligible_bridge,
    panel[required_panel],
    by = c("state_code_2001", "district_code_2001"),
    all.x = TRUE, sort = FALSE
  )
  if (anyNA(joined[[treatment]]) ||
      anyNA(joined$eligible_child_weight_0708) ||
      anyNA(joined$emi_enrolled_child_weight_0708) ||
      anyNA(joined$ling_total_speakers)) {
    stop("Historical-parent predictor aggregation is missing required descendant data.", call. = FALSE)
  }

  district_emie <- num(joined$emi_enrolled_child_weight_0708) /
    num(joined$eligible_child_weight_0708)
  mismatch <- is.finite(district_emie) & is.finite(num(joined[[treatment]])) &
    abs(district_emie - num(joined[[treatment]])) > 1e-8
  if (any(mismatch)) {
    stop("Historical-parent EMIE components do not reproduce district treatment values.", call. = FALSE)
  }

  rows <- lapply(split(seq_len(nrow(joined)), joined$panel_unit_id), function(i) {
    x <- joined[i, , drop = FALSE]
    denominator <- sum(num(x$eligible_child_weight_0708))
    numerator <- sum(num(x$emi_enrolled_child_weight_0708))
    states <- unique(plain_chr(x$state_code_2001))
    if (length(states) != 1L) {
      stop("Eligible historical parent spans multiple Census-2001 states.", call. = FALSE)
    }
    data.frame(
      panel_unit_id = x$panel_unit_id[[1L]],
      state_code_2001 = states[[1L]],
      district_code_2001 = NA_character_,
      pretrend_analysis_eligible = TRUE,
      pretrend_analysis_geography_status = x$parent_bridge_status[[1L]],
      emie_exposure = if (is.finite(denominator) && denominator > 0) {
        numerator / denominator
      } else {
        NA_real_
      },
      ling_distance_nonzero_mean_2001 =
        vanneman_parent_linguistic_distance_2001(x),
      n_descendant_2001_districts = nrow(x),
      stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(rows)

  all_units <- unique(bridge[c(
    "panel_unit_id", "parent_bridge_status", "preferred_vanneman_parent_eligible"
  )])
  missing_units <- setdiff(all_units$panel_unit_id, out$panel_unit_id)
  if (length(missing_units)) {
    blocked <- all_units[match(missing_units, all_units$panel_unit_id), , drop = FALSE]
    blocked_rows <- data.frame(
      panel_unit_id = blocked$panel_unit_id,
      state_code_2001 = NA_character_,
      district_code_2001 = NA_character_,
      pretrend_analysis_eligible = FALSE,
      pretrend_analysis_geography_status = blocked$parent_bridge_status,
      emie_exposure = NA_real_,
      ling_distance_nonzero_mean_2001 = NA_real_,
      n_descendant_2001_districts = NA_integer_,
      stringsAsFactors = FALSE
    )
    out <- safe_bind_rows(list(out, blocked_rows))
  }
  out[order(out$panel_unit_id), , drop = FALSE]
}

build_vanneman_pretrend_predictor_panel <- function(
    changes, district_panel, historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  predictors <- build_vanneman_strict_pretrend_predictors(
    changes, district_panel, treatment
  )
  attach_vanneman_pretrend_predictors(
    changes, predictors, historical_distance
  )
}

build_vanneman_parent_pretrend_predictor_panel <- function(
    changes, parent_bridge, district_panel, historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  predictors <- aggregate_vanneman_parent_predictors(
    parent_bridge, district_panel, treatment
  )
  attach_vanneman_pretrend_predictors(
    changes, predictors, historical_distance
  )
}

vanneman_pretrend_specification_registry <- function(panel) {
  x <- safe_df(panel)
  rows <- list(data.frame(
    predictor_id = "eventual_emie",
    predictor = "emie_exposure",
    sample_id = "full_pretrend",
    stringsAsFactors = FALSE
  ))
  if ("ling_distance_nonzero_mean_2001" %in% names(x)) {
    rows <- c(rows, list(data.frame(
      predictor_id = "census_2001_ld",
      predictor = "ling_distance_nonzero_mean_2001",
      sample_id = "full_pretrend",
      stringsAsFactors = FALSE
    )))
  }
  if (all(c(
      "historical_ld_eligible",
      "ling_distance_nonzero_mean_1991"
    ) %in% names(x))) {
    common <- list(data.frame(
      predictor_id = "eventual_emie",
      predictor = "emie_exposure",
      sample_id = "historical_ld_support",
      stringsAsFactors = FALSE
    ))
    if ("ling_distance_nonzero_mean_2001" %in% names(x)) {
      common <- c(common, list(data.frame(
        predictor_id = "census_2001_ld",
        predictor = "ling_distance_nonzero_mean_2001",
        sample_id = "historical_ld_support",
        stringsAsFactors = FALSE
      )))
    }
    common <- c(common, list(data.frame(
      predictor_id = "historical_ld_1991",
      predictor = "ling_distance_nonzero_mean_1991",
      sample_id = "historical_ld_support",
      stringsAsFactors = FALSE
    )))
    rows <- c(rows, common)
  }
  safe_bind_rows(rows)
}

vanneman_pretrend_sample <- function(
    panel, predictor, period_id, measures,
    sample_id = c("full_pretrend", "historical_ld_support")) {
  sample_id <- match.arg(sample_id)
  x <- safe_df(panel)
  keep <- x$preferred_vanneman_pretrend_eligible %in% TRUE &
    x$period_id == period_id & x$measure_id %in% measures &
    is.finite(num(x$change)) & is.finite(num(x$population_1961)) &
    num(x$population_1961) > 0 & nzchar(plain_chr(x$state_code_2001)) &
    is.finite(num(x[[predictor]]))

  if (identical(sample_id, "historical_ld_support")) {
    keep <- keep &
      x$historical_ld_eligible %in% TRUE &
      is.finite(num(x$emie_exposure))
    if ("ling_distance_nonzero_mean_2001" %in% names(x)) {
      keep <- keep & is.finite(num(x$ling_distance_nonzero_mean_2001))
    }
  }
  x[keep, , drop = FALSE]
}

vanneman_pretrend_sample_coverage <- function(panel) {
  x <- safe_df(panel)
  keep <- x$preferred_vanneman_pretrend_eligible %in% TRUE &
    is.finite(num(x$population_1961)) & num(x$population_1961) > 0 &
    nzchar(plain_chr(x$state_code_2001))
  coverage_fields <- c(
    "panel_unit_id", "state_code_2001", "population_1961",
    "emie_exposure", "historical_ld_eligible"
  )
  if ("ling_distance_nonzero_mean_2001" %in% names(x)) {
    coverage_fields <- c(coverage_fields, "ling_distance_nonzero_mean_2001")
  }
  units <- x[keep, coverage_fields, drop = FALSE]
  units <- units[!duplicated(units$panel_unit_id), , drop = FALSE]

  full <- is.finite(num(units$emie_exposure))
  current_ld <- if ("ling_distance_nonzero_mean_2001" %in% names(units)) {
    is.finite(num(units$ling_distance_nonzero_mean_2001))
  } else {
    rep(FALSE, nrow(units))
  }
  common <- full & current_ld & units$historical_ld_eligible %in% TRUE
  summarize <- function(sample_id, selected) {
    data.frame(
      sample_id = sample_id,
      n_units = sum(selected),
      n_states = length(unique(units$state_code_2001[selected])),
      population_1961 = sum(num(units$population_1961[selected])),
      share_of_full_units = if (sum(full) > 0) sum(selected) / sum(full) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  rows <- list(summarize("full_pretrend", full))
  if (any(current_ld)) {
    rows <- c(rows, list(summarize("census_2001_ld_support", full & current_ld)))
  }
  rows <- c(rows, list(summarize("historical_ld_support", common)))
  safe_bind_rows(rows)
}

estimate_vanneman_pretrend_association <- function(
    panel, predictor, period_id, measure_id,
    fixed_effect = c("none", "state"),
    sample_id = c("full_pretrend", "historical_ld_support")) {
  fixed_effect <- match.arg(fixed_effect)
  sample_id <- match.arg(sample_id)
  x <- vanneman_pretrend_sample(
    panel, predictor, period_id, measure_id, sample_id
  )
  if (!nrow(x)) {
    return(data.frame(
      predictor = predictor, sample_id = sample_id,
      period_id = period_id, measure_id = measure_id,
      fixed_effect = fixed_effect, estimate = NA_real_, std.error = NA_real_,
      p.value = NA_real_, standardized_effect = NA_real_,
      contains_estimated_source = NA, n = 0L, n_states = 0L,
      population_1961 = 0, status = "not_estimated",
      stringsAsFactors = FALSE
    ))
  }
  inference <- historical_weighted_term_inference(
    x, predictor, "change", "population_1961", "state_code_2001", fixed_effect
  )
  quality <- unique(x$contains_estimated_source)
  quality <- quality[!is.na(quality)]
  data.frame(
    predictor = predictor, sample_id = sample_id,
    period_id = period_id, measure_id = measure_id,
    fixed_effect = fixed_effect, estimate = inference$estimate,
    std.error = inference$std.error, p.value = inference$p.value,
    standardized_effect = inference$standardized_effect,
    contains_estimated_source = length(quality) == 1L && isTRUE(quality[[1L]]),
    n = inference$n, n_states = inference$n_states,
    population_1961 = inference$population_weight,
    status = "estimated", stringsAsFactors = FALSE
  )
}

estimate_vanneman_pretrend_joint_balance <- function(
    panel, predictor, period_id, domain,
    sample_id = c("full_pretrend", "historical_ld_support")) {
  sample_id <- match.arg(sample_id)
  metadata <- vanneman_pretrend_measure_registry()
  measures <- metadata$measure_id[metadata$domain == domain]
  x <- vanneman_pretrend_sample(panel, predictor, period_id, measures, sample_id)
  if (!nrow(x)) {
    return(data.frame(
      predictor = predictor, sample_id = sample_id,
      period_id = period_id, domain = domain,
      tested_measures = paste(measures, collapse = ";"),
      joint_f = NA_real_, joint_p = NA_real_, n = 0L, n_states = 0L,
      population_1961 = 0, status = "not_estimated", reason = "no_complete_cases",
      stringsAsFactors = FALSE
    ))
  }
  wide <- reshape(
    x[c("panel_unit_id", "state_code_2001", "population_1961",
        predictor, "measure_id", "change")],
    idvar = c("panel_unit_id", "state_code_2001", "population_1961", predictor),
    timevar = "measure_id", direction = "wide"
  )
  change_cols <- paste0("change.", measures)
  wide <- wide[stats::complete.cases(wide[c(change_cols, predictor, "state_code_2001", "population_1961")]), , drop = FALSE]
  if (!nrow(wide)) {
    return(data.frame(
      predictor = predictor, sample_id = sample_id,
      period_id = period_id, domain = domain,
      tested_measures = paste(measures, collapse = ";"),
      joint_f = NA_real_, joint_p = NA_real_, n = 0L, n_states = 0L,
      population_1961 = 0, status = "not_estimated", reason = "no_complete_cases",
      stringsAsFactors = FALSE
    ))
  }
  inference <- historical_weighted_joint_inference(
    wide, predictor, change_cols, "population_1961", "state_code_2001"
  )
  data.frame(
    predictor = predictor, sample_id = sample_id,
    period_id = period_id, domain = domain,
    tested_measures = paste(measures, collapse = ";"),
    joint_f = inference$joint_f, joint_p = inference$joint_p,
    n = inference$n, n_states = inference$n_states,
    population_1961 = inference$population_weight,
    status = inference$status, reason = inference$reason,
    stringsAsFactors = FALSE
  )
}

assemble_vanneman_pretrend_validation <- function(levels, changes, panel) {
  specifications <- vanneman_pretrend_specification_registry(panel)
  periods <- vanneman_pretrend_period_registry()$period_id
  measures <- vanneman_pretrend_measure_registry()$measure_id
  domains <- unique(vanneman_pretrend_measure_registry()$domain)

  estimates <- safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, , drop = FALSE]
    safe_bind_rows(lapply(periods, function(period_id) {
      safe_bind_rows(lapply(measures, function(measure_id) {
        safe_bind_rows(lapply(c("none", "state"), function(fixed_effect) {
          out <- estimate_vanneman_pretrend_association(
            panel, spec$predictor[[1L]], period_id, measure_id,
            fixed_effect, spec$sample_id[[1L]]
          )
          out$predictor_id <- spec$predictor_id[[1L]]
          out
        }))
      }))
    }))
  }))
  joint <- safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, , drop = FALSE]
    safe_bind_rows(lapply(periods, function(period_id) {
      safe_bind_rows(lapply(domains, function(domain) {
        out <- estimate_vanneman_pretrend_joint_balance(
          panel, spec$predictor[[1L]], period_id, domain,
          spec$sample_id[[1L]]
        )
        out$predictor_id <- spec$predictor_id[[1L]]
        out
      }))
    }))
  }))

  list(
    levels = levels,
    changes = changes,
    panel = panel,
    sample_coverage = vanneman_pretrend_sample_coverage(panel),
    estimates = estimates,
    joint_balance = joint
  )
}

build_vanneman_pretrend_validation <- function(
    levels, district_panel, historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  changes <- build_vanneman_pretrend_changes(levels)
  panel <- build_vanneman_pretrend_predictor_panel(
    changes, district_panel, historical_distance, treatment
  )
  assemble_vanneman_pretrend_validation(levels, changes, panel)
}

build_vanneman_parent_pretrend_validation <- function(
    levels, parent_bridge, district_panel, historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  changes <- build_vanneman_pretrend_changes(levels)
  panel <- build_vanneman_parent_pretrend_predictor_panel(
    changes, parent_bridge, district_panel, historical_distance, treatment
  )
  assemble_vanneman_pretrend_validation(levels, changes, panel)
}

save_vanneman_pretrend_validation <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance",
    prefix = "vanneman_pretrend") {
  paths <- c(
    levels = file.path(directory, paste0(prefix, "_levels.csv")),
    changes = file.path(directory, paste0(prefix, "_changes.csv")),
    sample_coverage = file.path(directory, paste0(prefix, "_sample_coverage.csv")),
    estimates = file.path(directory, paste0(prefix, "_balance.csv")),
    joint_balance = file.path(directory, paste0(prefix, "_balance_joint.csv"))
  )
  write_diagnostic_csv(x$levels, paths[["levels"]])
  write_diagnostic_csv(x$changes, paths[["changes"]])
  write_diagnostic_csv(x$sample_coverage, paths[["sample_coverage"]])
  write_diagnostic_csv(x$estimates, paths[["estimates"]])
  write_diagnostic_csv(x$joint_balance, paths[["joint_balance"]])
  unname(paths)
}
