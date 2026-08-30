# Canonical change-of-support representation for district transitions.
#
# Source-specific modules own evidence acquisition and adjudication. This module
# only normalizes accepted transition edges so later harmonization can reason
# about identities, splits, mergers, and many-to-many components consistently.

geography_transition_columns <- function() {
  c(
    "source_vintage", "target_vintage",
    "source_state_code", "source_district_code", "source_unit_id",
    "target_state_code", "target_district_code", "target_unit_id",
    "population_weight", "area_weight", "coverage",
    "mapping_class", "evidence_source"
  )
}

geography_transition_unit_id <- function(vintage, state_code, district_code) {
  paste(
    paste0("census", as.integer(vintage)),
    plain_chr(state_code),
    plain_chr(district_code),
    sep = "__"
  )
}

as_geography_transition <- function(
    transition, source_year, target_year,
    evidence_source = "district_transition") {
  x <- safe_df(transition)
  source_year <- as.integer(source_year)
  target_year <- as.integer(target_year)
  source_state <- paste0("state_code_", source_year)
  source_district <- paste0("district_code_", source_year)
  target_state <- paste0("state_code_", target_year)
  target_district <- paste0("district_code_", target_year)
  population_weight <- paste0("population_share_to_", target_year)
  area_weight <- paste0("area_share_to_", target_year)

  required <- c(
    source_state, source_district, target_state, target_district,
    "mapping_class"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "District transition lacks canonical geography fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  n <- nrow(x)
  out <- data.frame(
    source_vintage = rep(source_year, n),
    target_vintage = rep(target_year, n),
    source_state_code = plain_chr(x[[source_state]]),
    source_district_code = plain_chr(x[[source_district]]),
    target_state_code = plain_chr(x[[target_state]]),
    target_district_code = plain_chr(x[[target_district]]),
    population_weight = if (population_weight %in% names(x)) {
      num(x[[population_weight]])
    } else {
      rep(NA_real_, n)
    },
    area_weight = if (area_weight %in% names(x)) {
      num(x[[area_weight]])
    } else {
      rep(NA_real_, n)
    },
    coverage = if ("shrid_coverage" %in% names(x)) {
      num(x$shrid_coverage)
    } else {
      rep(NA_real_, n)
    },
    mapping_class = plain_chr(x$mapping_class),
    evidence_source = if ("source_id" %in% names(x)) {
      plain_chr(x$source_id)
    } else {
      rep(plain_chr(evidence_source), n)
    },
    stringsAsFactors = FALSE
  )
  out$source_unit_id <- geography_transition_unit_id(
    source_year, out$source_state_code, out$source_district_code
  )
  out$target_unit_id <- geography_transition_unit_id(
    target_year, out$target_state_code, out$target_district_code
  )
  out <- out[geography_transition_columns()]
  validate_geography_transition(out)
  annotate_geography_transition_topology(out)
}

validate_geography_transition <- function(transition) {
  x <- safe_df(transition)
  missing <- setdiff(geography_transition_columns(), names(x))
  if (length(missing)) {
    stop(
      "Canonical geography transition lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(x[c("source_unit_id", "target_unit_id")])) {
    stop(
      "Canonical geography transition contains duplicate source-target edges.",
      call. = FALSE
    )
  }
  for (variable in c("population_weight", "area_weight", "coverage")) {
    value <- num(x[[variable]])
    invalid <- is.finite(value) & (value < 0 | value > 1 + 1e-8)
    if (any(invalid)) {
      stop(
        "Canonical geography transition ", variable,
        " must lie between zero and one.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

annotate_geography_transition_topology <- function(transition) {
  x <- safe_df(transition)
  validate_geography_transition(x)
  if (!nrow(x)) {
    x$source_degree <- integer()
    x$target_degree <- integer()
    x$topology <- character()
    return(x)
  }

  source_degree <- table(x$source_unit_id)
  target_degree <- table(x$target_unit_id)
  x$source_degree <- as.integer(source_degree[x$source_unit_id])
  x$target_degree <- as.integer(target_degree[x$target_unit_id])
  x$topology <- ifelse(
    x$source_degree == 1L & x$target_degree == 1L,
    "one_to_one",
    ifelse(
      x$source_degree > 1L & x$target_degree == 1L,
      "split",
      ifelse(
        x$source_degree == 1L & x$target_degree > 1L,
        "merger",
        "many_to_many"
      )
    )
  )
  x
}
