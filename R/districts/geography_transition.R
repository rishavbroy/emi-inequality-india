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
    "population_weight", "area_weight",
    "source_coverage", "target_coverage",
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
    source_coverage = if ("shrid_coverage" %in% names(x)) {
      num(x$shrid_coverage)
    } else {
      rep(NA_real_, n)
    },
    target_coverage = rep(NA_real_, n),
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
  for (variable in c(
      "population_weight", "area_weight",
      "source_coverage", "target_coverage"
    )) {
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


attach_shrug_transition_coverage <- function(
    transition, shrid_bridge, source_year, target_year) {
  x <- safe_df(transition)
  validate_geography_transition(x)
  source_year <- as.integer(source_year)
  target_year <- as.integer(target_year)

  coverage_table <- function(year, other_year, side) {
    summary <- summarize_shrug_source_district_mapping(
      shrid_bridge, year, other_year, min_population_coverage = 1
    )
    state <- paste0("state_code_", year)
    district <- paste0("district_code_", year)
    if (!nrow(summary)) {
      return(data.frame(
        unit_id = character(), coverage = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      unit_id = geography_transition_unit_id(
        year, summary[[state]], summary[[district]]
      ),
      coverage = num(summary$shrid_coverage),
      stringsAsFactors = FALSE
    )
  }

  source <- coverage_table(source_year, target_year, "source")
  target <- coverage_table(target_year, source_year, "target")
  source_idx <- match(x$source_unit_id, source$unit_id)
  target_idx <- match(x$target_unit_id, target$unit_id)
  x$source_coverage <- source$coverage[source_idx]
  x$target_coverage <- target$coverage[target_idx]
  validate_geography_transition(x)
  x
}

build_geography_components <- function(transition) {
  x <- safe_df(transition)
  validate_geography_transition(x)
  if (!nrow(x)) {
    return(data.frame(
      harmonized_component_id = character(),
      vintage = integer(),
      unit_id = character(),
      side = character(),
      stringsAsFactors = FALSE
    ))
  }

  edges <- unique(x[c("source_unit_id", "target_unit_id")])
  graph <- igraph::graph_from_data_frame(
    edges, directed = FALSE
  )
  membership <- igraph::components(graph)$membership
  vertices <- names(membership)
  component_number <- as.integer(membership)
  component_anchor <- vapply(
    split(vertices, component_number),
    min,
    character(1)
  )
  component_order <- order(component_anchor)
  component_rank <- integer(length(component_anchor))
  component_rank[component_order] <- seq_along(component_order)
  component_id <- sprintf(
    "geo_component_%04d",
    component_rank[as.character(component_number)]
  )

  source_units <- unique(data.frame(
    unit_id = x$source_unit_id,
    vintage = as.integer(x$source_vintage),
    side = "source",
    stringsAsFactors = FALSE
  ))
  target_units <- unique(data.frame(
    unit_id = x$target_unit_id,
    vintage = as.integer(x$target_vintage),
    side = "target",
    stringsAsFactors = FALSE
  ))
  out <- safe_bind_rows(list(source_units, target_units))
  idx <- match(out$unit_id, vertices)
  out$harmonized_component_id <- component_id[idx]
  out <- out[c(
    "harmonized_component_id", "vintage", "unit_id", "side"
  )]
  out[order(
    out$harmonized_component_id, out$vintage, out$unit_id
  ), , drop = FALSE]
}

summarize_geography_components <- function(
    transition, components = build_geography_components(transition),
    complete_coverage_threshold = 1 - 1e-12) {
  x <- safe_df(transition)
  membership <- safe_df(components)
  validate_geography_transition(x)
  required <- c(
    "harmonized_component_id", "vintage", "unit_id", "side"
  )
  missing <- setdiff(required, names(membership))
  if (length(missing)) {
    stop(
      "Geography component membership lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(membership)) {
    return(data.frame(
      harmonized_component_id = character(),
      source_vintage = integer(),
      target_vintage = integer(),
      n_source_units = integer(),
      n_target_units = integer(),
      component_class = character(),
      source_coverage_complete = logical(),
      target_coverage_complete = logical(),
      deterministic_amalgamation_eligible = logical(),
      stringsAsFactors = FALSE
    ))
  }

  unit_component <- setNames(
    membership$harmonized_component_id,
    membership$unit_id
  )
  x$harmonized_component_id <- unname(
    unit_component[x$source_unit_id]
  )
  target_component <- unname(unit_component[x$target_unit_id])
  if (anyNA(x$harmonized_component_id) ||
      anyNA(target_component) ||
      any(x$harmonized_component_id != target_component)) {
    stop(
      "Geography component membership is inconsistent with transition edges.",
      call. = FALSE
    )
  }

  safe_bind_rows(lapply(
    split(seq_len(nrow(x)), x$harmonized_component_id),
    function(i) {
      part <- x[i, , drop = FALSE]
      source_units <- unique(part$source_unit_id)
      target_units <- unique(part$target_unit_id)
      source_coverage <- tapply(
        num(part$source_coverage),
        part$source_unit_id,
        function(z) unique(z[is.finite(z)])
      )
      target_coverage <- tapply(
        num(part$target_coverage),
        part$target_unit_id,
        function(z) unique(z[is.finite(z)])
      )
      coverage_complete <- function(values, units) {
        if (length(values) != length(units)) return(FALSE)
        all(vapply(values, function(z) {
          length(z) == 1L &&
            is.finite(z) &&
            z >= complete_coverage_threshold
        }, logical(1)))
      }
      source_complete <- coverage_complete(
        source_coverage, source_units
      )
      target_complete <- coverage_complete(
        target_coverage, target_units
      )
      n_source <- length(source_units)
      n_target <- length(target_units)
      component_class <- if (
          n_source == 1L && n_target == 1L) {
        "one_to_one"
      } else if (n_source == 1L) {
        "split"
      } else if (n_target == 1L) {
        "merger"
      } else {
        "many_to_many"
      }

      data.frame(
        harmonized_component_id =
          part$harmonized_component_id[[1L]],
        source_vintage = unique(part$source_vintage)[[1L]],
        target_vintage = unique(part$target_vintage)[[1L]],
        n_source_units = n_source,
        n_target_units = n_target,
        component_class = component_class,
        source_coverage_complete = source_complete,
        target_coverage_complete = target_complete,
        deterministic_amalgamation_eligible =
          source_complete && target_complete,
        stringsAsFactors = FALSE
      )
    }
  ))
}
