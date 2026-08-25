# Registered consumption-welfare outcomes for the canonical IV architecture.
# This module prepares outcome data and self-describing IV specifications; it
# does not choose between reduced-form and 2SLS interpretation.

read_consumption_iv_outcome_registry <- function(path) {
  if (!file.exists(path)) {
    stop("Consumption IV outcome registry is missing: ", path, call. = FALSE)
  }
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "welfare_specification_id", "outcome_id", "outcome_round",
    "baseline_round", "estimand", "analysis_transform", "treatment",
    "instrument", "adjustment_id", "construction_id", "panel_variant",
    "sample_rule", "tier"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption IV outcome registry is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  chr <- required
  for (nm in chr) x[[nm]] <- trimws(plain_chr(x[[nm]]))
  if (!nrow(x) || anyDuplicated(x$welfare_specification_id) ||
      any(vapply(x[required], function(v) any(is.na(v) | !nzchar(v)), logical(1)))) {
    stop("Consumption IV outcome registry contains empty or duplicate identifiers.", call. = FALSE)
  }
  if (any(!x$estimand %in% c("ancova", "change", "level"))) {
    stop("Consumption IV outcome registry contains an unknown estimand.", call. = FALSE)
  }
  if (any(!x$analysis_transform %in% c("identity", "log"))) {
    stop("Consumption IV outcome registry contains an unknown analysis transform.", call. = FALSE)
  }
  needs_baseline <- x$estimand %in% c("ancova", "change")
  if (any(needs_baseline & x$baseline_round == x$outcome_round)) {
    stop("Consumption IV outcome baseline and endpoint rounds must differ.", call. = FALSE)
  }
  x
}

consumption_iv_variable_name <- function(specification_id, role = c("outcome", "baseline")) {
  role <- match.arg(role)
  paste0(
    "welfare_iv__", gsub("[^A-Za-z0-9_]+", "_", plain_chr(specification_id)),
    "__", role
  )
}

transform_consumption_iv_value <- function(value, transform) {
  x <- num(value)
  switch(
    plain_chr(transform),
    identity = x,
    log = {
      out <- rep(NA_real_, length(x))
      keep <- positive_finite(x)
      out[keep] <- log(x[keep])
      out
    },
    stop("Unknown consumption IV analysis transform: ", transform, call. = FALSE)
  )
}

consumption_iv_round_rows <- function(welfare, outcome_id, round_id) {
  x <- safe_df(welfare)
  required <- c(
    "district_2001", "round_id", "outcome_id", "estimate",
    "preferred_eligible"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption welfare input lacks IV outcome fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  out <- x[
    plain_chr(x$outcome_id) == outcome_id &
      plain_chr(x$round_id) == round_id,
    required,
    drop = FALSE
  ]
  if (!nrow(out)) {
    stop(
      "Consumption IV outcome registry references unavailable welfare rows: ",
      outcome_id, " / ", round_id,
      call. = FALSE
    )
  }
  if (anyDuplicated(out$district_2001)) {
    stop(
      "Consumption welfare input is not unique by district/outcome/round.",
      call. = FALSE
    )
  }
  out
}

build_consumption_iv_specification_data <- function(welfare, specification) {
  spec <- safe_df(specification)
  if (nrow(spec) != 1L) {
    stop("A single consumption IV outcome specification is required.", call. = FALSE)
  }

  outcome_id <- spec$outcome_id[[1L]]
  endpoint_id <- spec$outcome_round[[1L]]
  baseline_id <- spec$baseline_round[[1L]]
  estimand <- spec$estimand[[1L]]
  transform <- spec$analysis_transform[[1L]]

  endpoint <- consumption_iv_round_rows(welfare, outcome_id, endpoint_id)
  endpoint_value <- transform_consumption_iv_value(endpoint$estimate, transform)
  endpoint_ok <- endpoint$preferred_eligible %in% TRUE & is.finite(endpoint_value)

  if (estimand == "level") {
    return(data.frame(
      target_unit_2001 = plain_chr(endpoint$district_2001),
      outcome_value = ifelse(endpoint_ok, endpoint_value, NA_real_),
      baseline_value = NA_real_,
      preferred_welfare_support = endpoint_ok,
      stringsAsFactors = FALSE
    ))
  }

  baseline <- consumption_iv_round_rows(welfare, outcome_id, baseline_id)
  joined <- merge(
    endpoint[c("district_2001", "estimate", "preferred_eligible")],
    baseline[c("district_2001", "estimate", "preferred_eligible")],
    by = "district_2001", all = FALSE, sort = FALSE,
    suffixes = c("_endpoint", "_baseline")
  )
  endpoint_value <- transform_consumption_iv_value(
    joined$estimate_endpoint, transform
  )
  baseline_value <- transform_consumption_iv_value(
    joined$estimate_baseline, transform
  )
  common_ok <- joined$preferred_eligible_endpoint %in% TRUE &
    joined$preferred_eligible_baseline %in% TRUE &
    is.finite(endpoint_value) & is.finite(baseline_value)

  outcome_value <- if (estimand == "ancova") {
    endpoint_value
  } else if (estimand == "change") {
    endpoint_value - baseline_value
  } else {
    stop("Unknown consumption IV estimand: ", estimand, call. = FALSE)
  }

  data.frame(
    target_unit_2001 = plain_chr(joined$district_2001),
    outcome_value = ifelse(common_ok, outcome_value, NA_real_),
    baseline_value = ifelse(common_ok, baseline_value, NA_real_),
    preferred_welfare_support = common_ok,
    stringsAsFactors = FALSE
  )
}

attach_consumption_iv_outcomes <- function(panel, welfare, registry) {
  out <- panel
  if (!"target_unit_2001" %in% names(out)) {
    stop("Analysis panel lacks target_unit_2001 for consumption IV outcomes.", call. = FALSE)
  }
  specs <- safe_df(registry)
  if (!nrow(specs)) {
    stop("Consumption IV outcome registry is empty.", call. = FALSE)
  }

  panel_key <- plain_chr(out$target_unit_2001)
  if (anyDuplicated(panel_key)) {
    stop("Analysis panel target_unit_2001 must be unique.", call. = FALSE)
  }

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, , drop = FALSE]
    data <- build_consumption_iv_specification_data(welfare, spec)
    pos <- match(panel_key, data$target_unit_2001)
    outcome_name <- consumption_iv_variable_name(
      spec$welfare_specification_id[[1L]], "outcome"
    )
    baseline_name <- consumption_iv_variable_name(
      spec$welfare_specification_id[[1L]], "baseline"
    )
    if (outcome_name %in% names(out) || baseline_name %in% names(out)) {
      stop("Consumption IV outcome columns would overwrite analysis-panel fields.", call. = FALSE)
    }
    out[[outcome_name]] <- data$outcome_value[pos]
    if (identical(spec$estimand[[1L]], "ancova")) {
      out[[baseline_name]] <- data$baseline_value[pos]
    }
  }
  out
}

compile_consumption_iv_specifications <- function(registry) {
  specs <- safe_df(registry)
  adjustments <- iv_adjustment_sets()
  constructions <- iv_instrument_constructions()

  rows <- lapply(seq_len(nrow(specs)), function(i) {
    x <- specs[i, , drop = FALSE]
    adjustment_id <- x$adjustment_id[[1L]]
    construction_id <- x$construction_id[[1L]]
    if (!adjustment_id %in% names(adjustments)) {
      stop("Unknown consumption IV adjustment_id: ", adjustment_id, call. = FALSE)
    }
    if (!construction_id %in% names(constructions)) {
      stop("Unknown consumption IV construction_id: ", construction_id, call. = FALSE)
    }
    adjustment <- adjustments[[adjustment_id]]
    construction <- constructions[[construction_id]]
    excluded <- plain_chr(construction$excluded)
    if (length(excluded) != 1L || !identical(excluded[[1L]], x$instrument[[1L]])) {
      stop(
        "Consumption IV registry instrument does not match its construction.",
        call. = FALSE
      )
    }

    outcome_name <- consumption_iv_variable_name(
      x$welfare_specification_id[[1L]], "outcome"
    )
    controls <- adjustment$controls
    if (identical(x$estimand[[1L]], "ancova")) {
      controls <- c(
        controls,
        consumption_iv_variable_name(
          x$welfare_specification_id[[1L]], "baseline"
        )
      )
    }

    row <- iv_specification_row(
      specification_id = paste0(
        "consumption__", x$welfare_specification_id[[1L]]
      ),
      adjustment_id = adjustment_id,
      adjustment = adjustment$label,
      construction_id = construction_id,
      construction = construction$label,
      outcome = outcome_name,
      treatment = x$treatment[[1L]],
      fixed_effect = adjustment$fixed_effect,
      controls = controls,
      included_language_controls = construction$included,
      excluded_instruments = construction$excluded,
      mapping_coverage_variable = construction$coverage,
      panel_variant = x$panel_variant[[1L]],
      sample_rule = x$sample_rule[[1L]],
      tier = x$tier[[1L]],
      sequence = i
    )
    row$welfare_specification_id <- x$welfare_specification_id[[1L]]
    row$welfare_outcome_id <- x$outcome_id[[1L]]
    row$outcome_round <- x$outcome_round[[1L]]
    row$baseline_round <- x$baseline_round[[1L]]
    row$estimand <- x$estimand[[1L]]
    row$analysis_transform <- x$analysis_transform[[1L]]
    row
  })
  bind_iv_specification_rows(rows)
}

summarize_consumption_iv_outcome_coverage <- function(panel, specifications) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  specs <- safe_df(specifications)
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    spec <- specs[i, , drop = FALSE]
    needed <- iv_specification_variables(spec, include_outcome = TRUE)
    missing <- setdiff(needed, names(x))
    complete <- if (length(missing)) {
      rep(FALSE, nrow(x))
    } else {
      stats::complete.cases(x[needed])
    }
    data.frame(
      specification_id = spec$specification_id[[1L]],
      welfare_specification_id = spec$welfare_specification_id[[1L]],
      outcome_round = spec$outcome_round[[1L]],
      baseline_round = spec$baseline_round[[1L]],
      estimand = spec$estimand[[1L]],
      n_panel = nrow(x),
      n_analysis_complete = sum(complete),
      analysis_share = if (nrow(x)) mean(complete) else NA_real_,
      status = if (length(missing)) "missing_columns" else if (sum(complete) < 3L) {
        "insufficient_complete_cases"
      } else {
        "ready"
      },
      missing_columns = if (length(missing)) paste(missing, collapse = ";") else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

validate_consumption_iv_outcome_coverage <- function(coverage) {
  x <- safe_df(coverage)
  required <- c(
    "specification_id", "n_analysis_complete", "analysis_share",
    "status", "missing_columns"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption IV outcome coverage is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  bad <- x$status != "ready"
  if (any(bad)) {
    detail <- paste(
      paste0(
        x$specification_id[bad], "=", x$status[bad],
        ifelse(
          is.na(x$missing_columns[bad]) | !nzchar(x$missing_columns[bad]),
          "",
          paste0("[", x$missing_columns[bad], "]")
        )
      ),
      collapse = "; "
    )
    stop(
      "Registered consumption IV outcome specifications are not analysis-ready: ",
      detail,
      call. = FALSE
    )
  }
  x
}

save_consumption_iv_outcome_coverage <- function(
    coverage,
    path = "outputs/diagnostics/extended/consumption/consumption_iv_outcome_coverage.csv") {
  write_diagnostic_csv(safe_df(coverage), path)
}
