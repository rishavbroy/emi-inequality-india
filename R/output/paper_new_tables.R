# Main-paper tables unique to paper-new.qmd.
#
# Statistical objects are computed upstream in the target graph. This module only
# reshapes publication evidence and keeps machine-readable CSV payloads separate
# from presentation-oriented table cells.

paper_schooling_market_measure_registry <- function() {
  data.frame(
    measure_id = c(
      "nss_enrollment", "nss_emi_enrolled", "nss_emie_all_children",
      "nss_emi_public", "nss_emi_private", "dise_emi_enrollment"
    ),
    label = unname(paper_schooling_display_labels()[c(
      "enrollment", "emi_enrolled", "emi_all_children",
      "public_emi", "private_emi", "dise_emi"
    )]),
    stringsAsFactors = FALSE
  )
}

paper_schooling_market_state_registry <- function() {
  schooling <- paper_schooling_market_measure_registry()
  schooling$variable <- unname(c(
    nss_enrollment = "enrollment_rate_0708",
    nss_emi_enrolled = "emi_share_enrolled_0708",
    nss_emie_all_children = "emi_exposure_all_children_0708",
    nss_emi_public = "emi_share_enrolled_public_0708",
    nss_emi_private = "emi_share_enrolled_private_0708",
    dise_emi_enrollment = "dise_emi_enrollment_share_total_0708"
  )[schooling$measure_id])

  rbind(
    data.frame(
      measure_id = "ling_distance_nonzero_mean",
      variable = "ling_distance_nonzero_mean",
      label = paper_linguistic_distance_display_labels()[["nonzero_mean"]],
      stringsAsFactors = FALSE
    ),
    schooling[c("measure_id", "variable", "label")]
  )
}

paper_state_membership_r_squared <- function(data, variable, state = "state_code_2001") {
  x <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else safe_df(data)
  if (!all(c(variable, state) %in% names(x))) return(NA_real_)
  y <- num(x[[variable]])
  g <- plain_chr(x[[state]])
  keep <- is.finite(y) & !is.na(g) & nzchar(g)
  y <- y[keep]
  g <- g[keep]
  if (length(y) < 2L || length(unique(g)) < 2L || !is.finite(stats::var(y)) || stats::var(y) == 0) {
    return(NA_real_)
  }
  fitted <- ave(y, g, FUN = mean)
  total_ss <- sum((y - mean(y))^2)
  residual_ss <- sum((y - fitted)^2)
  unname(1 - residual_ss / total_ss)
}

paper_schooling_market_geography_csv_data <- function(
    district_mechanisms, district_panel) {
  estimates <- safe_df(district_mechanisms$estimates)
  measures <- paper_schooling_market_measure_registry()
  specs <- c("unadjusted", "region_main", "state_main")
  required_estimate_fields <- c(
    "measure_id", "specification_id", "standardized_estimate",
    "standardized_std_error", "p.value", "n"
  )
  missing_estimate_fields <- setdiff(required_estimate_fields, names(estimates))
  if (length(missing_estimate_fields)) {
    stop(
      "Paper schooling-market table is missing regression fields: ",
      paste(missing_estimate_fields, collapse = ", "), ".", call. = FALSE
    )
  }

  assoc <- safe_bind_rows(lapply(seq_len(nrow(measures)), function(i) {
    id <- measures$measure_id[[i]]
    x <- estimates[
      estimates$measure_id == id & estimates$specification_id %in% specs,
      , drop = FALSE
    ]
    if (nrow(x) != 3L || !setequal(x$specification_id, specs)) {
      stop("Paper schooling-market table requires all three registered specifications for ", id, ".", call. = FALSE)
    }
    x <- x[match(specs, x$specification_id), , drop = FALSE]
    n <- unique(as.integer(x$n))
    if (length(n) != 1L) stop("Schooling-market specifications changed sample size for ", id, ".", call. = FALSE)
    data.frame(
      panel = "association",
      measure_id = id,
      measure = measures$label[[i]],
      specification_id = specs,
      statistic = "standardized_coefficient",
      estimate = num(x$standardized_estimate),
      std_error = num(x$standardized_std_error),
      p_value = num(x$p.value),
      n = n,
      stringsAsFactors = FALSE
    )
  }))

  state_registry <- paper_schooling_market_state_registry()
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  required_panel <- c("state_code_2001", state_registry$variable)
  missing_panel <- setdiff(required_panel, names(panel))
  if (length(missing_panel)) {
    stop("Paper schooling-market table is missing district-panel fields: ",
         paste(missing_panel, collapse = ", "), ".", call. = FALSE)
  }
  state_rows <- safe_bind_rows(lapply(seq_len(nrow(state_registry)), function(i) {
    variable <- state_registry$variable[[i]]
    value <- num(panel[[variable]])
    state <- plain_chr(panel$state_code_2001)
    n <- sum(is.finite(value) & !is.na(state) & nzchar(state))
    data.frame(
      panel = "state_organization",
      measure_id = state_registry$measure_id[[i]],
      measure = state_registry$label[[i]],
      specification_id = "state_membership",
      statistic = "r_squared",
      estimate = paper_state_membership_r_squared(panel, variable),
      std_error = NA_real_, p_value = NA_real_, n = as.integer(n),
      stringsAsFactors = FALSE
    )
  }))

  safe_bind_rows(list(assoc, state_rows))
}

make_paper_schooling_market_geography_table <- function(
    district_mechanisms, district_panel) {
  csv <- paper_schooling_market_geography_csv_data(
    district_mechanisms, district_panel
  )
  out <- data.frame(
    Term = "Linguistic distance from Hindi",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

paper_language_behavior_registry <- function() {
  data.frame(
    panel = c(rep("national", 4L), rep("hindi_belt", 3L)),
    model_number = seq_len(7L),
    specification_id = c(
      "english_linear", "english_distant", "hindi_linear", "multilingual_linear",
      "english_linear_hindi_belt", "english_distant_hindi_belt", "hindi_linear_hindi_belt"
    ),
    term = c(
      "shastry_degree", "distance_distant", "shastry_degree", "shastry_degree",
      "shastry_degree", "distance_distant", "shastry_degree"
    ),
    regressor = c(
      "Linguistic distance from Hindi", "Distant-language indicator",
      "Linguistic distance from Hindi", "Linguistic distance from Hindi",
      "Linguistic distance from Hindi", "Distant-language indicator",
      "Linguistic distance from Hindi"
    ),
    outcome = c(
      "Reported English", "Reported English", "Reported Hindi", "Multilingualism",
      "Reported English", "Reported English", "Reported Hindi"
    ),
    population = c(
      rep("Multilingual speakers", 3L), "Native speakers",
      rep("Multilingual speakers", 3L)
    ),
    sample = c(rep("All states", 4L), rep("Hindi-belt states", 3L)),
    stringsAsFactors = FALSE
  )
}

paper_language_behavior_csv_data <- function(c17_mechanism) {
  registry <- paper_language_behavior_registry()
  coefficients <- safe_df(c17_mechanism$coefficients)
  summaries <- safe_df(c17_mechanism$model_summary)

  rows <- lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry$specification_id[[i]]
    term <- registry$term[[i]]
    coefficient <- coefficients[
      coefficients$specification_id == spec & coefficients$term == term,
      , drop = FALSE
    ]
    model <- summaries[summaries$specification_id == spec, , drop = FALSE]
    if (nrow(coefficient) != 1L || nrow(model) != 1L ||
        !identical(plain_chr(coefficient$status), "estimated") ||
        !identical(plain_chr(model$status), "estimated")) {
      stop("Paper language-behavior table requires one estimated row for ", spec,
           " / ", term, ".", call. = FALSE)
    }
    data.frame(
      panel = registry$panel[[i]],
      model_number = registry$model_number[[i]],
      specification_id = spec,
      term = term,
      regressor = registry$regressor[[i]],
      outcome = registry$outcome[[i]],
      population = registry$population[[i]],
      sample = registry$sample[[i]],
      estimate = num(coefficient$estimate)[[1L]],
      std.error = num(coefficient$std.error)[[1L]],
      p.value = num(coefficient$p.value)[[1L]],
      partial_r_squared = num(coefficient$partial_r_squared)[[1L]],
      n = as.integer(model$n[[1L]]),
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(rows)
}

make_paper_language_behavior_table <- function(c17_mechanism) {
  csv <- paper_language_behavior_csv_data(c17_mechanism)
  # The machine-readable CSV remains long-form. The LaTeX writer reshapes these
  # registered estimates into the standard economics layout with specifications
  # in columns, coefficients over parenthesized standard errors, and GOF rows.
  out <- data.frame(
    Term = unique(csv$regressor),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

paper_conversion_complement_registry <- function() {
  data.frame(
    modifier_id = c("baseline_human_capital", "urbanization", "st_concentration"),
    label = c("Baseline human capital", "Urbanization", "ST concentration"),
    stringsAsFactors = FALSE
  )
}

paper_conversion_complement_csv_data <- function(conversion) {
  conversion_estimates <- safe_df(conversion$estimates)
  complements <- paper_conversion_complement_registry()
  treatments <- c("emi_all_children", "private_emi_all_children")
  expected <- merge(
    complements[c("modifier_id", "label")],
    data.frame(treatment_id = treatments, stringsAsFactors = FALSE),
    by = NULL
  )
  rows <- merge(
    expected,
    conversion_estimates,
    by = c("modifier_id", "treatment_id"),
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(rows) != 6L || any(is.na(rows$status) | plain_chr(rows$status) != "estimated") ||
      any(!is.finite(num(rows$interaction_per_10pp_schooling_per_modifier_sd)))) {
    stop("Paper conversion-complements table requires all six estimated schooling interactions.", call. = FALSE)
  }
  rows$panel <- "predetermined_complements"
  rows$predictor_id <- ifelse(
    rows$treatment_id == "emi_all_children", "all_child_emi", "private_emi"
  )
  rows$complement <- rows$label
  rows$interaction <- num(rows$interaction_per_10pp_schooling_per_modifier_sd)
  rows$std.error <- num(rows$interaction_std_error_state_clustered)
  rows$p.value <- num(rows$interaction_p_value_state_clustered)
  rows$p.value_holm <- num(rows$interaction_p_value_holm_family)
  rows$n <- as.integer(rows$n)
  rows[c(
    "panel", "modifier_id", "complement", "predictor_id", "interaction",
    "std.error", "p.value", "p.value_holm", "n"
  )]
}

paper_conversion_it_csv_data <- function(it_opportunity) {
  it <- safe_df(it_opportunity$estimates)
  it <- it[match(c("schooling_exposure", "linguistic_opportunity"), it$predictor_id), , drop = FALSE]
  if (nrow(it) != 2L || any(is.na(it$predictor_id)) ||
      any(is.na(it$status) | plain_chr(it$status) != "estimated") ||
      any(!is.finite(num(it$interaction_per_predictor_scale_per_modifier_sd)))) {
    stop("Paper conversion-complements table requires both estimated EC05 IT interactions.", call. = FALSE)
  }
  data.frame(
    panel = "predetermined_it_environment",
    modifier_id = "ec05_it_employment_share",
    complement = "Baseline IT employment share",
    predictor_id = ifelse(
      it$predictor_id == "schooling_exposure", "all_child_emi", "linguistic_distance"
    ),
    interaction = num(it$interaction_per_predictor_scale_per_modifier_sd),
    std.error = num(it$interaction_std_error_clustered),
    p.value = num(it$interaction_p_value_clustered),
    p.value_holm = num(it$interaction_p_value_holm_family),
    n = as.integer(it$n),
    stringsAsFactors = FALSE
  )
}

paper_conversion_complements_csv_data <- function(conversion, it_opportunity) {
  safe_bind_rows(list(
    paper_conversion_complement_csv_data(conversion),
    paper_conversion_it_csv_data(it_opportunity)
  ))
}

paper_economic_conversion_csv_data <- function(bridge, conversion) {
  welfare <- paper_schooling_welfare_csv_data(safe_df(bridge$estimates %||% data.frame()))
  if (!nrow(welfare)) {
    stop("Paper economic-conversion table requires the registered schooling-welfare evidence.", call. = FALSE)
  }
  welfare_out <- data.frame(
    panel = "schooling_welfare",
    result_id = paste(welfare$treatment_id, welfare$outcome_round, welfare$estimand, sep = "__"),
    measure = welfare$schooling_margin,
    predictor_id = welfare$treatment_id,
    complement_id = NA_character_,
    outcome_round = welfare$outcome_round,
    estimand = welfare$estimand,
    estimate = num(welfare$estimate_percent_per_10pp),
    std.error = num(welfare$std_error_percent_per_10pp),
    p.value = num(welfare$p_value_state_clustered),
    p.value_holm = num(welfare$p_value_holm_welfare),
    n = as.integer(welfare$n),
    unit = "percent real mean MPCE per 10pp schooling",
    stringsAsFactors = FALSE
  )

  complements <- paper_conversion_complement_csv_data(conversion)
  complement_label <- ifelse(
    complements$predictor_id == "all_child_emi", "All-child EMI", "Private EMI"
  )
  complement_out <- data.frame(
    panel = "predetermined_complements",
    result_id = paste(complements$predictor_id, complements$modifier_id, sep = "__"),
    measure = paste(complement_label, "x", complements$complement),
    predictor_id = complements$predictor_id,
    complement_id = complements$modifier_id,
    outcome_round = "hces_2022_23",
    estimand = "change",
    estimate = 100 * num(complements$interaction),
    std.error = 100 * num(complements$std.error),
    p.value = num(complements$p.value),
    p.value_holm = num(complements$p.value_holm),
    n = as.integer(complements$n),
    unit = "percent real mean MPCE per 10pp schooling per 1 SD complement",
    stringsAsFactors = FALSE
  )

  out <- safe_bind_rows(list(welfare_out, complement_out))
  if (nrow(out) != 26L || anyDuplicated(out$result_id)) {
    stop("Paper economic-conversion table must contain 20 welfare cells and six complement cells.", call. = FALSE)
  }
  out
}

make_paper_economic_conversion_table <- function(bridge, conversion) {
  csv <- paper_economic_conversion_csv_data(bridge, conversion)
  out <- data.frame(
    Term = unique(csv$measure),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

paper_local_development_registry <- function() {
  # Keep one conventional durable (television) rather than selecting the strongest
  # asset coefficient ex post; banking is represented separately as finance.
  data.frame(
    row_id = c(
      "household_literacy_depth", "household_graduate_access",
      "migration_skilled_recent_work", "migration_interstate_composition",
      "finance_banking_access", "asset_television_ownership",
      "economic_services_share", "economic_manufacturing_share",
      "economic_nonfarm_employment", "labor_lfpr", "labor_employment_rate"
    ),
    source_id = c(
      rep("household_capacity", 2L), rep("migration", 2L), rep("housing", 2L),
      rep("economic_census", 3L), rep("plfs_2017_18", 2L)
    ),
    outcome_id = c(
      "literacy_depth", "graduate_access",
      "skilled_recent_work_migration", "interstate_migrant_composition",
      "banking_access_change", "television_access_change",
      "services_employment_share_change", "manufacturing_employment_share_change",
      "nonfarm_employment_growth",
      "labor_force_participation_age15plus", "employment_rate_age15plus"
    ),
    adjustment_id = c(
      rep("state_main", 6L), rep("region_main", 3L), rep("state_main", 2L)
    ),
    domain = c(
      "Household capacity", "Household capacity", "Migration", "Migration",
      "Finance", "Assets", "Economic structure", "Economic structure",
      "Scale", "Labor", "Labor"
    ),
    label = c(
      "Households with 2+ literates (2001-11)",
      "Households with graduate access (2001-11)",
      "Skilled share of recent work migrants (2011)",
      "Interstate share among migrants (2011)",
      "Household banking access (2001-11)",
      "Household television ownership (2001-11)",
      "Services employment share (2005-13)",
      "Manufacturing employment share (2005-13)",
      "Log nonfarm employment (2005-13)",
      "Labor-force participation, age 15+ (2017-18)",
      "Employment rate, age 15+ (2017-18)"
    ),
    signal_interpretation = c(
      "Human-capital capacity higher", "Graduate access higher", "Skill sorting",
      "Interstate composition shifts", "Financial inclusion higher",
      "Durable ownership higher", "Services share higher", "Manufacturing share lower",
      "Broad employment changes", "Labor-force participation shifts", "Employment shifts"
    ),
    null_interpretation = c(
      "No adjusted capacity signal", "No adjusted graduate-access signal",
      "No adjusted skill-sorting signal", "No interstate-composition signal",
      "No adjusted banking signal", "No adjusted durable signal",
      "No adjusted services signal", "No adjusted manufacturing signal",
      "No broad employment boom", "No broad labor signal", "No broad labor signal"
    ),
    stringsAsFactors = FALSE
  )
}

paper_local_development_shared_row <- function(x, outcome_id, adjustment_id) {
  reduced <- safe_df(extract_posttreatment_mechanism_result(x)$reduced_form)
  row <- reduced[
    plain_chr(reduced$outcome_id) == outcome_id &
      plain_chr(reduced$adjustment_id) == adjustment_id &
      plain_chr(reduced$construction_id) == "nonzero_mean",
    , drop = FALSE
  ]
  if (nrow(row) != 1L || !identical(plain_chr(row$status), "estimated")) {
    stop(
      "Paper local-development table requires one estimated ", adjustment_id,
      " / nonzero_mean row for ", outcome_id, ".", call. = FALSE
    )
  }
  row
}

paper_local_development_csv_data <- function(
    household_capacity, migration, housing, economic_census,
    nss66_labor, plfs_2017_18_labor) {
  registry <- paper_local_development_registry()
  source_objects <- list(
    migration = migration,
    housing = housing,
    economic_census = economic_census,
    plfs_2017_18 = plfs_2017_18_labor
  )

  household <- safe_df(household_capacity$estimates)
  rows <- lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    if (spec$source_id[[1L]] == "household_capacity") {
      row <- household[
        plain_chr(household$predictor_id) == "linguistic_opportunity" &
          plain_chr(household$outcome_id) == spec$outcome_id[[1L]],
        , drop = FALSE
      ]
      if (nrow(row) != 1L || !identical(plain_chr(row$status), "estimated")) {
        stop(
          "Paper local-development table requires one estimated household-capacity row for ",
          spec$outcome_id[[1L]], ".", call. = FALSE
        )
      }
      estimate <- num(row$estimate)[[1L]]
      std_error <- num(row$std_error_state_clustered)[[1L]]
      p_value <- num(row$p_value_state_clustered)[[1L]]
      p_holm <- num(row$p_value_holm_predictor_family)[[1L]]
      n <- as.integer(row$n[[1L]])
    } else {
      row <- paper_local_development_shared_row(
        source_objects[[spec$source_id[[1L]]]],
        spec$outcome_id[[1L]], spec$adjustment_id[[1L]]
      )
      estimate <- num(row$estimate)[[1L]]
      std_error <- num(row$std.error)[[1L]]
      p_value <- num(row$p.value)[[1L]]
      p_holm <- num(row$p_holm_within_spec)[[1L]]
      n <- as.integer(row$n[[1L]])
    }
    if (!all(is.finite(c(estimate, std_error, p_value, p_holm, n)))) {
      stop("Paper local-development table contains non-finite registered evidence.", call. = FALSE)
    }
    data.frame(
      row_id = spec$row_id[[1L]], domain = spec$domain[[1L]],
      outcome = spec$label[[1L]], source_id = spec$source_id[[1L]],
      outcome_id = spec$outcome_id[[1L]], adjustment_id = spec$adjustment_id[[1L]],
      construction_id = "nonzero_mean",
      estimate = estimate, std.error = std_error,
      p.value = p_value, p.value_holm = p_holm, n = n,
      interpretation = if (p_holm < 0.05) {
        spec$signal_interpretation[[1L]]
      } else {
        spec$null_interpretation[[1L]]
      },
      stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(rows)

  # Labor rows display the later PLFS endpoint. NSS66 remains a required
  # publication dependency so the paper's broad-labor null is not inferred
  # from one survey wave only. Persist its matched adjusted p-values in the
  # semantic CSV without crowding the printed synthesis table.
  nss66 <- lapply(
    c("labor_force_participation_age15plus", "employment_rate_age15plus"),
    function(outcome) paper_local_development_shared_row(
      nss66_labor, outcome, "state_main"
    )
  )
  nss66 <- safe_bind_rows(nss66)
  nss66_p <- stats::setNames(
    num(nss66$p_holm_within_spec), plain_chr(nss66$outcome_id)
  )
  out$nss66_p_value_holm <- NA_real_
  labor <- out$source_id == "plfs_2017_18"
  out$nss66_p_value_holm[labor] <- unname(nss66_p[out$outcome_id[labor]])
  out
}

make_paper_local_development_table <- function(
    household_capacity, migration, housing, economic_census,
    nss66_labor, plfs_2017_18_labor) {
  csv <- paper_local_development_csv_data(
    household_capacity, migration, housing, economic_census,
    nss66_labor, plfs_2017_18_labor
  )
  out <- data.frame(
    Term = "Linguistic distance",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}
