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
  data.frame(
    variable = c(
      "ling_distance_nonzero_mean", "emi_exposure_all_children_0708",
      "emi_share_enrolled_0708", "emi_share_enrolled_public_0708",
      "emi_share_enrolled_private_0708", "enrollment_rate_0708"
    ),
    label = c(
      paper_linguistic_distance_display_labels()[["nonzero_mean"]],
      paper_schooling_display_labels()[["emi_all_children"]],
      paper_schooling_display_labels()[["emi_enrolled"]],
      paper_schooling_display_labels()[["public_emi"]],
      paper_schooling_display_labels()[["private_emi"]],
      paper_schooling_display_labels()[["enrollment"]]
    ),
    stringsAsFactors = FALSE
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
    district_mechanisms, nss_validation, district_panel) {
  estimates <- safe_df(district_mechanisms$estimates)
  measures <- paper_schooling_market_measure_registry()
  specs <- c("unadjusted", "region_main", "state_main")
  assoc <- lapply(seq_len(nrow(measures)), function(i) {
    id <- measures$measure_id[[i]]
    x <- estimates[
      estimates$measure_id == id & estimates$specification_id %in% specs,
      , drop = FALSE
    ]
    if (nrow(x) != 3L || !setequal(x$specification_id, specs)) {
      stop("Paper schooling-market table requires all three canonical specifications for ", id, ".", call. = FALSE)
    }
    value <- function(spec) num(x$standardized_estimate[x$specification_id == spec])[[1L]]
    n <- unique(as.integer(x$n))
    if (length(n) != 1L) stop("Schooling-market specifications changed sample size for ", id, ".", call. = FALSE)
    data.frame(
      panel = "association", measure_id = id, measure = measures$label[[i]],
      statistic = "standardized_linguistic_distance_association",
      raw = value("unadjusted"), region_controls = value("region_main"),
      state_controls_or_residual = value("state_main"), n = n,
      stringsAsFactors = FALSE
    )
  })

  state_registry <- paper_schooling_market_state_registry()
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  required_panel <- c("state_code_2001", state_registry$variable)
  missing_panel <- setdiff(required_panel, names(panel))
  if (length(missing_panel)) {
    stop("Paper schooling-market table is missing district-panel fields: ",
         paste(missing_panel, collapse = ", "), ".", call. = FALSE)
  }
  state_rows <- lapply(seq_len(nrow(state_registry)), function(i) {
    variable <- state_registry$variable[[i]]
    value <- num(panel[[variable]])
    state <- plain_chr(panel$state_code_2001)
    n <- sum(is.finite(value) & !is.na(state) & nzchar(state))
    data.frame(
      panel = "state_organization", measure_id = variable,
      measure = state_registry$label[[i]],
      statistic = "variance_explained_by_state_membership",
      raw = NA_real_, region_controls = NA_real_,
      state_controls_or_residual = paper_state_membership_r_squared(panel, variable),
      n = as.integer(n), stringsAsFactors = FALSE
    )
  })

  validation <- safe_df(nss_validation)
  expected <- c("enrolled_total_denominator", "all_child_context")
  validation <- validation[match(expected, validation$comparison), , drop = FALSE]
  if (nrow(validation) != 2L || any(is.na(validation$comparison)) ||
      any(plain_chr(validation$status) != "estimated")) {
    stop("Paper schooling-market table requires both estimated DISE-NSS validation comparisons.", call. = FALSE)
  }
  labels <- c(
    enrolled_total_denominator = "DISE vs NSS English-medium share among enrolled",
    all_child_context = "DISE vs NSS English-medium exposure among all children"
  )
  validation_rows <- data.frame(
    panel = "administrative_validation",
    measure_id = plain_chr(validation$comparison),
    measure = unname(labels[plain_chr(validation$comparison)]),
    statistic = "pearson_correlation",
    raw = num(validation$pearson), region_controls = NA_real_,
    state_controls_or_residual = num(validation$state_residual_pearson),
    n = as.integer(validation$n), stringsAsFactors = FALSE
  )

  safe_bind_rows(c(assoc, state_rows, list(validation_rows)))
}

paper_schooling_market_group <- function(label) {
  data.frame(
    Measure = paste0(label, ":"), Statistic = "", Raw = "",
    `Region + controls` = "", `State / residual` = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

paper_schooling_market_value <- function(x) {
  x <- num(x)
  ifelse(is.finite(x), sprintf("%.3f", x), "")
}

make_paper_schooling_market_geography_table <- function(
    district_mechanisms, nss_validation, district_panel) {
  csv <- paper_schooling_market_geography_csv_data(
    district_mechanisms, nss_validation, district_panel
  )
  assoc <- csv[csv$panel == "association", , drop = FALSE]
  states <- csv[csv$panel == "state_organization", , drop = FALSE]
  validation <- csv[csv$panel == "administrative_validation", , drop = FALSE]

  row <- function(measure, statistic, raw = NA_real_, region = NA_real_, state = NA_real_) {
    data.frame(
      Measure = measure, Statistic = statistic,
      Raw = paper_schooling_market_value(raw),
      `Region + controls` = paper_schooling_market_value(region),
      `State / residual` = paper_schooling_market_value(state),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  out <- safe_bind_rows(list(
    paper_schooling_market_group("Panel A. Association with linguistic distance"),
    safe_bind_rows(lapply(seq_len(nrow(assoc)), function(i) row(
      assoc$measure[[i]], "Standardized association", assoc$raw[[i]],
      assoc$region_controls[[i]], assoc$state_controls_or_residual[[i]]
    ))),
    paper_schooling_market_group("Panel B. State organization"),
    safe_bind_rows(lapply(seq_len(nrow(states)), function(i) row(
      states$measure[[i]], "Variance explained by states",
      state = states$state_controls_or_residual[[i]]
    ))),
    paper_schooling_market_group("Panel C. Independent administrative validation"),
    safe_bind_rows(lapply(seq_len(nrow(validation)), function(i) row(
      validation$measure[[i]], "DISE-NSS Pearson correlation",
      raw = validation$raw[[i]], state = validation$state_controls_or_residual[[i]]
    )))
  ))
  attr(out, "csv_data") <- csv
  out
}

paper_language_behavior_registry <- function() {
  data.frame(
    panel = c(rep("national", 4L), rep("hindi_belt", 3L)),
    specification_id = c(
      "english_linear", "english_distant", "hindi_linear", "multilingual_linear",
      "english_linear_hindi_belt", "english_distant_hindi_belt", "hindi_linear_hindi_belt"
    ),
    term = c(
      "shastry_degree", "distance_distant", "shastry_degree", "shastry_degree",
      "shastry_degree", "distance_distant", "shastry_degree"
    ),
    label = c(
      "Continuous distance -> English acquisition",
      "Distant-language contrast -> English acquisition",
      "Continuous distance -> Hindi acquisition",
      "Continuous distance -> multilingualism",
      "Continuous distance -> English acquisition",
      "Distant-language contrast -> English acquisition",
      "Continuous distance -> Hindi acquisition"
    ),
    sample_label = c(rep("National", 4L), rep("Hindi-belt states", 3L)),
    outcome_universe = c(
      rep("English among multilingual speakers", 2L),
      "Hindi among multilingual speakers",
      "Multilingual speakers among native speakers",
      rep("English among multilingual speakers", 2L),
      "Hindi among multilingual speakers"
    ),
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
      specification_id = spec,
      result = registry$label[[i]],
      term = term,
      estimate = num(coefficient$estimate)[[1L]],
      std.error = num(coefficient$std.error)[[1L]],
      p.value = num(coefficient$p.value)[[1L]],
      partial_r_squared = num(coefficient$partial_r_squared)[[1L]],
      n = as.integer(model$n[[1L]]),
      sample = registry$sample_label[[i]],
      outcome_universe = registry$outcome_universe[[i]],
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(rows)
}

paper_language_behavior_group <- function(label) {
  data.frame(
    Result = paste0(label, ":"), Estimate = "", SE = "", `p-value` = "",
    `Partial R2` = "", N = "", Sample = "", `Outcome / universe` = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

make_paper_language_behavior_table <- function(c17_mechanism) {
  csv <- paper_language_behavior_csv_data(c17_mechanism)
  display_rows <- function(x) {
    data.frame(
      Result = x$result,
      Estimate = sprintf("%.3f", x$estimate),
      SE = sprintf("%.3f", x$std.error),
      `p-value` = sprintf("%.3f", x$p.value),
      `Partial R2` = sprintf("%.3f", x$partial_r_squared),
      N = format(x$n, big.mark = ",", scientific = FALSE),
      Sample = x$sample,
      `Outcome / universe` = x$outcome_universe,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  national <- csv[csv$panel == "national", , drop = FALSE]
  hindi_belt <- csv[csv$panel == "hindi_belt", , drop = FALSE]
  out <- safe_bind_rows(list(
    paper_language_behavior_group("Panel A. National"),
    display_rows(national),
    paper_language_behavior_group("Panel B. Contextual limits"),
    display_rows(hindi_belt)
  ))
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

paper_conversion_complements_csv_data <- function(conversion, it_opportunity) {
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
  rows$panel <- "predetermined_capacity"
  rows$predictor_id <- ifelse(
    rows$treatment_id == "emi_all_children", "all_child_emi", "private_emi"
  )
  rows$complement <- rows$label
  rows$interaction <- num(rows$interaction_per_10pp_schooling_per_modifier_sd)
  rows$std.error <- num(rows$interaction_std_error_state_clustered)
  rows$p.value <- num(rows$interaction_p_value_state_clustered)
  rows$p.value_holm <- num(rows$interaction_p_value_holm_family)
  rows$n <- as.integer(rows$n)
  schooling <- rows[c(
    "panel", "modifier_id", "complement", "predictor_id", "interaction",
    "std.error", "p.value", "p.value_holm", "n"
  )]

  it <- safe_df(it_opportunity$estimates)
  it <- it[match(c("schooling_exposure", "linguistic_opportunity"), it$predictor_id), , drop = FALSE]
  if (nrow(it) != 2L || any(is.na(it$predictor_id)) ||
      any(is.na(it$status) | plain_chr(it$status) != "estimated") ||
      any(!is.finite(num(it$interaction_per_predictor_scale_per_modifier_sd)))) {
    stop("Paper conversion-complements table requires both estimated EC05 IT interactions.", call. = FALSE)
  }
  it_rows <- data.frame(
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
  safe_bind_rows(list(schooling, it_rows))
}

paper_economic_conversion_csv_data <- function(bridge, conversion, it_opportunity) {
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

  complements <- paper_conversion_complements_csv_data(conversion, it_opportunity)
  complement_label <- ifelse(
    complements$predictor_id == "all_child_emi", "All-child EMI",
    ifelse(complements$predictor_id == "private_emi", "Private EMI", "Linguistic distance")
  )
  predictor_scale <- ifelse(complements$predictor_id == "linguistic_distance", "one distance degree", "10pp schooling")
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
    unit = paste0("percent real mean MPCE per ", predictor_scale, " per 1 SD complement"),
    stringsAsFactors = FALSE
  )

  out <- safe_bind_rows(list(welfare_out, complement_out))
  if (nrow(out) != 28L || anyDuplicated(out$result_id)) {
    stop("Paper economic-conversion table must contain 20 welfare cells and eight complement cells.", call. = FALSE)
  }
  out
}

paper_economic_conversion_group <- function(label) {
  data.frame(
    `Schooling margin / interaction` = paste0(label, ":"),
    `2022 ANCOVA` = "", `2004-2022 change` = "", `2023 ANCOVA` = "",
    `2004-2023 change` = "", check.names = FALSE, stringsAsFactors = FALSE
  )
}

paper_economic_conversion_cell <- function(estimate, std.error, p.value_holm, digits = 2L) {
  paste0(
    sprintf(paste0("%.", digits, "f"), estimate), significance_stars(p.value_holm),
    " (", sprintf(paste0("%.", digits, "f"), std.error), ")"
  )
}

make_paper_economic_conversion_table <- function(bridge, conversion, it_opportunity) {
  csv <- paper_economic_conversion_csv_data(bridge, conversion, it_opportunity)
  columns <- paper_schooling_welfare_column_registry()
  treatments <- paper_schooling_welfare_treatment_labels()
  welfare <- csv[csv$panel == "schooling_welfare", , drop = FALSE]

  welfare_rows <- safe_bind_rows(lapply(names(treatments), function(id) {
    row <- data.frame(`Schooling margin / interaction` = unname(treatments[[id]]), check.names = FALSE)
    for (j in seq_len(nrow(columns))) {
      hit <- welfare$predictor_id == id & welfare$outcome_round == columns$outcome_round[[j]] &
        welfare$estimand == columns$estimand[[j]]
      x <- welfare[hit, , drop = FALSE]
      if (nrow(x) != 1L) stop("Paper economic-conversion table lost a registered welfare cell.", call. = FALSE)
      row[[columns$column[[j]]]] <- paper_economic_conversion_cell(
        x$estimate[[1L]], x$std.error[[1L]], x$p.value_holm[[1L]]
      )
    }
    row
  }))
  n_row <- data.frame(`Schooling margin / interaction` = "Observations", check.names = FALSE)
  for (j in seq_len(nrow(columns))) {
    hit <- welfare$outcome_round == columns$outcome_round[[j]] & welfare$estimand == columns$estimand[[j]]
    nvals <- unique(welfare$n[hit])
    if (length(nvals) != 1L) stop("Paper economic-conversion table lost common welfare support.", call. = FALSE)
    n_row[[columns$column[[j]]]] <- format(nvals[[1L]], big.mark = ",", scientific = FALSE)
  }

  complements <- csv[csv$panel == "predetermined_complements", , drop = FALSE]
  complement_rows <- safe_bind_rows(lapply(seq_len(nrow(complements)), function(i) {
    x <- complements[i, , drop = FALSE]
    row <- data.frame(`Schooling margin / interaction` = x$measure[[1L]], check.names = FALSE)
    for (label in columns$column) row[[label]] <- ""
    row[["2004-2022 change"]] <- paper_economic_conversion_cell(
      x$estimate[[1L]], x$std.error[[1L]], x$p.value_holm[[1L]]
    )
    row
  }))

  out <- safe_bind_rows(list(
    paper_economic_conversion_group("Panel A. Schooling and later welfare"),
    welfare_rows,
    n_row,
    paper_economic_conversion_group("Panel B. Predetermined complements"),
    complement_rows
  ))
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

paper_local_development_value <- function(x) {
  if (!is.finite(x)) return("")
  sprintf("%+.4f", x)
}

paper_local_development_p_value <- function(x) {
  if (!is.finite(x)) return("")
  if (x < 0.001) "<0.001" else sprintf("%.3f", x)
}

make_paper_local_development_table <- function(
    household_capacity, migration, housing, economic_census,
    nss66_labor, plfs_2017_18_labor) {
  csv <- paper_local_development_csv_data(
    household_capacity, migration, housing, economic_census,
    nss66_labor, plfs_2017_18_labor
  )
  out <- data.frame(
    Domain = csv$domain,
    `Representative outcome` = csv$outcome,
    Estimate = vapply(csv$estimate, paper_local_development_value, character(1)),
    `Raw p` = vapply(csv$p.value, paper_local_development_p_value, character(1)),
    `Holm p` = vapply(csv$p.value_holm, paper_local_development_p_value, character(1)),
    Interpretation = csv$interpretation,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

paper_identification_distance_registry <- function() {
  data.frame(
    construction_id = c(
      "nonzero_mean", "top3_legacy", "glottolog_mean",
      "dyen_noncognate", "distant_share", "distance_shares_all"
    ),
    label = unname(paper_linguistic_distance_display_labels()[c(
      "nonzero_mean", "top3_legacy", "glottolog_mean",
      "dyen_noncognate", "distant_share", "distance_shares_all"
    )]),
    interpretation = c(
      "Weak within states", "No scalar rescue", "No scalar rescue",
      "No scalar rescue", "No scalar rescue", "Raw joint prediction does not survive state absorption"
    ),
    stringsAsFactors = FALSE
  )
}

paper_identification_boundary_csv_data <- function(
    alternative_first_stages, consumption_dynamics) {
  if (!inherits(alternative_first_stages, "emi_alternative_distance_first_stages")) {
    stop("Paper identification table requires alternative-distance first-stage diagnostics.", call. = FALSE)
  }
  first_stage <- safe_df(alternative_first_stages$summary)
  registry <- paper_identification_distance_registry()
  adjustments <- c("unadjusted", "state_main")

  relevance <- lapply(seq_len(nrow(registry)), function(i) {
    construction <- registry$construction_id[[i]]
    x <- first_stage[
      first_stage$construction_id == construction & first_stage$adjustment_id %in% adjustments,
      , drop = FALSE
    ]
    if (nrow(x) != 2L || !setequal(plain_chr(x$adjustment_id), adjustments)) {
      stop(
        "Paper identification table requires unadjusted and state-main first stages for ",
        construction, ".", call. = FALSE
      )
    }
    get <- function(adjustment, column) {
      value <- x[x$adjustment_id == adjustment, column, drop = TRUE]
      if (length(value) != 1L) NA_real_ else num(value)[[1L]]
    }
    n <- unique(as.integer(x$n))
    if (length(n) != 1L) {
      stop("Identification first-stage comparison changed sample size for ", construction, ".", call. = FALSE)
    }
    data.frame(
      panel = "relevance_robustness",
      row_id = construction,
      result = registry$label[[i]],
      estimate = NA_real_, std.error = NA_real_, p.value = NA_real_,
      raw_f = get("unadjusted", "joint_excluded_f"),
      state_f = get("state_main", "joint_excluded_f"),
      effective_f = NA_real_,
      within_state_partial_r_squared = get("state_main", "partial_r_squared"),
      ar_p_beta0 = NA_real_, ar_n_components = NA_integer_,
      ar_disconnected = NA, ar_contains_zero = NA, ar_sign_identified = NA,
      ar_components = NA_character_,
      interpretation = registry$interpretation[[i]],
      n = n,
      stringsAsFactors = FALSE
    )
  })

  if (!is.list(consumption_dynamics) ||
      !all(c("summary", "anderson_rubin_grid") %in% names(consumption_dynamics))) {
    stop(
      "Paper identification table requires canonical consumption-IV dynamics outputs.",
      call. = FALSE
    )
  }
  dynamics <- safe_df(consumption_dynamics$summary)
  wanted <- c("long_2022__change", "long_2023__change")
  weak_iv <- dynamics[match(wanted, dynamics$welfare_specification_id), , drop = FALSE]
  if (nrow(weak_iv) != 2L || any(is.na(weak_iv$welfare_specification_id)) ||
      any(plain_chr(weak_iv$status) != "estimated") ||
      any(!is.finite(num(weak_iv$second_stage_estimate))) ||
      any(!is.finite(num(weak_iv$effective_f)))) {
    stop("Paper identification table requires both estimated long-run weak-IV rows.", call. = FALSE)
  }
  labels <- c(
    long_2022__change = "2022 long-change 2SLS",
    long_2023__change = "2023 long-change 2SLS"
  )
  outcome_rows <- data.frame(
    panel = "weak_iv_outcomes",
    row_id = plain_chr(weak_iv$welfare_specification_id),
    result = unname(labels[plain_chr(weak_iv$welfare_specification_id)]),
    estimate = num(weak_iv$second_stage_estimate),
    std.error = num(weak_iv$second_stage_std.error),
    p.value = num(weak_iv$second_stage_p.value),
    raw_f = NA_real_,
    state_f = NA_real_,
    effective_f = num(weak_iv$effective_f),
    within_state_partial_r_squared = NA_real_,
    ar_p_beta0 = num(weak_iv$anderson_rubin_p_beta0),
    ar_n_components = as.integer(weak_iv$ar_95_n_components),
    ar_disconnected = as.logical(weak_iv$ar_95_disconnected),
    ar_contains_zero = as.logical(weak_iv$ar_95_contains_zero),
    ar_sign_identified = as.logical(weak_iv$ar_95_sign_identified),
    ar_components = plain_chr(weak_iv$ar_95_components),
    interpretation = ifelse(
      as.logical(weak_iv$ar_95_disconnected) & !as.logical(weak_iv$ar_95_sign_identified),
      "Disconnected AR set spans both signs",
      "Weak-IV inference remains uninformative"
    ),
    n = as.integer(weak_iv$n),
    stringsAsFactors = FALSE
  )

  out <- safe_bind_rows(c(relevance, list(outcome_rows)))
  required_numeric <- c(
    "raw_f", "state_f", "within_state_partial_r_squared",
    "estimate", "std.error", "effective_f"
  )
  relevance_rows <- out$panel == "relevance_robustness"
  outcome_rows_idx <- out$panel == "weak_iv_outcomes"
  if (any(!is.finite(as.matrix(out[relevance_rows, c(
    "raw_f", "state_f", "within_state_partial_r_squared"
  ), drop = FALSE]))) ||
      any(!is.finite(as.matrix(out[outcome_rows_idx, c(
        "estimate", "std.error", "effective_f"
      ), drop = FALSE])))) {
    stop("Paper identification table requires finite registered diagnostics.", call. = FALSE)
  }
  if (!all(vapply(out[required_numeric], is.numeric, logical(1)))) {
    stop("Paper identification table diagnostics must remain numeric.", call. = FALSE)
  }
  out
}

paper_identification_group <- function(label) {
  data.frame(
    `Design / result` = paste0(label, ":"),
    `2SLS estimate (SE)` = "", `Raw F` = "", `State F` = "", `Effective F` = "",
    `What it shows` = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

paper_identification_ar_summary <- function(row) {
  p0 <- num(row$ar_p_beta0)[[1L]]
  if (isTRUE(row$ar_disconnected[[1L]]) && !isTRUE(row$ar_sign_identified[[1L]])) {
    return(paste0("AR 95% set disconnected across signs; p(0)=", sprintf("%.3f", p0)))
  }
  if (isTRUE(row$ar_contains_zero[[1L]])) {
    return(paste0("AR 95% set includes zero; p(0)=", sprintf("%.3f", p0)))
  }
  paste0("AR p(0)=", sprintf("%.3f", p0))
}

make_paper_identification_boundary_table <- function(
    alternative_first_stages, consumption_dynamics) {
  csv <- paper_identification_boundary_csv_data(
    alternative_first_stages, consumption_dynamics
  )
  relevance <- csv[csv$panel == "relevance_robustness", , drop = FALSE]
  outcomes <- csv[csv$panel == "weak_iv_outcomes", , drop = FALSE]

  relevance_display <- data.frame(
    `Design / result` = relevance$result,
    `2SLS estimate (SE)` = "",
    `Raw F` = sprintf("%.2f", relevance$raw_f),
    `State F` = sprintf("%.2f", relevance$state_f),
    `Effective F` = "",
    `What it shows` = paste0(
      relevance$interpretation,
      "; partial R2=", sprintf("%.3f", relevance$within_state_partial_r_squared)
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  outcomes_display <- data.frame(
    `Design / result` = outcomes$result,
    `2SLS estimate (SE)` = paste0(
      sprintf("%.3f", outcomes$estimate), " (", sprintf("%.3f", outcomes$std.error), ")"
    ),
    `Raw F` = "",
    `State F` = "",
    `Effective F` = sprintf("%.2f", outcomes$effective_f),
    `What it shows` = vapply(
      seq_len(nrow(outcomes)),
      function(i) paper_identification_ar_summary(outcomes[i, , drop = FALSE]),
      character(1)
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- safe_bind_rows(list(
    paper_identification_group("Panel A. Relevance across language constructions"),
    relevance_display,
    paper_identification_group("Panel B. Long-run weak-IV inference"),
    outcomes_display
  ))
  attr(out, "csv_data") <- csv
  out
}
