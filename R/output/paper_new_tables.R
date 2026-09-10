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
    label = c(
      "Enrollment", "EMI among enrolled", "All-child EMI",
      "Public EMI", "Private EMI", "DISE EMI"
    ),
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
      "Linguistic distance", "All-child EMI", "EMI among enrolled",
      "Public EMI", "Private EMI", "Enrollment"
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
    enrolled_total_denominator = "DISE EMI vs NSS EMI among enrolled",
    all_child_context = "DISE EMI vs NSS all-child EMI"
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
    paper_language_behavior_group("Panel A. National C-17"),
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

paper_conversion_cell <- function(row) {
  if (!nrow(row)) return("")
  paste0(
    sprintf("%.3f", num(row$interaction)[[1L]]),
    significance_stars(num(row$p.value_holm)[[1L]]),
    " (", sprintf("%.3f", num(row$std.error)[[1L]]), ")"
  )
}

paper_conversion_group <- function(label) {
  data.frame(
    Complement = paste0(label, ":"), `All-child EMI` = "", `Private EMI` = "",
    `Linguistic distance` = "", check.names = FALSE, stringsAsFactors = FALSE
  )
}

make_paper_conversion_complements_table <- function(conversion, it_opportunity) {
  csv <- paper_conversion_complements_csv_data(conversion, it_opportunity)
  capacity <- csv[csv$panel == "predetermined_capacity", , drop = FALSE]
  complement_order <- paper_conversion_complement_registry()
  capacity_rows <- safe_bind_rows(lapply(seq_len(nrow(complement_order)), function(i) {
    modifier <- complement_order$modifier_id[[i]]
    data.frame(
      Complement = complement_order$label[[i]],
      `All-child EMI` = paper_conversion_cell(
        capacity[capacity$modifier_id == modifier & capacity$predictor_id == "all_child_emi", , drop = FALSE]
      ),
      `Private EMI` = paper_conversion_cell(
        capacity[capacity$modifier_id == modifier & capacity$predictor_id == "private_emi", , drop = FALSE]
      ),
      `Linguistic distance` = "",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }))
  it <- csv[csv$panel == "predetermined_it_environment", , drop = FALSE]
  it_row <- data.frame(
    Complement = "Baseline IT employment share",
    `All-child EMI` = paper_conversion_cell(
      it[it$predictor_id == "all_child_emi", , drop = FALSE]
    ),
    `Private EMI` = "",
    `Linguistic distance` = paper_conversion_cell(
      it[it$predictor_id == "linguistic_distance", , drop = FALSE]
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- safe_bind_rows(list(
    paper_conversion_group("Panel A. Predetermined Census-2001 complements"),
    capacity_rows,
    paper_conversion_group("Panel B. Predetermined IT environment"),
    it_row
  ))
  attr(out, "csv_data") <- csv
  out
}
