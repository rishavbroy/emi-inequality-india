# Opt-in tables for paper-new.qmd.
#
# These exhibits may depend on non-redistributable research inputs used by the
# extended diagnostic graph. They are therefore built by scripts/render_paper_new.R
# after the relevant cached targets exist, not added to the strict public graph.

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
  unname(summary(stats::lm(y ~ factor(g)))$r.squared)
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
