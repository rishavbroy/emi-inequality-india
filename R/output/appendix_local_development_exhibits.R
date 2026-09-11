# Final-paper Appendix D: extended local-development outcomes.
#
# This module only reshapes canonical diagnostic objects into publication exhibits.
# Estimation, sample construction, multiplicity adjustment, and geography remain
# owned by their domain modules.

appendix_d_mechanism_rows <- function(x, source_label, expected_rows = NULL) {
  result <- extract_posttreatment_mechanism_result(x)
  reduced <- safe_df(result$reduced_form)
  required <- c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect", "term",
    "estimate", "std.error", "p.value", "p_holm_within_spec", "n", "status"
  )
  missing <- setdiff(required, names(reduced))
  if (length(missing)) {
    stop("Appendix D mechanism table is missing fields: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(expected_rows) && nrow(reduced) != expected_rows) {
    stop("Appendix D expected ", expected_rows, " registered rows for ", source_label,
         " but found ", nrow(reduced), ".", call. = FALSE)
  }
  if (!nrow(reduced) || any(plain_chr(reduced$status) != "estimated") ||
      any(!is.finite(num(reduced$estimate))) || any(!is.finite(num(reduced$std.error))) ||
      any(!is.finite(num(reduced$p.value))) || any(!is.finite(num(reduced$p_holm_within_spec))) ||
      any(!is.finite(num(reduced$n)))) {
    stop("Appendix D requires complete estimated mechanism rows for ", source_label, ".", call. = FALSE)
  }
  reduced$source <- source_label
  reduced[, c("source", required), drop = FALSE]
}

appendix_d_mechanism_display <- function(csv) {
  adjustment <- c(region_main = "Region + controls", state_main = "State + controls")
  construction <- c(
    nonzero_mean = "Shastry mean", glottolog_mean = "Glottolog mean",
    dyen_noncognate = "Dyen noncognate"
  )
  data.frame(
    Outcome = gsub("_", " ", plain_chr(csv$outcome_id)),
    Adjustment = unname(adjustment[plain_chr(csv$adjustment_id)]),
    `Distance measure` = unname(construction[plain_chr(csv$construction_id)]),
    Estimate = sprintf("%+.4f", num(csv$estimate)),
    SE = sprintf("%.4f", num(csv$std.error)),
    `Raw p` = ifelse(num(csv$p.value) < 0.001, "<0.001", sprintf("%.3f", num(csv$p.value))),
    `Holm p` = ifelse(num(csv$p_holm_within_spec) < 0.001, "<0.001", sprintf("%.3f", num(csv$p_holm_within_spec))),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

appendix_d1_migration <- function(migration) {
  csv <- appendix_d_mechanism_rows(migration, "Census migration", 48L)
  out <- appendix_d_mechanism_display(csv)
  attr(out, "csv_data") <- csv
  out
}

appendix_d2_migration_context <- function(migration) {
  reduced <- appendix_d_mechanism_rows(migration, "Census migration", 48L)
  national_ids <- c("skilled_recent_work_migration", "outside_state_recent_work_migration")
  national <- reduced[
    plain_chr(reduced$outcome_id) %in% national_ids &
      plain_chr(reduced$adjustment_id) == "state_main" &
      plain_chr(reduced$construction_id) == "nonzero_mean",
    , drop = FALSE
  ]
  if (nrow(national) != length(national_ids)) {
    stop("Appendix D2 requires the two registered national recent-work-migration comparisons.", call. = FALSE)
  }
  hindi <- safe_df(migration$hindi_belt_skilled_migration %||% data.frame())
  needed <- c("outcome_id", "estimate", "std.error", "p.value", "n", "n_states", "status")
  if (length(setdiff(needed, names(hindi))) || nrow(hindi) != 1L ||
      plain_chr(hindi$status)[[1L]] != "estimated") {
    stop("Appendix D2 requires the registered Hindi-belt skilled-migration restriction.", call. = FALSE)
  }
  csv <- safe_bind_rows(list(
    data.frame(
      sample = "National", result = plain_chr(national$outcome_id),
      estimate = num(national$estimate), std.error = num(national$std.error),
      p.value = num(national$p.value), n = as.integer(national$n),
      n_states = NA_integer_, stringsAsFactors = FALSE
    ),
    data.frame(
      sample = "Hindi-belt states", result = plain_chr(hindi$outcome_id),
      estimate = num(hindi$estimate), std.error = num(hindi$std.error),
      p.value = num(hindi$p.value), n = as.integer(hindi$n),
      n_states = as.integer(hindi$n_states), stringsAsFactors = FALSE
    )
  ))
  out <- data.frame(
    Sample = csv$sample,
    Result = gsub("_", " ", csv$result),
    Estimate = sprintf("%+.4f", csv$estimate),
    SE = sprintf("%.4f", csv$std.error),
    `p-value` = ifelse(csv$p.value < 0.001, "<0.001", sprintf("%.3f", csv$p.value)),
    N = formatC(csv$n, format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_d3_housing_assets <- function(housing) {
  csv <- appendix_d_mechanism_rows(housing, "Census housing/assets", 48L)
  out <- appendix_d_mechanism_display(csv)
  attr(out, "csv_data") <- csv
  out
}

appendix_d4_economic_census <- function(economic_census) {
  csv <- appendix_d_mechanism_rows(economic_census, "Economic Census", 36L)
  out <- appendix_d_mechanism_display(csv)
  attr(out, "csv_data") <- csv
  out
}

appendix_d5_labor <- function(nss66, plfs, plfs_conservative) {
  csv <- safe_bind_rows(list(
    appendix_d_mechanism_rows(nss66, "NSS 2009-10", 12L),
    appendix_d_mechanism_rows(plfs, "PLFS 2017-18 primary", 12L),
    appendix_d_mechanism_rows(plfs_conservative, "PLFS 2017-18 conservative lineage", 12L)
  ))
  if (nrow(csv) != 36L) stop("Appendix D5 must reconcile to the registered 36-model labor family.", call. = FALSE)
  out <- cbind(Survey = csv$source, appendix_d_mechanism_display(csv))
  attr(out, "csv_data") <- csv
  out
}

appendix_d6_household_capacity <- function(household_capacity) {
  csv <- safe_df(household_capacity$estimates %||% data.frame())
  required <- c(
    "outcome_id", "predictor_id", "predictor_role", "estimand", "estimate",
    "std_error_state_clustered", "p_value_state_clustered",
    "p_value_holm_predictor_family", "n", "n_states", "status"
  )
  if (length(setdiff(required, names(csv))) || nrow(csv) != 8L ||
      any(plain_chr(csv$status) != "estimated")) {
    stop("Appendix D6 requires the eight registered household-capacity trajectory models.", call. = FALSE)
  }
  out <- data.frame(
    Outcome = gsub("_", " ", plain_chr(csv$outcome_id)),
    Predictor = ifelse(plain_chr(csv$predictor_id) == "linguistic_opportunity", "Linguistic distance", "Observed all-child EMI"),
    Role = gsub("_", " ", plain_chr(csv$predictor_role)),
    Estimate = sprintf("%+.4f", num(csv$estimate)),
    SE = sprintf("%.4f", num(csv$std_error_state_clustered)),
    `Raw p` = sprintf("%.3f", num(csv$p_value_state_clustered)),
    `Holm p` = sprintf("%.3f", num(csv$p_value_holm_predictor_family)),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_d7_social_heterogeneity <- function(schooling_access, st_heterogeneity) {
  social <- safe_df(schooling_access$estimates %||% data.frame())
  st <- safe_df(st_heterogeneity$estimates %||% data.frame())
  social_required <- c("sample", "social_group", "outcome", "estimate", "std_error_state_clustered", "p_value_state_clustered", "p_value_holm_family", "n_districts")
  st_required <- c("sample", "outcome_id", "heterogeneity", "term", "estimate", "std_error_state_clustered", "p_value_state_clustered", "p_value_holm_family", "n_districts", "status")
  if (length(setdiff(social_required, names(social))) || !nrow(social) ||
      any(!is.finite(num(social$estimate))) || any(!is.finite(num(social$std_error_state_clustered))) ||
      any(!is.finite(num(social$p_value_state_clustered))) || any(!is.finite(num(social$p_value_holm_family))) ||
      any(!is.finite(num(social$n_districts)))) {
    stop("Appendix D7 requires the registered social-group distance-heterogeneity family.", call. = FALSE)
  }
  if (length(setdiff(st_required, names(st))) || !nrow(st) || any(plain_chr(st$status) != "estimated")) {
    stop("Appendix D7 requires the registered ST-concentration heterogeneity family.", call. = FALSE)
  }
  social_csv <- data.frame(
    family = "Social-group gap x distance", sample = plain_chr(social$sample),
    outcome = plain_chr(social$outcome), subgroup = plain_chr(social$social_group),
    design = "gap slope", estimate = num(social$estimate),
    std.error = num(social$std_error_state_clustered), p.value = num(social$p_value_state_clustered),
    p.value_holm = num(social$p_value_holm_family), n = as.integer(social$n_districts), stringsAsFactors = FALSE
  )
  st_csv <- data.frame(
    family = "ST concentration", sample = plain_chr(st$sample), outcome = plain_chr(st$outcome_id),
    subgroup = "ST concentration", design = plain_chr(st$heterogeneity), estimate = num(st$estimate),
    std.error = num(st$std_error_state_clustered), p.value = num(st$p_value_state_clustered),
    p.value_holm = num(st$p_value_holm_family), n = as.integer(st$n_districts), stringsAsFactors = FALSE
  )
  csv <- safe_bind_rows(list(social_csv, st_csv))
  out <- data.frame(
    Family = csv$family, Sample = gsub("_", " ", csv$sample),
    Outcome = gsub("_", " ", csv$outcome), Subgroup = csv$subgroup,
    Design = gsub("_", " ", csv$design), Estimate = sprintf("%+.4f", csv$estimate),
    SE = sprintf("%.4f", csv$std.error), `Holm p` = sprintf("%.3f", csv$p.value_holm),
    N = formatC(csv$n, format = "d", big.mark = ","), check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_d9_residual_spatial_diagnostics <- function(spatial) {
  x <- safe_df(spatial)
  ids <- c("consumption_iv_residual", "consumption_first_stage_residual")
  x <- x[match(ids, plain_chr(x$estimand)), , drop = FALSE]
  required <- c("estimand", "estimate", "p.value", "n", "contiguity", "weights_style", "status")
  if (length(setdiff(required, names(x))) || nrow(x) != length(ids) || any(is.na(x$estimand)) ||
      any(plain_chr(x$status) != "estimated") || any(!is.finite(num(x$estimate))) || any(!is.finite(num(x$p.value)))) {
    stop("Appendix D9 requires the preferred first- and second-stage residual Moran diagnostics.", call. = FALSE)
  }
  labels <- c(
    consumption_iv_residual = "Consumption IV residual",
    consumption_first_stage_residual = "Consumption first-stage residual"
  )
  csv <- x[, required, drop = FALSE]
  out <- data.frame(
    Residual = unname(labels[plain_chr(csv$estimand)]),
    `Moran I` = sprintf("%+.4f", num(csv$estimate)),
    `p-value` = sprintf("%.3f", num(csv$p.value)),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    Contiguity = plain_chr(csv$contiguity), `Weight style` = plain_chr(csv$weights_style),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_local_development_exhibits <- function(
    migration, housing, economic_census, nss66_labor, plfs_labor,
    plfs_conservative_labor, household_capacity, schooling_access,
    st_heterogeneity, spatial_autocorrelation) {
  list(
    appendix_d1_migration = appendix_d1_migration(migration),
    appendix_d2_migration_context = appendix_d2_migration_context(migration),
    appendix_d3_housing_assets = appendix_d3_housing_assets(housing),
    appendix_d4_economic_census = appendix_d4_economic_census(economic_census),
    appendix_d5_labor = appendix_d5_labor(nss66_labor, plfs_labor, plfs_conservative_labor),
    appendix_d6_household_capacity = appendix_d6_household_capacity(household_capacity),
    appendix_d7_social_heterogeneity = appendix_d7_social_heterogeneity(schooling_access, st_heterogeneity),
    appendix_d9_residual_spatial_diagnostics = appendix_d9_residual_spatial_diagnostics(spatial_autocorrelation)
  )
}

save_appendix_local_development_exhibits <- function(exhibits, cfg) {
  save_appendix_tables(
    exhibits,
    c(
      "appendix_d1_migration", "appendix_d2_migration_context",
      "appendix_d3_housing_assets", "appendix_d4_economic_census",
      "appendix_d5_labor", "appendix_d6_household_capacity",
      "appendix_d7_social_heterogeneity", "appendix_d9_residual_spatial_diagnostics"
    ),
    cfg
  )
}
