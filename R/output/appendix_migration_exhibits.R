# Final-paper migration summary.
#
# This module only reshapes the registered migration diagnostics for publication.
# Other local-development families remain analytical outputs rather than manuscript appendices.

appendix_migration_mechanism_rows <- function(x, source_label, expected_rows = NULL) {
  result <- extract_posttreatment_mechanism_result(x)
  reduced <- safe_df(result$reduced_form)
  required <- c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect", "term",
    "estimate", "std.error", "p.value", "p_holm_within_spec", "n", "status"
  )
  missing <- setdiff(required, names(reduced))
  if (length(missing)) {
    stop("Migration summary is missing fields: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(expected_rows) && nrow(reduced) != expected_rows) {
    stop("Migration summary expected ", expected_rows, " registered rows for ", source_label,
         " but found ", nrow(reduced), ".", call. = FALSE)
  }
  if (!nrow(reduced) || any(plain_chr(reduced$status) != "estimated") ||
      any(!is.finite(num(reduced$estimate))) || any(!is.finite(num(reduced$std.error))) ||
      any(!is.finite(num(reduced$p.value))) || any(!is.finite(num(reduced$p_holm_within_spec))) ||
      any(!is.finite(num(reduced$n)))) {
    stop("Migration summary requires complete estimated mechanism rows for ", source_label, ".", call. = FALSE)
  }
  reduced$source <- source_label
  reduced[, c("source", required), drop = FALSE]
}

appendix_migration_summary <- function(migration) {
  reduced <- appendix_migration_mechanism_rows(migration, "Census migration", 48L)
  preferred_ids <- c(
    "interstate_migrant_composition",
    "work_migration_reason",
    "education_migration_reason",
    "outside_state_recent_work_migration",
    "skilled_recent_work_migration"
  )
  labels <- c(
    interstate_migrant_composition = "Interstate share among migrants",
    work_migration_reason = "Work/employment share among migrants",
    education_migration_reason = "Education share among migrants",
    outside_state_recent_work_migration = "Outside-state share among recent work migrants",
    skilled_recent_work_migration = "Graduate/technical-degree share among recent work migrants"
  )
  national <- reduced[
    plain_chr(reduced$outcome_id) %in% preferred_ids &
      plain_chr(reduced$adjustment_id) == "state_main" &
      plain_chr(reduced$construction_id) == "nonzero_mean",
    , drop = FALSE
  ]
  national <- national[match(preferred_ids, plain_chr(national$outcome_id)), , drop = FALSE]
  if (nrow(national) != length(preferred_ids) || anyNA(national$outcome_id)) {
    stop("Migration summary requires the five registered preferred national comparisons.", call. = FALSE)
  }

  hindi <- safe_df(migration$hindi_belt_skilled_migration %||% data.frame())
  needed <- c(
    "outcome_id", "estimate", "std.error", "p.value", "n", "n_states",
    "adjustment_id", "construction_id", "fixed_effect", "status"
  )
  missing <- setdiff(needed, names(hindi))
  if (length(missing) || nrow(hindi) != 1L ||
      plain_chr(hindi$status)[[1L]] != "estimated" ||
      plain_chr(hindi$outcome_id)[[1L]] != "skilled_recent_work_migration") {
    stop("Migration summary requires the registered Hindi-belt skilled-migration restriction.", call. = FALSE)
  }

  csv <- safe_bind_rows(list(
    data.frame(
      panel = c(rep("All migrants", 3L), rep("Recent work migrants", 2L)),
      sample = "All states",
      outcome_id = plain_chr(national$outcome_id),
      outcome = unname(labels[plain_chr(national$outcome_id)]),
      estimate = num(national$estimate),
      std.error = num(national$std.error),
      p.value = num(national$p.value),
      p.value_for_stars = num(national$p_holm_within_spec),
      p.value_basis = "Holm-adjusted within preferred migration family",
      n = as.integer(national$n),
      n_states = NA_integer_,
      adjustment_id = plain_chr(national$adjustment_id),
      construction_id = plain_chr(national$construction_id),
      fixed_effect = plain_chr(national$fixed_effect),
      stringsAsFactors = FALSE
    ),
    data.frame(
      panel = "Recent work migrants",
      sample = "Hindi-belt states",
      outcome_id = plain_chr(hindi$outcome_id),
      outcome = unname(labels[plain_chr(hindi$outcome_id)]),
      estimate = num(hindi$estimate),
      std.error = num(hindi$std.error),
      p.value = num(hindi$p.value),
      p.value_for_stars = num(hindi$p.value),
      p.value_basis = "Raw p-value for predeclared single restriction",
      n = as.integer(hindi$n),
      n_states = as.integer(hindi$n_states),
      adjustment_id = plain_chr(hindi$adjustment_id),
      construction_id = plain_chr(hindi$construction_id),
      fixed_effect = plain_chr(hindi$fixed_effect),
      stringsAsFactors = FALSE
    )
  ))
  if (any(!is.finite(csv$estimate)) || any(!is.finite(csv$std.error)) ||
      any(!is.finite(csv$p.value_for_stars)) || any(!is.finite(csv$n))) {
    stop("Migration summary requires finite estimates, inference, and sample sizes.", call. = FALSE)
  }
  out <- data.frame(Term = "Linguistic distance from Hindi", stringsAsFactors = FALSE)
  attr(out, "csv_data") <- csv
  out
}



make_appendix_migration_exhibits <- function(migration) {
  list(appendix_migration_summary = appendix_migration_summary(migration))
}

save_appendix_migration_exhibits <- function(exhibits, cfg) {
  save_appendix_tables(exhibits, "appendix_migration_summary", cfg)
}
