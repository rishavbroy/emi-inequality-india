# Cross-source paper semantics for the English-opportunity mechanism sequence.
#
# This registry is reporting metadata only. It intentionally contains no
# formulas, estimators, or causal ordering assumptions: C-17, NSS, and DISE use
# different observational units and cannot be interpreted as a sequential
# mediation panel.

english_opportunity_measure_registry_path <- function(paths = build_paths()) {
  path_metadata(paths, "english_opportunity_measures.csv")
}

read_english_opportunity_measure_registry <- function(path) {
  if (!file.exists(path)) {
    stop("English-opportunity measure registry is missing: ", path, call. = FALSE)
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "measure_id", "variable", "source", "unit", "stage", "numerator",
    "denominator", "population", "source_side", "interpretation",
    "paper_role", "preferred"
  )
  if (!identical(names(out), required)) {
    stop("English-opportunity measure registry has an invalid schema.", call. = FALSE)
  }
  text_fields <- setdiff(required, "preferred")
  for (field in text_fields) out[[field]] <- trimws(plain_chr(out[[field]]))
  if (!nrow(out) || anyDuplicated(out$measure_id) || anyDuplicated(out$variable) ||
      any(vapply(out[text_fields], function(x) any(is.na(x) | !nzchar(x)), logical(1)))) {
    stop("English-opportunity measure registry contains empty or duplicate identifiers.", call. = FALSE)
  }

  preferred <- tolower(trimws(plain_chr(out$preferred)))
  if (any(!preferred %in% c("true", "false"))) {
    stop("English-opportunity preferred flags must be TRUE or FALSE.", call. = FALSE)
  }
  out$preferred <- preferred == "true"

  allowed <- list(
    source = c("census_2001_c17", "nss_64_education", "dise"),
    unit = c("state_language", "district"),
    stage = c(
      "capability_acquisition", "schooling_access", "formal_english_exposure",
      "institution_choice", "institutional_bundle", "school_system_structure",
      "school_quality"
    ),
    source_side = c("linguistic_behavior", "household_realized", "administrative_equilibrium", "administrative_supply")
  )
  for (field in names(allowed)) {
    invalid <- setdiff(unique(out[[field]]), allowed[[field]])
    if (length(invalid)) {
      stop("English-opportunity measure registry contains unknown ", field, ": ", paste(invalid, collapse = ", "), call. = FALSE)
    }
  }
  if (any(out$source == "census_2001_c17" & out$unit != "state_language") ||
      any(out$source != "census_2001_c17" & out$unit != "district")) {
    stop("English-opportunity source and unit semantics are inconsistent.", call. = FALSE)
  }
  out
}

save_english_opportunity_measure_registry <- function(
    registry,
    dir = "outputs/diagnostics/extended/mechanisms") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_diagnostic_csv(registry, file.path(dir, "english_opportunity_measures.csv"))
}
