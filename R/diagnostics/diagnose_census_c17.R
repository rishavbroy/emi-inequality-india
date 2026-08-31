# Census 2001 C-17 mechanism analysis at the state x native-language level.
#
# This is deliberately separate from the district IV machinery. It reproduces the
# behavioral margin in Shastry (2012): conditional on being multilingual, are
# speakers of languages farther from Hindi more likely to acquire English within
# the same state? C-17 is a single cross-section, so the pooled 1961/1991
# state-language clustering in Shastry is not available; HC1 inference is used for
# this mechanism diagnostic.

census_c17_mechanism_registry <- function() {
  data.frame(
    specification_id = c(
      "english_linear", "english_bins", "english_distant",
      "hindi_linear", "multilingual_linear",
      "english_linear_males", "english_linear_females"
    ),
    outcome = c(
      rep("english_share_multilingual", 3L),
      "hindi_share_multilingual", "multilingual_share_native",
      rep("english_share_multilingual", 2L)
    ),
    sex = c(rep("Persons", 5L), "Males", "Females"),
    distance_form = c("linear", "bins", "distant", rep("linear", 4L)),
    preferred = c(TRUE, rep(FALSE, 6L)),
    label = c(
      "English acquisition among multilingual speakers",
      "English acquisition: flexible distance bins",
      "English acquisition: distant-language indicator",
      "Hindi acquisition among multilingual speakers",
      "Multilingualism among native speakers",
      "English acquisition among multilingual male speakers",
      "English acquisition among multilingual female speakers"
    ),
    stringsAsFactors = FALSE
  )
}

prepare_census_c17_mechanism_data <- function(c17) {
  out <- safe_df(c17)
  required <- c(
    "state_code", "state_name", "native_language_code", "native_language",
    "sex", "native_speakers", "multilingual_speakers",
    "english_share_multilingual", "hindi_share_multilingual",
    "multilingual_share_native"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop("Census C-17 mechanism input is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  key <- paste(out$state_code, out$native_language_code, out$sex, sep = "|")
  if (anyDuplicated(key)) stop("Census C-17 mechanism input is not unique by state, language, and sex.", call. = FALSE)

  out$mother_tongue_code <- normalize_census_code(out$native_language_code, 6L)
  out$mother_tongue <- normalize_census_language_label(out$native_language)
  out$canonical_language <- out$mother_tongue
  out$shastry_degree <- resolve_shastry_language_degrees(out)
  out$distance_mapping_status <- ifelse(is.finite(out$shastry_degree), "mapped", "unmapped")
  out$distance_zero <- as.integer(is.finite(out$shastry_degree) & out$shastry_degree == 0)
  out$distance_distant <- as.integer(is.finite(out$shastry_degree) & out$shastry_degree >= 3)
  out$distance_bin <- factor(
    as.character(out$shastry_degree),
    levels = c("1", "0", "2", "3", "4", "5")
  )

  groups <- split(seq_len(nrow(out)), interaction(out$state_code, out$sex, drop = TRUE))
  out$native_share_state <- NA_real_
  out$state_modal_language <- 0L
  for (index in groups) {
    speakers <- num(out$native_speakers[index])
    total <- sum(speakers[is.finite(speakers)], na.rm = TRUE)
    if (!is.finite(total) || total <= 0) next
    out$native_share_state[index] <- speakers / total
    maximum <- max(speakers, na.rm = TRUE)
    modal <- index[is.finite(speakers) & speakers == maximum]
    if (length(modal) != 1L) {
      stop("Census C-17 has an ambiguous modal native language within a state/sex cell.", call. = FALSE)
    }
    out$state_modal_language[modal] <- 1L
  }

  if (any(is.finite(out$native_share_state) & (out$native_share_state < 0 | out$native_share_state > 1))) {
    stop("Census C-17 native-language state shares must lie in [0, 1].", call. = FALSE)
  }
  out
}

census_c17_mechanism_formula <- function(specification) {
  form <- as.character(specification$distance_form[[1L]])
  distance_terms <- switch(
    form,
    linear = c("shastry_degree", "distance_zero"),
    bins = "distance_bin",
    distant = c("distance_distant", "distance_zero"),
    stop("Unknown C-17 distance form: ", form, call. = FALSE)
  )
  stats::reformulate(
    c(distance_terms, "native_share_state", "state_modal_language", "factor(state_code)"),
    response = as.character(specification$outcome[[1L]])
  )
}

census_c17_distance_term <- function(term) {
  startsWith(term, "shastry_degree") |
    startsWith(term, "distance_zero") |
    startsWith(term, "distance_bin") |
    startsWith(term, "distance_distant")
}

census_c17_term_partial_r_squared <- function(model, term) {
  term_labels <- attr(stats::terms(model), "term.labels")
  if (!term %in% term_labels) return(NA_real_)
  restricted <- stats::update(
    model, stats::as.formula(paste(". ~ . -", term))
  )
  full_deviance <- stats::deviance(model)
  restricted_deviance <- stats::deviance(restricted)
  if (!is.finite(full_deviance) || !is.finite(restricted_deviance) ||
      restricted_deviance <= 0) return(NA_real_)
  value <- 1 - full_deviance / restricted_deviance
  max(0, min(1, value))
}

fit_census_c17_mechanism <- function(data, specification) {
  data <- safe_df(data)
  sex <- as.character(specification$sex[[1L]])
  outcome <- as.character(specification$outcome[[1L]])
  sample <- data[data$sex == sex & data$distance_mapping_status == "mapped", , drop = FALSE]
  needed <- c(
    outcome, "native_speakers", "native_share_state", "state_modal_language",
    "state_code", "shastry_degree", "distance_zero"
  )
  if (identical(specification$distance_form[[1L]], "bins")) needed <- c(needed, "distance_bin")
  if (identical(specification$distance_form[[1L]], "distant")) needed <- c(needed, "distance_distant")
  sample <- sample[stats::complete.cases(sample[needed]) & num(sample$native_speakers) > 0, , drop = FALSE]

  if (nrow(sample) < 10L || length(unique(sample$state_code)) < 2L ||
      length(unique(sample$shastry_degree)) < 2L) {
    return(list(
      coefficients = data.frame(
        specification_id = specification$specification_id[[1L]], term = NA_character_,
        estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
        partial_r_squared = NA_real_, signed_partial_correlation = NA_real_,
        status = "not_estimable", reason = "Insufficient state-language support.",
        stringsAsFactors = FALSE
      ),
      summary = data.frame(
        specification_id = specification$specification_id[[1L]], n = nrow(sample),
        n_states = length(unique(sample$state_code)),
        n_languages = length(unique(sample$native_language_code)), r.squared = NA_real_,
        status = "not_estimable", stringsAsFactors = FALSE
      )
    ))
  }

  model <- stats::lm(
    census_c17_mechanism_formula(specification),
    data = sample,
    weights = native_speakers
  )
  vcov <- sandwich::vcovHC(model, type = "HC1")
  coeftest <- lmtest::coeftest(model, vcov. = vcov)
  if (ncol(coeftest) != 4L) {
    stop("Census C-17 coefficient table must contain estimate, SE, statistic, and p-value columns.", call. = FALSE)
  }
  test <- data.frame(
    term = rownames(coeftest),
    estimate = unname(coeftest[, 1L]),
    std.error = unname(coeftest[, 2L]),
    statistic = unname(coeftest[, 3L]),
    p.value = unname(coeftest[, 4L]),
    stringsAsFactors = FALSE
  )
  test <- test[census_c17_distance_term(test$term), c(
    "term", "estimate", "std.error", "statistic", "p.value"
  ), drop = FALSE]
  test$partial_r_squared <- vapply(
    test$term, function(term) census_c17_term_partial_r_squared(model, term), numeric(1)
  )
  test$signed_partial_correlation <- ifelse(
    is.finite(test$partial_r_squared),
    sign(test$estimate) * sqrt(test$partial_r_squared),
    NA_real_
  )
  test$specification_id <- specification$specification_id[[1L]]
  test$status <- "estimated"
  test$reason <- NA_character_
  test <- test[c(
    "specification_id", "term", "estimate", "std.error", "statistic",
    "p.value", "partial_r_squared", "signed_partial_correlation",
    "status", "reason"
  )]

  summary <- summary(model)
  model_summary <- data.frame(
    specification_id = specification$specification_id[[1L]],
    outcome = outcome,
    sex = sex,
    distance_form = specification$distance_form[[1L]],
    n = stats::nobs(model),
    n_states = length(unique(sample$state_code)),
    n_languages = length(unique(sample$native_language_code)),
    r.squared = unname(summary$r.squared),
    adjusted.r.squared = unname(summary$adj.r.squared),
    status = "estimated",
    stringsAsFactors = FALSE
  )
  list(coefficients = test, summary = model_summary)
}

summarize_census_c17_mapping_coverage <- function(data) {
  data <- safe_df(data)
  groups <- split(seq_len(nrow(data)), data$sex)
  safe_bind_rows(lapply(names(groups), function(sex) {
    part <- data[groups[[sex]], , drop = FALSE]
    speakers <- num(part$native_speakers)
    mapped <- part$distance_mapping_status == "mapped"
    total <- sum(speakers[is.finite(speakers)], na.rm = TRUE)
    mapped_speakers <- sum(speakers[mapped & is.finite(speakers)], na.rm = TRUE)
    data.frame(
      sex = sex,
      n_state_language_cells = nrow(part),
      n_mapped_cells = sum(mapped),
      mapped_cell_share = if (nrow(part)) sum(mapped) / nrow(part) else NA_real_,
      mapped_native_speaker_share = if (total > 0) mapped_speakers / total else NA_real_,
      n_mapped_cells_without_multilinguals = sum(mapped & num(part$multilingual_speakers) <= 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

diagnose_census_c17_mechanism <- function(c17) {
  prepared <- prepare_census_c17_mechanism_data(c17)
  registry <- census_c17_mechanism_registry()
  fits <- lapply(seq_len(nrow(registry)), function(i) {
    fit_census_c17_mechanism(prepared, registry[i, , drop = FALSE])
  })
  list(
    registry = registry,
    mapping_coverage = summarize_census_c17_mapping_coverage(prepared),
    coefficients = safe_bind_rows(lapply(fits, `[[`, "coefficients")),
    model_summary = safe_bind_rows(lapply(fits, `[[`, "summary"))
  )
}

save_census_c17_mechanism_diagnostics <- function(
    diagnostics,
    dir = "outputs/diagnostics/extended/census_c17") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  output_manifest(c(
    registry = write_diagnostic_csv(
      diagnostics$registry,
      file.path(dir, "c17_mechanism_registry.csv")
    ),
    mapping_coverage = write_diagnostic_csv(
      diagnostics$mapping_coverage,
      file.path(dir, "c17_language_mapping_coverage.csv")
    ),
    coefficients = write_diagnostic_csv(
      diagnostics$coefficients,
      file.path(dir, "c17_mechanism_coefficients.csv")
    ),
    model_summary = write_diagnostic_csv(
      diagnostics$model_summary,
      file.path(dir, "c17_mechanism_model_summary.csv")
    )
  ))
}
