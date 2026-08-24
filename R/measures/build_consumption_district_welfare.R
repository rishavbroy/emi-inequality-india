# Design-aware district welfare estimates from canonical, lineaged consumption
# households. The sampling multiplier represents households; MPCE is a
# person-level welfare concept, so district MPCE means use multiplier ×
# household size × lineage allocation weight.

consumption_design_rows <- function(lineaged_households) {
  x <- safe_df(lineaged_households)
  required <- c(
    "survey_id", "household_id", "source_state_code", "sector", "subround",
    "fsu", "stratum", "sub_stratum", "household_size", "target_unit_2001",
    "lineage_status", "lineage_person_weight", "real_mpce"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Lineaged consumption households lack survey-design fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  resolved <- grepl("^resolved_", plain_chr(x$lineage_status))
  x <- x[resolved, , drop = FALSE]
  if (!nrow(x)) stop("No resolved consumption households are available for district welfare estimation.", call. = FALSE)

  x$sector <- price_sector(x$sector)
  x$lineage_person_weight <- num(x$lineage_person_weight)
  x$real_mpce <- num(x$real_mpce)

  # Round 66 has no urban sub-stratification. Preserve that design fact
  # explicitly instead of treating the released blank Sub_Stratum field as
  # missing data. Rural blanks remain invalid because rural strata are
  # subdivided in the 66th-round design.
  sub_stratum <- trimws(plain_chr(x$sub_stratum))
  blank_sub_stratum <- is.na(sub_stratum) | !nzchar(sub_stratum)
  invalid_sub_stratum <- blank_sub_stratum & x$sector != "urban"
  sub_stratum[blank_sub_stratum & x$sector == "urban"] <- "__none__"
  x$.design_sub_stratum <- sub_stratum

  valid <- !is.na(x$target_unit_2001) & nzchar(plain_chr(x$target_unit_2001)) &
    !is.na(x$fsu) & nzchar(plain_chr(x$fsu)) &
    !is.na(x$stratum) & nzchar(plain_chr(x$stratum)) &
    !invalid_sub_stratum & !is.na(x$sector) &
    positive_finite(x$lineage_person_weight) & positive_finite(x$real_mpce)
  if (!all(valid)) {
    counts <- c(
      target_unit_2001 = sum(is.na(x$target_unit_2001) | !nzchar(plain_chr(x$target_unit_2001))),
      fsu = sum(is.na(x$fsu) | !nzchar(plain_chr(x$fsu))),
      stratum = sum(is.na(x$stratum) | !nzchar(plain_chr(x$stratum))),
      sub_stratum = sum(invalid_sub_stratum),
      sector = sum(is.na(x$sector)),
      lineage_person_weight = sum(!positive_finite(x$lineage_person_weight)),
      real_mpce = sum(!positive_finite(x$real_mpce))
    )
    counts <- counts[counts > 0]
    stop(
      "Resolved consumption households contain invalid survey-design values: ",
      paste(paste(names(counts), counts, sep = "="), collapse = ", "),
      call. = FALSE
    )
  }

  # NSS stratum/sub-stratum identifiers repeat across states and sectors.
  # Build nested design identifiers rather than treating the raw numbers as
  # globally unique. Sub-round is a fieldwork period, not a sampling stratum.
  x$.design_stratum <- interaction(
    x$source_state_code, x$sector, x$stratum, x$.design_sub_stratum,
    drop = TRUE, lex.order = TRUE
  )
  x$.design_psu <- interaction(
    x$source_state_code, x$sector, x$fsu,
    drop = TRUE, lex.order = TRUE
  )
  x
}

consumption_survey_design_from_rows <- function(rows) {
  survey::svydesign(
    ids = ~.design_psu,
    strata = ~.design_stratum,
    weights = ~lineage_person_weight,
    data = safe_df(rows),
    nest = TRUE
  )
}

build_consumption_survey_design <- function(lineaged_households) {
  consumption_survey_design_from_rows(consumption_design_rows(lineaged_households))
}

with_consumption_survey_adjustment <- function(expr) {
  old_options <- options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
  on.exit(options(old_options), add = TRUE)
  withCallingHandlers(
    expr,
    warning = function(w) {
      # survey deliberately warns for domain-level lonely PSUs even when the
      # requested adjustment is applied. Muffle only that handled condition so
      # strict builds remain warning-clean; all other warnings still propagate.
      if (grepl("has only one PSU at stage", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

kish_effective_n <- function(weight) {
  w <- num(weight)
  w <- w[positive_finite(w)]
  if (!length(w)) return(NA_real_)
  denom <- sum(w^2)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sum(w)^2 / denom
}

consumption_district_support_from_rows <- function(x) {
  x <- safe_df(x)
  groups <- split(seq_len(nrow(x)), x$target_unit_2001)
  safe_bind_rows(lapply(groups, function(i) {
    part <- x[i, , drop = FALSE]
    data.frame(
      district_2001 = plain_chr(part$target_unit_2001[[1L]]),
      n_households = length(unique(part$household_id)),
      n_fsu = length(unique(part$.design_psu)),
      kish_effective_n = kish_effective_n(part$lineage_person_weight),
      stringsAsFactors = FALSE
    )
  }))
}

consumption_district_support <- function(lineaged_households) {
  consumption_district_support_from_rows(consumption_design_rows(lineaged_households))
}

estimate_consumption_district_mean <- function(lineaged_households) {
  x <- consumption_design_rows(lineaged_households)
  survey_id <- unique(plain_chr(x$survey_id))
  if (length(survey_id) != 1L) stop("District welfare estimation requires one survey at a time.", call. = FALSE)

  design <- consumption_survey_design_from_rows(x)
  estimates <- with_consumption_survey_adjustment(survey::svyby(
    ~real_mpce,
    ~target_unit_2001,
    design,
    survey::svymean,
    na.rm = TRUE,
    vartype = "se",
    keep.names = FALSE,
    drop.empty.groups = FALSE
  ))
  estimates <- as.data.frame(estimates, stringsAsFactors = FALSE)
  if (!all(c("target_unit_2001", "real_mpce", "se") %in% names(estimates))) {
    stop("survey::svyby returned an unexpected district-mean schema.", call. = FALSE)
  }

  support <- consumption_district_support_from_rows(x)
  out <- merge(
    data.frame(
      district_2001 = plain_chr(estimates$target_unit_2001),
      round_id = survey_id[[1L]],
      outcome_id = "real_mean_mpce",
      estimate = num(estimates$real_mpce),
      std_error = num(estimates$se),
      stringsAsFactors = FALSE
    ),
    support,
    by = "district_2001", all.x = TRUE, sort = FALSE
  )
  out$cv <- ifelse(positive_finite(out$estimate), out$std_error / out$estimate, NA_real_)
  valid <- positive_finite(out$estimate) & is.finite(out$std_error) & out$std_error >= 0
  out$status <- ifelse(valid, "estimated", "not_estimable")
  out$reason <- ifelse(valid, NA_character_, "non_finite_design_estimate")
  out <- out[c(
    "district_2001", "round_id", "outcome_id", "estimate", "std_error", "cv",
    "n_households", "n_fsu", "kish_effective_n", "status", "reason"
  )]
  out[order(out$district_2001), , drop = FALSE]
}

save_consumption_district_welfare <- function(
    outcomes, path = "outputs/diagnostics/public/consumption_district_welfare.csv") {
  write_diagnostic_csv(safe_df(outcomes), path)
}
