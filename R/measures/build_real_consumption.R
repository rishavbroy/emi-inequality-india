# Household-level real-consumption construction shared by registered survey adapters.


deflate_detailed_consumption_households <- function(households, deflators, specification) {
  hh <- safe_df(households)
  required <- c(
    "source_state_code", "sector", "subround", "nominal_mpce",
    "nominal_household_consumption", "household_size", "survey_weight"
  )
  missing <- setdiff(required, names(hh))
  if (length(missing)) {
    stop(
      "Canonical detailed-consumption households are missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(hh)) return(hh)

  out <- attach_survey_subround_deflator(
    hh, deflators, specification,
    state_col = "source_state_code", sector_col = "sector", subround_col = "subround"
  )
  out$real_mpce <- num(out$nominal_mpce) / num(out$price_deflator)
  out$real_household_consumption <-
    num(out$nominal_household_consumption) / num(out$price_deflator)

  valid <- positive_finite(out$real_mpce) & positive_finite(out$real_household_consumption)
  if (!all(valid)) {
    stop("Detailed-consumption deflation produced invalid real expenditure.", call. = FALSE)
  }
  implied_total <- out$real_mpce * num(out$household_size)
  tolerance <- sqrt(.Machine$double.eps) * pmax(1, abs(out$real_household_consumption))
  if (any(abs(implied_total - out$real_household_consumption) > tolerance)) {
    stop("Detailed-consumption real MPCE and household totals are inconsistent.", call. = FALSE)
  }
  out
}

prepare_consumption_households <- function(
    df, wave, district_keys, value_candidates, size_candidates, weight_candidates,
    state_candidates, sector_candidates, subround_candidates, deflators = NULL,
    survey_spec = NULL) {
  df <- safe_df(df)
  if (!nrow(df)) return(df)
  value_col <- first_col(df, value_candidates)
  size_col <- first_col(df, size_candidates)
  weight_col <- first_col(df, weight_candidates)
  if (is.null(value_col) || is.null(weight_col)) return(data.frame())

  hh_key <- first_col(df, c("HHID", "HH_ID", "household_id", "HHID_key"))
  if (!is.null(hh_key)) {
    district_key <- do.call(paste, c(df[district_keys], sep = "__"))
    df$.hh_distinct_key <- paste(district_key, canon(df[[hh_key]]), sep = "__")
    df <- collapse_identical_key_rows(
      df, ".hh_distinct_key", context = paste(wave, "consumption households")
    )
  }

  size <- if (is.null(size_col)) rep(1, nrow(df)) else num(df[[size_col]])
  nominal_value <- num(df[[value_col]])
  value_is_total <- tolower(value_col) %in% tolower(c("TOTAL", "HH_Con_exp_rs", "consumption", "hh_cons"))
  nominal_total <- if (value_is_total) nominal_value else nominal_value * size
  nominal_pc <- nominal_total / size
  weight <- num(df[[weight_col]])
  valid <- positive_finite(size) & positive_finite(nominal_total) & positive_finite(weight)
  df <- df[valid, , drop = FALSE]
  size <- size[valid]
  nominal_total <- nominal_total[valid]
  nominal_pc <- nominal_pc[valid]
  weight <- weight[valid]
  if (!nrow(df)) return(df)

  df$consumption_nominal_total <- nominal_total
  df$consumption_nominal_pc <- nominal_pc
  df$household_size_price <- size
  df$survey_weight_price <- weight

  if (!is.null(deflators)) {
    state_col <- first_col(df, state_candidates)
    sector_col <- first_col(df, sector_candidates)
    subround_col <- first_col(df, subround_candidates)
    missing <- c(state = is.null(state_col), sector = is.null(sector_col), subround = is.null(subround_col))
    if (any(missing)) stop("Survey household file lacks price keys: ", paste(names(missing)[missing], collapse = ", "), call. = FALSE)
    if (is.null(survey_spec)) {
      survey_spec <- consumption_survey_spec_for_wave(read_consumption_survey_registry(), wave)
    }
    df <- attach_survey_subround_deflator(df, deflators, survey_spec, state_col, sector_col, subround_col)
    df$consumption_real_total <- df$consumption_nominal_total / df$price_deflator
    df$consumption_real_pc <- df$consumption_nominal_pc / df$price_deflator
  }
  df
}

aggregate_consumption_households <- function(households, district_keys, suffix) {
  df <- safe_df(households)
  if (!nrow(df) || !length(district_keys)) return(data.frame())
  idx <- which(stats::complete.cases(df[district_keys]))
  split_i <- split(idx, interaction(df[idx, district_keys, drop = FALSE], drop = TRUE, sep = "__"))
  safe_bind_rows(lapply(split_i, function(i) {
    z <- df[i[[1]], district_keys, drop = FALSE]
    w <- num(df$survey_weight_price[i])
    size <- num(df$household_size_price[i])
    nominal <- num(df$consumption_nominal_total[i])
    out <- data.frame(
      z,
      stringsAsFactors = FALSE
    )
    out[[paste0("consumption_", suffix)]] <- mean_expenditure_per_person(nominal, size, w)
    out[[paste0("consumption_", suffix, "_household_weighted")]] <- mean_household_mpce(nominal, size, w)
    out[[paste0("gini_cons_", suffix)]] <- person_weighted_mpce_gini(nominal, size, w)
    out[[paste0("npeople_", suffix)]] <- sum(w * size, na.rm = TRUE)
    out[[paste0("nhouses_", suffix)]] <- sum(w, na.rm = TRUE)
    if ("consumption_real_total" %in% names(df)) {
      real <- num(df$consumption_real_total[i])
      out[[paste0("real_consumption_", suffix)]] <- mean_expenditure_per_person(real, size, w)
      out[[paste0("real_consumption_", suffix, "_household_weighted")]] <- mean_household_mpce(real, size, w)
      out[[paste0("gini_cons_", suffix, "_real")]] <- person_weighted_mpce_gini(real, size, w)
      out[[paste0("price_deflator_", suffix, "_person_weighted")]] <- stats::weighted.mean(
        num(df$price_deflator[i]), w * size, na.rm = TRUE
      )
      out[[paste0("price_fallback_household_share_", suffix)]] <- weighted_fallback_share(df$state_rule[i], w)
    }
    out
  }))
}

weighted_fallback_share <- function(rule, weight) {
  rule <- as.character(rule)
  weight <- num(weight)
  keep <- positive_finite(weight) & !is.na(rule)
  if (!any(keep)) return(NA_real_)
  100 * sum(weight[keep] * (rule[keep] != "direct")) / sum(weight[keep])
}
