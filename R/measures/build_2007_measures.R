# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build 2007 measures
#'
#' @return A tibble, model object, list, or file path depending on context.
build_2007_measures <- function(nss_2007_education, nss_2007_consumption, selection_data, ame_results, cfg) {
  edu <- std(safe_bind_rows(lapply(as_input_list(nss_2007_education), safe_df)), 2007L)
  if (!all(c("state_std", "district_std") %in% names(edu))) return(empty_panel())

  weight <- first_col(edu, c("weight", "WEIGHT", "multiplier"))
  emi <- first_col(edu, c("EMI", "emie", "MEDIUM_INSTRUCTION", "medium_instruction", "medium"))
  out <- unique(edu[c("state_std", "district_std")])
  out <- out[!is.na(out$district_std) & nzchar(out$district_std), , drop = FALSE]
  if (!is.null(emi)) {
    out <- merge(
      out,
      bydist(edu, emi, weight, "emie_2007", function(x, w) wmean(as.numeric(num(x) > 0), w)),
      all.x = TRUE
    )
  }
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  out
}

#' compute emie 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_emie_2007 <- function(df) {
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  emi <- first_col(df, c("EMI", "emie", "MEDIUM_INSTRUCTION", "medium_instruction", "medium"))
  bydist(std(df, 2007L), emi, weight, "emie_2007", function(x, w) wmean(as.numeric(num(x) > 0), w))
}

#' compute enrollment share 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_enrollment_share_2007 <- function(df) {
  df
}

#' compute consumption 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_consumption_2007 <- function(df) {
  value <- first_col(df, c("MPCE", "mpce", "consumption", "hh_cons"))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  bydist(std(df, 2007L), value, weight, "consumption_2007")
}

#' compute gini consumption 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_gini_consumption_2007 <- function(df) {
  value <- first_col(df, c("MPCE", "mpce", "consumption", "hh_cons"))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  bydist(std(df, 2007L), value, weight, "gini_consumption_2007", wgini)
}

#' compute baseline controls 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_baseline_controls_2007 <- function(df) {
  df
}

#' compute education freebies ivs 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_education_freebies_ivs_2007 <- function(df) {
  df
}
