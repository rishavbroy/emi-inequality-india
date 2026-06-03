# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build 2017 measures
#'
#' @return A tibble, model object, list, or file path depending on context.
build_2017_measures <- function(nss_2017_education, cfg) {
  df <- std(safe_bind_rows(lapply(as_input_list(nss_2017_education), safe_df)), 2017L)
  if (!all(c("state_std", "district_std") %in% names(df))) return(empty_panel())

  value <- first_col(df, c("MPCE", "mpce", "consumption", "hh_cons"))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  out <- unique(df[c("state_std", "district_std")])
  out <- out[!is.na(out$district_std) & nzchar(out$district_std), , drop = FALSE]
  if (!is.null(value)) out <- merge(out, bydist(df, value, weight, "consumption_2017"), all.x = TRUE)
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2017L)
  out
}

#' compute consumption 2017
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_consumption_2017 <- function(df) {
  value <- first_col(df, c("MPCE", "mpce", "consumption", "hh_cons"))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  bydist(std(df, 2017L), value, weight, "consumption_2017")
}

#' compute gini consumption 2017
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_gini_consumption_2017 <- function(df) {
  value <- first_col(df, c("MPCE", "mpce", "consumption", "hh_cons"))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  bydist(std(df, 2017L), value, weight, "gini_consumption_2017", wgini)
}

#' compute 2017 controls
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_2017_controls <- function(df) {
  df
}
