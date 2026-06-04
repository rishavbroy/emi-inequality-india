# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build 2017 measures
#'
#' @return A tibble, model object, list, or file path depending on context.
build_2017_measures <- function(nss_2017_education, cfg) {
  df <- std(safe_bind_rows(lapply(as_input_list(nss_2017_education), safe_df)), 2017L)
  if (!all(c("state_std", "district_std") %in% names(df))) return(empty_panel())

  value <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
  hh_size <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
  out <- unique(df[c("state_std", "district_std")])
  out <- out[!is.na(out$district_std) & nzchar(out$district_std), , drop = FALSE]
  if (identical(value, "HH_Con_exp_rs") && !is.null(hh_size)) {
    df$consumption_pc_2017 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2017"
  }
  if (!is.null(value)) out <- merge(out, bydist(df, value, weight, "consumption_2017"), all.x = TRUE)
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2017L)
  out
}

#' compute consumption 2017
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_consumption_2017 <- function(df) {
  df <- std(df, 2017L)
  value <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
  hh_size <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
  if (identical(value, "HH_Con_exp_rs") && !is.null(hh_size)) {
    df$consumption_pc_2017 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2017"
  }
  bydist(df, value, weight, "consumption_2017")
}

#' compute gini consumption 2017
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_gini_consumption_2017 <- function(df) {
  df <- std(df, 2017L)
  value <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
  hh_size <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
  if (identical(value, "HH_Con_exp_rs") && !is.null(hh_size)) {
    df$consumption_pc_2017 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2017"
  }
  bydist(df, value, weight, "gini_consumption_2017", wgini)
}

#' compute 2017 controls
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_2017_controls <- function(df) {
  df
}
