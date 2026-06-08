# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' build 2017 measures
#'
build_2017_measures <- function(nss_2017_education, cfg) {
  inputs <- as_input_list(nss_2017_education)
  df <- std(safe_bind_rows(lapply(inputs, safe_df)), 2017L)
  if (!all(c("state_std", "district_std") %in% names(df))) return(empty_panel())

  df <- normalize_2017_district_code(df)
  value <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
  hh_size <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
  out <- unique(df[c("state_std", "district_std")])
  if ("district_code_1718" %in% names(df)) out <- merge(out, unique(df[c("state_std", "district_std", "district_code_1718")]), by = c("state_std", "district_std"), all.x = TRUE)
  out <- out[!is.na(out$district_std) & nzchar(out$district_std), , drop = FALSE]
  if (identical(value, "HH_Con_exp_rs") && !is.null(hh_size)) {
    df$consumption_pc_2017 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2017"
  }
  if (!is.null(value)) {
    cons <- bydist(df, value, weight, "consumption_2017")
    out <- merge(out, cons, by = c("state_std", "district_std"), all.x = TRUE)
    gini <- bydist(df, value, weight, "gini_cons_1718", wgini)
    gini$n <- NULL
    out <- merge(out, gini, by = c("state_std", "district_std"), all.x = TRUE)
  }
  out <- attach_2017_district_names(out, inputs)
  out <- add_legacy_2017_aliases(out)
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2017L)
  out
}

#' compute consumption 2017
#'
compute_consumption_2017 <- function(df) {
  df <- std(normalize_2017_district_code(df), 2017L)
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
compute_gini_consumption_2017 <- function(df) {
  df <- std(normalize_2017_district_code(df), 2017L)
  value <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
  hh_size <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
  if (identical(value, "HH_Con_exp_rs") && !is.null(hh_size)) {
    df$consumption_pc_2017 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2017"
  }
  bydist(df, value, weight, "gini_cons_1718", wgini)
}

#' compute 2017 controls
#'
compute_2017_controls <- function(df) df

normalize_2017_district_code <- function(df) {
  df <- safe_df(df)
  if (!nrow(df)) return(df)
  region <- first_col(df, c("NSS_Region", "region_code_1718", "Region"))
  district <- first_col(df, c("District", "district_only_code_1718", "district"))
  if (!is.null(region) && !is.null(district)) {
    df$district_code_1718 <- paste0(gsub("[^0-9]", "", as.character(df[[region]])), gsub("[^0-9]", "", as.character(df[[district]])))
  }
  df
}

attach_2017_district_names <- function(out, inputs) {
  districts <- safe_df(inputs[["nss1718_districts"]] %||% data.frame())
  if (!nrow(districts) || !"district_code_1718" %in% names(out)) return(out)
  names(districts) <- gsub("[^A-Za-z0-9_]+", "_", names(districts))
  region_col <- first_col(districts, c("region_code_1718", "region_code", "X3"))
  state_col <- first_col(districts, c("state_1718", "state", "X2"))
  district_col <- first_col(districts, c("district_1718", "district", "X6"))
  district_code_col <- first_col(districts, c("district_only_code_1718", "district_only_code", "X7"))
  if (any(vapply(list(region_col, state_col, district_col, district_code_col), is.null, logical(1)))) return(out)
  lookup <- districts[!is.na(districts[[district_col]]) & nzchar(as.character(districts[[district_col]])), c(region_col, state_col, district_col, district_code_col), drop = FALSE]
  names(lookup) <- c("region_code_1718", "state_1718", "district_1718", "district_only_code_1718")
  lookup$region_code_1718 <- zoo_fill_down(as.character(lookup$region_code_1718))
  lookup$district_code_1718 <- paste0(gsub("[^0-9]", "", lookup$region_code_1718), gsub("[^0-9]", "", as.character(lookup$district_only_code_1718)))
  lookup$state_17 <- lookup$state_1718
  lookup$district_17 <- lookup$district_1718
  lookup$state_18 <- lookup$state_1718
  lookup$district_18 <- lookup$district_1718
  merge(out, lookup[c("district_code_1718", "state_1718", "district_1718", "state_17", "district_17", "state_18", "district_18")], by = "district_code_1718", all.x = TRUE)
}

add_legacy_2017_aliases <- function(out) {
  alias <- function(new, old) if (old %in% names(out) && !new %in% names(out)) out[[new]] <<- out[[old]]
  alias("consumption_1718", "consumption_2017")
  alias("gini_consumption_2017", "gini_cons_1718")
  alias("gini_cons_1718", "gini_consumption_2017")
  alias("n_2017", "n")
  alias("npeople_1718", "n")
  if (!"nhouses_1718" %in% names(out) && "n" %in% names(out)) out$nhouses_1718 <- out$n
  out
}

zoo_fill_down <- function(x) {
  last <- NA_character_
  for (i in seq_along(x)) {
    if (is.na(x[[i]]) || !nzchar(x[[i]])) x[[i]] <- last else last <- x[[i]]
  }
  x
}
