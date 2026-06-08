# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' canonicalize district name
#'
canonicalize_district_name <- function(x) {
  canon(x)
}

#' canonicalize state name
#'
canonicalize_state_name <- function(x) {
  canonicalize_district_name(x)
}

#' make district key
#'
make_district_key <- function(state, district, year) {
  paste(year, canonicalize_state_name(state), canonicalize_district_name(district), sep = "__")
}

#' build district keys 2001
#'
build_district_keys_2001 <- function(census_2001_languages) {
  key_df(census_2001_languages, 2001L)
}

#' build district keys 2007
#'
build_district_keys_2007 <- function(nss_2007_education, nss_2007_consumption = NULL) {
  unique(safe_bind_rows(lapply(c(as_input_list(nss_2007_education), as_input_list(nss_2007_consumption)), key_df, year = 2007L)))
}

#' build district keys 2017
#'
build_district_keys_2017 <- function(nss_2017_education) {
  unique(safe_bind_rows(lapply(as_input_list(nss_2017_education), key_df, year = 2017L)))
}

#' build district keys 2020
#'
build_district_keys_2020 <- function(boundaries_2020) {
  key_df(boundaries_2020, 2020L)
}

empty_district_keys <- function() {
  data.frame(
    state_std = character(),
    district_std = character(),
    source_year = integer(),
    district_key = character(),
    stringsAsFactors = FALSE
  )
}

as_input_list <- function(x) {
  if (is.null(x)) return(list())
  if (inherits(x, "data.frame")) return(list(x))
  x
}

key_df <- function(df, year) {
  df <- std(df, year)

  if (!all(c("state_std", "district_std") %in% names(df))) {
    return(empty_district_keys())
  }

  out <- unique(df[c("state_std", "district_std")])
  out <- out[!is.na(out$district_std) & nzchar(out$district_std), , drop = FALSE]

  if (!nrow(out)) {
    return(empty_district_keys())
  }

  out$source_year <- rep(as.integer(year), nrow(out))
  out$district_key <- make_district_key(out$state_std, out$district_std, year)
  out
}
