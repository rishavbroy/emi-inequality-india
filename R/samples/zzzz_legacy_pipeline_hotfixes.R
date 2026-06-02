# Hotfix layer sourced after zzz_legacy_pipeline_impl.R.
# These overrides keep the draft pipeline from failing when a raw/cleaned source
# has no recognizable district rows. That situation should produce a typed empty
# key table, not a low-level replacement-length error.

empty_district_keys <- function() {
  data.frame(
    state_std = character(),
    district_std = character(),
    source_year = integer(),
    district_key = character(),
    stringsAsFactors = FALSE
  )
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

build_district_keys_2001 <- function(census_2001_languages) {
  key_df(census_2001_languages, 2001L)
}

build_district_keys_2007 <- function(nss_2007_education, nss_2007_consumption = NULL) {
  unique(safe_bind_rows(lapply(c(nss_2007_education, nss_2007_consumption), key_df, year = 2007L)))
}

build_district_keys_2017 <- function(nss_2017_education) {
  unique(safe_bind_rows(lapply(nss_2017_education, key_df, year = 2017L)))
}

build_district_keys_2020 <- function(boundaries_2020) {
  key_df(boundaries_2020, 2020L)
}
