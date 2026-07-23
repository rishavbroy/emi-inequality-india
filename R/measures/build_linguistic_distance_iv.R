# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build linguistic distance iv
#'
build_linguistic_distance_iv <- function(census_2001_languages, cfg) {
  df <- std(safe_df(census_2001_languages), 2001L)
  if (!all(c("state_std", "district_std") %in% names(df)) || !nrow(df)) {
    return(linguistic_distance_out_of_pipeline("No Census 2001 district identifiers were available."))
  }

  value <- first_col(df, c("ling_distance", "ling_degrees", "wavg_ling_degrees", "distance_from_hindi", "linguistic_distance"))
  if (is.null(value)) {
    return(linguistic_distance_out_of_pipeline("No real linguistic-distance column could be identified."))
  }
  population <- first_col(df, c("spkr_tot", "speakers", "population", "tot_p"))
  out <- bydist(df, value, population, "wavg_ling_degrees")
  if (!nrow(out)) return(linguistic_distance_out_of_pipeline("No district-level linguistic-distance rows could be computed."))
  names_df <- unique(df[intersect(c("state_std", "district_std", "state", "district", "state_code", "district_code", "district_name"), names(df))])
  if (all(c("state_std", "district_std") %in% names(names_df))) {
    out <- merge(out, names_df, by = c("state_std", "district_std"), all.x = TRUE)
  }
  state_code <- first_col(out, c("state_code", "state"))
  if (!is.null(state_code)) out$state_01 <- census_2001_state_name(out[[state_code]])
  if ("district_name" %in% names(out)) out$district_01 <- out$district_name
  if (!"district_01" %in% names(out) && "district" %in% names(out)) out$district_01 <- out$district
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2001L)
  validate_linguistic_distance_ranges(out)
}



#' validate linguistic distance ranges
#'
validate_linguistic_distance_ranges <- function(df) {
  if ("wavg_ling_degrees" %in% names(df) && any(df$wavg_ling_degrees < 0 | df$wavg_ling_degrees > 5, na.rm = TRUE)) {
    stop("Linguistic-distance values must be in the 0-5 range.", call. = FALSE)
  }
  df
}

linguistic_distance_out_of_pipeline <- function(reason) {
  data.frame(
    status = "out_of_active_pipeline",
    reason = reason,
    stringsAsFactors = FALSE
  )
}
