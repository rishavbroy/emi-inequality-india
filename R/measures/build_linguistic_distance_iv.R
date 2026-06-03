# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build linguistic distance iv
#'
#' @return A tibble, model object, list, or file path depending on context.
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
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2001L)
  out
}

#' compute population weighted linguistic distance
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_population_weighted_linguistic_distance <- function(df) {
  build_linguistic_distance_iv(df, list())
}

#' compute linguistic distance variants
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_linguistic_distance_variants <- function(df) {
  df
}

#' demean iv within state
#'
#' @return A tibble, model object, list, or file path depending on context.
demean_iv_within_state <- function(df) {
  df
}

#' validate linguistic distance ranges
#'
#' @return A tibble, model object, list, or file path depending on context.
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
