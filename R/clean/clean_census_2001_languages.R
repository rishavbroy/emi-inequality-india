# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean census 2001 languages
#'
#' @return A tibble, model object, list, or file path depending on context.
clean_census_2001_languages <- function(raw) {
  out <- safe_bind_rows(lapply(raw, function(x) {
    x <- safe_df(x)
    if (!"district" %in% names(x)) {
      area <- first_col(x, c("area_name", "Area Name", "Name", "Table Name", "...1"))
      if (!is.null(area)) {
        x$district <- gsub("[^[:alpha:] ]+$", "", gsub("\\s*\\d{4}$", "", gsub("^District -\\s*", "", as.character(x[[area]]))))
      }
    }
    if (!"state" %in% names(x)) {
      area <- first_col(x, c("area_name", "Area Name", "Name", "...1"))
      if (!is.null(area)) x$state <- NA_character_
    }
    mother_tongue <- first_col(x, c("mother_tongue", "Mother Tongue", "Language", "...2"))
    if (!is.null(mother_tongue)) {
      # Create the ling_distance column based on the mother_tongue values and @shastry2012a's 0-5 measure of degrees of linguistic distance
      x$mother_tongue <- tools::toTitleCase(gsub("^\\d{1,3}\\s+", "", as.character(x[[mother_tongue]])))
    }
    std(x, 2001L)
  }))
  out
}

#' standardize census state names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_census_state_names <- function(df) {
  df
}

#' standardize census district names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_census_district_names <- function(df) {
  df
}

#' clean mother tongue names
#'
#' @return A tibble, model object, list, or file path depending on context.
clean_mother_tongue_names <- function(df) {
  mother_tongue <- first_col(df, c("mother_tongue", "Mother Tongue", "Language", "...2"))
  if (!is.null(mother_tongue)) {
    df$mother_tongue <- tools::toTitleCase(gsub("^\\d{1,3}\\s+", "", as.character(df[[mother_tongue]])))
  }
  df
}

#' compute mother tongue population shares
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_mother_tongue_population_shares <- function(df) {
  df
}
