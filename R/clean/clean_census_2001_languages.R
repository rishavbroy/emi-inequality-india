# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean census 2001 languages
#'
clean_census_2001_languages <- function(raw) {
  out <- safe_bind_rows(lapply(raw, function(x) {
    clean_census_2001_language_file(x)
  }))
  select_top_mother_tongues(out)
}

clean_census_2001_language_file <- function(x) {
  x <- safe_df(x)

  table <- first_col(x, c("table", "TABLE", "C-16 POPULATION BY MOTHER TONGUE", "...1"))
  state <- first_col(x, c("state_code", "STATE", "state", "...2"))
  district <- first_col(x, c("district_code", "DISTRICT", "district", "...3"))
  tehsil <- first_col(x, c("tehsil_code", "TEHSIL", "tehsil", "...4"))
  area <- first_col(x, c("area_name", "AREA NAME", "Area Name", "Name", "...5"))
  mother_tongue_code <- first_col(x, c("mother_tongue_code", "MOTHER TONGUE CODE", "...6"))
  mother_tongue <- first_col(x, c("mother_tongue", "MOTHER TONGUE", "Mother Tongue", "Language", "...7"))
  speakers <- first_col(x, c("spkr_tot", "TOTAL", "population", "speakers", "...8"))

  required <- list(table, state, district, tehsil, area, mother_tongue_code, mother_tongue, speakers)
  if (any(vapply(required, is.null, logical(1)))) return(clean_census_2001_language_fallback(x))

  out <- data.frame(
    table = as.character(x[[table]]),
    state_code = census_code(x[[state]], 2L),
    district_code = census_code(x[[district]], 2L),
    tehsil_code = census_code(x[[tehsil]], 4L),
    area_name = as.character(x[[area]]),
    mother_tongue_code = census_code(x[[mother_tongue_code]], 6L),
    mother_tongue = as.character(x[[mother_tongue]]),
    spkr_tot = num(x[[speakers]]),
    stringsAsFactors = FALSE
  )

  out <- out[
    out$table == "C0116" &
      !is.na(out$state_code) &
      !is.na(out$district_code) &
      out$district_code != "00" &
      out$tehsil_code == "0000" &
      grepl("^District - ", out$area_name %||% "") &
      grepl("0$", out$mother_tongue_code %||% "") &
      is.finite(out$spkr_tot),
    ,
    drop = FALSE
  ]

  if (!nrow(out)) return(out)

  out$state <- out$state_code
  out$district <- out$district_code
  out$district_name <- clean_census_area_name(out$area_name)
  out <- clean_mother_tongue_names(out)
  # Create the ling_distance column based on the mother_tongue values and @shastry2012a's 0-5 measure of degrees of linguistic distance
  out$ling_degrees <- linguistic_distance_degrees(out$mother_tongue)
  std(out, 2001L)
}

clean_census_2001_language_fallback <- function(x) {
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
  x <- clean_mother_tongue_names(x)
  if ("mother_tongue" %in% names(x)) {
    x$ling_degrees <- linguistic_distance_degrees(x$mother_tongue)
  }
  std(x, 2001L)
}

census_code <- function(x, width) {
  raw <- gsub("[^0-9]", "", as.character(x))
  raw[!nzchar(raw)] <- NA_character_
  ifelse(is.na(raw), NA_character_, sprintf(paste0("%0", width, "d"), as.integer(raw)))
}

clean_census_area_name <- function(x) {
  x <- gsub("^District -\\s*", "", as.character(x))
  x <- gsub("\\s*[0-9]{2}\\s*$", "", x)
  x <- gsub("[^[:alpha:]()& ]+$", "", x)
  trimws(gsub("\\s+", " ", x))
}

linguistic_distance_degrees <- function(mother_tongue) {
  x <- tools::toTitleCase(tolower(as.character(mother_tongue)))
  out <- rep(5, length(x))
  out[x %in% c("Hindi", "Urdu")] <- 0
  out[x %in% c("Gujarati", "Punjabi", "Rajasthani")] <- 1
  out[x %in% c("Konkani", "Marathi")] <- 2
  out[x %in% c("Assamese", "Bengali", "Bihari", "Oriya", "Odia")] <- 3
  out[x %in% c("Kashmiri", "Sindhi", "Sinhalese")] <- 4
  out
}

select_top_mother_tongues <- function(df, n = 3L) {
  df <- safe_df(df)
  if (!nrow(df) || !all(c("district_std", "spkr_tot") %in% names(df))) return(df)
  group_cols <- if ("state_std" %in% names(df) && any(!is.na(df$state_std) & nzchar(df$state_std))) {
    c("state_std", "district_std")
  } else {
    "district_std"
  }
  split_i <- split(seq_len(nrow(df)), interaction(df[group_cols], drop = TRUE))
  keep <- unlist(lapply(split_i, function(i) {
    i[order(num(df$spkr_tot[i]), decreasing = TRUE, na.last = NA)][seq_len(min(n, length(i)))]
  }), use.names = FALSE)
  df[sort(keep), , drop = FALSE]
}

#' clean mother tongue names
#'
clean_mother_tongue_names <- function(df) {
  mother_tongue <- first_col(df, c("mother_tongue", "Mother Tongue", "Language", "...7", "...2"))
  if (!is.null(mother_tongue)) {
    df$mother_tongue <- tools::toTitleCase(tolower(gsub("^\\d{1,3}\\s+", "", as.character(df[[mother_tongue]]))))
  }
  df
}
