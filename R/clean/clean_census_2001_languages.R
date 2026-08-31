# This file is part of the EMI inequality research pipeline.

#' Clean the full Census 2001 district mother-tongue distribution
#'
#' C-16 contains language-group subtotal rows whose codes end in 000 and mutually
#' exclusive mother-tongue rows beneath them. The analytical distribution keeps
#' only the latter and carries the group label down for Shastry's group-level
#' distance classification. No top-n truncation occurs during cleaning.
clean_census_2001_languages <- function(raw) {
  out <- safe_bind_rows(lapply(raw, clean_census_2001_language_file))
  validate_census_2001_language_distribution(out)
}

parse_census_2001_language_file <- function(x) {
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
  if (any(vapply(required, is.null, logical(1)))) return(data.frame())

  out <- data.frame(
    table = plain_chr(x[[table]]),
    state_code = normalize_census_code(x[[state]], 2L),
    district_code = normalize_census_code(x[[district]], 2L),
    tehsil_code = normalize_census_code(x[[tehsil]], 4L),
    area_name = plain_chr(x[[area]]),
    mother_tongue_code = normalize_census_code(x[[mother_tongue_code]], 6L),
    mother_tongue = normalize_census_language_label(x[[mother_tongue]]),
    spkr_tot = num(x[[speakers]]),
    stringsAsFactors = FALSE
  )
  out <- out[
    out$table == "C0116" &
      !is.na(out$state_code) & !is.na(out$district_code) &
      !is.na(out$tehsil_code) & !is.na(out$mother_tongue_code) &
      is.finite(out$spkr_tot),
    , drop = FALSE
  ]
  if (!nrow(out)) return(out)
  out$language_group_code <- substr(out$mother_tongue_code, 1L, 3L)
  out$is_language_group_total <- grepl("000$", out$mother_tongue_code)
  rownames(out) <- NULL
  out
}

clean_census_2001_language_file <- function(x) {
  out <- parse_census_2001_language_file(x)
  if (!nrow(out)) return(clean_census_2001_language_fallback(x))

  out <- out[
    out$district_code != "00" & out$tehsil_code == "0000" &
      grepl("^District - ", out$area_name %||% ""),
    , drop = FALSE
  ]
  if (!nrow(out)) return(out)

  group_rows <- out[out$is_language_group_total, c(
    "state_code", "district_code", "language_group_code", "mother_tongue"
  ), drop = FALSE]
  names(group_rows)[names(group_rows) == "mother_tongue"] <- "canonical_language"
  group_rows <- group_rows[!duplicated(group_rows[c(
    "state_code", "district_code", "language_group_code"
  )]), , drop = FALSE]

  leaves <- out[!out$is_language_group_total, , drop = FALSE]
  leaves <- merge(
    leaves, group_rows,
    by = c("state_code", "district_code", "language_group_code"),
    all.x = TRUE, sort = FALSE
  )
  leaves$state <- leaves$state_code
  leaves$district <- leaves$district_code
  leaves$district_name <- clean_census_area_name(leaves$area_name)
  std(leaves, 2001L)
}

census_2001_state_language_totals <- function(raw) {
  out <- safe_bind_rows(lapply(raw, parse_census_2001_language_file))
  out <- out[
    out$district_code == "00" & out$tehsil_code == "0000" &
      grepl("^State - ", out$area_name %||% "", ignore.case = TRUE) &
      out$is_language_group_total,
    , drop = FALSE
  ]
  if (!nrow(out)) stop("Census 2001 C-16 contains no state language-group totals.", call. = FALSE)
  out <- data.frame(
    state_code = out$state_code,
    native_language_code = out$mother_tongue_code,
    native_language = out$mother_tongue,
    native_speakers = out$spkr_tot,
    stringsAsFactors = FALSE
  )
  key <- paste(out$state_code, out$native_language_code, sep = "|")
  if (anyDuplicated(key)) stop("Census 2001 C-16 state language totals are not unique.", call. = FALSE)
  rownames(out) <- NULL
  out
}

clean_census_2001_language_fallback <- function(x) {
  x <- safe_df(x)
  if (!"district" %in% names(x)) {
    area <- first_col(x, c("area_name", "Area Name", "Name", "Table Name", "...1"))
    if (!is.null(area)) {
      x$district <- gsub("[^[:alpha:] ]+$", "", gsub("\\s*\\d{4}$", "", gsub("^District -\\s*", "", plain_chr(x[[area]]))))
    }
  }
  if (!"state" %in% names(x)) x$state <- NA_character_
  x <- clean_mother_tongue_names(x)
  if (!"canonical_language" %in% names(x) && "mother_tongue" %in% names(x)) {
    x$canonical_language <- x$mother_tongue
  }
  std(x, 2001L)
}

validate_census_2001_language_distribution <- function(df) {
  df <- safe_df(df)
  if (!nrow(df)) return(df)
  required <- c("state_std", "district_std", "mother_tongue_code", "spkr_tot", "canonical_language")
  if (!all(required %in% names(df))) stop("Clean C-16 distribution lacks required district-language fields.", call. = FALSE)
  key <- do.call(paste, c(lapply(df[c("state_std", "district_std", "mother_tongue_code")], plain_chr), sep = "__"))
  if (anyDuplicated(key)) stop("Clean C-16 distribution is not unique by district and mother-tongue code.", call. = FALSE)
  if (any(grepl("000$", df$mother_tongue_code))) stop("C-16 group subtotal rows must not remain in the analytical distribution.", call. = FALSE)
  df
}

clean_census_area_name <- function(x) {
  x <- gsub("^District -\\s*", "", plain_chr(x))
  x <- gsub("\\s*[0-9]{2}\\s*$", "", x)
  x <- gsub("[^[:alpha:]()& ]+$", "", x)
  trimws(gsub("\\s+", " ", x))
}

select_top_mother_tongues <- function(df, n = 3L) {
  df <- safe_df(df)
  if (!nrow(df) || !all(c("district_std", "spkr_tot") %in% names(df))) return(df)
  group_cols <- if ("state_std" %in% names(df) && any(!is.na(df$state_std) & nzchar(df$state_std))) c("state_std", "district_std") else "district_std"
  split_i <- split(seq_len(nrow(df)), interaction(df[group_cols], drop = TRUE))
  keep <- unlist(lapply(split_i, function(i) {
    ordered <- i[order(num(df$spkr_tot[i]), decreasing = TRUE, na.last = NA)]
    ordered[seq_len(min(n, length(ordered)))]
  }), use.names = FALSE)
  df[sort(keep), , drop = FALSE]
}

clean_mother_tongue_names <- function(df) {
  mother_tongue <- first_col(df, c("mother_tongue", "Mother Tongue", "Language", "...7", "...2"))
  if (!is.null(mother_tongue)) {
    df$mother_tongue <- normalize_census_language_label(df[[mother_tongue]])
  }
  df
}
