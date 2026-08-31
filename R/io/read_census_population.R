# Census population denominators used by harmonized administrative-count measures.

read_shrug_pca_2011_district <- function(path) {
  read_shrug_district_archive(
    path,
    "pc11_pca_clean_pc11dist.csv",
    "SHRUG Census-2011 PCA archive"
  )
}

clean_shrug_pca_2011_population <- function(x) {
  x <- safe_df(x)
  required <- c("pc11_state_id", "pc11_district_id", "pc11_pca_tot_p")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "SHRUG Census-2011 PCA is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  state_code <- normalize_census_code(x$pc11_state_id, 2L)
  district_code <- normalize_census_code(x$pc11_district_id, 3L)
  name_hit <- names(x)[grepl("district name", canon(names(x)), fixed = TRUE)]
  district_name <- if (length(name_hit)) {
    trimws(plain_chr(x[[name_hit[[1L]]]]))
  } else {
    paste(state_code, district_code, sep = "/")
  }
  out <- data.frame(
    state_code = state_code,
    district_code = district_code,
    district_name = district_name,
    population_total_2011 = num(x$pc11_pca_tot_p),
    stringsAsFactors = FALSE
  )
  if (nrow(out) != 640L) {
    stop("SHRUG Census-2011 PCA must contain exactly 640 district rows.", call. = FALSE)
  }
  if (any(!stats::complete.cases(out[c("state_code", "district_code")])) ||
      anyDuplicated(out[c("state_code", "district_code")])) {
    stop("SHRUG Census-2011 PCA district keys must be complete and unique.", call. = FALSE)
  }
  if (any(!is.finite(out$population_total_2011)) || any(out$population_total_2011 <= 0)) {
    stop("SHRUG Census-2011 PCA district populations must be finite and positive.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

read_census_2011_district_population <- function(path) {
  clean_shrug_pca_2011_population(read_shrug_pca_2011_district(path))
}
