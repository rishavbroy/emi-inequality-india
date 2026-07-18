# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' build 2017 measures
#'
build_2017_measures <- function(nss_2017_education, cfg) {
  inputs <- as_input_list(nss_2017_education)
  df <- std(safe_df(select_input_frame_2017(inputs, c("nss1718edu_block3", "block3", "block"))), 2017L)
  df <- normalize_2017_district_code(df)
  key <- district_group_vars_2017(df)
  if (!length(key) || !nrow(df)) return(empty_panel())

  value <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
  hh_size <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
  if (is.null(value) || is.null(weight)) return(empty_panel())

  hh_key <- first_col(df, c("HHID", "HH_ID", "household_id"))
  if (!is.null(hh_key)) {
    df$.hh_distinct_key <- paste(do.call(paste, c(df[key], sep = "__")), canon(df[[hh_key]]), sep = "__")
    df <- df[!duplicated(df$.hh_distinct_key), , drop = FALSE]
  }
  if (!is.null(hh_size)) {
    df$consumption_pc_2017 <- num(df[[value]]) / num(df[[hh_size]])
  } else {
    df$consumption_pc_2017 <- num(df[[value]])
  }

  idx <- which(stats::complete.cases(df[key]))
  split_i <- split(idx, interaction(df[idx, key, drop = FALSE], drop = TRUE, sep = "__"))
  out <- safe_bind_rows(lapply(split_i, function(i) {
    w <- num(df[[weight]][i])
    size <- if (!is.null(hh_size)) num(df[[hh_size]][i]) else rep(1, length(i))
    z <- df[i[[1]], key, drop = FALSE]
    z <- z[rep(1L, 1L), , drop = FALSE]
    data.frame(
      z,
      consumption_1718 = wmean(df$consumption_pc_2017[i], w),
      gini_cons_1718 = wgini(df$consumption_pc_2017[i], w),
      npeople_1718 = sum(w * size, na.rm = TRUE),
      nhouses_1718 = sum(w, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out <- attach_2017_district_names(out, inputs)
  if (all(c("state_std", "district_std") %in% names(out))) {
    out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2017L)
  }
  out
}

select_input_frame_2017 <- function(inputs, candidates) {
  inputs <- as_input_list(inputs)
  for (nm in candidates) {
    if (!is.null(inputs[[nm]]) && nrow(safe_df(inputs[[nm]]))) return(inputs[[nm]])
  }
  if (length(inputs) == 1L) return(inputs[[1L]])
  data.frame()
}

district_group_vars_2017 <- function(df) {
  if ("district_code_1718" %in% names(df)) {
    val <- as.character(df$district_code_1718)
    if (any(!is.na(val) & nzchar(val))) return("district_code_1718")
  }
  if (all(c("state_std", "district_std") %in% names(df))) return(c("state_std", "district_std"))
  character()
}



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
  districts <- parse_2017_district_lookup(inputs)
  if (!nrow(districts) || !"district_code_1718" %in% names(out)) return(out)
  merged <- merge(out, districts, by = "district_code_1718", all.x = TRUE, sort = FALSE)
  if ("state_1718" %in% names(merged)) merged$state_std <- canonicalize_state_name(merged$state_1718)
  if ("district_1718" %in% names(merged)) merged$district_std <- canonicalize_district_name(merged$district_1718)
  merged
}

parse_2017_district_lookup <- function(inputs) {
  districts <- safe_df(inputs[["nss1718_districts"]] %||% data.frame())
  if (!nrow(districts)) return(data.frame())

  names(districts) <- gsub("[^A-Za-z0-9_]+", "_", names(districts))
  region_col <- first_col(districts, c("region_code_1718", "region_code", "X3"))
  state_col <- first_col(districts, c("state_1718", "state", "X2"))
  district_col <- first_col(districts, c("district_1718", "district", "X6"))
  district_code_col <- first_col(districts, c("district_only_code_1718", "district_only_code", "X7"))

  # The checked-in Tabula CSV is headerless. readr treats the first Andaman row
  # as column names unless callers pass col_names, so recover that row here.
  if (any(vapply(list(region_col, state_col, district_col, district_code_col), is.null, logical(1)))) {
    raw_names <- names(districts)
    if (length(raw_names) >= 7L) {
      first <- stats::setNames(as.list(raw_names[seq_len(7L)]), paste0("V", seq_len(7L)))
      names(districts) <- paste0("V", seq_along(districts))
      districts <- rbind(as.data.frame(first, stringsAsFactors = FALSE), districts[seq_len(min(7L, ncol(districts)))])
      names(districts) <- c("state_serial", "state_1718_raw", "region_code_1718", "region_1718", "district_serial", "district_1718", "district_only_code_1718")
      region_col <- "region_code_1718"
      state_col <- "state_1718_raw"
      district_col <- "district_1718"
      district_code_col <- "district_only_code_1718"
    }
  }

  if (any(vapply(list(region_col, district_col, district_code_col), is.null, logical(1)))) return(data.frame())
  lookup <- districts[
    !is.na(districts[[district_col]]) & nzchar(as.character(districts[[district_col]])),
    c(region_col, district_col, district_code_col, state_col),
    drop = FALSE
  ]
  names(lookup) <- c("region_code_1718", "district_1718", "district_only_code_1718", "state_1718_raw")
  lookup$region_code_1718 <- zoo_fill_down(as.character(lookup$region_code_1718))
  lookup$district_only_code_1718 <- gsub("[()]", "", as.character(lookup$district_only_code_1718))
  lookup$district_code_1718 <- paste0(gsub("[^0-9]", "", lookup$region_code_1718), gsub("[^0-9]", "", lookup$district_only_code_1718))
  lookup$state_code_1718 <- substr(lookup$district_code_1718, 1, 2)

  states <- parse_2017_state_lookup(inputs)
  if (nrow(states)) lookup <- merge(lookup, states, by = "state_code_1718", all.x = TRUE, sort = FALSE)
  if (!"state_1718" %in% names(lookup)) lookup$state_1718 <- zoo_fill_down(as.character(lookup$state_1718_raw))
  lookup$state_17 <- lookup$state_1718
  lookup$district_17 <- lookup$district_1718
  lookup$state_18 <- lookup$state_1718
  lookup$district_18 <- lookup$district_1718
  lookup[!duplicated(lookup$district_code_1718), c("district_code_1718", "state_1718", "district_1718", "state_17", "district_17", "state_18", "district_18")]
}

parse_2017_state_lookup <- function(inputs) {
  states <- safe_df(inputs[["nss1718_state_codes"]] %||% data.frame())
  if (!nrow(states)) return(data.frame())
  state_col <- first_col(states, c("State_UT_name", "State.UT.name", "state_1718", "state", "X1"))
  code_col <- first_col(states, c("code", "state_code_1718", "state_code", "X2"))
  if (is.null(state_col) || is.null(code_col)) return(data.frame())
  out <- data.frame(
    state_code_1718 = sprintf("%02d", as.integer(num(states[[code_col]]))),
    state_1718 = normalize_2017_state_name(states[[state_col]]),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$state_code_1718) & nzchar(out$state_1718), , drop = FALSE]
}

normalize_2017_state_name <- function(x) {
  out <- as.character(x)
  out[canon(out) == canon("A & N Islands")] <- "Andaman & Nicobar Islands"
  out
}


zoo_fill_down <- function(x) {
  last <- NA_character_
  for (i in seq_along(x)) {
    if (is.na(x[[i]]) || !nzchar(x[[i]])) x[[i]] <- last else last <- x[[i]]
  }
  x
}
