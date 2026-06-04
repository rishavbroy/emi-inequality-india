# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build 2007 measures
#'
#' @return A tibble, model object, list, or file path depending on context.
build_2007_measures <- function(nss_2007_education, nss_2007_consumption, selection_data, ame_results, cfg) {
  edu <- std(safe_bind_rows(lapply(as_input_list(nss_2007_education), safe_df)), 2007L)
  if (!all(c("state_std", "district_std") %in% names(edu))) return(empty_panel())

  weight <- first_col(edu, c("weight", "WEIGHT", "multiplier"))
  emi <- first_col(edu, c("EMI", "emie", "MEDIUM_INSTRUCTION", "medium_instruction", "medium"))
  out <- unique(edu[c("state_std", "district_std")])
  out <- out[!is.na(out$district_std) & nzchar(out$district_std), , drop = FALSE]
  if (!is.null(emi)) {
    out <- merge(
      out,
      compute_emie_2007(edu),
      all.x = TRUE
    )
  }
  out <- merge_measure(out, compute_consumption_2007(safe_bind_rows(lapply(as_input_list(nss_2007_consumption), safe_df))))
  out <- merge_measure(out, compute_gini_consumption_2007(safe_bind_rows(lapply(as_input_list(nss_2007_consumption), safe_df))))
  out <- merge_measure(out, compute_baseline_controls_2007(edu))
  out <- merge_measure(out, compute_housing_controls_2007(safe_bind_rows(lapply(as_input_list(nss_2007_consumption), safe_df))))
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  out
}

merge_measure <- function(x, y) {
  y <- safe_df(y)
  if (!nrow(y) || !all(c("state_std", "district_std") %in% names(y))) return(x)
  duplicate <- intersect(setdiff(names(y), c("state_std", "district_std")), names(x))
  y <- y[setdiff(names(y), duplicate)]
  merge(x, y, by = c("state_std", "district_std"), all.x = TRUE)
}

#' compute emie 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_emie_2007 <- function(df) {
  df <- std(df, 2007L)
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  emi <- first_col(df, c("EMI", "emie", "MEDIUM_INSTRUCTION", "medium_instruction", "medium"))
  if (is.null(emi)) return(empty_panel())
  age <- first_col(df, c("AGE", "age"))
  if (!is.null(age)) {
    keep <- is.finite(num(df[[age]])) & num(df[[age]]) <= 19
    df <- df[keep, , drop = FALSE]
  }
  bydist(df, emi, weight, "emie_2007", function(x, w) {
    100 * wmean(english_medium_indicator(x, emi), w)
  })
}

english_medium_indicator <- function(x, column_name) {
  if (grepl("medium", column_name, ignore.case = TRUE)) {
    code <- gsub("[^0-9]", "", as.character(x))
    return(as.numeric(code %in% c("2", "02")))
  }
  as.numeric(num(x) > 0)
}

#' compute enrollment share 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_enrollment_share_2007 <- function(df) {
  df
}

#' compute consumption 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_consumption_2007 <- function(df) {
  df <- std(df, 2007L)
  df <- normalize_2007_consumption_district_key(df)
  value <- first_col(df, c("MPCE_Value", "MPCE", "mpce", "consumption", "hh_cons", "TOTAL"))
  hh_size <- first_col(df, c("HH_Size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("Multiplier", "weight", "WEIGHT", "multiplier"))
  if (identical(value, "TOTAL") && !is.null(hh_size)) {
    df$consumption_pc_2007 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2007"
  }
  bydist(df, value, weight, "consumption_2007")
}

#' compute gini consumption 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_gini_consumption_2007 <- function(df) {
  df <- std(df, 2007L)
  df <- normalize_2007_consumption_district_key(df)
  value <- first_col(df, c("MPCE_Value", "MPCE", "mpce", "consumption", "hh_cons", "TOTAL"))
  hh_size <- first_col(df, c("HH_Size", "HH_SIZE", "household_size"))
  weight <- first_col(df, c("Multiplier", "weight", "WEIGHT", "multiplier"))
  if (identical(value, "TOTAL") && !is.null(hh_size)) {
    df$consumption_pc_2007 <- num(df[[value]]) / num(df[[hh_size]])
    value <- "consumption_pc_2007"
  }
  bydist(df, value, weight, "gini_consumption_2007", wgini)
}

#' compute baseline controls 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_baseline_controls_2007 <- function(df) {
  df <- std(df, 2007L)
  if (!all(c("state_std", "district_std") %in% names(df))) return(empty_panel())
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  split_i <- split(seq_len(nrow(df)), interaction(df[c("state_std", "district_std")], drop = TRUE))
  safe_bind_rows(lapply(split_i, function(i) {
    z <- df[i[1], c("state_std", "district_std"), drop = FALSE]
    w <- if (!is.null(weight)) num(df[[weight]][i]) else rep(1, length(i))
    valid_w <- is.finite(w) & w > 0
    total_w <- sum(w[valid_w], na.rm = TRUE)
    weighted_share <- function(condition) {
      condition <- as.logical(condition)
      if (!is.finite(total_w) || total_w == 0) return(NA_real_)
      100 * sum(w[valid_w] * condition[valid_w], na.rm = TRUE) / total_w
    }
    age <- num(df$AGE[i] %||% NA)
    sex <- num(df$SEX[i] %||% NA)
    relation <- num(df$RELATION_TO_HEAD[i] %||% NA)
    edu <- num(df$EDUCATION_LEVEL[i] %||% NA)
    land <- sprintf("%02d", as.integer(num(df$LAND_POSSESSED_CODE[i] %||% NA)))
    z$pct_urban <- weighted_share(num(df$SECTOR[i] %||% NA) == 2)
    z$avg_hh_size <- wmean(df$HH_SIZE[i] %||% NA, w)
    z$dependency_ratio <- {
      dep <- is.finite(age) & (age <= 14 | age >= 65)
      work <- is.finite(age) & age >= 15 & age <= 64
      denom <- sum(w[valid_w] * work[valid_w], na.rm = TRUE)
      if (denom > 0) 100 * sum(w[valid_w] * dep[valid_w], na.rm = TRUE) / denom else NA_real_
    }
    z$pct_fem_head <- weighted_share(sex == 2 & relation == 1)
    z$pct_hindu <- weighted_share(num(df$RELIGION[i] %||% NA) == 1)
    z$pct_muslim <- weighted_share(num(df$RELIGION[i] %||% NA) == 2)
    z$pct_other_religion <- weighted_share(num(df$RELIGION[i] %||% NA) %in% c(3, 4, 5, 6, 7, 8))
    z$pct_st <- weighted_share(num(df$SOCIAL_GROUP[i] %||% NA) == 1)
    z$pct_sc <- weighted_share(num(df$SOCIAL_GROUP[i] %||% NA) == 2)
    z$pct_obc <- weighted_share(num(df$SOCIAL_GROUP[i] %||% NA) == 3)
    z$pct_small_land <- weighted_share(land %in% c("02", "03", "04"))
    z$pct_medium_land <- weighted_share(land %in% c("05", "06", "07"))
    z$pct_large_land <- weighted_share(land %in% c("08", "10", "11", "12"))
    heads <- relation == 1
    head_w <- sum(w[valid_w] * heads[valid_w], na.rm = TRUE)
    head_share <- function(condition) {
      if (!is.finite(head_w) || head_w == 0) return(NA_real_)
      100 * sum(w[valid_w] * heads[valid_w] * condition[valid_w], na.rm = TRUE) / head_w
    }
    z$pct_head_illiterate <- head_share(edu == 1)
    z$pct_head_lit_to_primary <- head_share(edu >= 2 & edu <= 7)
    z$pct_head_secondary_plus <- head_share(edu >= 8)
    z$head_secondary_plus_2007 <- z$pct_head_secondary_plus
    z$region <- df$REGION[i][[1]] %||% NA
    z
  }))
}

#' compute education freebies ivs 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_education_freebies_ivs_2007 <- function(df) {
  df
}

#' compute housing controls 2007
#'
#' @return District-level housing controls from the 2007 consumption household file.
compute_housing_controls_2007 <- function(df) {
  df <- std(df, 2007L)
  df <- normalize_2007_consumption_district_key(df)
  type <- first_col(df, c("Type_of_structure", "type_of_structure"))
  weight <- first_col(df, c("Multiplier", "weight", "WEIGHT", "multiplier"))
  if (is.null(type)) return(empty_panel())
  bydist(df, type, weight, "pucca_share_2007", function(x, w) 100 * wmean(as.numeric(num(x) == 1), w))
}

normalize_2007_consumption_district_key <- function(df) {
  district <- first_col(df, c("District", "district"))
  if (is.null(district) || !"district_std" %in% names(df)) return(df)
  raw <- gsub("[^0-9]", "", as.character(df[[district]]))
  use_tail <- nchar(raw) >= 5
  df$district_std[use_tail] <- substring(raw[use_tail], nchar(raw[use_tail]) - 1L)
  df
}
