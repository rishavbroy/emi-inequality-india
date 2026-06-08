# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build selection data
#'
build_selection_data <- function(nss_2007_education, district_keys_2007, cfg) {
  blocks <- as_input_list(nss_2007_education)
  b4 <- std(safe_df(blocks[["nss0708edu_block4"]] %||% data.frame()), 2007L)
  b5 <- std(safe_df(blocks[["nss0708edu_block5"]] %||% data.frame()), 2007L)
  b6 <- std(safe_df(blocks[["nss0708edu_block6"]] %||% data.frame()), 2007L)
  if (!nrow(b4) || !"PID" %in% names(b4) || !"AGE" %in% names(b4)) {
    df <- std(safe_bind_rows(lapply(blocks, safe_df)), 2007L)
    if (!"enrolled" %in% names(df)) df$enrolled <- NA_real_
    return(df)
  }

  kids <- b4[num(b4$AGE) >= 5 & num(b4$AGE) <= 19, , drop = FALSE]
  enrolled_ids <- unique(c(as.character(b5$PID %||% character()), as.character(b6$PID %||% character())))
  kids$enrolled <- as.integer(as.character(kids$PID) %in% enrolled_ids)

  keep_b5 <- intersect(
    c(
      "PID", "IS_EDU_FREE", "TUTION_FEE_WAIVED", "RECD_SCHOLARSHIP_STIPEND",
      "RECD_TXT_BOOKS", "RECD_STATIONERY", "MID_DAY_MEAL_ETC_RECD"
    ),
    names(b5)
  )
  keep_b6 <- intersect(
    c("PID", "TUTION_FEE", "EXAMINATION_FEE", "OTHER_FEES_PAYMENTS", "BOOKS", "STATIONERY", "UNIFORM", "TRANSPORT"),
    names(b6)
  )
  if (length(keep_b5) > 1L) kids <- merge(kids, collapse_to_unique_key(b5[keep_b5], "PID"), by = "PID", all.x = TRUE, suffixes = c("", "_b5"))
  if (length(keep_b6) > 1L) kids <- merge(kids, collapse_to_unique_key(b6[keep_b6], "PID"), by = "PID", all.x = TRUE, suffixes = c("", "_b6"))

  kids$AGE <- num(kids$AGE)
  kids$HH_SIZE <- num(kids$HH_SIZE)
  kids$SEX <- factor(num(kids$SEX), levels = c(1, 2), labels = c("Male", "Female"))
  kids$RELIGION <- factor(num(kids$RELIGION), levels = 1:8, labels = c("Hindu", "Muslim", "Christian", "Sikh", "Jain", "Buddhist", "Zoroastrian", "Other"))
  kids$SOCIAL_GROUP <- factor(num(kids$SOCIAL_GROUP), levels = c(1, 2, 3, 9), labels = c("Scheduled Tribe", "Scheduled Caste", "Other Backward Class", "Other"))
  kids$SECTOR <- factor(num(kids$SECTOR), levels = c(1, 2), labels = c("Rural", "Urban"))
  kids$DIST_FROM_NEAREST_PRIMARY_CLASS <- factor(num(kids$DIST_FROM_NEAREST_PRIMARY_CLASS))

  for (nm in c("IS_EDU_FREE", "TUTION_FEE_WAIVED", "RECD_SCHOLARSHIP_STIPEND", "RECD_TXT_BOOKS", "RECD_STATIONERY", "MID_DAY_MEAL_ETC_RECD")) {
    if (nm %in% names(kids)) kids[[paste0("num_", nm)]] <- as.numeric(num(kids[[nm]]) %in% c(1, 2))
  }
  for (nm in c("TUTION_FEE", "EXAMINATION_FEE", "OTHER_FEES_PAYMENTS", "BOOKS", "STATIONERY", "UNIFORM", "TRANSPORT")) {
    if (nm %in% names(kids)) kids[[nm]] <- num(kids[[nm]])
  }
  fee_cols <- intersect(c("TUTION_FEE", "EXAMINATION_FEE", "OTHER_FEES_PAYMENTS", "BOOKS", "STATIONERY", "UNIFORM", "TRANSPORT"), names(kids))
  if (length(fee_cols)) kids$ENROLLMENT_COST <- rowSums(kids[fee_cols], na.rm = TRUE)
  if ("ENROLLMENT_COST" %in% names(kids)) kids$num_ENROLLMENT_COST <- kids$ENROLLMENT_COST

  enrolled_only <- intersect(
    c(
      "num_IS_EDU_FREE", "num_TUTION_FEE_WAIVED", "num_RECD_SCHOLARSHIP_STIPEND",
      "num_RECD_TXT_BOOKS", "num_RECD_STATIONERY", "num_MID_DAY_MEAL_ETC_RECD",
      "num_ENROLLMENT_COST"
    ),
    names(kids)
  )
  if (length(enrolled_only) && all(c("state_std", "district_std") %in% names(kids))) {
    means <- district_enrolled_means(kids, enrolled_only)
    kids <- merge(kids, means, by = c("state_std", "district_std"), all.x = TRUE)
  }
  kids
}

#' construct child level selection sample
#'
construct_child_level_selection_sample <- function(df) {
  df
}

#' construct household covariates
#'
construct_household_covariates <- function(df) {
  df
}

#' construct district level context
#'
construct_district_level_context <- function(df) {
  df
}

#' define probit variables
#'
define_probit_variables <- function(df) {
  if (!"enrolled" %in% names(df)) df$enrolled <- NA_real_
  df
}

#' apply selection sample restrictions
#'
apply_selection_sample_restrictions <- function(df) {
  df
}


collapse_to_unique_key <- function(df, key) {
  if (!key %in% names(df) || !nrow(df)) return(df)
  df[!duplicated(df[[key]]), , drop = FALSE]
}

district_enrolled_means <- function(df, vars) {
  enrolled <- !is.na(df$enrolled) & df$enrolled == 1
  idx <- which(enrolled)
  if (!length(idx)) return(df[0, c("state_std", "district_std"), drop = FALSE])
  weight <- if ("weight" %in% names(df)) num(df$weight) else rep(1, nrow(df))
  groups <- interaction(df[idx, c("state_std", "district_std"), drop = TRUE], drop = TRUE)
  split_i <- split(idx, groups)
  safe_bind_rows(lapply(split_i, function(i) {
    z <- df[i[1], c("state_std", "district_std"), drop = FALSE]
    for (nm in vars) z[[paste0("dmean_", nm)]] <- wmean(df[[nm]][i], weight[i])
    z
  }))
}
