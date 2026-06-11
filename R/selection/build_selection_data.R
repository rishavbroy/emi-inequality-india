# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# The active selection-data builder intentionally mirrors the legacy Rmd's
# child-level sample construction.  The legacy model used NSS Block 5 and Block
# 6 schooling/expenditure records for enrolled children, then full-joined Block
# 4 demographics for every 5--19 year-old.  Joining only by PID is not enough:
# PID is not globally unique, and the legacy Rmd joined by household, FSU,
# district, state, stratum, substratum, and weight as well.

#' build selection data
#'
build_selection_data <- function(nss_2007_education, district_keys_2007, cfg) {
  blocks <- as_input_list(nss_2007_education)
  b3 <- std(safe_df(blocks[["nss0708edu_block3"]] %||% data.frame()), 2007L)
  b4 <- std(safe_df(blocks[["nss0708edu_block4"]] %||% data.frame()), 2007L)
  b5 <- std(safe_df(blocks[["nss0708edu_block5"]] %||% data.frame()), 2007L)
  b6 <- std(safe_df(blocks[["nss0708edu_block6"]] %||% data.frame()), 2007L)

  if (!nrow(b4) || !"PID" %in% names(b4) || !"AGE" %in% names(b4)) {
    df <- std(safe_bind_rows(lapply(blocks, safe_df)), 2007L)
    if (!"enrolled" %in% names(df)) df$enrolled <- NA_real_
    return(df)
  }

  b4 <- normalize_selection_identifiers(b4)
  b5 <- normalize_selection_identifiers(b5)
  b6 <- normalize_selection_identifiers(b6)
  b3 <- normalize_selection_identifiers(b3)

  # Legacy enrolled-child Block 5 data.
  selection_df <- b5[is.finite(num(b5$AGE)) & num(b5$AGE) <= 19, , drop = FALSE]
  selection_df$enrolled <- 1L
  selection_df <- data.frame(
    PID = selection_df$PID,
    district_code_0708 = selection_df$district_code_0708,
    enrolled = selection_df$enrolled,
    weight = selection_df$weight,
    FSU_SL_NO = selection_df$FSU_SL_NO,
    HHID = selection_df$HHID,
    STATE = selection_df$STATE,
    STRATUM = selection_df$STRATUM,
    SUB_STRATUM_NO = selection_df$SUB_STRATUM_NO,
    IS_EDU_FREE = legacy_yes_no(selection_df$IS_EDU_FREE, yes = c(1), no = c(2)),
    TUTION_FEE_WAIVED = legacy_yes_no(selection_df$TUTION_FEE_WAIVED, yes = c(1, 2), no = c(3, NA)),
    RECD_SCHOLARSHIP_STIPEND = legacy_yes_no(selection_df$RECD_SCHOLARSHIP_STIPEND, yes = c(1), no = c(2)),
    RECD_TXT_BOOKS = legacy_yes_no(selection_df$RECD_TXT_BOOKS, yes = c(1, 2), no = c(3, NA)),
    RECD_STATIONERY = legacy_yes_no(selection_df$RECD_STATIONERY, yes = c(1, 2), no = c(3, NA)),
    MID_DAY_MEAL_ETC_RECD = legacy_yes_no(selection_df$MID_DAY_MEAL_ETC_RECD, yes = c(1), no = c(2)),
    stringsAsFactors = FALSE
  )
  # Legacy Block 6 merge by the full survey-identification key.
  temp <- b6[is.finite(num(b6$AGE)) & num(b6$AGE) <= 19, , drop = FALSE]
  temp$enrolled <- 1L
  temp <- temp[intersect(
    c(
      "enrolled", "weight", "PID", "district_code_0708", "TUTION_FEE",
      "EXAMINATION_FEE", "OTHER_FEES_PAYMENTS", "BOOKS", "STATIONERY",
      "UNIFORM", "TRANSPORT", "FSU_SL_NO", "HHID", "STATE", "STRATUM",
      "SUB_STRATUM_NO"
    ),
    names(temp)
  )]
  selection_df <- legacy_full_join(selection_df, temp, selection_join_keys(enrolled = TRUE))

  # Legacy Block 4 full join brings in every 5--19 year-old, including those not
  # observed in the enrolled-child blocks.
  temp <- b4[is.finite(num(b4$AGE)) & num(b4$AGE) >= 5 & num(b4$AGE) <= 19, , drop = FALSE]
  temp <- data.frame(
    AGE = num(temp$AGE),
    SEX = factor(num(temp$SEX), levels = c(1, 2), labels = c("Male", "Female")),
    HH_SIZE = num(temp$HH_SIZE),
    RELIGION = factor(num(temp$RELIGION), levels = 1:8, labels = c("Hindu", "Muslim", "Christian", "Sikh", "Jain", "Buddhist", "Zoroastrian", "Other")),
    SOCIAL_GROUP = factor(num(temp$SOCIAL_GROUP), levels = c(1, 2, 3, 9), labels = c("Scheduled Tribe", "Scheduled Caste", "Other Backward Class", "Other")),
    SECTOR = factor(num(temp$SECTOR), levels = c(1, 2), labels = c("Rural", "Urban")),
    DIST_FROM_NEAREST_PRIMARY_CLASS = factor(
      num(temp$DIST_FROM_NEAREST_PRIMARY_CLASS),
      levels = 1:5,
      labels = c("d<1km", "1km <= d <2kms", "2kms<= d <3kms", "3kms <= d <5kms", "d>=5kms")
    ),
    DIST_FROM_UPPER_PRIMARY_CLASS = temp$DIST_FROM_UPPER_PRIMARY_CLASS %||% NA,
    DIST_FROM_SEC_CLASS = temp$DIST_FROM_SEC_CLASS %||% NA,
    RELATION_TO_HEAD = temp$RELATION_TO_HEAD %||% NA,
    TYPE_OF_INSTT = temp$TYPE_OF_INSTT %||% NA,
    district_code_0708 = temp$district_code_0708,
    PID = temp$PID,
    weight = temp$weight,
    FSU_SL_NO = temp$FSU_SL_NO,
    HHID = temp$HHID,
    STATE = temp$STATE,
    STRATUM = temp$STRATUM,
    SUB_STRATUM_NO = temp$SUB_STRATUM_NO,
    state_std = temp$state_std %||% NA,
    district_std = temp$district_std %||% NA,
    stringsAsFactors = FALSE
  )
  selection_df <- legacy_full_join(selection_df, temp, selection_join_keys(enrolled = FALSE))

  # Father's education proxy from the legacy priority ordering.  The legacy Rmd
  # wrote this as a join by HHID, but the imported HHID alone is not globally
  # unique after the refactor's raw readers standardize files independently. Use
  # the full household-survey key to preserve the legacy intent without creating
  # a many-to-many row explosion.
  selection_df$.legacy_household_key <- legacy_household_key(selection_df)
  father_proxy <- build_father_education_proxy(b4)
  if (nrow(father_proxy)) {
    selection_df <- merge(selection_df, father_proxy, by = ".legacy_household_key", all.x = TRUE)
  }

  # Any child absent from Blocks 5/6 is not enrolled.
  selection_df$enrolled[is.na(selection_df$enrolled)] <- 0L
  selection_df$enrolled <- factor(selection_df$enrolled, levels = c(0, 1), labels = c("No", "Yes"))
  selection_df$district_code_0708 <- as.factor(selection_df$district_code_0708)

  # Legacy enrollment-cost construction: documentation-based zero imputations
  # are allowed, but all remaining unknown cost components propagate NA.
  selection_df <- apply_legacy_cost_rules(selection_df)

  # Explicit legacy reference levels.
  selection_df$RELIGION <- stats::relevel(selection_df$RELIGION, ref = "Hindu")
  selection_df$SOCIAL_GROUP <- stats::relevel(selection_df$SOCIAL_GROUP, ref = "Other")
  selection_df$DIST_FROM_NEAREST_PRIMARY_CLASS <- stats::relevel(selection_df$DIST_FROM_NEAREST_PRIMARY_CLASS, ref = "d<1km")
  if ("father_educ" %in% names(selection_df)) selection_df$father_educ <- stats::relevel(selection_df$father_educ, ref = "Illiterate")

  selection_df <- attach_legacy_district_schooling_means(selection_df)
  selection_df <- attach_legacy_district_names(selection_df, blocks[["nss0708edu_metadata"]] %||% data.frame())
  selection_df$.legacy_household_key <- NULL
  selection_df$.legacy_child_key <- NULL
  selection_df
}

#' construct child level selection sample
construct_child_level_selection_sample <- function(df) df
#' construct household covariates
construct_household_covariates <- function(df) df
#' construct district level context
construct_district_level_context <- function(df) df
#' define probit variables
define_probit_variables <- function(df) { if (!"enrolled" %in% names(df)) df$enrolled <- NA_real_; df }
#' apply selection sample restrictions
apply_selection_sample_restrictions <- function(df) df

normalize_selection_identifiers <- function(df) {
  df <- safe_df(df)
  if (!nrow(df)) return(df)
  district_col <- first_col(df, c("district_code_0708", "district_code", "District", "DISTRICT", "district"))
  if (!is.null(district_col)) df$district_code_0708 <- plain_chr(df[[district_col]])
  aliases <- list(
    PID = c("PID", "pid", "person_id", "Person_ID", "person_serial_no", "Person_Serial_No"),
    weight = c("weight", "WEIGHT", "Multiplier", "MULT", "multiplier"),
    FSU_SL_NO = c("FSU_SL_NO", "fsu_sl_no", "FSU", "fsu", "FSU_Serial_No"),
    HHID = c("HHID", "HH_ID", "household_id", "Household_ID", "Sample_HH_No"),
    STATE = c("STATE", "State", "state", "state_code"),
    STRATUM = c("STRATUM", "Stratum", "stratum"),
    SUB_STRATUM_NO = c("SUB_STRATUM_NO", "Sub_Stratum_No", "sub_stratum_no", "SUBSTRATUM", "Substratum")
  )
  for (nm in names(aliases)) {
    hit <- first_col(df, aliases[[nm]])
    if (!is.null(hit)) df[[nm]] <- df[[hit]]
    if (!nm %in% names(df)) df[[nm]] <- NA
  }
  df
}

selection_join_keys <- function(enrolled = TRUE) {
  out <- c("PID", "district_code_0708", "weight", "FSU_SL_NO", "HHID", "STATE", "STRATUM", "SUB_STRATUM_NO")
  if (enrolled) out <- c("enrolled", out)
  out
}

legacy_full_join <- function(x, y, by) {
  by <- intersect(by, intersect(names(x), names(y)))
  if (!length(by)) return(safe_bind_rows(list(x, y)))
  dplyr::full_join(x, y, by = by)
}

dedupe_selection_join_rows <- function(df, by) {
  df <- safe_df(df)
  by <- intersect(by, names(df))
  if (!nrow(df) || !length(by)) return(df)
  key <- do.call(paste, c(lapply(df[by], function(x) canon(plain_chr(x))), sep = "\r"))
  df[!duplicated(key), , drop = FALSE]
}

legacy_yes_no <- function(x, yes = c(1), no = c(2)) {
  val <- num(x)
  out <- ifelse(val %in% yes, "Yes", "No")
  factor(out, levels = c("Yes", "No"))
}

legacy_household_key <- function(df) {
  df <- safe_df(df)
  key_cols <- c("STATE", "FSU_SL_NO", "STRATUM", "SUB_STRATUM_NO", "HHID")
  for (nm in key_cols) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  do.call(paste, c(lapply(df[key_cols], function(x) canon(plain_chr(x))), sep = "__"))
}

legacy_child_key <- function(df) {
  df <- safe_df(df)
  hh <- legacy_household_key(df)
  pid <- if ("PID" %in% names(df)) canon(plain_chr(df$PID)) else rep("", nrow(df))
  district <- if ("district_code_0708" %in% names(df)) canon(plain_chr(df$district_code_0708)) else rep("", nrow(df))
  paste(hh, district, pid, sep = "__")
}

enforce_selection_child_key_uniqueness <- function(df) {
  if (!nrow(df) || !"PID" %in% names(df)) return(df)
  pid <- canon(plain_chr(df$PID))
  if (!any(nzchar(pid) & !is.na(pid))) return(df)
  df$.legacy_household_key <- if (".legacy_household_key" %in% names(df)) df$.legacy_household_key else legacy_household_key(df)
  df$.legacy_child_key <- legacy_child_key(df)
  dup <- duplicated(df$.legacy_child_key)
  if (any(dup, na.rm = TRUE)) {
    df <- df[!dup, , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

build_father_education_proxy <- function(b4) {
  if (!all(c("HHID", "RELATION_TO_HEAD", "SEX", "EDUCATION_LEVEL") %in% names(b4))) return(data.frame())
  df <- b4
  df$.legacy_household_key <- legacy_household_key(df)
  df$educ_collapsed <- collapse_legacy_education(df$EDUCATION_LEVEL)
  df$father_rank <- ifelse(num(df$RELATION_TO_HEAD) == 1 & num(df$SEX) == 1, 1L,
    ifelse(num(df$RELATION_TO_HEAD) == 3 & num(df$SEX) == 1, 2L,
      ifelse(num(df$RELATION_TO_HEAD) == 7 & num(df$SEX) == 1, 4L, NA_integer_)))
  df <- df[is.finite(df$father_rank), , drop = FALSE]
  if (!nrow(df)) return(data.frame(.legacy_household_key = character(), father_educ = factor()))
  df <- df[order(df$.legacy_household_key, df$father_rank), , drop = FALSE]
  df <- df[!duplicated(df$.legacy_household_key), c(".legacy_household_key", "educ_collapsed"), drop = FALSE]
  names(df)[names(df) == "educ_collapsed"] <- "father_educ"
  df$father_educ <- factor(
    df$father_educ,
    levels = c("Illiterate", "Literate, no school", "Literate, school < primary", "Primary", "Upper primary", "Secondary", "Higher secondary", "Postsecondary+")
  )
  df
}

collapse_legacy_education <- function(x) {
  code <- sprintf("%02d", as.integer(num(x)))
  out <- rep(NA_character_, length(code))
  out[code == "01"] <- "Illiterate"
  out[code %in% c("02", "03", "04", "05")] <- "Literate, no school"
  out[code == "06"] <- "Literate, school < primary"
  out[code == "07"] <- "Primary"
  out[code == "08"] <- "Upper primary"
  out[code == "10"] <- "Secondary"
  out[code %in% c("11", "12")] <- "Higher secondary"
  out[code %in% c("13", "14")] <- "Postsecondary+"
  out
}

apply_legacy_cost_rules <- function(df) {
  for (nm in c("TUTION_FEE", "EXAMINATION_FEE", "OTHER_FEES_PAYMENTS", "BOOKS", "STATIONERY", "UNIFORM", "TRANSPORT")) {
    if (!nm %in% names(df)) df[[nm]] <- NA_real_
    df[[nm]] <- num(df[[nm]])
  }
  df$TUTION_FEE[is.na(df$TUTION_FEE) & (df$IS_EDU_FREE == "Yes" | df$TUTION_FEE_WAIVED == "Yes")] <- 0
  df$BOOKS[is.na(df$BOOKS) & df$RECD_TXT_BOOKS == "Yes"] <- 0
  df$STATIONERY[is.na(df$STATIONERY) & df$RECD_STATIONERY == "Yes"] <- 0
  df$ENROLLMENT_COST <- df$TUTION_FEE + df$EXAMINATION_FEE + df$OTHER_FEES_PAYMENTS + df$BOOKS + df$STATIONERY + df$UNIFORM + df$TRANSPORT
  df
}

attach_legacy_district_schooling_means <- function(df) {
  enrolled_only_vars <- c(
    "IS_EDU_FREE", "TUTION_FEE_WAIVED", "RECD_SCHOLARSHIP_STIPEND",
    "RECD_TXT_BOOKS", "RECD_STATIONERY", "MID_DAY_MEAL_ETC_RECD",
    "ENROLLMENT_COST"
  )
  for (nm in enrolled_only_vars) {
    if (!nm %in% names(df)) next
    df[[paste0("num_", nm)]] <- if (is.factor(df[[nm]])) as.numeric(df[[nm]] == "Yes") else num(df[[nm]])
  }
  num_vars <- paste0("num_", enrolled_only_vars)
  num_vars <- intersect(num_vars, names(df))
  if (!length(num_vars) || !"district_code_0708" %in% names(df)) return(df)

  enrolled <- df$enrolled == "Yes"
  idx <- which(enrolled & !is.na(df$district_code_0708))
  if (!length(idx)) return(df)
  split_i <- split(idx, df$district_code_0708[idx])
  means <- safe_bind_rows(lapply(split_i, function(i) {
    z <- data.frame(district_code_0708 = plain_chr(df$district_code_0708[i[[1]]]), stringsAsFactors = FALSE)
    w <- if ("weight" %in% names(df)) num(df$weight[i]) else rep(1, length(i))
    for (nm in num_vars) z[[paste0("dmean_", nm)]] <- wmean(df[[nm]][i], w)
    z
  }))
  merge(df, means, by = "district_code_0708", all.x = TRUE)
}

attach_legacy_district_names <- function(df, metadata) {
  lookup <- parse_2007_district_metadata(metadata)
  if (!nrow(lookup) || !"district_code_0708" %in% names(df)) return(df)
  lookup <- lookup[!duplicated(lookup$district_code_0708), , drop = FALSE]
  merge(df, lookup, by = "district_code_0708", all.x = TRUE)
}

parse_2007_district_metadata <- function(metadata) {
  metadata <- safe_df(metadata)
  if (!nrow(metadata)) return(data.frame())
  name_col <- first_col(metadata, c("name", "Name"))
  value_col <- first_col(metadata, c("ns1:catValu", "catValu", "value", "code"))
  label_col <- first_col(metadata, c("ns1:labl25", "labl25", "label", "Label"))
  if (any(vapply(list(name_col, value_col, label_col), is.null, logical(1)))) return(data.frame())

  districts <- metadata[metadata[[name_col]] == "district_code", c(value_col, label_col), drop = FALSE]
  states <- metadata[metadata[[name_col]] == "STATE", c(value_col, label_col), drop = FALSE]
  if (!nrow(districts) || !nrow(states)) return(data.frame())
  names(districts) <- c("district_code_0708", "district_0708")
  names(states) <- c("state_code_0708", "state_0708")
  districts$district_code_0708 <- plain_chr(districts$district_code_0708)
  districts <- districts[grepl("^[0-9]{5}$", districts$district_code_0708), , drop = FALSE]
  districts <- districts[!duplicated(districts$district_code_0708), , drop = FALSE]
  districts$state_code_0708 <- substr(districts$district_code_0708, 1, 2)
  states$state_code_0708 <- plain_chr(states$state_code_0708)
  states <- states[!duplicated(states$state_code_0708), , drop = FALSE]
  out <- merge(districts, states, by = "state_code_0708", all.x = TRUE)
  out <- out[!duplicated(out$district_code_0708), , drop = FALSE]
  out$state_07 <- out$state_0708
  out$district_07 <- out$district_0708
  out$state_08 <- out$state_0708
  out$district_08 <- out$district_0708
  out[c("district_code_0708", "state_0708", "district_0708", "state_07", "district_07", "state_08", "district_08")]
}

collapse_to_unique_key <- function(df, key) {
  if (!key %in% names(df) || !nrow(df)) return(df)
  df[!duplicated(df[[key]]), , drop = FALSE]
}

district_enrolled_means <- function(df, vars) {
  attach_legacy_district_schooling_means(df)[0, , drop = FALSE]
}
