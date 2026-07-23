# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# The 2007 district measures are intentionally block-specific.  The legacy Rmd
# did not bind all NSS education and consumption files before aggregating: EMIE
# comes from education Block 5, population/consumption/Gini from education Block
# 3, demographic controls from education Block 4, and pucca housing from the
# consumption household-characteristics file.

#' build 2007 measures
#'
build_2007_measures <- function(nss_2007_education, nss_2007_consumption, cfg) {
  edu <- as_input_list(nss_2007_education)
  cons <- as_input_list(nss_2007_consumption)

  b3 <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(edu, c("nss0708edu_block3", "block3"))), 2007L))
  b4 <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(edu, c("nss0708edu_block4", "block4"))), 2007L))
  b5 <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(edu, c("nss0708edu_block5", "block5", "block"))), 2007L))
  cons_hh <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(cons, c("nss0708cons_hhchar", "hhchar", "block"))), 2007L))

  out <- compute_emie_2007(b5)
  if (!nrow(out)) return(empty_panel())
  out <- merge_measure_2007(out, compute_education_household_measures_2007(b3))
  out <- merge_measure_2007(out, compute_baseline_controls_2007(b4, b3))
  out <- merge_measure_2007(out, compute_housing_controls_2007(cons_hh))
  out <- attach_2007_district_names(out, nss_2007_education)
  if (all(c("state_std", "district_std") %in% names(out))) {
    out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  }
  out
}

select_input_frame <- function(inputs, candidates) {
  inputs <- as_input_list(inputs)
  for (nm in candidates) {
    if (!is.null(inputs[[nm]]) && nrow(safe_df(inputs[[nm]]))) return(inputs[[nm]])
  }
  if (length(inputs) == 1L) return(inputs[[1L]])
  data.frame()
}

district_group_vars_2007 <- function(df) {
  if ("district_code_0708" %in% names(df)) {
    val <- plain_chr(df$district_code_0708)
    if (any(!is.na(val) & nzchar(val))) return("district_code_0708")
  }
  if (all(c("state_std", "district_std") %in% names(df))) return(c("state_std", "district_std"))
  character()
}


merge_measure_2007 <- function(x, y) {
  y <- safe_df(y)
  if (!nrow(y)) return(x)
  key <- intersect(c("district_code_0708", "state_std", "district_std"), intersect(names(x), names(y)))
  if ("district_code_0708" %in% key) {
    x_code <- plain_chr(x$district_code_0708)
    y_code <- plain_chr(y$district_code_0708)
    if (!any(!is.na(x_code) & nzchar(x_code)) || !any(!is.na(y_code) & nzchar(y_code))) {
      key <- setdiff(key, "district_code_0708")
    }
  }
  if (!length(key)) return(x)
  duplicate <- intersect(setdiff(names(y), key), names(x))
  y <- y[setdiff(names(y), duplicate)]
  merge(x, y, by = key, all.x = TRUE, sort = FALSE)
}

standardize_nss_2007_district_code <- function(df) {
  df <- safe_df(df)
  if (!nrow(df)) return(df)
  district <- first_col(df, c("district_code_0708", "district_code", "District", "DISTRICT", "district"))
  if (!is.null(district)) {
    raw <- gsub("[^0-9]", "", plain_chr(df[[district]]))
    df$district_code_0708 <- raw
    df$district_code_0708[nchar(df$district_code_0708) == 0L] <- NA_character_
  }
  df
}

by_district_code_2007 <- function(df, value = NULL, weight = NULL, name = "value", fun = wmean) {
  df <- standardize_nss_2007_district_code(df)
  key <- district_group_vars_2007(df)
  if (!length(key) || !nrow(df) || (is.null(value) && !identical(name, "n"))) {
    return(data.frame(district_code_0708 = character(), state_std = character(), district_std = character()))
  }
  idx <- which(stats::complete.cases(df[key]))
  if (!length(idx)) return(data.frame(district_code_0708 = character(), state_std = character(), district_std = character()))
  group <- interaction(df[idx, key, drop = FALSE], drop = TRUE, sep = "__")
  split_i <- split(idx, group)
  safe_bind_rows(lapply(split_i, function(i) {
    z <- df[i[[1]], key, drop = FALSE]
    z <- z[rep(1L, 1L), , drop = FALSE]
    w <- if (!is.null(weight) && weight %in% names(df)) df[[weight]][i] else NULL
    if (!is.null(value) && value %in% names(df)) z[[name]] <- fun(df[[value]][i], w)
    z$n <- length(i)
    z
  }))
}

#' compute emie 2007
#'
compute_emie_2007 <- function(df) {
  df <- standardize_nss_2007_district_code(std(df, 2007L))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  emi <- first_col(df, c("MEDIUM_INSTRUCTION", "medium_instruction", "medium", "EMI", "emie"))
  if (is.null(emi)) return(empty_panel())
  age <- first_col(df, c("AGE", "age"))
  if (!is.null(age)) {
    keep <- is.finite(num(df[[age]])) & num(df[[age]]) <= 19
    df <- df[keep, , drop = FALSE]
  }
  by_district_code_2007(df, emi, weight, "EMIE", function(x, w) {
    100 * wmean(english_medium_indicator(x, emi), w)
  })
}

english_medium_indicator <- function(x, column_name = NULL) {
  if (!is.null(column_name) && tolower(column_name) %in% c("emi", "emie", "english_medium")) {
    return(as.numeric(num(x) == 1))
  }
  code <- sprintf("%02d", as.integer(num(x)))
  as.numeric(code == "02")
}

#' compute education household measures 2007
#'
compute_education_household_measures_2007 <- function(df) {
  df <- standardize_nss_2007_district_code(std(df, 2007L))
  key <- district_group_vars_2007(df)
  if (!nrow(df) || !length(key)) return(data.frame(district_code_0708 = character(), state_std = character(), district_std = character()))
  weight <- first_col(df, c("weight", "WEIGHT", "Multiplier", "multiplier"))
  hh_size <- first_col(df, c("HH_SIZE", "HH_Size", "household_size"))
  total <- first_col(df, c("TOTAL", "total", "HH_Con_exp_rs", "consumption"))
  mpce <- first_col(df, c("MPCE", "mpce", "consumption_pc", "consumption_per_capita"))
  if (is.null(weight) || (is.null(mpce) && (is.null(hh_size) || is.null(total)))) return(data.frame(district_code_0708 = character()))
  hh_key <- first_col(df, c("HHID", "HH_ID", "household_id"))
  if (!is.null(hh_key)) {
    df$.hh_distinct_key <- paste(do.call(paste, c(df[district_group_vars_2007(df)], sep = "__")), canon(df[[hh_key]]), sep = "__")
    df <- df[!duplicated(df$.hh_distinct_key), , drop = FALSE]
  }
  idx <- which(stats::complete.cases(df[key]))
  split_i <- split(idx, interaction(df[idx, key, drop = FALSE], drop = TRUE, sep = "__"))
  safe_bind_rows(lapply(split_i, function(i) {
    w <- num(df[[weight]][i])
    size <- if (!is.null(hh_size)) num(df[[hh_size]][i]) else rep(1, length(i))
    cons_pc <- if (!is.null(mpce)) num(df[[mpce]][i]) else num(df[[total]][i]) / size
    z <- df[i[[1]], key, drop = FALSE]
    z <- z[rep(1L, 1L), , drop = FALSE]
    data.frame(
      z,
      npeople_0708 = sum(w * size, na.rm = TRUE),
      nhouses_0708 = sum(w, na.rm = TRUE),
      consumption_0708 = wmean(cons_pc, w),
      gini_cons_0708 = wgini(cons_pc, w),
      stringsAsFactors = FALSE
    )
  }))
}



#' Build one row per 2007 education household
#'
#' Block 3 is the household-level source of sector, household size, religion,
#' social group, land, region, and survey weight. Block 4 supplies the household
#' head's sex and education plus the person-level ages used for the dependency
#' ratio. Keeping those roles separate prevents household attributes repeated on
#' every member from becoming population-weighted by accident.
household_controls_frame_2007 <- function(person_df, household_df = data.frame()) {
  people <- safe_df(person_df)
  households <- safe_df(household_df)
  if (!nrow(people) && !nrow(households)) return(data.frame())

  if (nrow(households)) {
    households$.nss_2007_household_key <- nss_2007_household_key(households)
    households <- households[!duplicated(households$.nss_2007_household_key), , drop = FALSE]
  }

  if (nrow(people)) {
    people$.nss_2007_household_key <- nss_2007_household_key(people)
    relation <- num(people$RELATION_TO_HEAD %||% NA)
    split_i <- split(seq_len(nrow(people)), people$.nss_2007_household_key)
    head_rows <- vapply(split_i, function(i) {
      heads <- i[is.finite(relation[i]) & relation[i] == 1]
      if (length(heads)) heads[[1]] else i[[1]]
    }, integer(1))
    head_columns <- if (nrow(households)) {
      intersect(
        c(".nss_2007_household_key", "SEX", "EDUCATION_LEVEL", "RELATION_TO_HEAD"),
        names(people)
      )
    } else {
      names(people)
    }
    heads <- people[head_rows, head_columns, drop = FALSE]
    heads <- heads[!duplicated(heads$.nss_2007_household_key), , drop = FALSE]
  } else {
    heads <- data.frame(.nss_2007_household_key = character())
  }

  if (!nrow(households)) return(heads)
  if (!nrow(heads)) return(households)
  merge(households, heads, by = ".nss_2007_household_key", all.x = TRUE, sort = FALSE)
}

weighted_share_2007 <- function(condition, weight) {
  condition <- as.logical(condition)
  weight <- num(weight)
  keep <- is.finite(weight) & weight > 0 & !is.na(condition)
  denom <- sum(weight[keep], na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  100 * sum(weight[keep] * condition[keep], na.rm = TRUE) / denom
}

weighted_code_share_2007 <- function(value, codes, weight) {
  value <- num(value)
  condition <- ifelse(is.finite(value), value %in% codes, NA)
  weighted_share_2007(condition, weight)
}

#' compute baseline controls 2007
#'
#' Household attributes are aggregated from the household-level Block 3 using
#' household survey weights. Head characteristics come from one Block 4 head
#' record per household. The dependency ratio alone remains person-weighted.
compute_baseline_controls_2007 <- function(person_df, household_df = data.frame()) {
  people <- standardize_nss_2007_district_code(std(person_df, 2007L))
  household_source <- standardize_nss_2007_district_code(std(household_df, 2007L))
  key <- district_group_vars_2007(household_source)
  if (!length(key)) key <- district_group_vars_2007(people)
  if (!length(key) || (!nrow(people) && !nrow(household_source))) {
    return(data.frame(district_code_0708 = character(), state_std = character(), district_std = character()))
  }

  households <- household_controls_frame_2007(people, household_source)
  group_id <- function(df) {
    if (!nrow(df)) return(character())
    do.call(paste, c(lapply(df[key], plain_chr), sep = "__"))
  }
  people_groups <- split(seq_len(nrow(people)), group_id(people))
  household_groups <- split(seq_len(nrow(households)), group_id(households))
  groups <- union(names(household_groups), names(people_groups))
  groups <- groups[!is.na(groups) & nzchar(groups)]

  safe_bind_rows(lapply(groups, function(group) {
    people_i <- people_groups[[group]] %||% integer()
    household_i <- household_groups[[group]] %||% integer()
    district_people <- people[people_i, , drop = FALSE]
    district_households <- households[household_i, , drop = FALSE]
    source <- if (nrow(district_households)) district_households else district_people
    if (!nrow(source)) return(data.frame())

    household_weight <- first_col(district_households, c("weight", "WEIGHT", "Multiplier", "multiplier"))
    person_weight <- first_col(district_people, c("weight", "WEIGHT", "Multiplier", "multiplier"))
    if (is.null(household_weight)) return(data.frame())
    household_w <- num(district_households[[household_weight]])
    person_w <- if (!is.null(person_weight)) num(district_people[[person_weight]]) else numeric()
    age <- num(district_people$AGE %||% NA)
    relation_hh <- num(district_households$RELATION_TO_HEAD %||% NA)
    sex_hh <- num(district_households$SEX %||% NA)
    edu_hh <- num(district_households$EDUCATION_LEVEL %||% NA)
    religion_hh <- num(district_households$RELIGION %||% NA)
    social_hh <- num(district_households$SOCIAL_GROUP %||% NA)
    land_hh <- num(district_households$LAND_POSSESSED_CODE %||% NA)

    z <- source[1, key, drop = FALSE]
    z$pct_urban <- weighted_code_share_2007(district_households$SECTOR %||% NA, 2, household_w)
    z$avg_hh_size <- wmean(district_households$HH_SIZE %||% NA, household_w)
    z$dependency_ratio <- {
      valid <- is.finite(person_w) & person_w > 0 & is.finite(age)
      dep <- valid & (age <= 14 | age >= 65)
      work <- valid & age >= 15 & age <= 64
      denom <- sum(person_w[work], na.rm = TRUE)
      if (denom > 0) 100 * sum(person_w[dep], na.rm = TRUE) / denom else NA_real_
    }
    female_head <- ifelse(is.finite(relation_hh) & relation_hh == 1, sex_hh == 2, NA)
    z$pct_fem_head <- weighted_share_2007(female_head, household_w)
    z$pct_hindu <- weighted_code_share_2007(religion_hh, 1, household_w)
    z$pct_muslim <- weighted_code_share_2007(religion_hh, 2, household_w)
    z$pct_other_religion <- weighted_code_share_2007(religion_hh, c(3, 4, 5, 6, 7, 8), household_w)
    z$pct_st <- weighted_code_share_2007(social_hh, 1, household_w)
    z$pct_sc <- weighted_code_share_2007(social_hh, 2, household_w)
    z$pct_obc <- weighted_code_share_2007(social_hh, 3, household_w)
    z$pct_small_land <- weighted_code_share_2007(land_hh, c(2, 3, 4), household_w)
    z$pct_medium_land <- weighted_code_share_2007(land_hh, c(5, 6, 7), household_w)
    z$pct_large_land <- weighted_code_share_2007(land_hh, c(8, 10, 11, 12), household_w)
    z$pct_head_illiterate <- weighted_share_2007(edu_hh == 1, household_w)
    z$pct_head_lit_to_primary <- weighted_share_2007(edu_hh >= 2 & edu_hh <= 7, household_w)
    z$pct_head_secondary_plus <- weighted_share_2007(edu_hh >= 8, household_w)
    region <- plain_chr(district_households$REGION %||% district_people$REGION %||% NA)
    region <- region[!is.na(region) & nzchar(region)]
    z$region <- if (length(region)) region[[1]] else NA_character_
    z
  }))
}


#' compute housing controls 2007
#'
#' @return District-level housing controls from the 2007 consumption household file.
compute_housing_controls_2007 <- function(df) {
  df <- standardize_nss_2007_district_code(std(df, 2007L))
  type <- first_col(df, c("Type_of_structure", "type_of_structure"))
  weight <- first_col(df, c("Multiplier", "weight", "WEIGHT", "multiplier"))
  if (is.null(type) || is.null(weight) || !length(district_group_vars_2007(df))) return(data.frame(district_code_0708 = character(), state_std = character(), district_std = character()))
  hh <- first_col(df, c("HH_ID", "HHID", "household_id"))
  if (!is.null(hh)) {
    df$.hh_distinct_key <- paste(do.call(paste, c(df[district_group_vars_2007(df)], sep = "__")), canon(df[[hh]]), sep = "__")
    df <- df[!duplicated(df$.hh_distinct_key), , drop = FALSE]
  }
  by_district_code_2007(df, type, weight, "pct_pucca", function(x, w) 100 * wmean(as.numeric(num(x) == 1), w))
}





attach_2007_district_names <- function(out, nss_2007_education) {
  lookup <- parse_2007_district_metadata((as_input_list(nss_2007_education)[["nss0708edu_metadata"]]) %||% data.frame())
  if (!nrow(lookup) || !"district_code_0708" %in% names(out)) return(out)
  merged <- merge(out, lookup, by = "district_code_0708", all.x = TRUE, sort = FALSE)
  if ("state_0708" %in% names(merged)) merged$state_std <- canonicalize_state_name(merged$state_0708)
  if ("district_0708" %in% names(merged)) merged$district_std <- canonicalize_district_name(merged$district_0708)
  merged
}
