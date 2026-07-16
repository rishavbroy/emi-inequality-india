# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# The 2007 district measures are intentionally block-specific.  The legacy Rmd
# did not bind all NSS education and consumption files before aggregating: EMIE
# comes from education Block 5, population/consumption/Gini from education Block
# 3, demographic controls from education Block 4, and pucca housing from the
# consumption household-characteristics file.

#' build 2007 measures
#'
build_2007_measures <- function(nss_2007_education, nss_2007_consumption, selection_data, ame_results, cfg) {
  edu <- as_input_list(nss_2007_education)
  cons <- as_input_list(nss_2007_consumption)

  b3 <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(edu, c("nss0708edu_block3", "block3"))), 2007L))
  b4 <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(edu, c("nss0708edu_block4", "block4"))), 2007L))
  b5 <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(edu, c("nss0708edu_block5", "block5", "block"))), 2007L))
  cons_hh <- standardize_nss_2007_district_code(std(safe_df(select_input_frame(cons, c("nss0708cons_hhchar", "hhchar", "block"))), 2007L))

  out <- compute_emie_2007(b5)
  if (!nrow(out)) return(empty_panel())
  out <- merge_measure_2007(out, compute_education_household_measures_2007(b3))
  out <- merge_measure_2007(out, compute_baseline_controls_2007(b4))
  out <- merge_measure_2007(out, compute_housing_controls_2007(cons_hh))
  out <- merge_measure_2007(out, compute_district_imr_2007(selection_data))
  out <- attach_2007_district_names(out, nss_2007_education)
  out <- add_2007_measure_aliases(out)
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
    val <- as.character(df$district_code_0708)
    if (any(!is.na(val) & nzchar(val))) return("district_code_0708")
  }
  if (all(c("state_std", "district_std") %in% names(df))) return(c("state_std", "district_std"))
  character()
}

merge_measure <- function(x, y) merge_measure_2007(x, y)

merge_measure_2007 <- function(x, y) {
  y <- safe_df(y)
  if (!nrow(y)) return(x)
  key <- intersect(c("district_code_0708", "state_std", "district_std"), intersect(names(x), names(y)))
  if ("district_code_0708" %in% key) {
    x_code <- as.character(x$district_code_0708)
    y_code <- as.character(y$district_code_0708)
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
    raw <- gsub("[^0-9]", "", as.character(df[[district]]))
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
  by_district_code_2007(df, emi, weight, "emie_2007", function(x, w) {
    100 * wmean(english_medium_indicator(x, emi), w)
  }) |> add_emie_alias()
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
      consumption_2007 = wmean(cons_pc, w),
      gini_consumption_2007 = wgini(cons_pc, w),
      stringsAsFactors = FALSE
    )
  }))
}


#' compute consumption 2007
#'
compute_consumption_2007 <- function(df) {
  out <- compute_education_household_measures_2007(df)
  out[intersect(c("district_code_0708", "state_std", "district_std", "consumption_2007"), names(out))]
}

#' compute gini consumption 2007
#'
compute_gini_consumption_2007 <- function(df) {
  out <- compute_education_household_measures_2007(df)
  out[intersect(c("district_code_0708", "state_std", "district_std", "gini_consumption_2007"), names(out))]
}

#' compute baseline controls 2007
#'
compute_baseline_controls_2007 <- function(df) {
  df <- standardize_nss_2007_district_code(std(df, 2007L))
  key <- district_group_vars_2007(df)
  if (!length(key) || !nrow(df)) return(data.frame(district_code_0708 = character(), state_std = character(), district_std = character()))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  if (is.null(weight)) return(data.frame(district_code_0708 = character()))
  idx <- which(stats::complete.cases(df[key]))
  split_i <- split(idx, interaction(df[idx, key, drop = FALSE], drop = TRUE, sep = "__"))
  safe_bind_rows(lapply(split_i, function(i) {
    z <- df[i[[1]], key, drop = FALSE]
    z <- z[rep(1L, 1L), , drop = FALSE]
    w <- num(df[[weight]][i])
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

compute_district_imr_2007 <- function(selection_data) {
  df <- safe_df(selection_data)
  if (!nrow(df) || !all(c("district_code_0708", "IMR") %in% names(df))) return(data.frame(district_code_0708 = character()))
  weight <- first_col(df, c("weight", "WEIGHT", "multiplier"))
  by_district_code_2007(df, "IMR", weight, "avg_IMR")
}

normalize_2007_consumption_district_key <- function(df) {
  standardize_nss_2007_district_code(df)
}

add_emie_alias <- function(df) {
  if ("emie_2007" %in% names(df)) df$EMIE <- df$emie_2007
  df
}

add_2007_measure_aliases <- function(out) {
  alias <- function(new, old) if (old %in% names(out) && !new %in% names(out)) out[[new]] <<- out[[old]]
  alias("EMIE", "emie_2007")
  alias("consumption_0708", "consumption_2007")
  alias("gini_cons_0708", "gini_consumption_2007")
  alias("pucca_share_2007", "pct_pucca")
  alias("n_2007", "n")
  out
}

attach_2007_district_names <- function(out, nss_2007_education) {
  lookup <- parse_2007_district_metadata((as_input_list(nss_2007_education)[["nss0708edu_metadata"]]) %||% data.frame())
  if (!nrow(lookup) || !"district_code_0708" %in% names(out)) return(out)
  merged <- merge(out, lookup, by = "district_code_0708", all.x = TRUE, sort = FALSE)
  if ("state_0708" %in% names(merged)) merged$state_std <- canonicalize_state_name(merged$state_0708)
  if ("district_0708" %in% names(merged)) merged$district_std <- canonicalize_district_name(merged$district_0708)
  merged
}
