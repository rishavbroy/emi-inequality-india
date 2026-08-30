# Shared count-valued Census harmonization and accounting helpers.

census_2011_harmonized_count_schema <- function(count_cols) {
  count_cols <- unique(plain_chr(count_cols))
  count_cols <- count_cols[!is.na(count_cols) & nzchar(count_cols)]
  if (!length(count_cols)) {
    stop("Harmonized Census-2011 counts require at least one count column.", call. = FALSE)
  }
  out <- data.frame(
    target_unit_2001 = character(),
    census_2011_source_district_count = integer(),
    census_2011_source_districts = character(),
    census_2011_parent_reconstruction_complete = logical(),
    stringsAsFactors = FALSE
  )
  for (column in count_cols) out[[column]] <- numeric()
  out
}

harmonize_census_2011_counts_to_2001 <- function(
    x, district_transition_2001_2011, count_cols) {
  x <- safe_df(x)
  required <- c("state_code", "district_code", "district_name", count_cols)
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Census-2011 count frame lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census-2011 count frame is not unique by source district.", call. = FALSE)
  }
  x$source_unit_2011 <- paste0(
    "pc2011__", normalize_census_code(x$state_code, 2L), "__",
    normalize_census_code(x$district_code, 3L)
  )
  bridge <- build_complete_deterministic_transition_2011_to_2001(
    district_transition_2001_2011
  )
  x <- merge(x, bridge, by = "source_unit_2011", all.x = TRUE, sort = FALSE)
  mapped <- x[!is.na(x$target_unit_2001) & nzchar(x$target_unit_2001), , drop = FALSE]
  if (!nrow(mapped)) return(census_2011_harmonized_count_schema(count_cols))

  groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- mapped[index, , drop = FALSE]
    values <- vapply(count_cols, function(column) {
      value <- num(part[[column]])
      if (!length(value) || any(!is.finite(value)) || any(value < 0)) return(NA_real_)
      sum(value)
    }, numeric(1))
    if (any(!is.finite(values))) {
      stop(
        "Census-2011 deterministic count pool contains invalid counts for ",
        part$target_unit_2001[[1L]], ".",
        call. = FALSE
      )
    }
    row <- data.frame(
      target_unit_2001 = part$target_unit_2001[[1L]],
      census_2011_source_district_count = length(unique(part$source_unit_2011)),
      census_2011_source_districts = paste(sort(unique(part$district_name)), collapse = ";"),
      census_2011_parent_reconstruction_complete = all(
        part$census_2011_parent_reconstruction_complete %in% TRUE
      ),
      stringsAsFactors = FALSE
    )
    for (column in count_cols) row[[column]] <- values[[column]]
    row
  }))
  if (anyDuplicated(out$target_unit_2001)) {
    stop("Harmonized Census-2011 counts are not unique by Census-2001 target.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}


merge_census_district_sources <- function(
    left, right, left_label, right_label, right_exclude = "district_name") {
  left <- safe_df(left)
  right <- safe_df(right)
  keys <- c("state_code", "district_code")
  if (anyDuplicated(left[keys]) || anyDuplicated(right[keys])) {
    stop("Census source merge requires unique district rows.", call. = FALSE)
  }
  left_key <- paste(left$state_code, left$district_code, sep = "/")
  right_key <- paste(right$state_code, right$district_code, sep = "/")
  if (!setequal(left_key, right_key)) {
    stop(left_label, " and ", right_label, " district coverage differs.", call. = FALSE)
  }
  right_payload <- setdiff(names(right), c(keys, right_exclude))
  out <- merge(left, right[c(keys, right_payload)], by = keys, all = FALSE, sort = FALSE)
  out <- out[match(left_key, paste(out$state_code, out$district_code, sep = "/")), , drop = FALSE]
  rownames(out) <- NULL
  out
}

left_join_census_district_source <- function(
    left, right, left_label, right_label, right_exclude = "district_name") {
  left <- safe_df(left)
  right <- safe_df(right)
  keys <- c("state_code", "district_code")
  if (anyDuplicated(left[keys]) || anyDuplicated(right[keys])) {
    stop("Census source merge requires unique district rows.", call. = FALSE)
  }
  left_key <- paste(left$state_code, left$district_code, sep = "/")
  right_key <- paste(right$state_code, right$district_code, sep = "/")
  if (!all(right_key %in% left_key)) {
    stop(right_label, " contains districts outside ", left_label, ".", call. = FALSE)
  }
  right_payload <- setdiff(names(right), c(keys, right_exclude))
  out <- merge(left, right[c(keys, right_payload)], by = keys, all.x = TRUE, sort = FALSE)
  out <- out[match(left_key, paste(out$state_code, out$district_code, sep = "/")), , drop = FALSE]
  rownames(out) <- NULL
  out
}

safe_count_share <- function(numerator, denominator) {
  numerator <- num(numerator)
  denominator <- num(denominator)
  ifelse(
    is.finite(numerator) & numerator >= 0 & is.finite(denominator) & denominator > 0 &
      numerator <= denominator,
    numerator / denominator,
    NA_real_
  )
}

validate_census_matching_count <- function(
    left, right, left_column, right_column, label) {
  left <- safe_df(left)
  right <- safe_df(right)
  keys <- c("state_code", "district_code")
  needed_left <- c(keys, left_column)
  needed_right <- c(keys, right_column)
  missing_left <- setdiff(needed_left, names(left))
  missing_right <- setdiff(needed_right, names(right))
  if (length(missing_left) || length(missing_right)) {
    stop(label, " validation is missing required source columns.", call. = FALSE)
  }
  if (anyDuplicated(left[keys]) || anyDuplicated(right[keys])) {
    stop(label, " validation requires unique source districts.", call. = FALSE)
  }
  left_values <- left[needed_left]
  right_values <- right[needed_right]
  names(left_values)[[length(needed_left)]] <- "left_value"
  names(right_values)[[length(needed_right)]] <- "right_value"
  joined <- merge(left_values, right_values, by = keys, all = TRUE, sort = TRUE)
  left_value <- num(joined$left_value)
  right_value <- num(joined$right_value)
  complete <- is.finite(left_value) & is.finite(right_value)
  same <- complete & left_value == right_value
  if (!nrow(joined) || any(!same)) {
    bad <- joined[!same, , drop = FALSE]
    detail <- if (nrow(bad)) {
      paste0(bad$state_code[[1L]], "/", bad$district_code[[1L]])
    } else {
      "no shared districts"
    }
    stop(label, " counts disagree or district coverage differs; first mismatch: ", detail, ".", call. = FALSE)
  }
  data.frame(
    n_districts = nrow(joined),
    max_abs_difference = max(abs(left_value - right_value)),
    stringsAsFactors = FALSE
  )
}
