# Shared construction and validation for state-sector price deflators.

price_sector <- function(x) {
  value <- tolower(trimws(plain_chr(x)))
  ifelse(value %in% c("1", "r", "rur", "rural"), "rural",
    ifelse(value %in% c("2", "u", "urb", "urban"), "urban", NA_character_)
  )
}

validate_price_index <- function(x, keys = c("state_code", "sector", "year", "month")) {
  df <- safe_df(x)
  missing <- setdiff(c(keys, "index"), names(df))
  if (length(missing)) {
    stop("Price index is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(df[keys])) {
    stop("Price index has duplicate state-sector-month rows.", call. = FALSE)
  }
  if (any(!positive_finite(num(df$index)))) {
    stop("Price index contains non-positive or non-finite values.", call. = FALSE)
  }
  invisible(df)
}

read_price_state_crosswalk <- function(path = "data/metadata/price_state_crosswalk.csv") {
  path <- resolve_price_path(path)
  rules <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "target_state_code", "source_state_code", "sector", "valid_from", "valid_to",
    "rule_type", "reason"
  )
  missing <- setdiff(required, names(rules))
  if (length(missing)) {
    stop("Price-state crosswalk is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  rules$target_state_code <- trimws(as.character(rules$target_state_code))
  rules$source_state_code <- trimws(as.character(rules$source_state_code))
  rules$sector <- price_sector(rules$sector)
  rules$rule_type <- trimws(as.character(rules$rule_type))
  rules$reason <- trimws(as.character(rules$reason))
  rules$valid_from <- as.Date(rules$valid_from)
  blank_to <- !nzchar(trimws(as.character(rules$valid_to)))
  rules$valid_to[blank_to] <- NA_character_
  rules$valid_to <- as.Date(rules$valid_to)

  invalid <- !nzchar(rules$target_state_code) | !nzchar(rules$source_state_code) |
    is.na(rules$sector) | is.na(rules$valid_from) | !rules$rule_type %in% c("fallback", "inheritance") |
    !nzchar(rules$reason)
  if (any(invalid)) stop("Price-state crosswalk contains incomplete or invalid rules.", call. = FALSE)
  if (any(!is.na(rules$valid_to) & rules$valid_to < rules$valid_from)) {
    stop("Price-state crosswalk contains a rule ending before it begins.", call. = FALSE)
  }

  split_rules <- split(
    seq_len(nrow(rules)),
    interaction(rules$target_state_code, rules$sector, drop = TRUE)
  )
  overlapping <- vapply(split_rules, function(i) {
    from <- rules$valid_from[i]
    to <- rules$valid_to[i]
    to[is.na(to)] <- as.Date("9999-12-01")
    ord <- order(from, to)
    from <- from[ord]
    to <- to[ord]
    length(from) > 1L && any(from[-1L] <= to[-length(to)])
  }, logical(1))
  if (any(overlapping)) {
    stop(
      "Price-state crosswalk has overlapping rules for: ",
      paste(names(overlapping)[overlapping], collapse = ", "),
      call. = FALSE
    )
  }
  rules
}

read_tendulkar_poverty_lines <- function(path = "data/metadata/tendulkar_poverty_lines_2011_12.csv") {
  path <- resolve_price_path(path)
  poverty <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "state_code", "sector", "poverty_line_rupees", "source_state_code",
    "fallback_reason", "source_page", "source_table"
  )
  missing <- setdiff(required, names(poverty))
  if (length(missing)) {
    stop("Tendulkar metadata is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  poverty$state_code <- trimws(as.character(poverty$state_code))
  poverty$source_state_code <- trimws(as.character(poverty$source_state_code))
  poverty$sector <- price_sector(poverty$sector)
  poverty$poverty_line_rupees <- num(poverty$poverty_line_rupees)
  if (any(!nzchar(poverty$state_code)) || any(!nzchar(poverty$source_state_code)) ||
      any(is.na(poverty$sector)) || any(!positive_finite(poverty$poverty_line_rupees))) {
    stop("Tendulkar metadata contains incomplete or invalid rows.", call. = FALSE)
  }
  if (anyDuplicated(poverty[c("state_code", "sector")])) {
    stop("Tendulkar metadata must have one row per state and sector.", call. = FALSE)
  }
  poverty
}

build_tendulkar_spatial_relatives <- function(poverty_lines, reference_rupees = 816) {
  df <- safe_df(poverty_lines)
  required <- c("state_code", "sector", "poverty_line_rupees")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("Poverty-line table is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  df$state_code <- trimws(as.character(df$state_code))
  df$sector <- price_sector(df$sector)
  value <- num(df$poverty_line_rupees)
  reference_rupees <- num(reference_rupees)
  if (length(reference_rupees) != 1L || !positive_finite(reference_rupees)) {
    stop("The common poverty-line reference must be one positive finite value.", call. = FALSE)
  }
  if (any(!nzchar(df$state_code)) || any(is.na(df$sector)) || any(!positive_finite(value))) {
    stop("Poverty lines must have valid state, sector, and positive values.", call. = FALSE)
  }
  if (anyDuplicated(df[c("state_code", "sector")])) {
    stop("Poverty-line table must have one row per state and sector.", call. = FALSE)
  }
  df$spatial_price_relative <- value / reference_rupees
  df
}

price_link_factor <- function(old_index, new_index) {
  old <- num(old_index)
  new <- num(new_index)
  keep <- positive_finite(old) & positive_finite(new)
  if (!any(keep)) return(NA_real_)
  stats::median(new[keep] / old[keep], na.rm = TRUE)
}

apply_price_state_rules <- function(temporal_index, state_rules, start_period = NULL, end_period = NULL) {
  idx <- safe_df(temporal_index)
  rules <- safe_df(state_rules)
  validate_price_index(idx, keys = c("state_code", "sector", "period"))
  required <- c("target_state_code", "source_state_code", "sector", "valid_from", "valid_to", "rule_type", "reason")
  missing <- setdiff(required, names(rules))
  if (length(missing)) {
    stop("Price-state rules are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  idx$period <- as.Date(idx$period)
  rules$valid_from <- as.Date(rules$valid_from)
  rules$valid_to <- as.Date(rules$valid_to)
  if (is.null(start_period)) start_period <- min(idx$period)
  if (is.null(end_period)) end_period <- max(idx$period)
  start_period <- price_boundary(start_period)
  end_period <- price_boundary(end_period)
  if (end_period < start_period) stop("Price-state range ends before it begins.", call. = FALSE)

  periods <- sort(unique(idx$period[idx$period >= start_period & idx$period <= end_period]))
  direct_pairs <- unique(idx[idx$state_code != "ALL_INDIA", c("state_code", "sector"), drop = FALSE])
  rule_pairs <- unique(rules[c("target_state_code", "sector")])
  names(rule_pairs)[names(rule_pairs) == "target_state_code"] <- "state_code"
  state_sector_pairs <- unique(safe_bind_rows(list(direct_pairs, rule_pairs)))
  state_sector_pairs <- state_sector_pairs[
    nzchar(as.character(state_sector_pairs$state_code)) &
      !is.na(price_sector(state_sector_pairs$sector)),
    c("state_code", "sector"),
    drop = FALSE
  ]
  state_sector_pairs$sector <- price_sector(state_sector_pairs$sector)
  state_sector_pairs <- state_sector_pairs[order(
    state_sector_pairs$state_code, state_sector_pairs$sector
  ), , drop = FALSE]

  grid <- merge(
    state_sector_pairs,
    data.frame(period = periods),
    by = NULL,
    all = TRUE,
    sort = FALSE
  )
  grid$.price_row <- seq_len(nrow(grid))

  direct <- merge(
    grid,
    idx,
    by = c("state_code", "sector", "period"),
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", "_source")
  )
  direct <- direct[order(direct$.price_row), , drop = FALSE]
  direct$temporal_state_source <- ifelse(!is.na(direct$index), direct$state_code, NA_character_)
  direct$state_rule <- ifelse(!is.na(direct$index), "direct", NA_character_)
  direct$fallback_reason <- NA_character_

  source_columns <- setdiff(names(idx), c("state_code", "sector", "period"))
  combine_price_fallback_reasons <- function(current, upstream) {
    current <- trimws(as.character(current))
    upstream <- trimws(as.character(upstream))
    has_upstream <- !is.na(upstream) & nzchar(upstream)
    current[has_upstream] <- paste(current[has_upstream], upstream[has_upstream], sep = " -> ")
    current
  }

  # Resolve documented fallback chains one link at a time. This matters when an
  # official territorial donor (for example, Goa for Daman and Diu) itself lacks
  # the historical sector index and therefore uses a second documented donor.
  repeat {
    missing_rows <- which(!positive_finite(num(direct$index)))
    if (!length(missing_rows)) break

    requested <- direct[missing_rows, c(".price_row", "state_code", "sector", "period"), drop = FALSE]
    names(requested)[names(requested) == "state_code"] <- "target_state_code"
    candidates <- merge(
      requested, rules,
      by = c("target_state_code", "sector"),
      all.x = TRUE, sort = FALSE
    )
    applicable <- !is.na(candidates$valid_from) &
      candidates$period >= candidates$valid_from &
      (is.na(candidates$valid_to) | candidates$period <= candidates$valid_to)
    candidates <- candidates[applicable, , drop = FALSE]
    if (anyDuplicated(candidates$.price_row)) {
      stop("More than one price-state rule applies to the same state-sector-month.", call. = FALSE)
    }

    donor_columns <- c("state_code", "sector", "period", source_columns,
      "temporal_state_source", "fallback_reason")
    donor <- direct[donor_columns]
    all_india <- idx[idx$state_code == "ALL_INDIA",
      c("state_code", "sector", "period", source_columns), drop = FALSE
    ]
    if (nrow(all_india)) {
      all_india$temporal_state_source <- "ALL_INDIA"
      all_india$fallback_reason <- NA_character_
      donor <- safe_bind_rows(list(donor, all_india[donor_columns]))
    }
    names(donor)[names(donor) == "state_code"] <- "source_state_code"
    names(donor)[names(donor) == "temporal_state_source"] <- "donor_temporal_state_source"
    names(donor)[names(donor) == "fallback_reason"] <- "donor_fallback_reason"
    candidates <- merge(
      candidates,
      donor,
      by = c("source_state_code", "sector", "period"),
      all.x = TRUE,
      sort = FALSE
    )
    candidate_by_row <- match(direct$.price_row[missing_rows], candidates$.price_row)
    found <- !is.na(candidate_by_row) &
      positive_finite(num(candidates$index[candidate_by_row]))
    if (!any(found)) break

    target_rows <- missing_rows[found]
    source_rows <- candidate_by_row[found]
    for (column in source_columns) {
      direct[[column]][target_rows] <- candidates[[column]][source_rows]
    }
    ultimate_source <- candidates$donor_temporal_state_source[source_rows]
    use_immediate <- is.na(ultimate_source) | !nzchar(ultimate_source)
    ultimate_source[use_immediate] <- candidates$source_state_code[source_rows][use_immediate]
    direct$temporal_state_source[target_rows] <- ultimate_source
    direct$state_rule[target_rows] <- candidates$rule_type[source_rows]
    direct$fallback_reason[target_rows] <- combine_price_fallback_reasons(
      candidates$reason[source_rows],
      candidates$donor_fallback_reason[source_rows]
    )
  }

  unresolved <- !positive_finite(num(direct$index))
  if (any(unresolved)) {
    bad <- unique(direct[unresolved, c("state_code", "sector"), drop = FALSE])
    stop(
      "No direct or documented fallback temporal price series for: ",
      paste(paste(bad$state_code, bad$sector, sep = "/"), collapse = ", "),
      call. = FALSE
    )
  }

  direct$.price_row <- NULL
  direct <- direct[order(direct$state_code, direct$sector, direct$period), , drop = FALSE]
  rownames(direct) <- NULL
  validate_price_index(direct, keys = c("state_code", "sector", "period"))
  direct
}

build_state_sector_deflator <- function(
    temporal_index, spatial_relatives, reference_period = NULL, reference_index = NULL) {
  idx <- safe_df(temporal_index)
  spatial <- safe_df(spatial_relatives)
  validate_price_index(idx, keys = c("state_code", "sector", "period"))
  required_spatial <- c("state_code", "sector", "spatial_price_relative")
  missing <- setdiff(required_spatial, names(spatial))
  if (length(missing)) {
    stop("Spatial price relatives are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(spatial[c("state_code", "sector")])) {
    stop("Spatial price relatives must have one row per state and sector.", call. = FALSE)
  }
  if (any(!positive_finite(num(spatial$spatial_price_relative)))) {
    stop("Spatial price relatives must be positive and finite.", call. = FALSE)
  }

  if (is.null(reference_index)) {
    if (is.null(reference_period)) stop("A price reference period or reference-index table is required.", call. = FALSE)
    reference_period <- as.Date(reference_period)
    ref <- idx[idx$period %in% reference_period, , drop = FALSE]
    if (!nrow(ref)) stop("No observations fall in the requested price reference period.", call. = FALSE)
    ref_mean <- stats::aggregate(index ~ state_code + sector, ref, function(x) mean(num(x), na.rm = TRUE))
    names(ref_mean)[names(ref_mean) == "index"] <- "reference_index"
  } else {
    ref_mean <- safe_df(reference_index)
    required_reference <- c("state_code", "sector", "reference_index")
    missing_reference <- setdiff(required_reference, names(ref_mean))
    if (length(missing_reference)) {
      stop("Reference-price table is missing columns: ", paste(missing_reference, collapse = ", "), call. = FALSE)
    }
    if (anyDuplicated(ref_mean[c("state_code", "sector")]) || any(!positive_finite(ref_mean$reference_index))) {
      stop("Reference-price table must have one positive row per state-sector.", call. = FALSE)
    }
    ref_mean <- ref_mean[required_reference]
  }
  out <- merge(
    idx,
    spatial,
    by = c("state_code", "sector"), all.x = TRUE, sort = FALSE
  )
  out <- merge(out, ref_mean, by = c("state_code", "sector"), all.x = TRUE, sort = FALSE)
  if ("source_state_code" %in% names(out) && any(!positive_finite(out$reference_index))) {
    donor_reference <- ref_mean
    names(donor_reference)[names(donor_reference) == "state_code"] <- "source_state_code"
    names(donor_reference)[names(donor_reference) == "reference_index"] <- "donor_reference_index"
    out <- merge(out, donor_reference, by = c("source_state_code", "sector"), all.x = TRUE, sort = FALSE)
    missing_reference <- !positive_finite(out$reference_index)
    out$reference_index[missing_reference] <- out$donor_reference_index[missing_reference]
    out$donor_reference_index <- NULL
  }
  out$temporal_price_relative <- num(out$index) / num(out$reference_index)
  out$price_deflator <- num(out$spatial_price_relative) * num(out$temporal_price_relative)
  if (any(!positive_finite(out$price_deflator))) {
    stop("Deflator construction left missing or invalid values.", call. = FALSE)
  }
  out <- out[order(out$state_code, out$sector, out$period), , drop = FALSE]
  rownames(out) <- NULL
  out
}

build_state_sector_price_deflators <- function(
    temporal_series,
    state_rules = read_price_state_crosswalk(),
    poverty_lines = read_tendulkar_poverty_lines(),
    reference_period = seq(as.Date("2011-07-01"), as.Date("2012-06-01"), by = "month"),
    reference_index = NULL,
    start_period = NULL,
    end_period = NULL,
    reference_rupees = 816) {
  temporal_index <- if (inherits(temporal_series, "emi_temporal_price_series")) {
    temporal_series$index
  } else {
    safe_df(temporal_series)
  }
  expanded <- apply_price_state_rules(
    temporal_index,
    state_rules,
    start_period = start_period,
    end_period = end_period
  )
  spatial <- build_tendulkar_spatial_relatives(poverty_lines, reference_rupees)
  missing_spatial <- setdiff(
    unique(paste(expanded$state_code, expanded$sector, sep = "\r")),
    unique(paste(spatial$state_code, spatial$sector, sep = "\r"))
  )
  if (length(missing_spatial)) {
    stop("Tendulkar metadata does not cover every temporal state-sector series.", call. = FALSE)
  }
  build_state_sector_deflator(
    expanded,
    spatial,
    reference_period = reference_period,
    reference_index = reference_index
  )
}

validate_consumption_price_window <- function(deflators, price_window) {
  d <- safe_df(deflators)
  w <- safe_df(price_window)
  if (nrow(w) != 1L || !all(c("start_period", "end_period") %in% names(w))) {
    stop("Consumption price window must contain one start_period/end_period row.", call. = FALSE)
  }
  required <- c("state_code", "sector", "period", "price_deflator")
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    stop("Consumption price deflators are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  start <- price_boundary(w$start_period[[1L]])
  end <- price_boundary(w$end_period[[1L]])
  if (end < start) stop("Consumption price window ends before it begins.", call. = FALSE)

  periods <- sort(unique(price_month_start(d$period)))
  required_periods <- seq(start, end, by = "month")
  missing_periods <- setdiff(as.character(required_periods), as.character(periods))
  if (length(missing_periods)) {
    stop(
      "Price deflator table does not cover registered consumption month(s): ",
      paste(utils::head(missing_periods, 12L), collapse = ", "),
      if (length(missing_periods) > 12L) " ..." else "",
      call. = FALSE
    )
  }
  if (any(!positive_finite(d$price_deflator))) {
    stop("Consumption price deflator table contains invalid values.", call. = FALSE)
  }
  d
}

attach_household_deflator <- function(households, deflators, state_col, sector_col, period_col) {
  hh <- safe_df(households)
  d <- safe_df(deflators)
  hh$.price_row <- seq_len(nrow(hh))
  hh$.price_state_code <- as.character(hh[[state_col]])
  hh$.price_sector <- price_sector(hh[[sector_col]])
  hh$.price_period <- as.character(as.Date(hh[[period_col]]))
  d$.price_state_code <- as.character(d$state_code)
  d$.price_sector <- price_sector(d$sector)
  d$.price_period <- as.character(as.Date(d$period))
  keep <- intersect(
    c(
      ".price_state_code", ".price_sector", ".price_period", "price_deflator",
      "price_source", "temporal_state_source", "state_rule", "fallback_reason",
      "source_state_code", "spatial_price_relative"
    ),
    names(d)
  )
  out <- merge(hh, d[keep], by = c(".price_state_code", ".price_sector", ".price_period"), all.x = TRUE, sort = FALSE)
  out <- out[order(out$.price_row), , drop = FALSE]
  out$.price_row <- NULL
  rownames(out) <- NULL
  if (any(!positive_finite(out$price_deflator))) {
    stop("At least one household lacks a valid state-sector-period price deflator.", call. = FALSE)
  }
  out
}
