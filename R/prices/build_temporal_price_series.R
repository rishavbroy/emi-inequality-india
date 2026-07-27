# Construct one monthly state-sector temporal price series from the validated
# RBI/Labour Bureau source tables. The 2007-08 side uses CPI-RL for rural areas
# and state-weighted CPI-IW for urban areas. State CPI-Rural and CPI-Urban on
# the 2012 base take over in January 2013.

price_month_start <- function(x) {
  out <- as.Date(x)
  if (anyNA(out)) {
    stop("Price periods must be valid dates.", call. = FALSE)
  }
  as.Date(format(out, "%Y-%m-01"))
}

price_boundary <- function(x) {
  out <- price_month_start(x)
  if (length(out) != 1L) {
    stop("Price-series boundary must be one valid date.", call. = FALSE)
  }
  out
}

select_pre_2013_price_series <- function(price_sources) {
  required <- c("cpi_alrl", "cpi_iw_states")
  missing <- setdiff(required, names(price_sources))
  if (length(missing)) {
    stop("Price sources are missing pre-2013 tables: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  rural <- safe_df(price_sources$cpi_alrl)
  urban <- safe_df(price_sources$cpi_iw_states)
  if ("cpi_iw_all_india" %in% names(price_sources)) {
    urban <- rbind(urban, safe_df(price_sources$cpi_iw_all_india))
  }

  rural <- rural[rural$labour_series == "rural_labour", , drop = FALSE]
  if (!nrow(rural)) stop("CPI-RL has no rural-labour observations.", call. = FALSE)

  rural$sector <- "rural"
  rural$price_source <- "cpi_rl_state"
  urban$sector <- "urban"
  urban$price_source <- "cpi_iw_state"

  keep <- c("state_code", "sector", "period", "index", "price_source")
  out <- rbind(rural[keep], urban[keep])
  validate_price_index(out, keys = c("state_code", "sector", "period"))
  out[order(out$state_code, out$sector, out$period), , drop = FALSE]
}

select_post_2013_price_series <- function(price_sources) {
  if (!"cpi_ruc_2012" %in% names(price_sources)) {
    stop("Price sources are missing the 2012-base state CPI-R/U table.", call. = FALSE)
  }
  out <- safe_df(price_sources$cpi_ruc_2012)
  out$price_source <- ifelse(out$sector == "rural", "cpi_rural_2012", "cpi_urban_2012")
  keep <- c("state_code", "sector", "period", "index", "price_source")
  out <- out[keep]
  validate_price_index(out, keys = c("state_code", "sector", "period"))
  out[order(out$state_code, out$sector, out$period), , drop = FALSE]
}

summarise_price_links <- function(old_index, new_index, overlap_start, overlap_end) {
  old <- safe_df(old_index)
  new <- safe_df(new_index)
  overlap_start <- price_boundary(overlap_start)
  overlap_end <- price_boundary(overlap_end)
  if (overlap_end < overlap_start) stop("Price-link overlap end precedes its start.", call. = FALSE)

  old <- old[old$period >= overlap_start & old$period <= overlap_end, , drop = FALSE]
  new <- new[new$period >= overlap_start & new$period <= overlap_end, , drop = FALSE]
  paired <- merge(
    old[c("state_code", "sector", "period", "index")],
    new[c("state_code", "sector", "period", "index")],
    by = c("state_code", "sector", "period"), suffixes = c("_old", "_new"), sort = FALSE
  )
  if (!nrow(paired)) stop("Old and new price series have no common state-sector months in the link window.", call. = FALSE)

  split_i <- split(seq_len(nrow(paired)), interaction(paired$state_code, paired$sector, drop = TRUE))
  out <- do.call(rbind, lapply(split_i, function(i) {
    ratio <- num(paired$index_new[i]) / num(paired$index_old[i])
    data.frame(
      state_code = paired$state_code[i[1]],
      sector = paired$sector[i[1]],
      link_factor = stats::median(ratio),
      link_months = length(ratio),
      link_ratio_mad = stats::mad(ratio, center = stats::median(ratio), constant = 1),
      link_ratio_min = min(ratio),
      link_ratio_max = max(ratio),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  if (any(!positive_finite(out$link_factor))) stop("Price linking produced an invalid factor.", call. = FALSE)
  out[order(out$state_code, out$sector), , drop = FALSE]
}

validate_temporal_price_chain <- function(index, switch_date) {
  out <- safe_df(index)
  validate_price_index(out, keys = c("state_code", "sector", "period"))
  switch_date <- price_boundary(switch_date)

  wrong_pre <- out$period < switch_date & !out$price_source %in% c("cpi_rl_state", "cpi_iw_state")
  wrong_post <- out$period >= switch_date & !out$price_source %in% c("cpi_rural_2012", "cpi_urban_2012")
  if (any(wrong_pre | wrong_post)) {
    stop("Temporal price chain violates the pre/post-2013 source rule.", call. = FALSE)
  }
  if (any(!positive_finite(out$index))) stop("Temporal price chain contains invalid values.", call. = FALSE)
  invisible(out)
}

build_temporal_price_series <- function(
    price_sources,
    switch_date = as.Date("2013-01-01"),
    overlap_start = as.Date("2013-01-01"),
    overlap_end = as.Date("2014-12-01"),
    minimum_link_months = 6L,
    pre_switch_start = NULL,
    pre_switch_end = NULL) {
  switch_date <- price_boundary(switch_date)
  old <- select_pre_2013_price_series(price_sources)
  new <- select_post_2013_price_series(price_sources)
  links <- summarise_price_links(old, new, overlap_start, overlap_end)

  minimum_link_months <- as.integer(minimum_link_months)
  if (length(minimum_link_months) != 1L || is.na(minimum_link_months) || minimum_link_months < 1L) {
    stop("minimum_link_months must be one positive integer.", call. = FALSE)
  }

  old_groups <- unique(old[old$period < switch_date, c("state_code", "sector"), drop = FALSE])
  linked_groups <- links[links$link_months >= minimum_link_months, c("state_code", "sector"), drop = FALSE]
  linked_key <- paste(linked_groups$state_code, linked_groups$sector, sep = "\r")
  missing_key <- !paste(old_groups$state_code, old_groups$sector, sep = "\r") %in% linked_key
  if (any(missing_key)) {
    bad <- old_groups[missing_key, , drop = FALSE]
    stop(
      "Pre-2013 price series lack a sufficient direct link to CPI-R/U: ",
      paste(paste(bad$state_code, bad$sector, sep = "/"), collapse = ", "),
      call. = FALSE
    )
  }

  old <- merge(old, links, by = c("state_code", "sector"), all.x = TRUE, sort = FALSE)
  old <- old[old$period < switch_date, , drop = FALSE]
  if (!is.null(pre_switch_start) || !is.null(pre_switch_end)) {
    if (is.null(pre_switch_start) || is.null(pre_switch_end)) {
      stop("Both pre_switch_start and pre_switch_end are required when trimming the old price series.", call. = FALSE)
    }
    pre_switch_start <- price_boundary(pre_switch_start)
    pre_switch_end <- price_boundary(pre_switch_end)
    if (pre_switch_end < pre_switch_start || pre_switch_end >= switch_date) {
      stop("The pre-switch production window must end before the switch date.", call. = FALSE)
    }
    old <- old[old$period >= pre_switch_start & old$period <= pre_switch_end, , drop = FALSE]
  }
  old$index_unlinked <- old$index
  old$index <- num(old$index) * num(old$link_factor)

  new <- new[new$period >= switch_date, , drop = FALSE]
  new$link_factor <- 1
  new$link_months <- NA_integer_
  new$link_ratio_mad <- NA_real_
  new$link_ratio_min <- NA_real_
  new$link_ratio_max <- NA_real_
  new$index_unlinked <- new$index

  columns <- c(
    "state_code", "sector", "period", "index", "index_unlinked", "price_source",
    "link_factor", "link_months", "link_ratio_mad", "link_ratio_min", "link_ratio_max"
  )
  index <- rbind(old[columns], new[columns])
  index$year <- as.integer(format(index$period, "%Y"))
  index$month <- as.integer(format(index$period, "%m"))
  index <- index[order(index$state_code, index$sector, index$period), , drop = FALSE]
  rownames(index) <- NULL
  validate_temporal_price_chain(index, switch_date)

  structure(
    list(index = index, links = links, switch_date = switch_date),
    class = "emi_temporal_price_series"
  )
}

summarise_ruc_base_overlap <- function(price_sources) {
  required <- c("cpi_ruc_2010", "cpi_ruc_2012")
  missing <- setdiff(required, names(price_sources))
  if (length(missing)) {
    stop("Price sources are missing CPI-R/U overlap tables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  old <- safe_df(price_sources$cpi_ruc_2010)
  new <- safe_df(price_sources$cpi_ruc_2012)
  start <- max(min(old$period), min(new$period))
  end <- min(max(old$period), max(new$period))
  summarise_price_links(old, new, start, end)
}


build_ruc_reference_index <- function(
    price_sources,
    reference_period = seq(as.Date("2011-07-01"), as.Date("2012-06-01"), by = "month"),
    state_rules = read_price_state_crosswalk(),
    poverty_lines = read_tendulkar_poverty_lines()) {
  required <- c("cpi_ruc_2010", "cpi_ruc_2012")
  missing <- setdiff(required, names(price_sources))
  if (length(missing)) {
    stop("Price sources are missing CPI-R/U tables for the reference index: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  old <- safe_df(price_sources$cpi_ruc_2010)
  links <- summarise_ruc_base_overlap(price_sources)
  reference_period <- price_month_start(reference_period)
  old <- old[old$period %in% reference_period, , drop = FALSE]
  if (!nrow(old)) stop("The base-2010 CPI-R/U file has no observations in the reference period.", call. = FALSE)

  old <- merge(old, links[c("state_code", "sector", "link_factor")],
    by = c("state_code", "sector"), all.x = TRUE, sort = FALSE
  )
  old$index_2012_base <- num(old$index) * num(old$link_factor)
  split_i <- split(seq_len(nrow(old)), interaction(old$state_code, old$sector, drop = TRUE))
  direct <- safe_bind_rows(lapply(split_i, function(i) {
    months <- unique(old$period[i])
    if (length(months) != length(reference_period) || any(!positive_finite(old$link_factor[i]))) {
      return(NULL)
    }
    data.frame(
      state_code = old$state_code[i[1]],
      sector = old$sector[i[1]],
      reference_index = mean(num(old$index_2012_base[i])),
      reference_months = length(months),
      reference_state_source = old$state_code[i[1]],
      reference_rule = "direct",
      reference_fallback_reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }))

  desired <- unique(safe_df(poverty_lines)[c("state_code", "sector")])
  desired$sector <- price_sector(desired$sector)
  out <- merge(desired, direct, by = c("state_code", "sector"), all.x = TRUE, sort = FALSE)
  missing_rows <- which(!positive_finite(out$reference_index))
  if (length(missing_rows)) {
    rules <- safe_df(state_rules)
    rules$valid_from <- as.Date(rules$valid_from)
    rules$valid_to <- as.Date(rules$valid_to)
    applicable <- rules$valid_from <= min(reference_period) &
      (is.na(rules$valid_to) | rules$valid_to >= max(reference_period))
    rules <- rules[applicable, , drop = FALSE]

    requested <- out[missing_rows, c("state_code", "sector"), drop = FALSE]
    names(requested)[names(requested) == "state_code"] <- "target_state_code"
    candidates <- merge(requested, rules, by = c("target_state_code", "sector"), all.x = TRUE, sort = FALSE)
    if (anyDuplicated(candidates[c("target_state_code", "sector")])) {
      stop("More than one price-state rule applies to a reference state-sector.", call. = FALSE)
    }
    donor <- direct
    names(donor)[names(donor) == "state_code"] <- "source_state_code"
    candidates <- merge(candidates, donor, by = c("source_state_code", "sector"), all.x = TRUE, sort = FALSE)
    candidate_key <- paste(candidates$target_state_code, candidates$sector, sep = "\r")
    requested_key <- paste(out$state_code[missing_rows], out$sector[missing_rows], sep = "\r")
    matched <- match(requested_key, candidate_key)
    found <- !is.na(matched) & positive_finite(candidates$reference_index[matched])
    if (any(found)) {
      target <- missing_rows[found]
      source <- matched[found]
      out$reference_index[target] <- candidates$reference_index[source]
      out$reference_months[target] <- candidates$reference_months[source]
      out$reference_state_source[target] <- candidates$source_state_code[source]
      out$reference_rule[target] <- candidates$rule_type[source]
      out$reference_fallback_reason[target] <- candidates$reason[source]
    }
  }

  if (anyDuplicated(out[c("state_code", "sector")]) || any(!positive_finite(out$reference_index))) {
    bad <- out[!positive_finite(out$reference_index), c("state_code", "sector"), drop = FALSE]
    stop(
      "The CPI-R/U reference index is unresolved for: ",
      paste(paste(bad$state_code, bad$sector, sep = "/"), collapse = ", "),
      call. = FALSE
    )
  }
  out[order(out$state_code, out$sector), , drop = FALSE]
}
