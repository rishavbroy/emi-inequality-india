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
    minimum_link_months = 6L) {
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
