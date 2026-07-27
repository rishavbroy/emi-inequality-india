# State-sector price deflators for NSS consumption measures.

price_sector <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- ifelse(value %in% c('1', 'r', 'rural'), 'rural',
    ifelse(value %in% c('2', 'u', 'urban'), 'urban', NA_character_))
  out
}

validate_price_index <- function(x, keys = c('state_code', 'sector', 'year', 'month')) {
  df <- safe_df(x)
  missing <- setdiff(c(keys, 'index'), names(df))
  if (length(missing)) stop('Price index is missing columns: ', paste(missing, collapse = ', '), call. = FALSE)
  if (anyDuplicated(df[keys])) stop('Price index has duplicate state-sector-month rows.', call. = FALSE)
  if (any(!is.finite(num(df$index)) | num(df$index) <= 0)) stop('Price index contains non-positive or non-finite values.', call. = FALSE)
  invisible(df)
}

#' Tendulkar state-sector spatial price relatives
#'
#' A common all-India rural denominator preserves the official rural-urban price
#' difference while also retaining differences across states.
build_tendulkar_spatial_relatives <- function(poverty_lines, reference_rupees = 816) {
  df <- safe_df(poverty_lines)
  required <- c('state_code', 'sector', 'poverty_line_rupees')
  missing <- setdiff(required, names(df))
  if (length(missing)) stop('Poverty-line table is missing columns: ', paste(missing, collapse = ', '), call. = FALSE)
  value <- num(df$poverty_line_rupees)
  if (any(!is.finite(value) | value <= 0)) stop('Poverty lines must be positive and finite.', call. = FALSE)
  df$spatial_price_relative <- value / reference_rupees
  df
}

#' Link two index series over common months
#'
#' The median overlap ratio is resistant to isolated transcription errors. The
#' returned factor converts the old-series level to the new-series level.
price_link_factor <- function(old_index, new_index) {
  old <- num(old_index)
  new <- num(new_index)
  keep <- positive_finite(old) & positive_finite(new)
  if (!any(keep)) return(NA_real_)
  stats::median(new[keep] / old[keep], na.rm = TRUE)
}

#' Combine spatial and temporal price relatives
build_state_sector_deflator <- function(temporal_index, spatial_relatives, reference_period) {
  idx <- safe_df(temporal_index)
  spatial <- safe_df(spatial_relatives)
  validate_price_index(idx)
  keys <- c('state_code', 'sector')
  ref <- idx[idx$period %in% reference_period, , drop = FALSE]
  if (!nrow(ref)) stop('No observations fall in the requested price reference period.', call. = FALSE)
  ref_mean <- aggregate(index ~ state_code + sector, ref, function(x) mean(num(x), na.rm = TRUE))
  names(ref_mean)[names(ref_mean) == 'index'] <- 'reference_index'
  out <- merge(idx, ref_mean, by = keys, all.x = TRUE, sort = FALSE)
  out <- merge(out, spatial[c(keys, 'spatial_price_relative')], by = keys, all.x = TRUE, sort = FALSE)
  out$temporal_price_relative <- num(out$index) / num(out$reference_index)
  out$price_deflator <- num(out$spatial_price_relative) * num(out$temporal_price_relative)
  if (any(!positive_finite(out$price_deflator))) stop('Deflator construction left missing or invalid values.', call. = FALSE)
  out
}

#' Attach one price deflator to every NSS household
attach_household_deflator <- function(households, deflators, state_col, sector_col, period_col) {
  hh <- safe_df(households)
  d <- safe_df(deflators)
  hh$.price_state_code <- as.character(hh[[state_col]])
  hh$.price_sector <- price_sector(hh[[sector_col]])
  hh$.price_period <- as.character(hh[[period_col]])
  d$.price_state_code <- as.character(d$state_code)
  d$.price_sector <- price_sector(d$sector)
  d$.price_period <- as.character(d$period)
  keep <- c('.price_state_code', '.price_sector', '.price_period', 'price_deflator', 'price_source', 'fallback_reason')
  keep <- intersect(keep, names(d))
  out <- merge(hh, d[keep], by = c('.price_state_code', '.price_sector', '.price_period'), all.x = TRUE, sort = FALSE)
  if (any(!positive_finite(out$price_deflator))) stop('At least one household lacks a valid state-sector-period price deflator.', call. = FALSE)
  out
}
