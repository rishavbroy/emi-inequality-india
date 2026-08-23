# Convert registered consumption-survey timing fields to the monthly price system.
# Legacy nss_* entry points remain thin compatibility wrappers.

nss_wave_months <- function(wave, registry = read_consumption_survey_registry()) {
  survey_period_months(consumption_survey_spec_for_wave(registry, wave))
}

normalise_nss_subround <- function(x) {
  value <- suppressWarnings(as.integer(gsub("[^0-9]", "", plain_chr(x))))
  value[!value %in% 1:4] <- NA_integer_
  value
}

survey_subround_for_month <- function(period, specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  if (!identical(spec$price_timing[[1]], "quarterly_subround")) {
    stop("Survey does not use quarterly NSS sub-round price timing: ", spec$survey_id[[1]], call. = FALSE)
  }
  group_months <- as.integer(spec$price_group_months[[1]])
  months <- survey_period_months(spec)
  if (length(months) %% group_months != 0L) {
    stop("Survey months cannot be partitioned into complete price groups.", call. = FALSE)
  }
  period <- price_month_start(period)
  position <- match(period, months)
  out <- rep(NA_integer_, length(period))
  ok <- !is.na(position)
  out[ok] <- ((position[ok] - 1L) %/% group_months) + 1L
  out
}

nss_subround_for_month <- function(period, wave, registry = read_consumption_survey_registry()) {
  survey_subround_for_month(period, consumption_survey_spec_for_wave(registry, wave))
}

build_survey_subround_deflators <- function(deflators, specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  if (!identical(spec$price_timing[[1]], "quarterly_subround")) {
    stop("Survey does not use quarterly NSS sub-round price timing: ", spec$survey_id[[1]], call. = FALSE)
  }
  group_months <- as.integer(spec$price_group_months[[1]])
  d <- safe_df(deflators)
  required <- c("state_code", "sector", "period", "price_deflator")
  missing <- setdiff(required, names(d))
  if (length(missing)) stop("Price deflators are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  d$period <- price_month_start(d$period)
  d$subround <- survey_subround_for_month(d$period, spec)
  d <- d[!is.na(d$subround), , drop = FALSE]
  if (!nrow(d)) stop("Price deflators do not cover the requested consumption survey.", call. = FALSE)

  key <- interaction(d$state_code, d$sector, d$subround, drop = TRUE, sep = "\r")
  out <- safe_bind_rows(lapply(split(seq_len(nrow(d)), key), function(i) {
    if (length(i) != group_months) {
      stop("Each survey state-sector sub-round must contain exactly ", group_months, " price months.", call. = FALSE)
    }
    z <- d[i[[1]], c("state_code", "sector"), drop = FALSE]
    z$subround <- d$subround[i[[1]]]
    z$period_start <- min(d$period[i])
    z$period_end <- max(d$period[i])
    z$price_deflator <- mean(num(d$price_deflator[i]))
    z$spatial_price_relative <- unique_or_na(d$spatial_price_relative[i])
    z$price_source <- collapse_price_provenance(d$price_source[i])
    z$temporal_state_source <- collapse_price_provenance(d$temporal_state_source[i])
    z$state_rule <- collapse_price_provenance(d$state_rule[i])
    z$fallback_reason <- collapse_price_provenance(d$fallback_reason[i])
    z
  }))
  if (any(!positive_finite(out$price_deflator))) stop("Survey sub-round deflators contain invalid values.", call. = FALSE)
  if (anyDuplicated(out[c("state_code", "sector", "subround")])) stop("Duplicate survey sub-round deflators.", call. = FALSE)
  out[order(out$state_code, out$sector, out$subround), , drop = FALSE]
}

build_nss_subround_deflators <- function(deflators, wave, registry = read_consumption_survey_registry()) {
  build_survey_subround_deflators(deflators, consumption_survey_spec_for_wave(registry, wave))
}

unique_or_na <- function(x) {
  x <- unique(x[!is.na(x)])
  if (length(x) == 1L) x[[1]] else NA
}

collapse_price_provenance <- function(x) {
  x <- sort(unique(trimws(as.character(x))))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) paste(x, collapse = ";") else NA_character_
}

nss_state_name_from_code <- function(x) {
  code <- sprintf("%02d", as.integer(num(x)))
  out <- census_2001_state_name(code)
  out[code == "36"] <- "Telangana"
  out
}

resolve_nss_price_state <- function(x, poverty_lines = read_tendulkar_poverty_lines()) {
  value <- trimws(plain_chr(x))
  direct <- toupper(value)
  codes <- unique(as.character(poverty_lines$state_code))
  out <- ifelse(direct %in% codes, direct, NA_character_)

  numeric_code <- grepl("^[0-9]{1,2}$", value)
  state_name <- rep(NA_character_, length(value))
  state_name[numeric_code] <- nss_state_name_from_code(value[numeric_code])
  state_name[!numeric_code] <- value[!numeric_code]
  name_key <- canonicalize_state_name(state_name)
  metadata_key <- canonicalize_state_name(poverty_lines$state_name)
  mapped <- poverty_lines$state_code[match(name_key, metadata_key)]
  out[is.na(out)] <- mapped[is.na(out)]
  out
}

attach_survey_subround_deflator <- function(households, deflators, specification, state_col, sector_col, subround_col) {
  hh <- safe_df(households)
  hh$.price_row <- seq_len(nrow(hh))
  hh$.price_state_code <- resolve_nss_price_state(hh[[state_col]])
  hh$.price_sector <- price_sector(hh[[sector_col]])
  hh$.price_subround <- normalise_nss_subround(hh[[subround_col]])
  if (anyNA(hh$.price_state_code) || anyNA(hh$.price_sector) || anyNA(hh$.price_subround)) {
    stop("Survey households contain unresolved state, sector, or sub-round price keys.", call. = FALSE)
  }

  d <- build_survey_subround_deflators(deflators, specification)
  names(d)[names(d) == "state_code"] <- ".price_state_code"
  names(d)[names(d) == "sector"] <- ".price_sector"
  names(d)[names(d) == "subround"] <- ".price_subround"
  out <- merge(hh, d, by = c(".price_state_code", ".price_sector", ".price_subround"), all.x = TRUE, sort = FALSE)
  out <- out[order(out$.price_row), , drop = FALSE]
  out$.price_row <- NULL
  rownames(out) <- NULL
  if (any(!positive_finite(out$price_deflator))) stop("At least one survey household lacks a valid sub-round price deflator.", call. = FALSE)
  out
}

attach_nss_subround_deflator <- function(households, deflators, wave, state_col, sector_col, subround_col,
                                         registry = read_consumption_survey_registry()) {
  attach_survey_subround_deflator(
    households, deflators, consumption_survey_spec_for_wave(registry, wave),
    state_col, sector_col, subround_col
  )
}
