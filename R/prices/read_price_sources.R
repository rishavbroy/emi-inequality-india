# Readers for RBI DBIE consumer-price extracts and CPI-IW state aggregation.
#
# RBI enhanced-query downloads use SDMX-like column names such as STATE_CODE,
# TIME_PERIOD, OBS_VALUE, BASE_PER, COVERAGE_GEO_RN, and COMD_ITEM. The readers
# retain source codes and return a small common schema used by later deflator
# construction.

price_column_names <- function(x) {
  toupper(gsub("[^A-Za-z0-9]+", "_", trimws(as.character(x))))
}

price_column <- function(df, candidates, required = TRUE) {
  hit <- intersect(price_column_names(candidates), names(df))
  if (length(hit)) return(hit[[1]])
  if (required) {
    stop("Price file is missing a required column; expected one of: ",
      paste(candidates, collapse = ", "), call. = FALSE
    )
  }
  NULL
}

read_rbi_price_csv <- function(path) {
  if (!file.exists(path)) stop("Price file does not exist: ", path, call. = FALSE)
  out <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- price_column_names(names(out))
  if (!nrow(out)) stop("Price file has no observations: ", path, call. = FALSE)
  out
}

price_month <- function(x) {
  date <- as.Date(as.character(x))
  if (anyNA(date)) stop("Price file contains an invalid TIME_PERIOD value.", call. = FALSE)
  as.Date(format(date, "%Y-%m-01"))
}

price_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)), fixed = TRUE)))
  if (any(!positive_finite(value))) {
    stop("Price file contains a missing, non-finite, or non-positive index value.", call. = FALSE)
  }
  value
}

read_cpi_ruc_state <- function(path, expected_base = NULL) {
  raw <- read_rbi_price_csv(path)
  state_col <- price_column(raw, c("STATE_CODE"))
  date_col <- price_column(raw, c("TIME_PERIOD", "TIME"))
  value_col <- price_column(raw, c("OBS_VALUE", "VALUE_IN_ACTUALS", "VALUE"))
  coverage_col <- price_column(raw, c("COVERAGE_GEO_RN", "GEOGRAPHICAL_COVERAGE"))
  item_col <- price_column(raw, c("COMD_ITEM", "COMMODITY"), required = FALSE)
  base_col <- price_column(raw, c("BASE_PER", "BASE_PERIOD"), required = FALSE)

  keep <- rep(TRUE, nrow(raw))
  if (!is.null(item_col)) {
    item <- toupper(trimws(as.character(raw[[item_col]])))
    keep <- keep & (item %in% c("C_GIAG", "GENERAL_INDEX", "GENERAL INDEX") |
      grepl("GENERAL", item, fixed = TRUE))
  }
  sector <- price_sector(raw[[coverage_col]])
  keep <- keep & !is.na(sector)
  if (!is.null(expected_base) && !is.null(base_col)) {
    keep <- keep & grepl(as.character(expected_base), as.character(raw[[base_col]]), fixed = TRUE)
  }

  out <- data.frame(
    state_code = as.character(raw[[state_col]][keep]),
    sector = sector[keep],
    period = price_month(raw[[date_col]][keep]),
    index = price_numeric(raw[[value_col]][keep]),
    stringsAsFactors = FALSE
  )
  out$year <- as.integer(format(out$period, "%Y"))
  out$month <- as.integer(format(out$period, "%m"))
  out$base_period <- if (is.null(base_col)) NA_character_ else as.character(raw[[base_col]][keep])
  out$source_file <- basename(path)
  validate_price_index(out)
  out[order(out$state_code, out$sector, out$period), , drop = FALSE]
}

classify_alrl_series <- function(raw) {
  candidates <- intersect(
    c("ELEMENT", "DATAFLOW", "COMD_ITEM", "COMMODITY", "COVERAGE_GEO_RN", "SERIES_NAME"),
    names(raw)
  )
  text <- if (length(candidates)) {
    apply(raw[candidates], 1, function(x) paste(toupper(as.character(x)), collapse = " "))
  } else {
    rep("", nrow(raw))
  }
  out <- rep(NA_character_, length(text))
  out[grepl("AGRICULTURAL LABOUR|CPI[_ -]?AL|AGRL", text)] <- "agricultural_labour"
  out[grepl("RURAL LABOUR|CPI[_ -]?RL", text)] <- "rural_labour"
  out
}

read_cpi_alrl_state <- function(path) {
  raw <- read_rbi_price_csv(path)
  state_col <- price_column(raw, c("STATE_CODE"))
  date_col <- price_column(raw, c("TIME_PERIOD", "TIME"))
  value_col <- price_column(raw, c("OBS_VALUE", "VALUE_IN_ACTUALS", "VALUE"))
  series <- classify_alrl_series(raw)
  if (all(is.na(series))) {
    stop("Could not distinguish CPI-AL from CPI-RL rows in the RBI file.", call. = FALSE)
  }
  keep <- !is.na(series)
  out <- data.frame(
    state_code = as.character(raw[[state_col]][keep]),
    labour_series = series[keep],
    sector = "rural",
    period = price_month(raw[[date_col]][keep]),
    index = price_numeric(raw[[value_col]][keep]),
    stringsAsFactors = FALSE
  )
  out$year <- as.integer(format(out$period, "%Y"))
  out$month <- as.integer(format(out$period, "%m"))
  out$source_file <- basename(path)
  keys <- c("state_code", "labour_series", "year", "month")
  validate_price_index(out, keys = keys)
  out[order(out$state_code, out$labour_series, out$period), , drop = FALSE]
}

normalise_cpi_iw_centre <- function(x) {
  key <- toupper(gsub("[^A-Z0-9]+", "", trimws(as.character(x))))
  aliases <- c(
    VIJAYAWADA = "VIJAYWADA", VIZAG = "VISAKHAPATNAM",
    VISHAKHAPATNAM = "VISAKHAPATNAM", DOOMDOOMATINSUKIA = "DDTINSUKIA",
    DOOMDOOMATINSUKIA = "DDTINSUKIA", MUNGERJAMALPUR = "MONGERJAMALPUR",
    BHILAI = "BHILLAI", VADODARA = "VADODRA", BANGALORE = "BENGALURU",
    MADIKERI = "MERCARA", KOLLAM = "QUILON", TIRUCHIRAPPALLI = "TIRUCHIRAPALLY",
    TRICHY = "TIRUCHIRAPALLY", WARANGAL = "WARRANGAL"
  )
  replace <- key %in% names(aliases)
  key[replace] <- unname(aliases[key[replace]])
  key
}

read_cpi_iw_centres <- function(path, base_year = 2001) {
  raw <- read_rbi_price_csv(path)
  centre_col <- price_column(raw, c(
    "CENTRE", "CENTER", "CENTRE_NAME", "CENTER_NAME", "CENTRE_CODE",
    "CENTER_CODE", "GEOGRAPHICAL_COVERAGE", "GEOGRAPHICAL_COVERAGE_RN",
    "STATE_CODE"
  ))
  date_col <- price_column(raw, c("TIME_PERIOD", "TIME"))
  value_col <- price_column(raw, c("OBS_VALUE", "VALUE_IN_ACTUALS", "VALUE"))
  base_col <- price_column(raw, c("BASE_PER", "BASE_PERIOD", "BASE_YEAR"), required = FALSE)
  item_col <- price_column(raw, c("COMD_ITEM", "COMMODITY", "ELEMENT", "SERIES_NAME"), required = FALSE)

  keep <- rep(TRUE, nrow(raw))
  if (!is.null(base_col)) {
    keep <- keep & grepl(as.character(base_year), as.character(raw[[base_col]]), fixed = TRUE)
  }
  if (!is.null(item_col)) {
    item <- toupper(as.character(raw[[item_col]]))
    general <- grepl("GENERAL|CPI.IW|CPI_IW", item)
    if (any(general)) keep <- keep & general
  }
  centre_raw <- trimws(as.character(raw[[centre_col]]))
  keep <- keep & !toupper(centre_raw) %in% c("ALL INDIA", "ALL_INDIA", "ALL-INDIA")

  out <- data.frame(
    centre = centre_raw[keep],
    centre_key = normalise_cpi_iw_centre(centre_raw[keep]),
    period = price_month(raw[[date_col]][keep]),
    index = price_numeric(raw[[value_col]][keep]),
    stringsAsFactors = FALSE
  )
  out$year <- as.integer(format(out$period, "%Y"))
  out$month <- as.integer(format(out$period, "%m"))
  out$base_period <- if (is.null(base_col)) NA_character_ else as.character(raw[[base_col]][keep])
  out$source_file <- basename(path)
  if (anyDuplicated(out[c("centre_key", "year", "month")])) {
    stop("CPI-IW file has duplicate centre-month observations after filtering.", call. = FALSE)
  }
  out[order(out$centre_key, out$period), , drop = FALSE]
}

read_cpi_iw_weights <- function(path) {
  weights <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("state_code", "state_name", "centre", "weight")
  missing <- setdiff(required, names(weights))
  if (length(missing)) {
    stop("CPI-IW weights are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  weights$centre_key <- normalise_cpi_iw_centre(weights$centre)
  weights$weight <- num(weights$weight)
  if (any(!positive_finite(weights$weight)) || anyDuplicated(weights$centre_key)) {
    stop("CPI-IW centre weights must be positive and unique.", call. = FALSE)
  }
  if (abs(sum(weights$weight) - 100) > 1e-8) {
    stop("CPI-IW centre weights must sum to 100.", call. = FALSE)
  }
  weights
}

aggregate_cpi_iw_to_state <- function(centre_index, centre_weights) {
  index <- safe_df(centre_index)
  weights <- safe_df(centre_weights)
  required_index <- c("centre_key", "period", "index")
  required_weights <- c("centre_key", "state_code", "weight")
  missing <- c(setdiff(required_index, names(index)), setdiff(required_weights, names(weights)))
  if (length(missing)) stop("CPI-IW aggregation is missing columns: ", paste(unique(missing), collapse = ", "), call. = FALSE)

  unmatched <- setdiff(unique(index$centre_key), weights$centre_key)
  if (length(unmatched)) {
    stop("CPI-IW observations have no official centre weight: ", paste(unmatched, collapse = ", "), call. = FALSE)
  }
  joined <- merge(index, weights[c("centre_key", "state_code", "weight")], by = "centre_key", all.x = TRUE, sort = FALSE)
  expected <- stats::aggregate(centre_key ~ state_code, weights, length)
  names(expected)[2] <- "expected_centres"
  observed <- stats::aggregate(centre_key ~ state_code + period, joined, function(x) length(unique(x)))
  names(observed)[3] <- "observed_centres"
  observed <- merge(observed, expected, by = "state_code", all.x = TRUE, sort = FALSE)
  incomplete <- observed$observed_centres != observed$expected_centres
  if (any(incomplete)) {
    bad <- observed[incomplete, c("state_code", "period", "observed_centres", "expected_centres")]
    stop("CPI-IW state aggregation has incomplete centre coverage; first incomplete row: ",
      paste(bad[1, ], collapse = "/"), call. = FALSE
    )
  }

  split_rows <- split(seq_len(nrow(joined)), interaction(joined$state_code, joined$period, drop = TRUE))
  out <- do.call(rbind, lapply(split_rows, function(i) {
    data.frame(
      state_code = joined$state_code[i[1]], sector = "urban", period = joined$period[i[1]],
      index = stats::weighted.mean(num(joined$index[i]), num(joined$weight[i])),
      centre_count = length(unique(joined$centre_key[i])), stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out$year <- as.integer(format(out$period, "%Y"))
  out$month <- as.integer(format(out$period, "%m"))
  validate_price_index(out)
  out[order(out$state_code, out$period), , drop = FALSE]
}

read_price_sources <- function(paths, cpi_iw_weights_file = "data/metadata/cpi_iw_centres_2001.csv") {
  required <- c("cpi_alrl", "cpi_iw", "cpi_ruc_2010", "cpi_ruc_2012")
  missing <- setdiff(required, names(paths))
  if (length(missing)) stop("Price-source paths are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  weights <- read_cpi_iw_weights(cpi_iw_weights_file)
  iw_centres <- read_cpi_iw_centres(paths$cpi_iw, base_year = 2001)
  list(
    cpi_alrl = read_cpi_alrl_state(paths$cpi_alrl),
    cpi_iw_centres = iw_centres,
    cpi_iw_states = aggregate_cpi_iw_to_state(iw_centres, weights),
    cpi_ruc_2010 = read_cpi_ruc_state(paths$cpi_ruc_2010, expected_base = 2010),
    cpi_ruc_2012 = read_cpi_ruc_state(paths$cpi_ruc_2012, expected_base = 2012),
    cpi_iw_weights = weights
  )
}
