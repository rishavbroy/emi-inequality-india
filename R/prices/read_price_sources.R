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

resolve_price_path <- function(path) {
  path <- as.character(path)[[1]]
  if (file.exists(path)) return(path)

  project_root <- Sys.getenv("EMI_PROJECT_ROOT", unset = "")
  if (nzchar(project_root)) {
    project_path <- file.path(project_root, path)
    if (file.exists(project_path)) return(project_path)
  }

  path
}

read_rbi_price_csv <- function(path) {
  path <- resolve_price_path(path)
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
  type_col <- price_column(
    raw,
    c("TYPE_LABOU_RN", "TYPE_LABOUR_RN", "LABOUR_TYPE", "LABOURER_TYPE"),
    required = FALSE
  )
  if (!is.null(type_col)) {
    value <- toupper(trimws(as.character(raw[[type_col]])))
    out <- rep(NA_character_, length(value))
    out[value %in% c("AL", "AGRICULTURAL LABOUR", "AGRICULTURAL LABOURER", "AGRICULTURAL LABOURERS")] <-
      "agricultural_labour"
    out[value %in% c("RL", "RURAL LABOUR", "RURAL LABOURER", "RURAL LABOURERS")] <-
      "rural_labour"
    return(out)
  }

  candidates <- intersect(
    c("ELEMENT", "COMD_ITEM", "COMMODITY", "COVERAGE_GEO_RN", "SERIES_NAME"),
    names(raw)
  )
  text <- if (length(candidates)) {
    apply(raw[candidates], 1, function(x) paste(toupper(as.character(x)), collapse = " "))
  } else {
    rep("", nrow(raw))
  }
  out <- rep(NA_character_, length(text))
  out[grepl("AGRICULTURAL LABOUR", text, fixed = TRUE)] <- "agricultural_labour"
  out[grepl("RURAL LABOUR", text, fixed = TRUE)] <- "rural_labour"
  out
}

read_cpi_alrl_state <- function(path) {
  raw <- read_rbi_price_csv(path)
  state_col <- price_column(raw, c("STATE_CODE"))
  date_col <- price_column(raw, c("TIME_PERIOD", "TIME"))
  value_col <- price_column(raw, c("OBS_VALUE", "VALUE_IN_ACTUALS", "VALUE"))
  series <- classify_alrl_series(raw)
  if (anyNA(series)) {
    stop("Could not distinguish CPI-AL from CPI-RL for every row in the RBI file.", call. = FALSE)
  }
  keep <- rep(TRUE, length(series))
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
  key <- gsub("[^A-Z0-9]+", "", toupper(trimws(as.character(x))))
  aliases <- c(
    VIJAYAWADA = "VIJAYWADA",
    VIZAG = "VISAKHAPATNAM",
    VISHAKHAPATNAM = "VISAKHAPATNAM",
    DOOMDOOMATINSUKIA = "DDTINSUKIA",
    DMTINSUKIA = "DDTINSUKIA",
    MUNGERJAMALPUR = "MONGERJAMALPUR",
    MONGHYRJAMALPUR = "MONGERJAMALPUR",
    BHILAI = "BHILLAI",
    VADODARA = "VADODRA",
    BANGALORE = "BENGALURU",
    MADIKERI = "MERCARA",
    KOLLAM = "QUILON",
    QUILLON = "QUILON",
    ALWAYEERNAKULAM = "ERNAKULAM",
    COONOR = "COONOOR",
    HALDI = "HALDIA",
    OTHERSGOA = "GOA",
    OTHERSHIMACHALPRADESH = "HIMACHALPRADESH",
    OTHERSTRIPURA = "TRIPURA",
    PONDICHERRY = "PUDUCHERRY",
    SHOLAPUR = "SOLAPUR",
    TEZPURRANGAPARA = "RANGAPARATEZPUR",
    TIRUCHIRAPPALLI = "TIRUCHIRAPALLY",
    TRICHY = "TIRUCHIRAPALLY",
    TRICHIRAPALLY = "TIRUCHIRAPALLY",
    WARANGAL = "WARRANGAL"
  )
  alias_i <- match(key, names(aliases))
  replace <- !is.na(alias_i)
  key[replace] <- unname(aliases[alias_i[replace]])
  key
}

cpi_iw_geography_column <- function(raw) {
  centre_candidates <- c(
    "CENTER_RN", "CENTRE_RN", "CENTRE", "CENTER", "CENTRE_NAME", "CENTER_NAME",
    "CENTRE_CODE", "CENTER_CODE", "GEOGRAPHICAL_COVERAGE", "GEOGRAPHICAL_COVERAGE_RN"
  )
  hit <- intersect(centre_candidates, names(raw))
  if (length(hit)) return(hit[[1]])
  price_column(raw, "STATE_CODE")
}

cpi_iw_all_india <- function(x) {
  key <- gsub("[^A-Z0-9]+", "", toupper(trimws(as.character(x))))
  key %in% c("ALLINDIA", "ALINDIA")
}

collapse_identical_price_rows <- function(out, keys, label) {
  duplicate <- duplicated(out[keys]) | duplicated(out[keys], fromLast = TRUE)
  if (!any(duplicate)) return(out)

  groups <- split(which(duplicate), interaction(out[duplicate, keys, drop = FALSE], drop = TRUE, sep = "\r"))
  conflict <- vapply(groups, function(i) length(unique(num(out$index[i]))) != 1L, logical(1))
  if (any(conflict)) {
    stop(label, " has conflicting duplicate observations after filtering.", call. = FALSE)
  }
  out[!duplicated(out[keys]), , drop = FALSE]
}

read_cpi_iw_all_india <- function(path, base_year = 2001) {
  raw <- read_rbi_price_csv(path)
  coverage_col <- cpi_iw_geography_column(raw)
  date_col <- price_column(raw, c("TIME_PERIOD", "TIME"))
  value_col <- price_column(raw, c("OBS_VALUE", "VALUE_IN_ACTUALS", "VALUE"))
  base_col <- price_column(raw, c("BASE_PER", "BASE_PERIOD", "BASE_YEAR"), required = FALSE)
  item_col <- price_column(raw, c("COMD_ITEM", "COMMODITY", "ELEMENT", "SERIES_NAME"), required = FALSE)

  coverage <- toupper(trimws(as.character(raw[[coverage_col]])))
  keep <- cpi_iw_all_india(coverage)
  if (!is.null(base_col)) {
    keep <- keep & grepl(as.character(base_year), as.character(raw[[base_col]]), fixed = TRUE)
  }
  if (!is.null(item_col)) {
    item <- toupper(as.character(raw[[item_col]]))
    general <- grepl("GENERAL|CPI.IW|CPI_IW", item)
    if (any(general)) keep <- keep & general
  }
  if (!any(keep)) stop("CPI-IW file has no All-India general-index rows on the requested base.", call. = FALSE)

  out <- data.frame(
    state_code = "ALL_INDIA",
    sector = "urban",
    period = price_month(raw[[date_col]][keep]),
    index = price_numeric(raw[[value_col]][keep]),
    stringsAsFactors = FALSE
  )
  out$year <- as.integer(format(out$period, "%Y"))
  out$month <- as.integer(format(out$period, "%m"))
  out$source_file <- basename(path)
  validate_price_index(out)
  out[order(out$period), , drop = FALSE]
}

read_cpi_iw_centres <- function(path, base_year = 2001) {
  raw <- read_rbi_price_csv(path)
  centre_col <- cpi_iw_geography_column(raw)
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
  keep <- keep & !cpi_iw_all_india(centre_raw)

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
  out <- collapse_identical_price_rows(
    out,
    keys = c("centre_key", "year", "month"),
    label = "CPI-IW file"
  )
  out[order(out$centre_key, out$period), , drop = FALSE]
}

validate_cpi_iw_weights <- function(weights, expected_total = 100, tolerance = 1e-8) {
  weights <- safe_df(weights)
  required <- c("state_code", "state_name", "centre", "weight")
  missing <- setdiff(required, names(weights))
  if (length(missing)) {
    stop("CPI-IW weights are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  weights$state_code <- trimws(as.character(weights$state_code))
  weights$state_name <- trimws(as.character(weights$state_name))
  weights$centre <- trimws(as.character(weights$centre))
  weights$centre_key <- normalise_cpi_iw_centre(weights$centre)
  weights$weight <- suppressWarnings(as.numeric(weights$weight))

  blank_identity <- !nzchar(weights$state_code) | !nzchar(weights$state_name) |
    !nzchar(weights$centre) | !nzchar(weights$centre_key)
  if (any(blank_identity | is.na(blank_identity))) {
    stop("CPI-IW centre weights contain a missing centre or state identity.", call. = FALSE)
  }
  if (anyNA(weights$weight) || any(!is.finite(weights$weight)) || any(weights$weight <= 0)) {
    stop("CPI-IW centre weights must be finite and positive.", call. = FALSE)
  }
  duplicate <- duplicated(weights$centre_key) | duplicated(weights$centre_key, fromLast = TRUE)
  if (any(duplicate)) {
    stop(
      "CPI-IW centre identities must be unique after name normalization: ",
      paste(unique(weights$centre[duplicate]), collapse = ", "),
      call. = FALSE
    )
  }
  if (abs(sum(weights$weight) - expected_total) > tolerance) {
    stop("CPI-IW centre weights must sum to ", expected_total, ".", call. = FALSE)
  }
  weights
}

read_cpi_iw_weights <- function(path) {
  path <- resolve_price_path(path)
  if (!file.exists(path)) stop("CPI-IW weights file does not exist: ", path, call. = FALSE)
  weights <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  validate_cpi_iw_weights(weights)
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

validate_price_source_paths <- function(paths) {
  required <- c("cpi_alrl", "cpi_iw", "cpi_ruc_2010", "cpi_ruc_2012")
  if (is.null(names(paths))) {
    stop("Price-source paths must be named.", call. = FALSE)
  }
  missing <- setdiff(required, names(paths))
  if (length(missing)) {
    stop("Price-source paths are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- as.list(paths[required])
  bad <- !vapply(out, function(x) length(x) == 1L && !is.na(x) && nzchar(as.character(x)), logical(1))
  if (any(bad)) {
    stop("Price-source paths must contain one nonempty path per source.", call. = FALSE)
  }
  lapply(out, as.character)
}

cpi_iw_state_periods <- function(
    estimation_start = as.Date("2004-07-01"),
    estimation_end = as.Date("2012-12-01"),
    link_start = as.Date("2013-01-01"),
    link_end = as.Date("2014-12-01")) {
  estimation_start <- price_boundary(estimation_start)
  estimation_end <- price_boundary(estimation_end)
  link_start <- price_boundary(link_start)
  link_end <- price_boundary(link_end)
  if (estimation_end < estimation_start || link_end < link_start) {
    stop("CPI-IW state aggregation windows must be ordered.", call. = FALSE)
  }
  unique(c(
    seq(estimation_start, estimation_end, by = "month"),
    seq(link_start, link_end, by = "month")
  ))
}

read_price_sources <- function(paths, cpi_iw_weights_file = "data/metadata/cpi_iw_centres_2001.csv") {
  paths <- validate_price_source_paths(paths)
  weights <- read_cpi_iw_weights(cpi_iw_weights_file)
  iw_centres <- read_cpi_iw_centres(paths$cpi_iw, base_year = 2001)
  iw_state_input <- iw_centres[iw_centres$period %in% cpi_iw_state_periods(), , drop = FALSE]
  list(
    cpi_alrl = read_cpi_alrl_state(paths$cpi_alrl),
    cpi_iw_centres = iw_centres,
    cpi_iw_states = aggregate_cpi_iw_to_state(iw_state_input, weights),
    cpi_iw_all_india = read_cpi_iw_all_india(paths$cpi_iw, base_year = 2001),
    cpi_ruc_2010 = read_cpi_ruc_state(paths$cpi_ruc_2010, expected_base = 2010),
    cpi_ruc_2012 = read_cpi_ruc_state(paths$cpi_ruc_2012, expected_base = 2012),
    cpi_iw_weights = weights
  )
}

price_source_paths <- function(paths) {
  rows <- require_manifest_files(paths, source_id = "prices")
  wanted <- c(
    cpi_alrl = "price_cpi_alrl_state",
    cpi_iw = "price_cpi_iw_centres",
    cpi_ruc_2010 = "price_cpi_ruc_2010",
    cpi_ruc_2012 = "price_cpi_ruc_2012"
  )
  found <- match(unname(wanted), rows$file_id)
  if (anyNA(found)) stop("The price manifest does not register all four production CPI files.", call. = FALSE)
  validate_price_source_paths(stats::setNames(rows$absolute_path[found], names(wanted)))
}
