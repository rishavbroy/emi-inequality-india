# Census worker tables used for historical validity and local economic-structure mechanisms.

census_worker_manifest_files <- function(paths, table, manifest_file = NULL, census_year = 2011L) {
  census_year <- as.integer(census_year)
  table <- toupper(trimws(plain_chr(table)))
  supported <- switch(
    as.character(census_year),
    `2001` = c("B04", "B25", "B26"),
    `2011` = c("B04", "B06", "B25A", "B25B"),
    character()
  )
  if (length(table) != 1L || is.na(table) || !table %in% supported) {
    stop(
      sprintf(
        "Census %d worker reader supports: %s.",
        census_year, paste(supported, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  census_manifest_files(paths, census_year, table, manifest_file)
}

read_census_worker_sheet <- function(path, skip) {
  need_pkg("readxl", "Census worker workbooks")
  readxl::read_excel(
    path,
    sheet = 1L,
    skip = skip,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
}

census_industry_groups <- function() {
  c(
    agriculture = "agriculture",
    mining = "mining",
    manufacturing = "manufacturing",
    utilities = "utilities",
    construction = "construction",
    trade = "trade",
    transport = "transport",
    accommodation_food = "accommodation_food",
    information_communication = "information_communication",
    finance_realestate_professional = "finance_realestate_professional",
    administrative_public = "administrative_public",
    education_health = "education_health",
    other_services = "other_services"
  )
}

census_industry_count_columns <- function(prefix) {
  c(
    paste0(prefix, "_workers_total"),
    paste0(prefix, "_cultivators"),
    paste0(prefix, "_agricultural_labourers"),
    paste0(prefix, "_agriculture_other"),
    paste0(prefix, "_mining"),
    paste0(prefix, "_manufacturing"),
    paste0(prefix, "_utilities"),
    paste0(prefix, "_construction"),
    paste0(prefix, "_trade"),
    paste0(prefix, "_transport"),
    paste0(prefix, "_accommodation_food"),
    paste0(prefix, "_information_communication"),
    paste0(prefix, "_finance_realestate_professional"),
    paste0(prefix, "_administrative_public"),
    paste0(prefix, "_education_health"),
    paste0(prefix, "_other_services")
  )
}

parse_census_b04_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 64L) stop("Census 2011 B04 sheet has fewer than 64 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = trimws(plain_chr(raw[[6L]])),
    main_workers_total = num(raw[[7L]]),
    main_cultivators = num(raw[[10L]]),
    main_agricultural_labourers = num(raw[[13L]]),
    main_agriculture_other = num(raw[[16L]]),
    main_mining = num(raw[[19L]]),
    main_manufacturing = num(raw[[22L]]) + num(raw[[25L]]),
    main_utilities = num(raw[[28L]]),
    main_construction = num(raw[[31L]]),
    main_trade = num(raw[[34L]]) + num(raw[[37L]]),
    main_accommodation_food = num(raw[[40L]]),
    main_transport = num(raw[[43L]]),
    main_information_communication = num(raw[[46L]]) + num(raw[[49L]]),
    main_finance_realestate_professional = num(raw[[52L]]),
    main_administrative_public = num(raw[[55L]]),
    main_education_health = num(raw[[58L]]),
    main_other_services = num(raw[[61L]]) + num(raw[[64L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "B0104" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$residence == "Total" & out$age_group == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

parse_census_b06_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 67L) stop("Census 2011 B06 sheet has fewer than 67 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = trimws(plain_chr(raw[[6L]])),
    marginal_workers_3_6_months = num(raw[[7L]]),
    marginal_workers_less_than_3_months = num(raw[[10L]]),
    marginal_cultivators = num(raw[[13L]]),
    marginal_agricultural_labourers = num(raw[[16L]]),
    marginal_agriculture_other = num(raw[[19L]]),
    marginal_mining = num(raw[[22L]]),
    marginal_manufacturing = num(raw[[25L]]) + num(raw[[28L]]),
    marginal_utilities = num(raw[[31L]]),
    marginal_construction = num(raw[[34L]]),
    marginal_trade = num(raw[[37L]]) + num(raw[[40L]]),
    marginal_accommodation_food = num(raw[[43L]]),
    marginal_transport = num(raw[[46L]]),
    marginal_information_communication = num(raw[[49L]]) + num(raw[[52L]]),
    marginal_finance_realestate_professional = num(raw[[55L]]),
    marginal_administrative_public = num(raw[[58L]]),
    marginal_education_health = num(raw[[61L]]),
    marginal_other_services = num(raw[[64L]]) + num(raw[[67L]]),
    stringsAsFactors = FALSE
  )
  out$marginal_workers_total <- out$marginal_workers_3_6_months +
    out$marginal_workers_less_than_3_months
  keep <- out$table == "B0706" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$residence == "Total" & out$age_group == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

validate_census_industry_partition <- function(x, prefix) {
  x <- safe_df(x)
  total <- paste0(prefix, "_workers_total")
  parts <- setdiff(census_industry_count_columns(prefix), total)
  required <- c("state_code", "district_code", total, parts)
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Census industry partition is missing required columns.", call. = FALSE)
  values <- as.matrix(data.frame(lapply(x[required[-(1:2)]], num), check.names = FALSE))
  if (any(!is.finite(values)) || any(values < 0)) {
    stop("Census industry counts must be finite and nonnegative.", call. = FALSE)
  }
  part_sum <- rowSums(as.matrix(data.frame(lapply(x[parts], num), check.names = FALSE)))
  if (any(num(x[[total]]) != part_sum)) {
    stop("Census industry categories do not sum exactly to total workers.", call. = FALSE)
  }
  x
}

read_census_b04_2011_file <- function(path) {
  validate_census_industry_partition(
    parse_census_b04_2011_sheet(read_census_worker_sheet(path, 7L)), "main"
  )
}

read_census_b06_2011_file <- function(path) {
  validate_census_industry_partition(
    parse_census_b06_2011_sheet(read_census_worker_sheet(path, 7L)), "marginal"
  )
}

read_census_worker_district_files <- function(files, reader, label) {
  out <- safe_bind_rows(lapply(files, reader))
  if (!nrow(out) || anyDuplicated(out[c("state_code", "district_code")])) {
    stop(label, " files must yield one row per district.", call. = FALSE)
  }
  out[order(out$state_code, out$district_code), , drop = FALSE]
}

read_census_b04_2011_district <- function(files) {
  read_census_worker_district_files(files, read_census_b04_2011_file, "Census B04")
}

read_census_b06_2011_district <- function(files) {
  read_census_worker_district_files(files, read_census_b06_2011_file, "Census B06")
}

census_occupation_divisions <- function() c(as.character(1:9), "X")

parse_census_b25_2011_sheet <- function(raw, worker_type = c("main", "marginal")) {
  worker_type <- match.arg(worker_type)
  raw <- safe_df(raw)
  if (ncol(raw) < 10L) stop("Census 2011 B25 sheet has fewer than 10 columns.", call. = FALSE)
  expected_table <- if (worker_type == "main") "B0425A" else "B0525B"
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    district_name = clean_census_district_label(raw[[4L]]),
    division = trimws(plain_chr(raw[[5L]])),
    subdivision = trimws(plain_chr(raw[[6L]])),
    group = trimws(plain_chr(raw[[7L]])),
    family = trimws(plain_chr(raw[[8L]])),
    occupation_name = trimws(plain_chr(raw[[9L]])),
    persons = num(raw[[10L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == expected_table & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$subdivision == "00" & out$group == "000" &
    out$family == "0000" & out$division %in% c("0", census_occupation_divisions())
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

summarise_census_b25_2011_district <- function(rows, worker_type = c("main", "marginal")) {
  worker_type <- match.arg(worker_type)
  x <- safe_df(rows)
  key <- paste(x$state_code, x$district_code, x$division, sep = "|")
  if (anyDuplicated(key)) stop("Census B25 contains duplicate district-by-division rows.", call. = FALSE)
  groups <- split(seq_len(nrow(x)), paste(x$state_code, x$district_code, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- x[index, , drop = FALSE]
    by_division <- setNames(seq_len(nrow(part)), part$division)
    if (!"0" %in% names(by_division)) {
      stop("Census B25 district is missing its published total row.", call. = FALSE)
    }
    value <- function(division) {
      if (!division %in% names(by_division)) return(0)
      num(part$persons[by_division[[division]]])[[1L]]
    }
    total <- value("0")
    divisions <- vapply(census_occupation_divisions(), value, numeric(1))
    if (!is.finite(total) || total < 0 || any(!is.finite(divisions)) || any(divisions < 0) ||
        total != sum(divisions)) {
      stop("Census B25 occupation divisions do not sum exactly to the table total.", call. = FALSE)
    }
    prefix <- if (worker_type == "main") "main" else "marginal"
    result <- data.frame(
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]],
      stringsAsFactors = FALSE
    )
    result[[paste0(prefix, "_workers_excl_cultivators_aglab_total")]] <- total
    for (division in names(divisions)) {
      result[[paste0(prefix, "_occupation_division_", tolower(division))]] <- divisions[[division]]
    }
    result
  }))
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Census B25 summaries are not unique by district.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

read_census_b25_2011_file <- function(path, worker_type) {
  raw <- read_census_worker_sheet(path, 5L)
  summarise_census_b25_2011_district(
    parse_census_b25_2011_sheet(raw, worker_type), worker_type
  )
}

read_census_b25a_2011_district <- function(files) {
  read_census_worker_district_files(
    files, function(path) read_census_b25_2011_file(path, "main"), "Census B25A"
  )
}

read_census_b25b_2011_district <- function(files) {
  read_census_worker_district_files(
    files, function(path) read_census_b25_2011_file(path, "marginal"), "Census B25B"
  )
}


census_2001_industry_groups <- function() {
  c(
    agriculture = "agriculture",
    mining = "mining",
    manufacturing = "manufacturing",
    utilities = "utilities",
    construction = "construction",
    trade = "trade",
    accommodation_food = "accommodation_food",
    transport_communication = "transport_communication",
    finance_realestate_business = "finance_realestate_business",
    public_social_other_services = "public_social_other_services"
  )
}

census_2001_industry_count_columns <- function() {
  c("main_workers_total", paste0("main_", unname(census_2001_industry_groups())))
}

parse_census_b04_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 49L) stop("Census 2001 B04 sheet has fewer than 49 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])),
    age_group = trimws(plain_chr(raw[[7L]])),
    main_workers_total = num(raw[[8L]]),
    main_agriculture = num(raw[[11L]]) + num(raw[[14L]]) + num(raw[[17L]]),
    main_mining = num(raw[[20L]]),
    main_manufacturing = num(raw[[23L]]) + num(raw[[26L]]),
    main_utilities = num(raw[[29L]]),
    main_construction = num(raw[[32L]]),
    main_trade = num(raw[[35L]]),
    main_accommodation_food = num(raw[[38L]]),
    main_transport_communication = num(raw[[41L]]),
    main_finance_realestate_business = num(raw[[44L]]),
    main_public_social_other_services = num(raw[[47L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "B0104" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & out$residence == "Total" & out$age_group == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  parts <- setdiff(census_2001_industry_count_columns(), "main_workers_total")
  if (any(!is.finite(as.matrix(out[c("main_workers_total", parts)]))) ||
      any(as.matrix(out[c("main_workers_total", parts)]) < 0) ||
      any(rowSums(as.matrix(out[parts])) != out$main_workers_total)) {
    stop("Census 2001 B04 industrial categories do not sum exactly to main workers.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

read_census_b04_2001_file <- function(path) {
  parse_census_b04_2001_sheet(read_census_worker_sheet(path, 7L))
}

read_census_b04_2001_district <- function(files) {
  read_census_worker_district_files(files, read_census_b04_2001_file, "Census 2001 B04")
}

parse_census_2001_worker_label <- function(x, state_code) {
  label <- trimws(plain_chr(x))
  district <- grepl("^District\\s*-", label, ignore.case = TRUE)
  code <- ifelse(
    district,
    sub("^.*\\s([0-9]{2})\\s*$", "\\1", label),
    NA_character_
  )
  data.frame(
    state_code = rep(normalize_census_code(state_code, 2L), length(label)),
    district_code = normalize_census_code(code, 2L),
    district_name = clean_census_district_label(label),
    is_district = district,
    stringsAsFactors = FALSE
  )
}

parse_census_b25_2001_sheet <- function(raw, state_code) {
  raw <- safe_df(raw)
  if (ncol(raw) < 8L) stop("Census 2001 B25 sheet has fewer than 8 columns.", call. = FALSE)
  identity <- parse_census_2001_worker_label(raw[[2L]], state_code)
  division <- toupper(trimws(plain_chr(raw[[3L]])))
  subdivision <- toupper(trimws(plain_chr(raw[[4L]])))
  group <- toupper(trimws(plain_chr(raw[[5L]])))
  family <- toupper(trimws(plain_chr(raw[[6L]])))
  top <- (division %in% as.character(0:9) & subdivision == "00") |
    (division == "X" & subdivision == "X9")
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    identity[c("state_code", "district_code", "district_name")],
    division = division,
    persons = num(raw[[8L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "B0425" & identity$is_district & top & group == "000" & family == "0000"
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

summarise_census_b25_2001_district <- function(rows) {
  x <- safe_df(rows)
  groups <- split(seq_len(nrow(x)), paste(x$state_code, x$district_code, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- x[index, , drop = FALSE]
    by_division <- setNames(seq_len(nrow(part)), part$division)
    expected <- c("0", census_occupation_divisions())
    if (!all(expected %in% names(by_division))) {
      stop("Census 2001 B25 district is missing a top-level occupation division.", call. = FALSE)
    }
    values <- vapply(expected, function(division) {
      num(part$persons[by_division[[division]]])[[1L]]
    }, numeric(1))
    if (any(!is.finite(values)) || any(values < 0) || values[["0"]] != sum(values[-1L])) {
      stop("Census 2001 B25 occupation divisions do not sum exactly to the table total.", call. = FALSE)
    }
    result <- data.frame(
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]],
      main_workers_excl_cultivators_aglab_total = values[["0"]],
      stringsAsFactors = FALSE
    )
    for (division in census_occupation_divisions()) {
      result[[paste0("main_occupation_division_", tolower(division))]] <- values[[division]]
    }
    result
  }))
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Census 2001 B25 summaries are not unique by district.", call. = FALSE)
  }
  out
}

read_census_b25_2001_file <- function(path) {
  state_code <- sub("^.*_([0-9]{2})\\.xls$", "\\1", basename(path), ignore.case = TRUE)
  summarise_census_b25_2001_district(
    parse_census_b25_2001_sheet(read_census_worker_sheet(path, 7L), state_code)
  )
}

read_census_b25_2001_district <- function(files) {
  read_census_worker_district_files(files, read_census_b25_2001_file, "Census 2001 B25")
}

parse_census_b26_2001_sheet <- function(raw, state_code) {
  raw <- safe_df(raw)
  if (ncol(raw) < 10L) stop("Census 2001 B26 sheet has fewer than 10 columns.", call. = FALSE)
  identity <- parse_census_2001_worker_label(raw[[2L]], state_code)
  division <- toupper(trimws(plain_chr(raw[[3L]])))
  subdivision <- toupper(trimws(plain_chr(raw[[4L]])))
  top <- (division %in% as.character(0:9) & subdivision == "00") |
    (division == "X" & subdivision == "X9")
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    identity[c("state_code", "district_code", "district_name")],
    division = division,
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = trimws(plain_chr(raw[[6L]])),
    main_persons = num(raw[[7L]]),
    marginal_persons = num(raw[[10L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "B26" & identity$is_district & top &
    out$residence == "Total" & out$age_group == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

summarise_census_b26_2001_district <- function(rows) {
  x <- safe_df(rows)
  groups <- split(seq_len(nrow(x)), paste(x$state_code, x$district_code, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- x[index, , drop = FALSE]
    by_division <- setNames(seq_len(nrow(part)), part$division)
    expected <- c("0", census_occupation_divisions())
    if (!all(expected %in% names(by_division))) {
      stop("Census 2001 B26 district is missing a top-level occupation division.", call. = FALSE)
    }
    value <- function(column) {
      vapply(expected, function(division) {
        num(part[[column]][by_division[[division]]])[[1L]]
      }, numeric(1))
    }
    main <- value("main_persons")
    marginal <- value("marginal_persons")
    if (any(!is.finite(c(main, marginal))) || any(c(main, marginal) < 0) ||
        main[["0"]] != sum(main[-1L]) || marginal[["0"]] != sum(marginal[-1L])) {
      stop("Census 2001 B26 occupation divisions do not sum exactly to worker totals.", call. = FALSE)
    }
    result <- data.frame(
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]],
      main_workers_excl_cultivators_aglab_total = main[["0"]],
      marginal_workers_excl_cultivators_aglab_total = marginal[["0"]],
      stringsAsFactors = FALSE
    )
    for (division in census_occupation_divisions()) {
      name <- tolower(division)
      result[[paste0("main_occupation_division_", name)]] <- main[[division]]
      result[[paste0("marginal_occupation_division_", name)]] <- marginal[[division]]
    }
    result
  }))
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Census 2001 B26 summaries are not unique by district.", call. = FALSE)
  }
  out
}

read_census_b26_2001_file <- function(path) {
  state_code <- sub("^.*_([0-9]{2})\\.xls$", "\\1", basename(path), ignore.case = TRUE)
  summarise_census_b26_2001_district(
    parse_census_b26_2001_sheet(read_census_worker_sheet(path, 7L), state_code)
  )
}

read_census_b26_2001_district <- function(files) {
  read_census_worker_district_files(files, read_census_b26_2001_file, "Census 2001 B26")
}
