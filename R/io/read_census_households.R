# Census 2011 household human-capital and worker-intensity tables.

census_household_manifest_files <- function(paths, table, manifest_file = NULL, census_year = 2011L) {
  census_year <- as.integer(census_year)
  table <- toupper(trimws(plain_chr(table)))
  supported <- list(
    `2001` = c("HH09", "HH13", "HH15", "HH15A"),
    `2011` = c("HH08", "HH10", "HH11")
  )
  valid <- supported[[as.character(census_year)]]
  if (is.null(valid) || length(table) != 1L || is.na(table) || !table %in% valid) {
    stop(
      "Census household reader supports 2001 HH09/HH13/HH15/HH15A and 2011 HH08/HH10/HH11.",
      call. = FALSE
    )
  }
  census_manifest_files(paths, census_year, table, manifest_file)
}

normalize_census_household_category <- function(x) {
  gsub("[[:space:]]+", " ", trimws(plain_chr(x)))
}

read_census_household_sheet <- function(path) {
  need_pkg("readxl", "Census household workbooks")
  readxl::read_excel(
    path, sheet = 1L, skip = 6L, col_names = FALSE,
    col_types = "text", .name_repair = "minimal"
  )
}

summarise_census_household_rows <- function(rows, labels, label) {
  x <- safe_df(rows)
  x$category <- normalize_census_household_category(x$category)
  labels <- normalize_census_household_category(labels)
  key <- paste(x$state_code, x$district_code, x$category, sep = "|")
  if (anyDuplicated(key)) stop(label, " contains duplicate district-category rows.", call. = FALSE)
  groups <- split(seq_len(nrow(x)), paste(x$state_code, x$district_code, sep = "|"))
  out <- lapply(groups, function(index) {
    part <- x[index, , drop = FALSE]
    by_label <- setNames(seq_len(nrow(part)), part$category)
    missing <- setdiff(labels, names(by_label))
    if (length(missing)) {
      stop(label, " district is missing required rows: ", paste(missing, collapse = "; "), call. = FALSE)
    }
    part[unname(by_label[labels]), , drop = FALSE]
  })
  out
}

parse_census_hh08_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 13L) stop("Census 2011 HH08 sheet has fewer than 13 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    category = normalize_census_household_category(raw[[6L]]),
    households = num(raw[[7L]]),
    size_lt3 = num(raw[[8L]]), size_3 = num(raw[[9L]]), size_4 = num(raw[[10L]]),
    size_5 = num(raw[[11L]]), size_6 = num(raw[[12L]]), size_7_plus = num(raw[[13L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "HH08" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$residence == "Total" &
    out$category %in% c("Total", "None", "1", "2", "3", "4+")
  out <- out[keep %in% TRUE, , drop = FALSE]
  size_cols <- c("size_lt3", "size_3", "size_4", "size_5", "size_6", "size_7_plus")
  sizes <- as.matrix(data.frame(lapply(out[size_cols], num), check.names = FALSE))
  if (any(!is.finite(out$households)) || any(out$households < 0) ||
      any(!is.finite(sizes)) || any(sizes < 0) || any(rowSums(sizes) != out$households)) {
    stop("Census HH08 household-size cells must be finite, nonnegative, and exhaust each row total.", call. = FALSE)
  }
  out
}

summarise_census_hh08_2011_district <- function(rows) {
  labels <- c("Total", "None", "1", "2", "3", "4+")
  groups <- summarise_census_household_rows(rows, labels, "Census HH08")
  out <- safe_bind_rows(lapply(groups, function(part) {
    values <- setNames(num(part$households), part$category)
    if (values[["Total"]] != sum(values[c("None", "1", "2", "3", "4+")])) {
      stop("Census HH08 literacy-count categories do not exhaust total households.", call. = FALSE)
    }
    data.frame(
      state_code = part$state_code[[1L]], district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]], households_total = values[["Total"]],
      households_no_literate = values[["None"]], households_1_literate = values[["1"]],
      households_2_literates = values[["2"]], households_3_literates = values[["3"]],
      households_4_plus_literates = values[["4+"]], stringsAsFactors = FALSE
    )
  }))
  out$households_with_literate_member <- out$households_total - out$households_no_literate
  out
}

parse_census_hh10_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 13L) stop("Census 2011 HH10 sheet has fewer than 13 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])), state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L), district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])), category = normalize_census_household_category(raw[[6L]]),
    households = num(raw[[7L]]), households_age15_plus = num(raw[[8L]]),
    size_1 = num(raw[[9L]]), size_2 = num(raw[[10L]]), size_3_6 = num(raw[[11L]]),
    size_7_10 = num(raw[[12L]]), size_11_plus = num(raw[[13L]]), stringsAsFactors = FALSE
  )
  labels <- c(
    "Households with atleast one member literate", "Households with No matriculate and above",
    "Households with at least one matriculate and above", "Households with at least one male matriculate and above",
    "Households with at least one female matriculate and above", "Households with at least one graduate and above",
    "Households with at least one male graduate and above", "Households with at least one female graduate and above"
  )
  keep <- out$table == "HH10" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$residence == "Total" & out$category %in% labels
  out <- out[keep %in% TRUE, , drop = FALSE]
  sizes <- as.matrix(data.frame(lapply(out[c("size_1", "size_2", "size_3_6", "size_7_10", "size_11_plus")], num), check.names = FALSE))
  if (any(!is.finite(out$households)) || any(out$households < 0) ||
      any(!is.finite(out$households_age15_plus)) || any(out$households_age15_plus < 0) ||
      any(out$households_age15_plus > out$households) || any(!is.finite(sizes)) || any(sizes < 0) ||
      any(rowSums(sizes) != out$households_age15_plus)) {
    stop("Census HH10 age-15+ household-size cells do not satisfy the published denominator contract.", call. = FALSE)
  }
  out
}

summarise_census_hh10_2011_district <- function(rows) {
  labels <- c(
    literate = "Households with atleast one member literate",
    no_matriculate = "Households with No matriculate and above",
    matriculate = "Households with at least one matriculate and above",
    male_matriculate = "Households with at least one male matriculate and above",
    female_matriculate = "Households with at least one female matriculate and above",
    graduate = "Households with at least one graduate and above",
    male_graduate = "Households with at least one male graduate and above",
    female_graduate = "Households with at least one female graduate and above"
  )
  groups <- summarise_census_household_rows(rows, unname(labels), "Census HH10")
  safe_bind_rows(lapply(groups, function(part) {
    by_label <- setNames(seq_len(nrow(part)), part$category)
    value <- function(id) num(part$households[by_label[[labels[[id]]]]])[[1L]]
    value_age15 <- function(id) num(part$households_age15_plus[by_label[[labels[[id]]]]])[[1L]]
    mat <- value("matriculate"); grad <- value("graduate")
    male_mat <- value("male_matriculate"); female_mat <- value("female_matriculate")
    male_grad <- value("male_graduate"); female_grad <- value("female_graduate")
    if (any(c(male_mat, female_mat, grad) > mat) || male_grad > male_mat || female_grad > female_mat ||
        male_grad > grad || female_grad > grad ||
        any(vapply(c("matriculate", "male_matriculate", "female_matriculate", "graduate", "male_graduate", "female_graduate"),
          function(id) value_age15(id) != value(id), logical(1)))) {
      stop("Census HH10 matriculate/graduate subset counts violate their published nesting.", call. = FALSE)
    }
    data.frame(
      state_code = part$state_code[[1L]], district_code = part$district_code[[1L]], district_name = part$district_name[[1L]],
      households_with_literate_member = value("literate"), households_no_matriculate = value("no_matriculate"),
      households_age15_plus = value_age15("no_matriculate") + value_age15("matriculate"),
      households_with_matriculate = mat, households_with_male_matriculate = male_mat,
      households_with_female_matriculate = female_mat, households_with_graduate = grad,
      households_with_male_graduate = male_grad, households_with_female_graduate = female_grad,
      stringsAsFactors = FALSE
    )
  }))
}

parse_census_hh11_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 16L) stop("Census 2011 HH11 sheet has fewer than 16 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])), state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L), district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])), category = normalize_census_household_category(raw[[6L]]),
    households = num(raw[[7L]]), workers_total = num(raw[[8L]]), main_workers = num(raw[[9L]]),
    marginal_workers_3_6_months = num(raw[[10L]]), marginal_workers_lt3_months = num(raw[[11L]]),
    size_1 = num(raw[[12L]]), size_2 = num(raw[[13L]]), size_3_6 = num(raw[[14L]]),
    size_7_10 = num(raw[[15L]]), size_11_plus = num(raw[[16L]]), stringsAsFactors = FALSE
  )
  keep <- out$table == "HH11" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$residence == "Total" & out$category %in% c("Total", "None", "1", "2", "3", "4+")
  out <- out[keep %in% TRUE, , drop = FALSE]
  sizes <- as.matrix(data.frame(lapply(out[c("size_1", "size_2", "size_3_6", "size_7_10", "size_11_plus")], num), check.names = FALSE))
  worker_parts <- out$main_workers + out$marginal_workers_3_6_months + out$marginal_workers_lt3_months
  if (any(!is.finite(out$households)) || any(out$households < 0) || any(!is.finite(out$workers_total)) ||
      any(out$workers_total < 0) || any(!is.finite(sizes)) || any(sizes < 0) ||
      any(rowSums(sizes) != out$households) || any(worker_parts != out$workers_total)) {
    stop("Census HH11 household-size or worker-status accounting identity fails.", call. = FALSE)
  }
  out
}

summarise_census_hh11_2011_district <- function(rows) {
  labels <- c("Total", "None", "1", "2", "3", "4+")
  groups <- summarise_census_household_rows(rows, labels, "Census HH11")
  safe_bind_rows(lapply(groups, function(part) {
    by_label <- setNames(seq_len(nrow(part)), part$category)
    hh <- function(label) num(part$households[by_label[[label]]])[[1L]]
    total <- hh("Total")
    if (total != sum(vapply(c("None", "1", "2", "3", "4+"), hh, numeric(1)))) {
      stop("Census HH11 worker-count categories do not exhaust total households.", call. = FALSE)
    }
    total_row <- part[by_label[["Total"]], , drop = FALSE]
    data.frame(
      state_code = part$state_code[[1L]], district_code = part$district_code[[1L]], district_name = part$district_name[[1L]],
      households_total = total, households_no_workers = hh("None"), households_1_worker = hh("1"),
      households_2_workers = hh("2"), households_3_workers = hh("3"), households_4_plus_workers = hh("4+"),
      workers_total = num(total_row$workers_total), main_workers = num(total_row$main_workers),
      marginal_workers_3_6_months = num(total_row$marginal_workers_3_6_months),
      marginal_workers_lt3_months = num(total_row$marginal_workers_lt3_months), stringsAsFactors = FALSE
    )
  }))
}

read_census_household_district_files <- function(files, parser, summariser, label) {
  out <- safe_bind_rows(lapply(files, function(path) summariser(parser(read_census_household_sheet(path)))))
  if (!nrow(out) || anyDuplicated(out[c("state_code", "district_code")])) {
    stop(label, " files must yield one row per district.", call. = FALSE)
  }
  out[order(out$state_code, out$district_code), , drop = FALSE]
}

read_census_hh08_2011_district <- function(files) read_census_household_district_files(files, parse_census_hh08_2011_sheet, summarise_census_hh08_2011_district, "Census HH08")
read_census_hh10_2011_district <- function(files) read_census_household_district_files(files, parse_census_hh10_2011_sheet, summarise_census_hh10_2011_district, "Census HH10")
read_census_hh11_2011_district <- function(files) read_census_household_district_files(files, parse_census_hh11_2011_sheet, summarise_census_hh11_2011_district, "Census HH11")


parse_census_hh09_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 14L) stop("Census 2001 HH09 sheet has fewer than 14 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])), state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L), district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])), category = normalize_census_household_category(raw[[7L]]),
    households = num(raw[[8L]]), size_lt3 = num(raw[[9L]]), size_3 = num(raw[[10L]]),
    size_4 = num(raw[[11L]]), size_5 = num(raw[[12L]]), size_6 = num(raw[[13L]]),
    size_7_plus = num(raw[[14L]]), stringsAsFactors = FALSE
  )
  keep <- out$table == "HH09" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & toupper(out$residence) == "TOTAL" &
    out$category %in% c("Total", "None", "1", "2", "3", "4+")
  out <- out[keep %in% TRUE, , drop = FALSE]
  sizes <- as.matrix(data.frame(lapply(out[c("size_lt3", "size_3", "size_4", "size_5", "size_6", "size_7_plus")], num), check.names = FALSE))
  if (any(!is.finite(out$households)) || any(out$households < 0) || any(!is.finite(sizes)) ||
      any(sizes < 0) || any(rowSums(sizes) != out$households)) {
    stop("Census 2001 HH09 household-size cells must exhaust each row total.", call. = FALSE)
  }
  out
}

summarise_census_hh09_2001_district <- function(rows) {
  rows <- safe_df(rows)
  labels <- c("Total", "None", "1", "2", "3", "4+")
  groups <- summarise_census_household_rows(rows, labels, "Census 2001 HH09")
  out <- safe_bind_rows(lapply(groups, function(part) {
    values <- setNames(num(part$households), part$category)
    if (values[["Total"]] != sum(values[c("None", "1", "2", "3", "4+")])) {
      stop("Census 2001 HH09 literacy-count categories do not exhaust total households.", call. = FALSE)
    }
    data.frame(
      state_code = part$state_code[[1L]], district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]], households_total = values[["Total"]],
      households_no_literate = values[["None"]], households_1_literate = values[["1"]],
      households_2_literates = values[["2"]], households_3_literates = values[["3"]],
      households_4_plus_literates = values[["4+"]],
      households_with_literate_member = values[["Total"]] - values[["None"]], stringsAsFactors = FALSE
    )
  }))
  out
}

parse_census_hh13_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 14L) stop("Census 2001 HH13 sheet has fewer than 14 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])), state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L), district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])), category = normalize_census_household_category(raw[[7L]]),
    households = num(raw[[8L]]), age15_1 = num(raw[[9L]]), age15_2 = num(raw[[10L]]),
    age15_3_6 = num(raw[[11L]]), age15_7_10 = num(raw[[12L]]), age15_11_plus = num(raw[[13L]]),
    age15_none = num(raw[[14L]]), stringsAsFactors = FALSE
  )
  labels <- c(
    "Households with No matriculate and above", "Households with at least one matriculate and above",
    "Households with at least one female matriculate and above", "Households with at least one graduate and above",
    "Households with at least one female graduate and above"
  )
  keep <- out$table == "HH13" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & toupper(out$residence) == "TOTAL" & out$category %in% labels
  out <- out[keep %in% TRUE, , drop = FALSE]
  numeric_cols <- c("age15_1", "age15_2", "age15_3_6", "age15_7_10", "age15_11_plus", "age15_none")
  sizes <- as.matrix(data.frame(lapply(out[numeric_cols], num), check.names = FALSE))

  # The Odisha workbook is structurally shifted: column 8 is blank, column 9
  # contains the row total, columns 10--14 contain the five positive age-15+
  # buckets, and the `None` bucket is omitted. Detect that layout by its own
  # accounting identity rather than by state name, then canonicalize it before
  # applying the common validation below.
  shifted <- !is.finite(out$households) & apply(is.finite(sizes), 1L, all) &
    sizes[, 1L] == rowSums(sizes[, -1L, drop = FALSE])
  if (any(shifted)) {
    out$households[shifted] <- sizes[shifted, 1L]
    out$age15_1[shifted] <- sizes[shifted, 2L]
    out$age15_2[shifted] <- sizes[shifted, 3L]
    out$age15_3_6[shifted] <- sizes[shifted, 4L]
    out$age15_7_10[shifted] <- sizes[shifted, 5L]
    out$age15_11_plus[shifted] <- sizes[shifted, 6L]
    out$age15_none[shifted] <- 0
    sizes <- as.matrix(data.frame(lapply(out[numeric_cols], num), check.names = FALSE))
  }

  if (any(!is.finite(out$households)) || any(out$households < 0) ||
      any(!is.finite(sizes)) || any(sizes < 0) ||
      any(rowSums(sizes) != out$households)) {
    stop("Census 2001 HH13 age-15+ household cells must be nonnegative and exhaust each published row total.", call. = FALSE)
  }
  out
}

summarise_census_hh13_2001_district <- function(rows) {
  labels <- c(
    no_matriculate = "Households with No matriculate and above",
    matriculate = "Households with at least one matriculate and above",
    female_matriculate = "Households with at least one female matriculate and above",
    graduate = "Households with at least one graduate and above",
    female_graduate = "Households with at least one female graduate and above"
  )
  groups <- summarise_census_household_rows(rows, unname(labels), "Census 2001 HH13")
  safe_bind_rows(lapply(groups, function(part) {
    by_label <- setNames(seq_len(nrow(part)), part$category)
    age15_count <- function(id) {
      row <- part[by_label[[labels[[id]]]], , drop = FALSE]
      sum(num(row[c("age15_1", "age15_2", "age15_3_6", "age15_7_10", "age15_11_plus")]))
    }
    no_mat_age15 <- age15_count("no_matriculate"); mat <- age15_count("matriculate")
    female_mat <- age15_count("female_matriculate"); grad <- age15_count("graduate"); female_grad <- age15_count("female_graduate")
    if (female_mat > mat || grad > mat || female_grad > grad || female_grad > female_mat) {
      stop("Census 2001 HH13 age-15+ education subset counts violate their published nesting.", call. = FALSE)
    }
    data.frame(
      state_code = part$state_code[[1L]], district_code = part$district_code[[1L]], district_name = part$district_name[[1L]],
      households_age15_plus = no_mat_age15 + mat,
      households_with_matriculate = mat, households_with_female_matriculate = female_mat,
      households_with_graduate = grad, households_with_female_graduate = female_grad,
      stringsAsFactors = FALSE
    )
  }))
}

parse_census_hh15_2001_sheet <- function(raw, table = "HH15") {
  raw <- safe_df(raw)
  if (ncol(raw) < 13L) stop("Census 2001 HH15 sheet has fewer than 13 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])), state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L), district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])), category = normalize_census_household_category(raw[[7L]]),
    households = num(raw[[8L]]), aux_1 = num(raw[[9L]]), aux_2 = num(raw[[10L]]),
    aux_3 = num(raw[[11L]]), aux_4 = num(raw[[12L]]), aux_5 = num(raw[[13L]]), stringsAsFactors = FALSE
  )
  keep <- out$table == table & !is.na(out$state_code) & !is.na(out$district_code) & out$district_code != "00" &
    toupper(out$residence) == "TOTAL" & out$category %in% c("Total", "None", "1", "2", "3", "4+")
  out <- out[keep %in% TRUE, , drop = FALSE]
  aux <- as.matrix(data.frame(lapply(out[c("aux_1", "aux_2", "aux_3", "aux_4", "aux_5")], num), check.names = FALSE))
  if (any(!is.finite(out$households)) || any(out$households < 0) || any(!is.finite(aux)) ||
      any(aux < 0) || any(rowSums(aux) != out$households)) {
    stop("Census 2001 HH15 auxiliary cells must exhaust each row total.", call. = FALSE)
  }
  out
}

summarise_census_hh15_2001_district <- function(rows, label = "Census 2001 HH15") {
  groups <- summarise_census_household_rows(rows, c("Total", "None", "1", "2", "3", "4+"), label)
  safe_bind_rows(lapply(groups, function(part) {
    values <- setNames(num(part$households), part$category)
    if (values[["Total"]] != sum(values[c("None", "1", "2", "3", "4+")])) {
      stop(label, " worker-count categories do not exhaust total households.", call. = FALSE)
    }
    data.frame(
      state_code = part$state_code[[1L]], district_code = part$district_code[[1L]], district_name = part$district_name[[1L]],
      households_total = values[["Total"]], households_no_workers = values[["None"]],
      households_1_worker = values[["1"]], households_2_workers = values[["2"]],
      households_3_workers = values[["3"]], households_4_plus_workers = values[["4+"]], stringsAsFactors = FALSE
    )
  }))
}

read_census_hh09_2001_district <- function(files) read_census_household_district_files(files, parse_census_hh09_2001_sheet, summarise_census_hh09_2001_district, "Census 2001 HH09")
read_census_hh13_2001_district <- function(files) read_census_household_district_files(files, parse_census_hh13_2001_sheet, summarise_census_hh13_2001_district, "Census 2001 HH13")
read_census_hh15_2001_district <- function(files) read_census_household_district_files(files, parse_census_hh15_2001_sheet, summarise_census_hh15_2001_district, "Census 2001 HH15")
read_census_hh15a_2001_district <- function(files) read_census_household_district_files(
  files,
  function(raw) parse_census_hh15_2001_sheet(raw, "HH15A"),
  function(rows) summarise_census_hh15_2001_district(rows, "Census 2001 HH15 Appendix"),
  "Census 2001 HH15 Appendix"
)
