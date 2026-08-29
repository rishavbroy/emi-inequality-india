# External geography benchmark for the Vanneman historical panel.
#
# Liu, Shamdasani, and Taraz (2023) publish a replication package containing
# a 339-unit Vanneman district crosswalk plus a separate 1991 Census crosswalk.
# These files are useful as an external benchmark, but they are not the
# production geography authority for this project.

liu_vanneman_benchmark_paths <- function(paths = build_paths()) {
  root <- path_project(paths, "data/raw/maggieliuDataCodeClimate2023")
  c(
    vanneman_crosswalk = file.path(root, "Vanneman_district_crosswalk.dta"),
    panel4_copy = file.path(root, "panel4_lst.data"),
    pca1991_crosswalk = file.path(root, "PCA_census1991_dist_match.dta"),
    pca2011_crosswalk = file.path(root, "PCA_census2011_dist_match.dta"),
    vanneman_dictionary = file.path(root, "dm-Stata/lst-dm-01a-Vanneman_dictionary.dct"),
    clean_vanneman_do = file.path(root, "dm-Stata/lst-dm-01a-clean_Vanneman_data.do"),
    pca_1961_1991_do = file.path(root, "dm-Stata/lst-dm-01b-make_pca_1961_1991.do"),
    pca_1961_2011_do = file.path(root, "dm-Stata/lst-dm-01d-make_pca_1961_2011.do")
  )
}


liu_vanneman_construction_contract <- function(paths = build_paths()) {
  files <- liu_vanneman_benchmark_paths(paths)
  required <- c("vanneman_dictionary", "clean_vanneman_do", "pca_1961_1991_do", "pca_1961_2011_do")
  missing <- files[required][!file.exists(files[required])]
  if (length(missing)) {
    stop("Liu et al. Vanneman construction contract is missing files: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  clean <- paste(readLines(files[["clean_vanneman_do"]], warn = FALSE), collapse = "\n")
  pca_early <- paste(readLines(files[["pca_1961_1991_do"]], warn = FALSE), collapse = "\n")
  pca_all <- paste(readLines(files[["pca_1961_2011_do"]], warn = FALSE), collapse = "\n")
  dictionary <- paste(readLines(files[["vanneman_dictionary"]], warn = FALSE), collapse = "\n")

  checks <- c(
    dictionary_targets_panel = grepl("panel4", dictionary, ignore.case = TRUE),
    stable_id_merge = grepl("merge 1:1 state_id dist_id using Data/Source/PCA/Vanneman_district_crosswalk\\.dta", clean),
    early_builder_keeps_vanneman_ids = grepl("rename state_id_[[:space:]]+st_code", pca_early) &&
      grepl("rename dist_id_[[:space:]]+dist_code", pca_early),
    six_census_appends_early_panel = grepl("append using Data/Derived/PCA/pca_1961_1991\\.dta", pca_all),
    six_census_rebuilds_harmonized_ids = grepl("egen state_id = group\\(statename_temp\\)", pca_all) &&
      grepl("egen district_id = group\\(state_id dtname_temp\\)", pca_all)
  )
  data.frame(
    check = names(checks),
    passed = unname(checks),
    interpretation = c(
      "Dictionary explicitly targets the bundled Vanneman panel source.",
      "Published cleaning code merges the 339-row name crosswalk 1:1 on Vanneman stable state/district IDs.",
      "The 1961-1991 builder carries Vanneman stable IDs forward as its state/district codes.",
      "The six-census builder appends the already-cleaned 1961-1991 panel before later harmonization.",
      "The six-census builder creates a separate harmonized geography from normalized state/district names."
    ),
    stringsAsFactors = FALSE
  )
}

read_liu_vanneman_crosswalk <- function(path) {
  if (!file.exists(path)) stop("Missing Liu et al. Vanneman crosswalk: ", path, call. = FALSE)
  x <- as.data.frame(haven::read_dta(path), stringsAsFactors = FALSE)
  required <- c(
    "state_id", "dist_id", "dist_name_Vanneman", "state_name_Vanneman",
    "state_name_david", "dist_name_david", "state_dist"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Liu et al. Vanneman crosswalk lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$vanneman_state_id <- normalize_census_code(as.integer(x$state_id), 2L)
  x$vanneman_district_id <- normalize_census_code(as.integer(x$dist_id), 2L)
  x$panel_unit_id <- paste0(x$vanneman_state_id, x$vanneman_district_id)
  if (anyDuplicated(x$panel_unit_id)) {
    stop("Liu et al. Vanneman crosswalk has duplicate stable panel IDs.", call. = FALSE)
  }
  expected_state_dist <- as.integer(x$vanneman_state_id) * 100L + as.integer(x$vanneman_district_id)
  if (any(!is.finite(x$state_dist)) || any(as.integer(x$state_dist) != expected_state_dist)) {
    stop("Liu et al. Vanneman crosswalk state_dist is inconsistent with state_id/dist_id.", call. = FALSE)
  }
  x
}

read_liu_panel4_label_inventory <- function(path) {
  if (!file.exists(path)) stop("Missing Liu et al. panel4 copy: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE)
  if (!length(lines) || any(nchar(lines) < 10L)) {
    stop("Liu et al. panel4 copy contains malformed fixed-width records.", call. = FALSE)
  }
  keep <- substr(lines, 5L, 7L) == "000" & substr(lines, 8L, 9L) == "91"
  out <- data.frame(
    vanneman_state_id = substr(lines[keep], 1L, 2L),
    vanneman_district_id = substr(lines[keep], 3L, 4L),
    liu_panel4_label_1991 = trimws(substr(lines[keep], 11L, nchar(lines[keep]))),
    stringsAsFactors = FALSE
  )
  out$panel_unit_id <- paste0(out$vanneman_state_id, out$vanneman_district_id)
  if (anyDuplicated(out$panel_unit_id)) {
    stop("Liu et al. panel4 copy has duplicate 1991 panel labels.", call. = FALSE)
  }
  out
}

read_liu_pca1991_crosswalk <- function(path) {
  if (!file.exists(path)) stop("Missing Liu et al. 1991 PCA crosswalk: ", path, call. = FALSE)
  x <- as.data.frame(haven::read_dta(path), stringsAsFactors = FALSE)
  required <- c("state", "district", "state_id", "district_id", "st_code", "statename", "dist_code", "dtname")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Liu et al. 1991 PCA crosswalk lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$state_code_1991 <- normalize_census_code(as.integer(x$st_code), 2L)
  x$district_code_1991 <- normalize_census_code(as.integer(x$dist_code), 2L)
  x$district_name_1991 <- as.character(x$dtname)
  x
}

liu_direct_district_aliases <- function(path) {
  if (!file.exists(path)) stop("Missing Liu et al. six-census construction script: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE)
  start <- grep("gen[[:space:]]+dtname_temp[[:space:]]*=[[:space:]]*lower\\(dtname\\)", lines, ignore.case = TRUE)
  end <- grep("Generate consistent state and district identifiers", lines, ignore.case = TRUE)
  if (length(start) != 1L || length(end) != 1L || end <= start) {
    stop("Liu et al. six-census script lacks the expected district-name harmonization block.", call. = FALSE)
  }
  rule_lines <- seq.int(start + 1L, end - 1L)
  keep <- grepl("^[[:space:]]*replace[[:space:]]+dtname_temp[[:space:]]*=", lines[rule_lines])
  rule_lines <- rule_lines[keep]
  rules <- lines[rule_lines]
  parsed <- lapply(seq_along(rules), function(i) {
    target <- sub('.*dtname_temp[[:space:]]*=[[:space:]]*"([^"]*)".*', "\\1", rules[[i]])
    hits <- regmatches(rules[[i]], gregexpr('dtname_temp[[:space:]]*==[[:space:]]*"[^"]*"', rules[[i]], perl = TRUE))[[1]]
    if (!length(hits)) return(NULL)
    sources <- sub('.*==[[:space:]]*"([^"]*)"', "\\1", hits)
    data.frame(
      source_name = trimws(sources), target_name = trimws(target),
      source_line = rule_lines[[i]], stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(parsed)
  if (!nrow(out)) stop("Liu et al. six-census script yielded no direct district-name rules.", call. = FALSE)
  out$source_key <- canonicalize_district_name(out$source_name)
  out$target_key <- canonicalize_district_name(out$target_name)
  unique(out)
}

validate_vanneman_panel4_dist91_adjudications <- function(
    adjudications, panel_crosswalk, paths = build_paths()) {
  reviewed <- safe_df(adjudications)
  panel <- safe_df(panel_crosswalk)
  if (!nrow(reviewed)) return(reviewed)
  files <- liu_vanneman_benchmark_paths(paths)
  required_files <- files[c("vanneman_crosswalk", "pca1991_crosswalk", "pca_1961_2011_do")]
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files)) {
    stop("Vanneman adjudication evidence is missing Liu et al. files: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }
  required <- c("panel_unit_id", "dist91_state_id", "dist91_district_id", "decision", "source_id", "evidence")
  missing <- setdiff(required, names(reviewed))
  if (length(missing)) stop("Vanneman adjudications lack fields: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(reviewed$panel_unit_id)) stop("Vanneman adjudications have duplicate stable panel IDs.", call. = FALSE)
  if (any(reviewed$decision != "accepted_one_to_one")) stop("Vanneman adjudication decision must be accepted_one_to_one.", call. = FALSE)
  if (any(reviewed$source_id != "maggieliuDataCodeClimate2023")) {
    stop("Vanneman direct-alias adjudications must cite the registered Liu et al. replication source.", call. = FALSE)
  }
  if (any(reviewed$evidence != "published_direct_alias_and_raw_1991_code")) {
    stop("Vanneman adjudications must declare the published direct-alias/raw-code evidence contract.", call. = FALSE)
  }

  idx <- match(reviewed$panel_unit_id, panel$panel_unit_id)
  if (anyNA(idx)) stop("Vanneman adjudication references an unknown stable panel ID.", call. = FALSE)
  if (any(panel$mapping_class[idx] != "label_review_required")) {
    stop("Vanneman adjudications may promote only label_review_required cases.", call. = FALSE)
  }
  if (any(panel$dist91_state_id[idx] != reviewed$dist91_state_id)) {
    stop("Vanneman adjudication changes the author-documented panel-to-1991 state mapping.", call. = FALSE)
  }
  if (any(reviewed$dist91_district_id == "00")) {
    stop("Vanneman adjudications may not promote district-00 aggregates.", call. = FALSE)
  }

  dist91 <- vanneman_dist91_geography_inventory(vanneman_historical_paths(paths)[["dist91"]])
  target_key <- paste(reviewed$dist91_state_id, reviewed$dist91_district_id, sep = "__")
  dist91_key <- paste(dist91$dist91_state_id, dist91$dist91_district_id, sep = "__")
  didx <- match(target_key, dist91_key)
  if (anyNA(didx)) stop("Vanneman adjudication target is absent from the author dist91 geography.", call. = FALSE)

  external <- read_liu_vanneman_crosswalk(files[["vanneman_crosswalk"]])
  eidx <- match(reviewed$panel_unit_id, external$panel_unit_id)
  if (anyNA(eidx)) stop("Liu et al. stable-ID crosswalk does not cover a Vanneman adjudication.", call. = FALSE)
  pca <- read_liu_pca1991_crosswalk(files[["pca1991_crosswalk"]])
  pca_key <- paste(pca$state_code_1991, pca$district_code_1991, sep = "__")
  aliases <- liu_direct_district_aliases(files[["pca_1961_2011_do"]])

  pca_name <- character(nrow(reviewed))
  alias_line <- integer(nrow(reviewed))
  for (i in seq_len(nrow(reviewed))) {
    candidates <- pca[pca_key == target_key[[i]], , drop = FALSE]
    candidate_names <- unique(candidates$district_name_1991)
    if (length(candidate_names) != 1L) {
      stop("Vanneman adjudication target is not a unique raw Census-1991 code/name in Liu et al.", call. = FALSE)
    }
    pca_name[[i]] <- candidate_names[[1L]]
    if (canonicalize_district_name(pca_name[[i]]) != dist91$dist91_label_key[didx[[i]]]) {
      stop("Vanneman dist91 label disagrees with Liu et al. raw Census-1991 code/name for an adjudication target.", call. = FALSE)
    }
    raw_key <- canonicalize_district_name(pca_name[[i]])
    stable_name_key <- canonicalize_district_name(external$dist_name_david[eidx[[i]]])
    direct <- aliases[
      (aliases$source_key == raw_key & aliases$target_key == stable_name_key) |
        (aliases$source_key == stable_name_key & aliases$target_key == raw_key),
      , drop = FALSE
    ]
    if (!nrow(direct)) {
      stop("Vanneman adjudication lacks a direct published Liu district-name alias rule.", call. = FALSE)
    }
    alias_line[[i]] <- min(direct$source_line)
  }

  reviewed$dist91_district_label <- dist91$dist91_district_label[didx]
  reviewed$liu_harmonized_name <- as.character(external$dist_name_david[eidx])
  reviewed$liu_pca1991_label <- pca_name
  reviewed$liu_alias_source_line <- alias_line
  reviewed$evidence_status <- "verified_direct_alias"
  reviewed[order(reviewed$panel_unit_id), , drop = FALSE]
}

summarize_liu_pca1991_crosswalk <- function(path) {
  x <- read_liu_pca1991_crosswalk(path)
  harmonized_key <- paste(as.integer(x$state_id), as.integer(x$district_id), sep = "__")
  census_code_key <- paste(as.integer(x$st_code), as.integer(x$dist_code), sep = "__")
  nonunique_census_pairs <- sum(table(census_code_key) > 1L)
  data.frame(
    metric = c(
      "pca1991_rows", "pca1991_harmonized_groups", "pca1991_nonunique_census_code_pairs"
    ),
    value = c(nrow(x), length(unique(harmonized_key)), nonunique_census_pairs),
    note = c(
      "Rows represent 1991 Census district assignments in the Liu et al. replication source.",
      "These harmonized PCA group IDs are a separate geography and must not be treated as Vanneman panel IDs.",
      "Count of raw st_code/dist_code pairs appearing more than once; nonzero values require state-name-aware interpretation rather than blind numeric joins."
    ),
    stringsAsFactors = FALSE
  )
}

save_vanneman_panel4_dist91_adjudication_evidence <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_panel4_dist91_adjudication_evidence.csv") {
  write_diagnostic_csv(x, path)
}

build_vanneman_liu_geography_benchmark <- function(panel_geography, paths = build_paths()) {
  panel <- safe_df(panel_geography)
  required <- c(
    "vanneman_state_id", "vanneman_district_id", "district_label_1961", "district_label_1991"
  )
  missing <- setdiff(required, names(panel))
  if (length(missing)) {
    stop("Vanneman panel geography lacks fields for Liu benchmark: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  panel$panel_unit_id <- paste0(panel$vanneman_state_id, panel$vanneman_district_id)
  if (anyDuplicated(panel$panel_unit_id)) stop("Vanneman panel geography has duplicate stable IDs.", call. = FALSE)

  files <- liu_vanneman_benchmark_paths(paths)
  missing_files <- files[!file.exists(files)]
  if (length(missing_files)) {
    stop("Liu et al. Vanneman benchmark is missing files: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }
  external <- read_liu_vanneman_crosswalk(files[["vanneman_crosswalk"]])
  if (!setequal(panel$panel_unit_id, external$panel_unit_id)) {
    stop("Liu et al. Vanneman crosswalk does not cover exactly the current stable panel IDs.", call. = FALSE)
  }

  idx <- match(panel$panel_unit_id, external$panel_unit_id)
  panel$liu_vanneman_name <- as.character(external$dist_name_Vanneman[idx])
  panel$liu_harmonized_name <- as.character(external$dist_name_david[idx])
  panel$liu_vanneman_state_name <- as.character(external$state_name_Vanneman[idx])
  panel$liu_harmonized_state_name <- as.character(external$state_name_david[idx])
  panel$panel_1961_name_agrees_liu_original <-
    canonicalize_district_name(panel$district_label_1961) == canonicalize_district_name(panel$liu_vanneman_name)
  panel$panel_1991_name_agrees_liu_harmonized <-
    canonicalize_district_name(panel$district_label_1991) == canonicalize_district_name(panel$liu_harmonized_name)

  external_panel <- read_liu_panel4_label_inventory(files[["panel4_copy"]])
  if (any(!external_panel$panel_unit_id %in% external$panel_unit_id)) {
    stop("Liu et al. panel4 copy contains stable IDs absent from its Vanneman crosswalk.", call. = FALSE)
  }
  panel_idx <- match(panel$panel_unit_id, external_panel$panel_unit_id)
  panel$liu_panel4_copy_present <- !is.na(panel_idx)
  panel$liu_panel4_label_1991 <- external_panel$liu_panel4_label_1991[panel_idx]
  panel$liu_panel4_1991_name_agrees_current <- ifelse(
    panel$liu_panel4_copy_present,
    canonicalize_district_name(panel$district_label_1991) ==
      canonicalize_district_name(panel$liu_panel4_label_1991),
    NA
  )
  panel$benchmark_status <- ifelse(
    !panel$liu_panel4_copy_present,
    "external_panel_copy_missing",
    ifelse(
      panel$panel_1991_name_agrees_liu_harmonized & panel$liu_panel4_1991_name_agrees_current,
      "external_identity_and_1991_label_agree",
      "external_identity_agrees_label_review"
    )
  )

  construction_contract <- liu_vanneman_construction_contract(paths)
  if (any(!construction_contract$passed)) {
    stop("Liu et al. Vanneman construction scripts do not satisfy the documented geography contract.", call. = FALSE)
  }
  pca_summary <- summarize_liu_pca1991_crosswalk(files[["pca1991_crosswalk"]])
  summary <- rbind(
    data.frame(
      metric = c(
        "current_panel_units", "liu_vanneman_crosswalk_units", "liu_panel4_copy_units",
        "liu_panel4_copy_missing_units", "panel_1991_name_agreements_with_liu_harmonized"
      ),
      value = c(
        nrow(panel), nrow(external), nrow(external_panel), sum(!panel$liu_panel4_copy_present),
        sum(panel$panel_1991_name_agrees_liu_harmonized, na.rm = TRUE)
      ),
      note = c(
        "Stable Vanneman units in the canonical archived panel4 geography.",
        "Unique Vanneman stable IDs in Liu et al.'s published external crosswalk.",
        "1991 label units in Liu et al.'s bundled panel4_lst.data; this copy is benchmark-only.",
        "Stable Vanneman IDs present in the canonical panel but absent from Liu et al.'s panel4 copy.",
        "Exact canonical-name agreements between the canonical panel 1991 label and Liu et al.'s independently curated harmonized name."
      ),
      stringsAsFactors = FALSE
    ),
    pca_summary
  )

  list(
    panel_comparison = panel[order(panel$vanneman_state_id, panel$vanneman_district_id), , drop = FALSE],
    source_summary = summary,
    construction_contract = construction_contract
  )
}

save_vanneman_liu_geography_benchmark <- function(
    x,
    directory = "outputs/diagnostics/extended/instrument_relevance") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    panel_comparison = file.path(directory, "vanneman_liu_geography_benchmark.csv"),
    source_summary = file.path(directory, "vanneman_liu_geography_benchmark_summary.csv"),
    construction_contract = file.path(directory, "vanneman_liu_construction_contract.csv")
  )
  write_diagnostic_csv(x$panel_comparison, paths[["panel_comparison"]])
  write_diagnostic_csv(x$source_summary, paths[["source_summary"]])
  write_diagnostic_csv(x$construction_contract, paths[["construction_contract"]])
  unname(paths)
}
