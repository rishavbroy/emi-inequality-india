# This file is part of the EMI inequality research pipeline.

read_shastry_language_adjudications <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "shastry_language_adjudications.csv")
  }
  if (!file.exists(path)) stop("Missing Shastry adjudication ledger: ", path, call. = FALSE)
  out <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = c(mother_tongue_code = "character")
  )
  required <- c(
    "mother_tongue_code", "mother_tongue", "assigned_shastry_degree",
    "shastry_anchor", "lsi_classification", "lsi_volume", "lsi_year",
    "lsi_pages", "lsi_url", "lsi_evidence",
    "lexical_source", "lexical_pages", "lexical_url", "lexical_evidence",
    "decision_basis", "confidence", "sensitivity_degrees", "review_status", "notes"
  )
  if (!identical(names(out), required)) stop("Shastry adjudication ledger has an invalid schema.", call. = FALSE)

  out$mother_tongue_code <- sprintf("%06d", suppressWarnings(as.integer(out$mother_tongue_code)))
  if (anyDuplicated(out$mother_tongue_code)) stop("Shastry adjudication ledger has duplicate mother-tongue codes.", call. = FALSE)

  status <- plain_chr(out$review_status)
  if (any(!status %in% c("accepted", "frozen_unresolved"))) {
    stop("Shastry adjudication ledger must be frozen: use accepted or frozen_unresolved.", call. = FALSE)
  }
  accepted <- status == "accepted"
  frozen <- status == "frozen_unresolved"
  degree <- num(out$assigned_shastry_degree)
  if (any(accepted & (!is.finite(degree) | degree < 0 | degree > 5 | degree != round(degree)))) {
    stop("Accepted Shastry adjudications require an integer degree from zero through five.", call. = FALSE)
  }
  evidence_fields <- c("lsi_volume", "lsi_pages", "lsi_url", "lsi_evidence", "decision_basis", "confidence")
  for (field in evidence_fields) {
    value <- plain_chr(out[[field]])
    missing <- is.na(value) | !nzchar(trimws(value))
    if (any(accepted & missing)) {
      stop("Accepted Shastry adjudications require ", field, ".", call. = FALSE)
    }
  }
  lexical_required <- accepted &
    grepl("kogan|asjp", plain_chr(out$decision_basis), ignore.case = TRUE)
  for (field in c("lexical_source", "lexical_pages", "lexical_url", "lexical_evidence")) {
    value <- plain_chr(out[[field]])
    missing <- is.na(value) | !nzchar(trimws(value))
    if (any(lexical_required & missing)) {
      stop("Lexically adjudicated Shastry rows require ", field, ".", call. = FALSE)
    }
  }

  if (any(frozen & is.finite(degree))) {
    stop("Frozen unresolved Shastry adjudications cannot carry a production degree.", call. = FALSE)
  }
  frozen_notes <- plain_chr(out$notes)
  if (any(frozen & (is.na(frozen_notes) | !nzchar(trimws(frozen_notes))))) {
    stop("Frozen unresolved Shastry adjudications require an explicit reason.", call. = FALSE)
  }

  out$assigned_shastry_degree <- degree
  out
}

parse_shastry_sensitivity_degrees <- function(x) {
  values <- plain_chr(x)
  lapply(values, function(value) {
    if (is.na(value) || !nzchar(trimws(value))) return(numeric())
    pieces <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
    degree <- suppressWarnings(as.numeric(pieces))
    if (any(!is.finite(degree) | degree < 0 | degree > 5 | degree != round(degree))) {
      stop("Shastry sensitivity degrees must be semicolon-separated integers from zero through five.", call. = FALSE)
    }
    sort(unique(degree))
  })
}

shastry_adjudication_scenario_degree <- function(
  adjudications,
  scenario = c("preferred", "sensitivity_low", "sensitivity_high")
) {
  scenario <- match.arg(scenario)
  primary <- num(adjudications$assigned_shastry_degree)
  if (scenario == "preferred") return(primary)

  sensitivity <- parse_shastry_sensitivity_degrees(adjudications$sensitivity_degrees)
  vapply(seq_len(nrow(adjudications)), function(i) {
    candidates <- c(primary[[i]], sensitivity[[i]])
    candidates <- candidates[is.finite(candidates)]
    if (!length(candidates)) return(NA_real_)
    if (scenario == "sensitivity_low") min(candidates) else max(candidates)
  }, numeric(1))
}

apply_shastry_language_adjudications <- function(
  rows,
  degree,
  adjudications = read_shastry_language_adjudications(),
  scenario = c("preferred", "sensitivity_low", "sensitivity_high")
) {
  scenario <- match.arg(scenario)
  out <- num(degree)
  if (!"mother_tongue_code" %in% names(rows) || !nrow(adjudications)) return(out)

  code <- sprintf("%06d", suppressWarnings(as.integer(rows$mother_tongue_code)))
  idx <- match(code, adjudications$mother_tongue_code)
  scenario_degree <- shastry_adjudication_scenario_degree(adjudications, scenario)
  eligible <- !is.na(idx) & is.finite(scenario_degree[idx])
  if (scenario == "preferred") {
    eligible <- eligible & plain_chr(adjudications$review_status[idx]) == "accepted"
  }
  fill <- eligible & !is.finite(out)
  out[fill] <- scenario_degree[idx[fill]]
  out
}

resolve_shastry_language_degrees <- function(
  rows,
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications(),
  scenario = c("preferred", "sensitivity_low", "sensitivity_high")
) {
  scenario <- match.arg(scenario)
  language <- census_mother_tongue_identity(rows)
  degree <- linguistic_distance_degrees(language, rows$canonical_language, concordance)
  degree <- apply_shastry_language_adjudications(
    rows, degree, adjudications, scenario = scenario
  )

  family_class <- if ("shastry_family_class" %in% names(rows)) {
    plain_chr(rows$shastry_family_class)
  } else {
    family <- if ("glottolog_family_id" %in% names(rows)) {
      plain_chr(rows$glottolog_family_id)
    } else if ("family_id" %in% names(rows)) {
      plain_chr(rows$family_id)
    } else {
      rep(NA_character_, nrow(rows))
    }
    ifelse(
      is.na(family) | !nzchar(family),
      NA_character_,
      ifelse(family == "indo1319", "indo_european", "non_indo_european")
    )
  }
  non_ie <- !is.finite(degree) &
    language != "English" &
    !is.na(family_class) &
    family_class == "non_indo_european"
  degree[non_ie] <- 5
  degree
}

prepare_shastry_language_rows <- function(
  rows,
  glottolog = NULL,
  glottolog_crosswalk = NULL,
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications()
) {
  out <- safe_df(rows)
  if (!is.null(glottolog) && !is.null(glottolog_crosswalk)) {
    out <- attach_glottolog_language_distance(out, glottolog, glottolog_crosswalk)
  }
  out$ling_degrees <- resolve_shastry_language_degrees(
    out, concordance, adjudications, scenario = "preferred"
  )
  out$ling_degrees_sensitivity_low <- resolve_shastry_language_degrees(
    out, concordance, adjudications, scenario = "sensitivity_low"
  )
  out$ling_degrees_sensitivity_high <- resolve_shastry_language_degrees(
    out, concordance, adjudications, scenario = "sensitivity_high"
  )
  out
}


#' Resolve the reviewed Census-1991 Atlas language inventory on the frozen Shastry basis
resolve_language_atlas_1991_shastry_mapping <- function(
  registry = read_language_atlas_1991_languages(),
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications(),
  scenario = c("preferred", "sensitivity_low", "sensitivity_high")
) {
  scenario <- match.arg(scenario)
  rows <- safe_df(registry)
  required <- c("language_1991", "canonical_language", "shastry_family_class")
  if (!all(required %in% names(rows))) {
    stop("Language Atlas 1991 registry lacks Shastry-resolution fields.", call. = FALSE)
  }
  rows$mother_tongue <- rows$language_1991
  adjudication_label <- normalize_language_label(adjudications$mother_tongue)
  if (anyDuplicated(adjudication_label)) {
    stop("Shastry adjudication labels must be unique for Atlas exact-label reuse.", call. = FALSE)
  }
  adjudication_idx <- match(normalize_language_label(rows$mother_tongue), adjudication_label)
  rows$mother_tongue_code <- ifelse(
    is.na(adjudication_idx),
    NA_character_,
    plain_chr(adjudications$mother_tongue_code[adjudication_idx])
  )
  rows$shastry_degree <- resolve_shastry_language_degrees(
    rows, concordance = concordance, adjudications = adjudications, scenario = scenario
  )
  rows$shastry_mapping_status <- ifelse(
    normalize_language_label(rows$language_1991) == "English",
    "special_english",
    ifelse(is.finite(rows$shastry_degree), "mapped", "frozen_unresolved")
  )
  rows
}
