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
    "lsi_pages", "lsi_url", "lsi_evidence", "decision_basis",
    "confidence", "sensitivity_degrees", "review_status", "notes"
  )
  if (!identical(names(out), required)) stop("Shastry adjudication ledger has an invalid schema.", call. = FALSE)

  out$mother_tongue_code <- sprintf("%06d", suppressWarnings(as.integer(out$mother_tongue_code)))
  if (anyDuplicated(out$mother_tongue_code)) stop("Shastry adjudication ledger has duplicate mother-tongue codes.", call. = FALSE)

  accepted <- plain_chr(out$review_status) == "accepted"
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
  out$assigned_shastry_degree <- degree
  out
}

apply_shastry_language_adjudications <- function(rows, degree, adjudications = read_shastry_language_adjudications()) {
  out <- num(degree)
  if (!"mother_tongue_code" %in% names(rows) || !nrow(adjudications)) return(out)

  code <- sprintf("%06d", suppressWarnings(as.integer(rows$mother_tongue_code)))
  idx <- match(code, adjudications$mother_tongue_code)
  accepted <- !is.na(idx) & plain_chr(adjudications$review_status[idx]) == "accepted"
  fill <- accepted & !is.finite(out)
  out[fill] <- num(adjudications$assigned_shastry_degree[idx[fill]])
  out
}

resolve_shastry_language_degrees <- function(
  rows,
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications()
) {
  language <- census_mother_tongue_identity(rows)
  degree <- linguistic_distance_degrees(language, rows$canonical_language, concordance)
  apply_shastry_language_adjudications(rows, degree, adjudications)
}
