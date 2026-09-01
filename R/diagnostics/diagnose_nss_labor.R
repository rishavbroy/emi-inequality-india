# Source-contract diagnostics for NSS/PLFS labor microdata.

validate_nss64_source_pair <- function(usual_activity, migration, ddi_contract = NULL) {
  if (anyDuplicated(usual_activity$person_key) || anyDuplicated(migration$person_key)) {
    stop("NSS64 Block 4 and Block 6 person keys must each be unique.", call. = FALSE)
  }
  missing_migration <- setdiff(usual_activity$person_key, migration$person_key)
  missing_activity <- setdiff(migration$person_key, usual_activity$person_key)
  if (length(missing_migration) || length(missing_activity)) {
    stop("NSS64 Block 4 and Block 6 must cover the same household members.", call. = FALSE)
  }
  if (!is.null(ddi_contract)) {
    expected <- setNames(ddi_contract$case_count, ddi_contract$file_id)
    if (!identical(as.numeric(nrow(usual_activity)), as.numeric(expected[["F4"]])) ||
        !identical(as.numeric(nrow(migration)), as.numeric(expected[["F6"]]))) {
      stop("NSS64 person-source row counts do not match the DDI contract.", call. = FALSE)
    }
  }
  data.frame(
    source = c("usual_activity_block4", "migration_block6"),
    rows = c(nrow(usual_activity), nrow(migration)),
    unique_people = c(length(unique(usual_activity$person_key)), length(unique(migration$person_key))),
    positive_weight_share = c(mean(usual_activity$survey_weight > 0), mean(migration$survey_weight > 0)),
    stringsAsFactors = FALSE
  )
}

save_nss64_source_validation <- function(x, root = "outputs/diagnostics/extended/labor") {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(root, "nss64_source_validation.csv")
  utils::write.csv(x, path, row.names = FALSE, na = "")
  path
}
