# Blocking survey-level QA for canonical household consumption.

read_consumption_mpce_benchmarks <- function(path) {
  if (!file.exists(path)) stop("Consumption MPCE benchmark file is missing: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "survey_id", "sector", "mpce_definition", "expected_mpce",
    "tolerance_abs_rupees", "source_label", "source_url"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop("Consumption MPCE benchmark file is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out$survey_id <- trimws(plain_chr(out$survey_id))
  out$sector <- tolower(trimws(plain_chr(out$sector)))
  out$mpce_definition <- trimws(plain_chr(out$mpce_definition))
  out$expected_mpce <- suppressWarnings(as.numeric(out$expected_mpce))
  out$tolerance_abs_rupees <- suppressWarnings(as.numeric(out$tolerance_abs_rupees))
  out$source_label <- trimws(plain_chr(out$source_label))
  out$source_url <- trimws(plain_chr(out$source_url))
  if (!nrow(out) || any(!out$sector %in% c("rural", "urban"))) {
    stop("Consumption MPCE benchmarks must contain rural/urban rows.", call. = FALSE)
  }
  if (anyNA(out$expected_mpce) || any(!is.finite(out$expected_mpce)) || any(out$expected_mpce <= 0)) {
    stop("Consumption MPCE benchmarks require positive finite expected_mpce values.", call. = FALSE)
  }
  if (anyNA(out$tolerance_abs_rupees) || any(!is.finite(out$tolerance_abs_rupees)) || any(out$tolerance_abs_rupees < 0)) {
    stop("Consumption MPCE benchmarks require non-negative finite tolerances.", call. = FALSE)
  }
  key <- paste(out$survey_id, out$sector, sep = "\r")
  if (anyDuplicated(key)) stop("Consumption MPCE benchmarks must be unique by survey_id and sector.", call. = FALSE)
  out
}

consumption_sector_name <- function(x) {
  value <- trimws(plain_chr(x))
  out <- rep(NA_character_, length(value))
  out[value %in% c("1", "rural")] <- "rural"
  out[value %in% c("2", "urban")] <- "urban"
  out
}

estimate_consumption_mpce_by_sector <- function(households) {
  x <- safe_df(households)
  required <- c("sector", "household_size", "survey_weight", "nominal_mpce")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Canonical consumption households are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  sector <- consumption_sector_name(x$sector)
  size <- num(x$household_size)
  weight <- num(x$survey_weight)
  mpce <- num(x$nominal_mpce)
  valid <- !is.na(sector) & is.finite(size) & size > 0 & is.finite(weight) & weight > 0 & is.finite(mpce) & mpce > 0
  if (!all(valid)) {
    stop("Canonical consumption households contain invalid sector, size, weight, or MPCE values.", call. = FALSE)
  }

  person_weight <- weight * size
  rows <- lapply(c("rural", "urban"), function(s) {
    keep <- sector == s
    if (!any(keep)) stop("Canonical consumption households contain no ", s, " observations.", call. = FALSE)
    data.frame(
      sector = s,
      estimate_mpce = sum(person_weight[keep] * mpce[keep]) / sum(person_weight[keep]),
      sample_households = sum(keep),
      weighted_persons = sum(person_weight[keep]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

validate_consumption_mpce_reconstruction <- function(households, benchmarks, survey_id) {
  id <- trimws(as.character(survey_id))
  expected <- safe_df(benchmarks)
  expected <- expected[expected$survey_id == id, , drop = FALSE]
  if (!nrow(expected)) stop("No MPCE benchmarks registered for survey: ", id, call. = FALSE)

  estimated <- estimate_consumption_mpce_by_sector(households)
  pos <- match(expected$sector, estimated$sector)
  if (anyNA(pos)) stop("MPCE reconstruction is missing a benchmark sector for survey: ", id, call. = FALSE)
  expected$estimate_mpce <- estimated$estimate_mpce[pos]
  expected$sample_households <- estimated$sample_households[pos]
  expected$weighted_persons <- estimated$weighted_persons[pos]
  expected$abs_difference <- abs(expected$estimate_mpce - expected$expected_mpce)
  expected$relative_difference <- expected$abs_difference / expected$expected_mpce
  expected$passed <- expected$abs_difference <= expected$tolerance_abs_rupees
  rownames(expected) <- NULL

  if (any(!expected$passed)) {
    bad <- expected[!expected$passed, c("survey_id", "sector", "estimate_mpce", "expected_mpce", "abs_difference", "tolerance_abs_rupees"), drop = FALSE]
    detail <- apply(bad, 1, function(row) {
      paste0(
        row[["survey_id"]], "/", row[["sector"]],
        ": estimated=", format(as.numeric(row[["estimate_mpce"]]), digits = 8),
        ", expected=", format(as.numeric(row[["expected_mpce"]]), digits = 8),
        ", abs_diff=", format(as.numeric(row[["abs_difference"]]), digits = 8),
        ", tolerance=", format(as.numeric(row[["tolerance_abs_rupees"]]), digits = 8)
      )
    })
    stop("Historical consumption MPCE reconstruction failed official benchmark(s): ", paste(detail, collapse = "; "), call. = FALSE)
  }
  expected
}

combine_consumption_mpce_validations <- function(...) {
  pieces <- lapply(list(...), safe_df)
  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0L]
  if (!length(pieces)) return(data.frame())
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

save_consumption_mpce_validation <- function(validation, path = file.path("outputs", "diagnostics", "public", "consumption_mpce_reconstruction.csv")) {
  out <- safe_df(validation)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, path, row.names = FALSE, na = "")
  path
}
