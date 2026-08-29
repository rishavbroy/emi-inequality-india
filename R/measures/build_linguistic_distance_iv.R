# This file is part of the EMI inequality research pipeline.

#' Read the auditable Shastry language-distance concordance
read_shastry_language_distance <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "shastry_language_distance.csv")
  }
  if (!file.exists(path)) stop("Missing Shastry language-distance concordance: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("canonical_language", "distance_from_hindi")
  if (!all(required %in% names(out))) stop("Language-distance concordance has an invalid schema.", call. = FALSE)
  out$canonical_language <- tools::toTitleCase(tolower(trimws(out$canonical_language)))
  out$distance_from_hindi <- num(out$distance_from_hindi)
  if (anyDuplicated(out$canonical_language)) stop("Language-distance concordance has duplicate language rows.", call. = FALSE)
  if (any(!is.finite(out$distance_from_hindi) | out$distance_from_hindi < 0 | out$distance_from_hindi > 5)) {
    stop("Language-distance concordance values must be finite integers from zero through five.", call. = FALSE)
  }
  out
}


language_atlas_1991_columns <- function() {
  4:117
}

#' Read the reviewed Census-1991 Language Atlas column registry
read_language_atlas_1991_languages <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "language_atlas_1991_languages.csv")
  }
  if (!file.exists(path)) stop("Missing Language Atlas 1991 language registry: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "atlas_column", "language_1991", "canonical_language", "scheduled_1991",
    "language_family_1991", "shastry_family_class", "source_basis", "review_status"
  )
  if (!identical(names(out), required)) stop("Language Atlas 1991 language registry has an invalid schema.", call. = FALSE)
  out$atlas_column <- suppressWarnings(as.integer(out$atlas_column))
  if (!identical(out$atlas_column, language_atlas_1991_columns())) stop("Language Atlas 1991 registry must cover columns 4 through 117 exactly once.", call. = FALSE)
  if (anyDuplicated(normalize_language_label(out$language_1991))) stop("Language Atlas 1991 registry has duplicate language labels.", call. = FALSE)
  scheduled_text <- tolower(plain_chr(out$scheduled_1991))
  if (any(!scheduled_text %in% c("true", "false")) || sum(scheduled_text == "true") != 18L) {
    stop("Language Atlas 1991 registry must identify exactly 18 scheduled languages.", call. = FALSE)
  }
  expected_families <- c(
    "Indo-Aryan" = 19L, "Germanic" = 1L, "Dravidian" = 17L,
    "Austro-Asiatic" = 14L, "Tibeto-Burmese" = 62L, "Semito-Hamitic" = 1L
  )
  observed_families <- table(plain_chr(out$language_family_1991))
  if (!identical(as.integer(observed_families[names(expected_families)]), unname(expected_families)) ||
      any(!names(observed_families) %in% names(expected_families))) {
    stop("Language Atlas 1991 registry does not match the reviewed language-family counts.", call. = FALSE)
  }
  family_class <- plain_chr(out$shastry_family_class)
  if (any(!family_class %in% c("indo_european", "non_indo_european", "special_english"))) {
    stop("Language Atlas 1991 registry has an invalid Shastry family class.", call. = FALSE)
  }
  if (!all(plain_chr(out$review_status) == "accepted")) stop("Language Atlas 1991 language registry must contain reviewed accepted rows only.", call. = FALSE)
  out
}

language_atlas_1991_accepted_source_schema <- function() {
  c(
    "state_code_1991", "district_code_1991", "state_name_1991",
    "atlas_population_candidate", "pca91_population", "atlas_column",
    "language_1991", "canonical_language", "accepted_speaker_count",
    "accepted_count_basis", "cell_review_decision", "cell_review_basis",
    "page", "raw_value", "speaker_count_candidate", "parse_status",
    "alignment_status", "n_atlas_language_columns", "n_accepted_values",
    "n_review_required", "accepted_speaker_lower_bound",
    "accepted_speaker_lower_bound_share_atlas", "coverage_status"
  )
}

validate_language_atlas_1991_accepted_source <- function(
  rows,
  registry = read_language_atlas_1991_languages()
) {
  out <- safe_df(rows)
  if (!identical(names(out), language_atlas_1991_accepted_source_schema())) {
    stop("Accepted Language Atlas 1991 source has an invalid schema.", call. = FALSE)
  }
  state <- suppressWarnings(as.integer(out$state_code_1991))
  district <- suppressWarnings(as.integer(out$district_code_1991))
  out$atlas_column <- suppressWarnings(as.integer(out$atlas_column))
  if (anyNA(state) || anyNA(district) || any(state < 1L) || any(district < 1L) ||
      anyNA(out$atlas_column) || any(!out$atlas_column %in% language_atlas_1991_columns())) {
    stop("Accepted Language Atlas 1991 source has invalid Census or Atlas codes.", call. = FALSE)
  }
  out$state_code_1991 <- sprintf("%02d", state)
  out$district_code_1991 <- sprintf("%02d", district)
  key <- paste(out$state_code_1991, out$district_code_1991, out$atlas_column, sep = "__")
  if (anyDuplicated(key)) {
    stop("Accepted Language Atlas 1991 source has duplicate district-language keys.", call. = FALSE)
  }
  registry_i <- match(out$atlas_column, registry$atlas_column)
  if (anyNA(registry_i) ||
      any(normalize_language_label(out$language_1991) != normalize_language_label(registry$language_1991[registry_i])) ||
      any(normalize_language_label(out$canonical_language) != normalize_language_label(registry$canonical_language[registry_i]))) {
    stop("Accepted Language Atlas 1991 source disagrees with the frozen language registry.", call. = FALSE)
  }
  accepted <- num(out$accepted_speaker_count)
  candidate <- num(out$speaker_count_candidate)
  noninteger <- is.finite(accepted) & abs(accepted - round(accepted)) > 1e-10
  if (any(is.finite(accepted) & accepted < 0) || any(noninteger)) {
    stop("Accepted Language Atlas 1991 speaker counts must be non-negative integers.", call. = FALSE)
  }
  basis <- plain_chr(out$accepted_count_basis)
  decision <- plain_chr(out$cell_review_decision)
  unreviewed <- is.na(decision) | !nzchar(decision)
  valid_basis <- c(
    "machine_candidate", "unresolved",
    "reviewed_machine_candidate", "reviewed_replacement", "reviewed_unresolved"
  )
  if (any(!basis %in% valid_basis)) {
    stop("Accepted Language Atlas 1991 source has an invalid accepted-count basis.", call. = FALSE)
  }
  expected_review <- c(
    reviewed_machine_candidate = "accept_extracted",
    reviewed_replacement = "replace_count",
    reviewed_unresolved = "leave_unresolved"
  )
  reviewed <- basis %in% names(expected_review)
  if (any(reviewed & decision != unname(expected_review[basis]))) {
    stop("Accepted Language Atlas 1991 review decisions disagree with accepted-count provenance.", call. = FALSE)
  }
  if (any(!reviewed & !unreviewed)) {
    stop("Accepted Language Atlas 1991 unreviewed cells must not carry review decisions.", call. = FALSE)
  }
  unresolved <- basis %in% c("unresolved", "reviewed_unresolved")
  if (any(unresolved & is.finite(accepted)) || any(!unresolved & !is.finite(accepted))) {
    stop("Accepted Language Atlas 1991 count presence disagrees with accepted-count provenance.", call. = FALSE)
  }
  machine <- basis %in% c("machine_candidate", "reviewed_machine_candidate")
  if (any(machine & (!is.finite(candidate) | abs(accepted - candidate) > 1e-10))) {
    stop("Accepted Language Atlas 1991 machine counts disagree with their extraction candidates.", call. = FALSE)
  }
  out
}

language_atlas_1991_district_summary <- function(district) {
  population <- unique(num(district$atlas_population_candidate))
  pca_population <- unique(num(district$pca91_population))
  if (length(population) != 1L || !is.finite(population) || population <= 0 ||
      length(pca_population) != 1L || !is.finite(pca_population) || pca_population <= 0) {
    stop("Accepted Language Atlas 1991 district populations are internally inconsistent.", call. = FALSE)
  }

  speakers <- num(district$accepted_speaker_count)
  accepted <- is.finite(speakers) & speakers >= 0
  accepted_total <- sum(speakers[accepted], na.rm = TRUE)
  columns <- sort(unique(as.integer(district$atlas_column)))
  n_columns <- length(columns)
  n_accepted <- sum(accepted)
  n_review <- nrow(district) - n_accepted
  coverage <- accepted_total / population
  inventory <- language_atlas_1991_columns()
  inventory_size <- length(inventory)
  status <- if (accepted_total > population) {
    "speaker_sum_exceeds_atlas_population"
  } else if (n_columns < inventory_size) {
    "incomplete_alignment"
  } else if (n_accepted < inventory_size) {
    "unresolved_cells"
  } else {
    "complete_accepted_inventory"
  }

  repeated <- list(
    n_atlas_language_columns = n_columns,
    n_accepted_values = n_accepted,
    n_review_required = n_review,
    accepted_speaker_lower_bound = accepted_total,
    accepted_speaker_lower_bound_share_atlas = coverage,
    coverage_status = status
  )
  for (field in names(repeated)) {
    source <- unique(district[[field]])
    if (length(source) != 1L) {
      stop("Accepted Language Atlas 1991 district metadata are internally inconsistent.", call. = FALSE)
    }
    expected <- repeated[[field]]
    if (is.numeric(expected)) {
      if (!isTRUE(all.equal(num(source), expected, tolerance = 1e-10))) {
        stop("Accepted Language Atlas 1991 district coverage fields disagree with cell-level counts.", call. = FALSE)
      }
    } else if (!identical(plain_chr(source), expected)) {
      stop("Accepted Language Atlas 1991 coverage status disagrees with cell-level counts.", call. = FALSE)
    }
  }
  if (n_columns == inventory_size && !identical(columns, inventory)) {
    stop("Accepted Language Atlas 1991 district does not contain the complete Atlas column inventory.", call. = FALSE)
  }
  list(
    population = population, pca_population = pca_population,
    speakers = speakers, accepted = accepted, accepted_total = accepted_total,
    n_columns = n_columns, n_accepted = n_accepted, n_review = n_review,
    coverage = coverage, coverage_status = status
  )
}

read_language_atlas_1991_accepted_source <- function(path) {
  if (!file.exists(path)) stop("Missing accepted Language Atlas 1991 source: ", path, call. = FALSE)
  validate_language_atlas_1991_accepted_source(utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  ))
}


speaker_weighted_mean <- function(speakers, values, index = rep(TRUE, length(speakers))) {
  speakers <- num(speakers)
  values <- num(values)
  index <- as.logical(index) & is.finite(speakers) & speakers >= 0 & is.finite(values)
  denominator <- sum(speakers[index], na.rm = TRUE)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  sum(speakers[index] * values[index], na.rm = TRUE) / denominator
}

historical_linguistic_distance_bounds <- function(
    speakers, degree, language, population, nonzero_degree_range = c(1, 5)) {
  speakers <- num(speakers)
  degree <- num(degree)
  language <- normalize_language_label(language)
  if (!is.numeric(population) || length(population) != 1L ||
      !is.finite(population) || population <= 0) {
    stop("Historical linguistic-distance bounds require a positive district population.", call. = FALSE)
  }
  accepted <- is.finite(speakers) & speakers >= 0
  english <- accepted & language == "English"
  known_nonzero <- accepted & is.finite(degree) & degree > 0 & !english
  known_zero <- accepted & is.finite(degree) & degree == 0 & !english
  known_irrelevant <- known_zero | english

  nonzero_speakers <- sum(speakers[known_nonzero], na.rm = TRUE)
  numerator <- sum(speakers[known_nonzero] * degree[known_nonzero], na.rm = TRUE)
  known_irrelevant_speakers <- sum(speakers[known_irrelevant], na.rm = TRUE)
  resolved_speakers <- nonzero_speakers + known_irrelevant_speakers
  unresolved_mass <- max(0, population - resolved_speakers)
  point <- if (nonzero_speakers > 0) numerator / nonzero_speakers else NA_real_

  nonzero_degree_range <- sort(unique(num(nonzero_degree_range)))
  if (length(nonzero_degree_range) < 2L || any(!is.finite(nonzero_degree_range)) ||
      any(nonzero_degree_range <= 0)) {
    stop("Historical linguistic-distance bounds require a finite positive degree range.", call. = FALSE)
  }
  degree_min <- min(nonzero_degree_range)
  degree_max <- max(nonzero_degree_range)
  if (!is.finite(point) || !is.finite(degree_min) || !is.finite(degree_max)) {
    lower <- upper <- width <- NA_real_
  } else if (unresolved_mass <= 0) {
    lower <- upper <- point
    width <- 0
  } else {
    lower_if_nonzero <- (numerator + unresolved_mass * degree_min) /
      (nonzero_speakers + unresolved_mass)
    upper_if_nonzero <- (numerator + unresolved_mass * degree_max) /
      (nonzero_speakers + unresolved_mass)
    # Unresolved population may instead be Hindi/English/unmapped and therefore
    # leave the accepted-speaker mean unchanged. Include both possibilities.
    lower <- min(point, lower_if_nonzero)
    upper <- max(point, upper_if_nonzero)
    width <- upper - lower
  }

  list(
    point = point,
    lower = lower,
    upper = upper,
    width = width,
    nonzero_speakers = nonzero_speakers,
    known_irrelevant_speakers = known_irrelevant_speakers,
    resolved_speakers = resolved_speakers,
    unresolved_mass_upper_bound = unresolved_mass,
    degree_min = degree_min,
    degree_max = degree_max
  )
}

historical_linguistic_distance_1991_candidates <- function(
  atlas_source,
  registry = read_language_atlas_1991_languages(),
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications()
) {
  rows <- if (is.character(atlas_source) && length(atlas_source) == 1L) {
    read_language_atlas_1991_accepted_source(atlas_source)
  } else {
    safe_df(atlas_source)
  }
  rows <- validate_language_atlas_1991_accepted_source(rows, registry)

  preferred <- resolve_language_atlas_1991_shastry_mapping(
    registry, concordance, adjudications, scenario = "preferred"
  )
  sensitivity_low <- resolve_language_atlas_1991_shastry_mapping(
    registry, concordance, adjudications, scenario = "sensitivity_low"
  )
  sensitivity_high <- resolve_language_atlas_1991_shastry_mapping(
    registry, concordance, adjudications, scenario = "sensitivity_high"
  )
  preferred_nonzero_degrees <- preferred$shastry_degree[
    is.finite(preferred$shastry_degree) & preferred$shastry_degree > 0
  ]
  if (!length(preferred_nonzero_degrees)) {
    stop("Historical Shastry mapping has no positive distance support.", call. = FALSE)
  }
  nonzero_degree_range <- range(preferred_nonzero_degrees)
  inventory_size <- length(language_atlas_1991_columns())
  if (nrow(registry) != inventory_size) {
    stop("Historical Atlas registry size disagrees with the frozen column inventory.", call. = FALSE)
  }
  registry_i <- match(rows$atlas_column, preferred$atlas_column)
  rows$shastry_degree <- preferred$shastry_degree[registry_i]
  rows$shastry_degree_sensitivity_low <- sensitivity_low$shastry_degree[registry_i]
  rows$shastry_degree_sensitivity_high <- sensitivity_high$shastry_degree[registry_i]
  if (!identical(is.finite(rows$shastry_degree_sensitivity_low), is.finite(rows$shastry_degree_sensitivity_high))) {
    stop("Historical Shastry sensitivity mappings must have identical support.", call. = FALSE)
  }

  split_i <- split(
    seq_len(nrow(rows)),
    interaction(rows[c("state_code_1991", "district_code_1991")], drop = TRUE)
  )
  out <- safe_bind_rows(lapply(split_i, function(i) {
    district <- rows[i, , drop = FALSE]
    source <- language_atlas_1991_district_summary(district)
    population <- source$population
    speakers <- source$speakers
    language <- district$language_1991
    accepted <- source$accepted
    english <- accepted & normalize_language_label(language) == "English"
    mapped <- accepted & is.finite(district$shastry_degree) & !english
    mapped_low <- accepted & is.finite(district$shastry_degree_sensitivity_low) & !english
    mapped_high <- accepted & is.finite(district$shastry_degree_sensitivity_high) & !english
    nonzero <- mapped & district$shastry_degree > 0
    nonzero_low <- mapped_low & district$shastry_degree_sensitivity_low > 0
    nonzero_high <- mapped_high & district$shastry_degree_sensitivity_high > 0
    accepted_total <- source$accepted_total
    mapped_total <- sum(speakers[mapped], na.rm = TRUE)
    bounds <- historical_linguistic_distance_bounds(
      speakers, district$shastry_degree, language, population,
      nonzero_degree_range = nonzero_degree_range
    )

    result <- data.frame(
      state_code_1991 = plain_chr(district$state_code_1991[[1L]]),
      district_code_1991 = plain_chr(district$district_code_1991[[1L]]),
      state_name_1991 = plain_chr(district$state_name_1991[[1L]]),
      atlas_population_1991 = population,
      pca91_population = source$pca_population,
      n_atlas_language_columns = as.integer(source$n_columns),
      complete_atlas_alignment_1991 = source$n_columns == inventory_size,
      n_accepted_languages = source$n_accepted,
      n_unresolved_aligned_languages = source$n_review,
      n_unaligned_atlas_languages = inventory_size - source$n_columns,
      n_unresolved_languages = inventory_size - source$n_accepted,
      accepted_speaker_coverage_1991 = source$coverage,
      shastry_mapped_accepted_speaker_share_1991 = if (accepted_total > 0) mapped_total / accepted_total else NA_real_,
      shastry_mapped_population_share_1991 = if (population > 0) mapped_total / population else NA_real_,
      accepted_nonzero_mapped_speakers_1991 = bounds$nonzero_speakers,
      distance_resolved_speakers_1991 = bounds$resolved_speakers,
      distance_resolved_speaker_share_1991 = bounds$resolved_speakers / population,
      distance_unresolved_mass_upper_bound_1991 = bounds$unresolved_mass_upper_bound,
      distance_unresolved_mass_upper_bound_share_1991 = bounds$unresolved_mass_upper_bound / population,
      ling_distance_nonzero_mean_accepted_1991 = bounds$point,
      ling_distance_nonzero_lower_bound_1991 = bounds$lower,
      ling_distance_nonzero_upper_bound_1991 = bounds$upper,
      ling_distance_nonzero_bound_width_1991 = bounds$width,
      atlas_source_status = if (source$coverage_status == "speaker_sum_exceeds_atlas_population") {
        "population_bound_violation"
      } else if (!is.finite(bounds$point)) {
        "no_nonzero_mapped_speakers"
      } else {
        "candidate"
      },
      stringsAsFactors = FALSE
    )
    result$ling_distance_nonzero_mean_sensitivity_low_accepted_1991 <- speaker_weighted_mean(
      speakers, district$shastry_degree_sensitivity_low, nonzero_low
    )
    result$ling_distance_nonzero_mean_sensitivity_high_accepted_1991 <- speaker_weighted_mean(
      speakers, district$shastry_degree_sensitivity_high, nonzero_high
    )
    result
  }))
  validate_linguistic_distance_ranges(transform(
    out,
    ling_distance_nonzero_mean = ling_distance_nonzero_mean_accepted_1991,
    ling_distance_nonzero_mean_sensitivity_low = ling_distance_nonzero_mean_sensitivity_low_accepted_1991,
    ling_distance_nonzero_mean_sensitivity_high = ling_distance_nonzero_mean_sensitivity_high_accepted_1991
  ))[, names(out), drop = FALSE]
}

historical_linguistic_preferred_source_quality <- function() {
  data.frame(
    min_accepted_coverage = 0.99,
    max_distance_bound_width = 0.50,
    selection_basis = paste(
      "Source-only rule frozen before persistence/first-stage execution:",
      "at least 99% accepted speaker mass and at most half a Shastry degree",
      "of worst-case unresolved-mass uncertainty."
    ),
    stringsAsFactors = FALSE
  )
}

apply_historical_linguistic_distance_quality_gate <- function(
    candidates, min_accepted_coverage, max_distance_bound_width) {
  if (!is.numeric(min_accepted_coverage) || length(min_accepted_coverage) != 1L ||
      !is.finite(min_accepted_coverage) || min_accepted_coverage <= 0 || min_accepted_coverage > 1) {
    stop("Historical language coverage threshold must lie in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(max_distance_bound_width) || length(max_distance_bound_width) != 1L ||
      !is.finite(max_distance_bound_width) || max_distance_bound_width <= 0 || max_distance_bound_width > 5) {
    stop("Historical language distance-bound threshold must lie in (0, 5].", call. = FALSE)
  }
  out <- safe_df(candidates)
  required <- c(
    "atlas_source_status", "accepted_speaker_coverage_1991",
    "ling_distance_nonzero_mean_accepted_1991", "ling_distance_nonzero_bound_width_1991",
    "ling_distance_nonzero_mean_sensitivity_low_accepted_1991",
    "ling_distance_nonzero_mean_sensitivity_high_accepted_1991"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop("Historical linguistic-distance candidates lack quality fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  coverage <- num(out$accepted_speaker_coverage_1991)
  width <- num(out$ling_distance_nonzero_bound_width_1991)
  out$min_accepted_coverage <- min_accepted_coverage
  out$max_distance_bound_width <- max_distance_bound_width
  out$historical_language_status <- ifelse(
    out$atlas_source_status == "population_bound_violation", "population_bound_violation",
    ifelse(out$atlas_source_status == "no_nonzero_mapped_speakers", "no_nonzero_mapped_speakers",
      ifelse(!is.finite(coverage) | coverage < min_accepted_coverage, "below_coverage_threshold",
        ifelse(!is.finite(width) | width > max_distance_bound_width, "distance_bound_too_wide", "eligible")))
  )
  eligible <- out$historical_language_status == "eligible"
  out$ling_distance_nonzero_mean_1991 <- ifelse(
    eligible, num(out$ling_distance_nonzero_mean_accepted_1991), NA_real_
  )
  out$ling_distance_nonzero_mean_sensitivity_low_1991 <- ifelse(
    eligible, num(out$ling_distance_nonzero_mean_sensitivity_low_accepted_1991), NA_real_
  )
  out$ling_distance_nonzero_mean_sensitivity_high_1991 <- ifelse(
    eligible, num(out$ling_distance_nonzero_mean_sensitivity_high_accepted_1991), NA_real_
  )
  validate_linguistic_distance_ranges(transform(
    out,
    ling_distance_nonzero_mean = ling_distance_nonzero_mean_1991,
    ling_distance_nonzero_mean_sensitivity_low = ling_distance_nonzero_mean_sensitivity_low_1991,
    ling_distance_nonzero_mean_sensitivity_high = ling_distance_nonzero_mean_sensitivity_high_1991
  ))[, names(out), drop = FALSE]
}

build_historical_linguistic_distance_1991 <- function(
  atlas_source,
  min_accepted_coverage,
  max_distance_bound_width,
  registry = read_language_atlas_1991_languages(),
  concordance = read_shastry_language_distance(),
  adjudications = read_shastry_language_adjudications()
) {
  candidates <- historical_linguistic_distance_1991_candidates(
    atlas_source, registry, concordance, adjudications
  )
  apply_historical_linguistic_distance_quality_gate(
    candidates, min_accepted_coverage, max_distance_bound_width
  )
}

apply_preferred_historical_linguistic_distance_quality_gate <- function(candidates) {
  rule <- historical_linguistic_preferred_source_quality()
  apply_historical_linguistic_distance_quality_gate(
    candidates,
    min_accepted_coverage = rule$min_accepted_coverage[[1L]],
    max_distance_bound_width = rule$max_distance_bound_width[[1L]]
  )
}

build_preferred_historical_linguistic_distance_1991 <- function(
    atlas_source,
    registry = read_language_atlas_1991_languages(),
    concordance = read_shastry_language_distance(),
    adjudications = read_shastry_language_adjudications()) {
  candidates <- historical_linguistic_distance_1991_candidates(
    atlas_source, registry, concordance, adjudications
  )
  apply_preferred_historical_linguistic_distance_quality_gate(candidates)
}

historical_linguistic_distance_quality_grid <- function(
    candidates,
    coverage_thresholds = c(0.95, 0.98, 0.99, 0.995, 0.999),
    bound_width_thresholds = c(0.10, 0.25, 0.50, 1.00)) {
  x <- safe_df(candidates)
  required <- c(
    "atlas_population_1991", "atlas_source_status", "complete_atlas_alignment_1991",
    "accepted_speaker_coverage_1991", "ling_distance_nonzero_bound_width_1991"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Historical linguistic-distance candidates lack sensitivity fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  coverage_thresholds <- sort(unique(num(coverage_thresholds)))
  bound_width_thresholds <- sort(unique(num(bound_width_thresholds)))
  if (!length(coverage_thresholds) || any(!is.finite(coverage_thresholds) | coverage_thresholds <= 0 | coverage_thresholds > 1)) {
    stop("Historical language sensitivity coverage thresholds must lie in (0, 1].", call. = FALSE)
  }
  if (!length(bound_width_thresholds) || any(!is.finite(bound_width_thresholds) | bound_width_thresholds <= 0 | bound_width_thresholds > 5)) {
    stop("Historical language sensitivity bound widths must lie in (0, 5].", call. = FALSE)
  }
  safe_bind_rows(lapply(coverage_thresholds, function(coverage_threshold) {
    safe_bind_rows(lapply(bound_width_thresholds, function(bound_width_threshold) {
      eligible <- x$atlas_source_status == "candidate" &
        num(x$accepted_speaker_coverage_1991) >= coverage_threshold &
        num(x$ling_distance_nonzero_bound_width_1991) <= bound_width_threshold
      data.frame(
        min_accepted_coverage = coverage_threshold,
        max_distance_bound_width = bound_width_threshold,
        n_districts = sum(eligible, na.rm = TRUE),
        atlas_population_1991 = sum(num(x$atlas_population_1991[eligible]), na.rm = TRUE),
        n_complete_atlas_alignment = sum(eligible & x$complete_atlas_alignment_1991 %in% TRUE, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  }))
}


normalize_language_label <- function(x) {
  tools::toTitleCase(tolower(trimws(plain_chr(x))))
}

census_mother_tongue_identity <- function(df) {
  canonical <- normalize_language_label(df$canonical_language)
  if (!"mother_tongue" %in% names(df)) return(canonical)
  mother <- normalize_language_label(df$mother_tongue)
  missing <- is.na(mother) | !nzchar(mother)
  mother[missing] <- canonical[missing]
  mother
}

linguistic_distance_degrees <- function(
  mother_tongue,
  canonical_language = mother_tongue,
  concordance = read_shastry_language_distance()
) {
  mother <- normalize_language_label(mother_tongue)
  canonical <- normalize_language_label(canonical_language)
  distance <- concordance$distance_from_hindi[
    match(mother, concordance$canonical_language)
  ]

  # C-16's Hindi/Urdu language groups contain distinct mother tongues. A child
  # row must not inherit zero distance solely from that group subtotal.
  protected_zero_group <- canonical %in% c("Hindi", "Urdu") &
    !is.na(mother) & nzchar(mother) & mother != canonical
  fallback <- !is.finite(distance) & !protected_zero_group
  distance[fallback] <- concordance$distance_from_hindi[
    match(canonical[fallback], concordance$canonical_language)
  ]
  distance
}

validate_supplied_linguistic_distances <- function(x) {
  value <- num(x)
  if (any(is.finite(value) & (value < 0 | value > 5))) {
    stop("ling_degrees must be in the 0-5 range.", call. = FALSE)
  }
  invisible(value)
}

#' Build the complete suite of Census 2001 linguistic-distance constructions
build_linguistic_distance_iv <- function(
  census_2001_languages,
  cfg = list(),
  glottolog = NULL,
  glottolog_crosswalk = NULL,
  historical_linguistics = NULL,
  shastry_adjudications = read_shastry_language_adjudications()
) {
  df <- std(safe_df(census_2001_languages), 2001L)
  if ("ling_degrees" %in% names(df)) validate_supplied_linguistic_distances(df$ling_degrees)

  required <- c("state_std", "district_std", "spkr_tot", "canonical_language")
  if (!nrow(df) || !all(required %in% names(df))) {
    return(linguistic_distance_out_of_pipeline("No real linguistic-distance column or full cleaned C-16 distribution was available."))
  }

  language <- census_mother_tongue_identity(df)
  if (!is.null(glottolog) && !is.null(glottolog_crosswalk)) {
    df <- attach_glottolog_language_distance(df, glottolog, glottolog_crosswalk)
  } else if (!"glottolog_edge_distance" %in% names(df)) {
    df$glottolog_edge_distance <- NA_real_
  }
  if (!"ling_degrees" %in% names(df)) {
    df$ling_degrees <- resolve_shastry_language_degrees(
      df, adjudications = shastry_adjudications, scenario = "preferred"
    )
  }
  if (!"ling_degrees_sensitivity_low" %in% names(df)) {
    df$ling_degrees_sensitivity_low <- resolve_shastry_language_degrees(
      df, adjudications = shastry_adjudications, scenario = "sensitivity_low"
    )
  }
  if (!"ling_degrees_sensitivity_high" %in% names(df)) {
    df$ling_degrees_sensitivity_high <- resolve_shastry_language_degrees(
      df, adjudications = shastry_adjudications, scenario = "sensitivity_high"
    )
  }

  if (!is.null(historical_linguistics)) {
    df <- attach_dyen_language_distance(df, historical_linguistics)
  } else if (!"dyen_noncognate_pct" %in% names(df)) {
    df$dyen_noncognate_pct <- NA_real_
  }

  is_english <- language == "English"
  df$distance_mapping_status <- ifelse(
    is_english,
    "special_english",
    ifelse(is.finite(num(df$ling_degrees)), "mapped", "unmapped")
  )

  split_i <- split(seq_len(nrow(df)), interaction(df[c("state_std", "district_std")], drop = TRUE))
  out <- safe_bind_rows(lapply(split_i, function(i) build_district_language_constructions(df[i, , drop = FALSE])))
  if (!nrow(out)) return(linguistic_distance_out_of_pipeline("No district linguistic-distance constructions could be computed."))

  names_df <- unique(df[intersect(c(
    "state_std", "district_std", "state", "district", "state_code", "district_code", "district_name"
  ), names(df))])
  out <- merge(out, names_df, by = c("state_std", "district_std"), all.x = TRUE, sort = FALSE)
  state_code <- first_col(out, c("state_code", "state"))
  if (!is.null(state_code)) out$state_01 <- census_2001_state_name(out[[state_code]])
  if ("district_name" %in% names(out)) out$district_01 <- out$district_name
  if (!"district_01" %in% names(out) && "district" %in% names(out)) out$district_01 <- out$district
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2001L)
  validate_linguistic_distance_ranges(out)
}

build_district_language_constructions <- function(df) {
  speakers <- num(df$spkr_tot)
  distance <- num(df$ling_degrees)
  distance_low <- num(df$ling_degrees_sensitivity_low %||% distance)
  distance_high <- num(df$ling_degrees_sensitivity_high %||% distance)
  if (!identical(is.finite(distance_low), is.finite(distance_high))) {
    stop("Shastry low/high sensitivity mappings must have identical mapped support.", call. = FALSE)
  }
  glottolog_distance <- num(df$glottolog_edge_distance %||% rep(NA_real_, nrow(df)))
  dyen_distance <- num(df$dyen_noncognate_pct %||% rep(NA_real_, nrow(df)))
  language <- census_mother_tongue_identity(df)
  valid_speakers <- is.finite(speakers) & speakers >= 0
  total <- sum(speakers[valid_speakers], na.rm = TRUE)
  english <- valid_speakers & language == "English"
  mapped <- valid_speakers & is.finite(distance) & !english
  unmapped <- valid_speakers & !is.finite(distance) & !english
  mapped_total <- sum(speakers[mapped], na.rm = TRUE)
  nonzero <- mapped & distance > 0

  share <- function(condition, denominator = total) {
    if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
    100 * sum(speakers[valid_speakers & condition], na.rm = TRUE) / denominator
  }
  top3 <- select_top_mother_tongues(df[mapped, , drop = FALSE], 3L)
  top3_distance <- if (nrow(top3)) wmean(top3$ling_degrees, top3$spkr_tot) else NA_real_
  top3_coverage <- if (is.finite(total) && total > 0 && nrow(top3)) 100 * sum(num(top3$spkr_tot), na.rm = TRUE) / total else NA_real_

  out <- df[1, c("state_std", "district_std"), drop = FALSE]
  out$ling_distance_nonzero_mean <- speaker_weighted_mean(speakers, distance, nonzero)

  sensitivity_mapped <- valid_speakers & is.finite(distance_low) & !english
  sensitivity_nonzero_low <- sensitivity_mapped & distance_low > 0
  sensitivity_nonzero_high <- sensitivity_mapped & distance_high > 0
  out$ling_distance_nonzero_mean_sensitivity_low <- speaker_weighted_mean(
    speakers, distance_low, sensitivity_nonzero_low
  )
  out$ling_distance_nonzero_mean_sensitivity_high <- speaker_weighted_mean(
    speakers, distance_high, sensitivity_nonzero_high
  )
  sensitivity_total <- sum(speakers[sensitivity_mapped], na.rm = TRUE)
  out$ling_sensitivity_mapped_speaker_share <- if (is.finite(total) && total > 0) {
    100 * sensitivity_total / total
  } else {
    NA_real_
  }
  out$ling_share_distance_ge3 <- share(mapped & distance >= 3)
  for (degree in 0:5) {
    out[[paste0("ling_share_distance_", degree)]] <- share(mapped & distance == degree)
    out[[paste0("ling_mapped_share_distance_", degree)]] <- share(
      mapped & distance == degree,
      denominator = mapped_total
    )
  }
  out$hindi_share <- share(language == "Hindi")
  out$urdu_share <- share(language == "Urdu")
  out$hindi_urdu_share <- share(language %in% c("Hindi", "Urdu"))
  out$native_english_share <- share(english)

  glottolog_reference <- valid_speakers & language %in% c("Hindi", "Urdu", "English")
  glottolog_eligible <- valid_speakers & !glottolog_reference
  glottolog_mapped <- glottolog_eligible & is.finite(glottolog_distance)
  glottolog_unmapped <- glottolog_eligible & !is.finite(glottolog_distance)
  glottolog_eligible_total <- sum(speakers[glottolog_eligible], na.rm = TRUE)
  out$ling_distance_glottolog_nonhindi_mean <- speaker_weighted_mean(speakers, glottolog_distance, glottolog_mapped)
  out$ling_glottolog_mapped_speaker_share <- if (is.finite(glottolog_eligible_total) && glottolog_eligible_total > 0) {
    100 * sum(speakers[glottolog_mapped], na.rm = TRUE) / glottolog_eligible_total
  } else {
    NA_real_
  }
  out$ling_glottolog_unmapped_speaker_share <- if (is.finite(glottolog_eligible_total) && glottolog_eligible_total > 0) {
    100 * sum(speakers[glottolog_unmapped], na.rm = TRUE) / glottolog_eligible_total
  } else {
    NA_real_
  }

  dyen_reference <- valid_speakers & language %in% c("Hindi", "Urdu", "English")
  dyen_eligible <- valid_speakers & !dyen_reference
  dyen_mapped <- dyen_eligible & is.finite(dyen_distance)
  dyen_unmapped <- dyen_eligible & !is.finite(dyen_distance)
  dyen_eligible_total <- sum(speakers[dyen_eligible], na.rm = TRUE)
  out$ling_distance_dyen_noncognate_pct <- speaker_weighted_mean(speakers, dyen_distance, dyen_mapped)
  out$ling_dyen_mapped_speaker_share <- if (is.finite(dyen_eligible_total) && dyen_eligible_total > 0) {
    100 * sum(speakers[dyen_mapped], na.rm = TRUE) / dyen_eligible_total
  } else {
    NA_real_
  }
  out$ling_dyen_unmapped_speaker_share <- if (is.finite(dyen_eligible_total) && dyen_eligible_total > 0) {
    100 * sum(speakers[dyen_unmapped], na.rm = TRUE) / dyen_eligible_total
  } else {
    NA_real_
  }

  out$ling_distance_top3_legacy <- top3_distance
  out$ling_top3_speaker_coverage <- top3_coverage
  out$ling_mapped_speaker_share <- if (is.finite(total) && total > 0) 100 * mapped_total / total else NA_real_
  out$ling_unmapped_speaker_share <- share(unmapped)
  out$ling_total_speakers <- total

  # Historical compatibility aliases remain explicit for archived comparisons and benchmarks.
  out$wavg_ling_degrees <- out$ling_distance_top3_legacy
  out
}

linguistic_distance_excluded_instruments <- function(denominator = c("all", "mapped")) {
  denominator <- match.arg(denominator)
  prefix <- if (identical(denominator, "mapped")) "ling_mapped_share_distance_" else "ling_share_distance_"
  paste0(prefix, 1:5)
}

linguistic_distance_language_controls <- function() {
  c("hindi_share", "urdu_share", "hindi_urdu_share", "native_english_share")
}

validate_linguistic_distance_ranges <- function(df) {
  distance_cols <- intersect(c(
    "ling_distance_nonzero_mean",
    "ling_distance_nonzero_mean_sensitivity_low",
    "ling_distance_nonzero_mean_sensitivity_high",
    "ling_distance_top3_legacy", "wavg_ling_degrees",
    "ling_distance_glottolog_nonhindi_mean", "ling_distance_dyen_noncognate_pct"
  ), names(df))
  for (nm in setdiff(distance_cols, c(
    "ling_distance_glottolog_nonhindi_mean", "ling_distance_dyen_noncognate_pct"
  ))) {
    value <- num(df[[nm]])
    if (any(is.finite(value) & (value < 0 | value > 5))) stop(nm, " must be in the 0-5 range.", call. = FALSE)
  }
  if ("ling_distance_glottolog_nonhindi_mean" %in% names(df)) {
    value <- num(df$ling_distance_glottolog_nonhindi_mean)
    if (any(is.finite(value) & value < 0)) stop("Glottolog distance must be non-negative.", call. = FALSE)
  }
  if ("ling_distance_dyen_noncognate_pct" %in% names(df)) {
    value <- num(df$ling_distance_dyen_noncognate_pct)
    if (any(is.finite(value) & (value < 0 | value > 100))) {
      stop("Dyen noncognate distance must be in the 0-100 range.", call. = FALSE)
    }
  }
  share_cols <- grep("(^ling_(mapped_)?share_distance_|_share$|speaker_coverage$)", names(df), value = TRUE)
  for (nm in share_cols) {
    value <- num(df[[nm]])
    if (any(is.finite(value) & (value < 0 | value > 100))) stop(nm, " must be in the 0-100 range.", call. = FALSE)
  }
  df
}

linguistic_distance_out_of_pipeline <- function(reason) {
  data.frame(status = "out_of_active_pipeline", reason = reason, stringsAsFactors = FALSE)
}
