# Completion workflow for district lineage.
#
# This module converts diagnostic candidates into review-ready artifacts,
# assembles preferred and sensitivity crosswalks from accepted metadata, and
# reports the remaining methodological work. It never promotes a candidate to
# accepted status without a tracked adjudication.

empty_adjudication_draft <- function() {
  data.frame(
    source_row_id = character(), wave = character(), raw_state = character(),
    raw_district = character(), unit_id = character(), method = character(),
    source_id = character(), status = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

build_adjudication_draft <- function(source_roster, adjudication_queue, candidates) {
  roster <- safe_df(source_roster)
  queue <- safe_df(adjudication_queue)
  candidates <- safe_df(candidates)
  if (!nrow(roster)) return(empty_adjudication_draft())

  adjudication_status <- if ("adjudication_status" %in% names(queue)) {
    queue$adjudication_status
  } else {
    rep(NA_character_, nrow(queue))
  }
  unresolved_queue <- queue[
    !(adjudication_status %in% c("accepted", "excluded")),
    c(
      "source_row_id", "recommended_unit", "recommended_method",
      "review_class", "recommended_vintage"
    ),
    drop = FALSE
  ]
  if (!nrow(unresolved_queue)) return(empty_adjudication_draft())

  top <- merge(
    unresolved_queue[c("source_row_id", "recommended_unit")],
    candidates[c("source_row_id", "candidate_unit", "candidate_source_id")],
    by.x = c("source_row_id", "recommended_unit"),
    by.y = c("source_row_id", "candidate_unit"),
    all.x = TRUE,
    sort = FALSE
  )
  top <- top[!duplicated(top$source_row_id), , drop = FALSE]
  top <- top[c("source_row_id", "recommended_unit", "candidate_source_id")]
  names(top) <- c("source_row_id", "unit_id", "source_id")

  out <- merge(
    roster[c("source_row_id", "wave", "raw_state", "raw_district")],
    unresolved_queue,
    by = "source_row_id", all = FALSE, sort = FALSE
  )
  out <- merge(out, top, by = "source_row_id", all.x = TRUE, sort = FALSE)
  out$unit_id <- ifelse(
    !is.na(out$recommended_unit) & nzchar(out$recommended_unit),
    out$recommended_unit,
    out$unit_id
  )
  out$method <- paste0("proposed_", out$recommended_method)
  out$status <- "needs_review"
  out$note <- paste0(
    "Generated review draft: ", out$review_class,
    "; preferred reference vintage=", out$recommended_vintage,
    ". Confirm administrative continuity and source evidence before changing status."
  )
  out$source_id[is.na(out$source_id) | !nzchar(out$source_id)] <- NA_character_

  out[c(
    "source_row_id", "wave", "raw_state", "raw_district", "unit_id",
    "method", "source_id", "status", "note"
  )]
}

empty_sensitivity_crosswalk <- function() {
  data.frame(
    source_row_id = character(), wave = character(), source_code = character(),
    target_unit_2001 = character(), weight = numeric(), basis = character(),
    source_id = character(), panel_variant = character(),
    stringsAsFactors = FALSE
  )
}

build_sensitivity_crosswalk <- function(
  primary_crosswalk, allocation_weights, conservative_eligibility = data.frame()
) {
  primary <- safe_df(primary_crosswalk)
  allocations <- safe_df(allocation_weights)
  eligibility <- safe_df(conservative_eligibility)
  eligibility_cols <- c(
    "source_row_id", "wave", "source_code", "terminal_unit",
    "status", "eligible_conservative"
  )
  for (nm in setdiff(eligibility_cols, names(eligibility))) {
    eligibility[[nm]] <- rep(NA, nrow(eligibility))
  }
  pieces <- list()

  if (nrow(primary)) {
    pieces[[length(pieces) + 1L]] <- data.frame(
      source_row_id = primary$source_row_id,
      wave = primary$wave,
      source_code = primary$source_code,
      target_unit_2001 = primary$target_unit_2001,
      weight = 1,
      basis = primary$mapping_class,
      source_id = NA_character_,
      panel_variant = "deterministic",
      stringsAsFactors = FALSE
    )
  }

  accepted <- allocations$status %in% "accepted"
  allocatable <- eligibility[
    eligibility$status %in% "accepted" &
      eligibility$eligible_conservative %in% FALSE &
      !is.na(eligibility$terminal_unit) &
      nzchar(eligibility$terminal_unit),
    c("source_row_id", "wave", "source_code", "terminal_unit"),
    drop = FALSE
  ]
  if (nrow(allocations) && any(accepted) && nrow(allocatable)) {
    a <- allocations[
      accepted,
      c(
        "source_unit", "target_2001", "weight",
        "basis", "source_id"
      ),
      drop = FALSE
    ]
    linked <- merge(
      allocatable,
      a,
      by.x = "terminal_unit",
      by.y = "source_unit",
      all = FALSE,
      sort = FALSE
    )
    if (nrow(linked)) {
      pieces[[length(pieces) + 1L]] <- data.frame(
        source_row_id = linked$source_row_id,
        wave = linked$wave,
        source_code = linked$source_code,
        target_unit_2001 = linked$target_2001,
        weight = linked$weight,
        basis = linked$basis,
        source_id = linked$source_id,
        panel_variant = "population_allocation",
        stringsAsFactors = FALSE
      )
    }
  }

  out <- safe_bind_rows(pieces)
  if (!nrow(out)) return(empty_sensitivity_crosswalk())
  if (any(!is.finite(out$weight) | out$weight < 0)) {
    stop("Sensitivity crosswalk weights must be finite and nonnegative.", call. = FALSE)
  }
  source_weight <- aggregate(
    out$weight,
    list(source_row_id = out$source_row_id),
    sum
  )
  if (any(abs(source_weight$x - 1) > 1e-8)) {
    stop(
      "Sensitivity crosswalk weights must sum to one within source row.",
      call. = FALSE
    )
  }
  out
}

read_legacy_mapping_reviews <- function(x) {
  x <- safe_df(x)
  required <- c(
    "review_id", "source_row_id", "review_scope",
    "conservative_target_unit_2001", "legacy_target_unit_2001",
    "decision", "source_id", "status", "note"
  )
  for (nm in setdiff(required, names(x))) {
    x[[nm]] <- rep(NA_character_, nrow(x))
  }
  x <- x[!is.na(x$review_id) & nzchar(x$review_id), required, drop = FALSE]
  if (anyDuplicated(x$review_id)) {
    stop("Legacy mapping reviews must have unique review_id values.", call. = FALSE)
  }
  invalid <- unique(x$status[!x$status %in% c(
    "accepted", "excluded", "needs_review"
  )])
  if (length(invalid)) {
    stop(
      "Unknown legacy mapping review status: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }
  x
}

empty_legacy_comparison <- function() {
  data.frame(
    source_row_id = character(), wave = character(), source_code = character(),
    conservative_target_unit_2001 = character(), legacy_target_unit_2001 = character(),
    comparison_status = character(), review_decision = character(),
    review_status = character(), stringsAsFactors = FALSE
  )
}

build_legacy_crosswalk_comparison <- function(
  primary_crosswalk, legacy_panel, reviews = data.frame()
) {
  x <- safe_df(primary_crosswalk)
  panel <- safe_df(legacy_panel)
  reviews <- read_legacy_mapping_reviews(reviews)
  if (!nrow(x)) return(empty_legacy_comparison())

  legacy_for_wave <- function(wave) {
    code_col <- if (identical(wave, "nss_2007_08")) {
      "district_code_0708"
    } else {
      "district_code_1718"
    }
    if (!all(c(code_col, "district_panel_id") %in% names(panel))) {
      return(data.frame(
        source_code = character(),
        legacy_target_unit_2001 = character(),
        legacy_mapping_count = integer(),
        stringsAsFactors = FALSE
      ))
    }
    raw <- unique(data.frame(
      source_code = plain_chr(panel[[code_col]]),
      legacy_target_unit_2001 =
        sub("^2001__", "pc2001__", plain_chr(panel$district_panel_id)),
      stringsAsFactors = FALSE
    ))
    raw <- raw[!is.na(raw$source_code) & nzchar(raw$source_code), , drop = FALSE]
    if (!nrow(raw)) {
      return(data.frame(
        source_code = character(),
        legacy_target_unit_2001 = character(),
        legacy_mapping_count = integer(),
        stringsAsFactors = FALSE
      ))
    }
    groups <- split(raw$legacy_target_unit_2001, raw$source_code)
    data.frame(
      source_code = names(groups),
      legacy_target_unit_2001 = vapply(
        groups,
        function(z) if (length(unique(z)) == 1L) unique(z) else NA_character_,
        character(1)
      ),
      legacy_mapping_count = vapply(
        groups, function(z) length(unique(z)), integer(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  groups <- split(seq_len(nrow(x)), x$wave)
  out <- safe_bind_rows(lapply(names(groups), function(wave) {
    rows <- x[groups[[wave]], , drop = FALSE]
    current <- legacy_for_wave(wave)
    rows <- merge(rows, current, by = "source_code", all.x = TRUE, sort = FALSE)
    rows$legacy_mapping_count[
      is.na(rows$legacy_mapping_count)
    ] <- 0L
    data.frame(
      source_row_id = rows$source_row_id,
      wave = rows$wave,
      source_code = rows$source_code,
      conservative_target_unit_2001 = rows$target_unit_2001,
      legacy_target_unit_2001 = rows$legacy_target_unit_2001,
      comparison_status = ifelse(
        rows$legacy_mapping_count > 1L,
        "ambiguous_legacy_mapping",
        ifelse(
          is.na(rows$legacy_target_unit_2001),
          "missing_from_legacy_panel",
          ifelse(
            rows$target_unit_2001 == rows$legacy_target_unit_2001,
            "same_target",
            "changed_target"
          )
        )
      ),
      stringsAsFactors = FALSE
    )
  }))
  mapping_reviews <- reviews[
    reviews$review_scope %in% "mapping_difference",
    c("source_row_id", "decision", "status"),
    drop = FALSE
  ]
  names(mapping_reviews) <- c(
    "source_row_id", "review_decision", "review_status"
  )
  out <- merge(
    out, mapping_reviews,
    by = "source_row_id", all.x = TRUE, sort = FALSE
  )
  out$review_status[
    out$comparison_status %in% c(
      "same_target", "missing_from_legacy_panel"
    )
  ] <- "not_required"
  out$review_decision[
    out$comparison_status %in% "missing_from_legacy_panel"
  ] <- "coverage_addition"
  out
}

empty_geometry_carrybacks <- function() {
  data.frame(
    target_unit_2001 = character(), source_unit_2011 = character(),
    source_id = character(), status = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

read_geometry_carrybacks <- function(x) {
  x <- safe_df(x)
  required <- c(
    "target_unit_2001", "source_unit_2011", "source_id", "status", "note"
  )
  for (nm in setdiff(required, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
  x <- x[
    !is.na(x$target_unit_2001) & nzchar(x$target_unit_2001),
    required,
    drop = FALSE
  ]
  if (!nrow(x)) return(empty_geometry_carrybacks())
  if (anyDuplicated(x$target_unit_2001)) {
    stop("Geometry carry-backs must have unique 2001 target units.", call. = FALSE)
  }
  if (anyDuplicated(x$source_unit_2011)) {
    stop("Geometry carry-backs must have unique 2011 source units.", call. = FALSE)
  }
  invalid_status <- unique(x$status[!x$status %in% c(
    "accepted", "excluded", "needs_review"
  )])
  if (length(invalid_status)) {
    stop(
      "Unknown geometry carry-back status: ",
      paste(invalid_status, collapse = ", "),
      call. = FALSE
    )
  }
  accepted <- x$status %in% "accepted"
  incomplete <- accepted & (
    is.na(x$source_unit_2011) | !nzchar(x$source_unit_2011) |
      is.na(x$source_id) | !nzchar(x$source_id)
  )
  if (any(incomplete)) {
    stop(
      "Accepted geometry carry-backs require source_unit_2011 and source_id.",
      call. = FALSE
    )
  }
  x
}

district_geometry_unit_ids_2011 <- function(geometry_2011) {
  need_pkg("sf", "Census 2011 district geometry carry-backs")
  if (!inherits(geometry_2011, "sf")) {
    stop("Census 2011 district geometry must be an sf object.", call. = FALSE)
  }
  attributes <- sf::st_drop_geometry(geometry_2011)
  state <- first_col(attributes, c("pc11_state_id", "state_code"))
  district <- first_col(attributes, c("pc11_district_id", "district_code"))
  if (is.null(state) || is.null(district)) {
    stop(
      "Census 2011 district geometry lacks state or district codes.",
      call. = FALSE
    )
  }
  paste(
    "pc2011",
    pad_admin_code(attributes[[state]], 2L),
    pad_admin_code(attributes[[district]], 3L),
    sep = "__"
  )
}

as_unit_geometry <- function(x, unit_id = x$unit_id) {
  need_pkg("sf", "district geometry schema normalization")
  if (!inherits(x, "sf")) {
    stop("District geometry must be an sf object.", call. = FALSE)
  }
  unit_id <- plain_chr(unit_id)
  if (length(unit_id) != nrow(x)) {
    stop(
      "District geometry unit IDs must have one value per feature.",
      call. = FALSE
    )
  }
  sf::st_sf(
    unit_id = unit_id,
    geometry = sf::st_geometry(x),
    crs = sf::st_crs(x)
  )
}

apply_geometry_carrybacks <- function(
  geometry_2001, geometry_2011, carrybacks
) {
  need_pkg("sf", "Census 2001 geometry carry-backs")
  carrybacks <- read_geometry_carrybacks(carrybacks)
  carrybacks <- carrybacks[carrybacks$status %in% "accepted", , drop = FALSE]
  if (!nrow(carrybacks)) return(geometry_2001)
  if (!inherits(geometry_2001, "sf") || !inherits(geometry_2011, "sf")) {
    stop("Both geometry inputs must be sf objects.", call. = FALSE)
  }

  existing <- unique(plain_chr(geometry_2001$unit_id))
  carrybacks <- carrybacks[
    !carrybacks$target_unit_2001 %in% existing,
    ,
    drop = FALSE
  ]
  if (!nrow(carrybacks)) return(geometry_2001)

  source_ids <- district_geometry_unit_ids_2011(geometry_2011)
  source_rows <- match(carrybacks$source_unit_2011, source_ids)
  if (anyNA(source_rows)) {
    stop(
      "Accepted geometry carry-backs reference unknown Census 2011 units: ",
      paste(carrybacks$source_unit_2011[is.na(source_rows)], collapse = ", "),
      call. = FALSE
    )
  }

  additions <- geometry_2011[source_rows, , drop = FALSE]
  if (!is.na(sf::st_crs(geometry_2001)) &&
      sf::st_crs(additions) != sf::st_crs(geometry_2001)) {
    additions <- sf::st_transform(additions, sf::st_crs(geometry_2001))
  }

  base <- as_unit_geometry(geometry_2001)
  additions <- as_unit_geometry(
    additions,
    unit_id = carrybacks$target_unit_2001
  )
  out <- rbind(base, additions)
  out <- make_valid_sf(out)
  if (anyDuplicated(out$unit_id)) {
    stop("Geometry carry-backs produced duplicate Census 2001 units.", call. = FALSE)
  }
  out
}

read_zipped_gpkg <- function(path) {
  need_pkg("sf", "zipped SHRID geometry")
  if (!file.exists(path)) {
    stop("Missing SHRID geometry archive: ", path, call. = FALSE)
  }
  extract_dir <- tempfile("shrid-geometry-")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = extract_dir)
  gpkg <- list.files(
    extract_dir, pattern = "\\.gpkg$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  if (length(gpkg) != 1L) {
    stop(
      "Expected exactly one GeoPackage in SHRID archive; found ",
      length(gpkg), ".", call. = FALSE
    )
  }
  sf::st_read(gpkg, quiet = TRUE)
}

save_lineage_geometry_2001 <- function(
  geometry_2001, admin_2001,
  path = "outputs/derived/district_lineage/district_2001.gpkg"
) {
  need_pkg("sf", "Census 2001 district geometry output")
  if (!inherits(geometry_2001, "sf") || !nrow(geometry_2001)) {
    stop("Census 2001 geometry is empty or not an sf object.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) unlink(path)
  sf::st_write(geometry_2001, path, quiet = TRUE)
  qa <- geometry_qa(geometry_2001, admin_2001)
  qa_path <- file.path(dirname(path), "district_2001_qa.csv")
  utils::write.csv(qa, qa_path, row.names = FALSE, na = "")
  c(path, qa_path)
}

make_valid_sf <- function(x) {
  need_pkg("sf", "district geometry validity repair")
  if (!inherits(x, "sf") || !nrow(x)) return(x)

  valid <- sf::st_is_valid(x)
  invalid <- is.na(valid) | !valid
  if (any(invalid)) {
    repaired <- sf::st_make_valid(x[invalid, , drop = FALSE])
    sf::st_geometry(x)[invalid] <- sf::st_geometry(repaired)
  }
  x
}

dissolve_shrid_geometry_2001 <- function(shrid_geometry, bridge) {
  need_pkg("sf", "Census 2001 district geometry construction")
  geometry <- safe_df(shrid_geometry)
  bridge <- safe_df(bridge)
  if (!inherits(shrid_geometry, "sf")) {
    stop("SHRID geometry must be an sf object.", call. = FALSE)
  }
  required_geometry <- c("shrid2")
  required_bridge <- c(
    "shrid2", "state_code_2001", "district_code_2001", "deterministic_2001"
  )
  if (!all(required_geometry %in% names(geometry))) {
    stop("SHRID geometry is missing shrid2.", call. = FALSE)
  }
  missing_bridge <- setdiff(required_bridge, names(bridge))
  if (length(missing_bridge)) {
    stop("SHRID bridge is missing: ", paste(missing_bridge, collapse = ", "), call. = FALSE)
  }

  membership <- unique(bridge[
    bridge$deterministic_2001 %in% TRUE,
    c("shrid2", "state_code_2001", "district_code_2001"),
    drop = FALSE
  ])
  joined <- merge(shrid_geometry, membership, by = "shrid2", all = FALSE, sort = FALSE)
  if (!nrow(joined)) {
    return(joined)
  }
  joined <- make_valid_sf(joined)
  joined$unit_id <- paste0(
    "pc2001__", joined$state_code_2001, "__", joined$district_code_2001
  )
  joined$.member <- 1L
  out <- aggregate(
    joined[".member"],
    by = list(unit_id = joined$unit_id),
    FUN = sum,
    do_union = TRUE
  )
  out$.member <- NULL
  out <- make_valid_sf(out)
  valid <- sf::st_is_valid(out)
  invalid <- is.na(valid) | !valid
  if (any(invalid)) {
    stop(
      "Census 2001 dissolve produced ",
      sum(invalid),
      " invalid geometries after repair.",
      call. = FALSE
    )
  }
  out["unit_id"]
}

geometry_unit_coverage <- function(geometry_2001, admin_2001) {
  admin <- safe_df(admin_2001)
  expected <- unique(admin[c(
    "unit_id", "state_code", "district_code", "state_std", "district_std"
  )])
  observed <- if (inherits(geometry_2001, "sf") && nrow(geometry_2001)) {
    unique(plain_chr(geometry_2001$unit_id))
  } else {
    character()
  }

  expected$expected <- TRUE
  expected$observed <- expected$unit_id %in% observed
  expected$coverage_status <- ifelse(
    expected$observed, "present", "missing_geometry"
  )

  unexpected <- setdiff(observed, expected$unit_id)
  if (length(unexpected)) {
    expected <- safe_bind_rows(list(
      expected,
      data.frame(
        unit_id = unexpected,
        state_code = NA_character_,
        district_code = NA_character_,
        state_std = NA_character_,
        district_std = NA_character_,
        expected = FALSE,
        observed = TRUE,
        coverage_status = "unexpected_geometry",
        stringsAsFactors = FALSE
      )
    ))
  }
  expected
}

geometry_qa <- function(geometry_2001, admin_2001) {
  need_pkg("sf", "Census 2001 district geometry validation")
  coverage <- geometry_unit_coverage(geometry_2001, admin_2001)
  available <- inherits(geometry_2001, "sf") && nrow(geometry_2001) > 0L
  valid <- if (available) sf::st_is_valid(geometry_2001) else logical()

  data.frame(
    metric = c(
      "geometry_available", "geometry_rows", "expected_admin_units",
      "missing_admin_units", "unexpected_geometry_units", "invalid_geometries"
    ),
    value = c(
      available,
      if (available) nrow(geometry_2001) else 0L,
      sum(coverage$expected %in% TRUE),
      sum(coverage$coverage_status == "missing_geometry"),
      sum(coverage$coverage_status == "unexpected_geometry"),
      sum(is.na(valid) | !valid)
    ),
    stringsAsFactors = FALSE
  )
}

accepted_sensitivity_mapping_status <- function(
  conservative_eligibility, sensitivity_crosswalk
) {
  eligibility <- safe_df(conservative_eligibility)
  sensitivity <- safe_df(sensitivity_crosswalk)

  accepted_ids <- unique(stats::na.omit(
    eligibility$source_row_id[eligibility$status %in% "accepted"]
  ))
  mapped_ids <- unique(stats::na.omit(sensitivity$source_row_id))
  unmapped_ids <- setdiff(accepted_ids, mapped_ids)

  data.frame(
    n_accepted = length(accepted_ids),
    n_mapped = length(intersect(accepted_ids, mapped_ids)),
    n_unmapped = length(unmapped_ids),
    coverage_complete = length(accepted_ids) > 0L && !length(unmapped_ids),
    stringsAsFactors = FALSE
  )
}

lineage_completion_steps <- function(
  source_roster, source_matches, adjudication_queue, evidence_requests,
  allocation_validation, allocation_weights, conservative_crosswalk,
  primary_crosswalk, full_reviewed_crosswalk, geometry_qa,
  conservative_eligibility = data.frame()
) {
  roster <- safe_df(source_roster)
  matches <- safe_df(source_matches)
  queue <- safe_df(adjudication_queue)
  evidence <- safe_df(evidence_requests)
  conservative <- safe_df(conservative_crosswalk)
  primary <- safe_df(primary_crosswalk)
  full_reviewed <- safe_df(full_reviewed_crosswalk)
  geometry_qa <- safe_df(geometry_qa)

  resolved_ids <- unique(matches$source_row_id[matches$status %in% c("accepted", "excluded")])
  roster_ids <- unique(roster$source_row_id)
  fuzzy_open <- queue$review_class %in% c(
    "high_precision_fuzzy_candidate", "fuzzy_candidates", "no_candidate"
  ) & !(queue$adjudication_status %in% c("accepted", "excluded"))

  reviewed_validation <- validate_adjudicated_allocation_weights(allocation_weights)
  allocation_status <- allocation_coverage_status(
    allocation_validation, reviewed_validation,
    allocation_decision_status(allocation_weights)
  )
  mapping_status <- accepted_sensitivity_mapping_status(
    conservative_eligibility, full_reviewed
  )
  geometry_value <- function(metric, default = NA_real_) {
    value <- geometry_qa$value[geometry_qa$metric == metric]
    if (length(value)) value[[1]] else default
  }
  geometry_available <- isTRUE(as.logical(geometry_value("geometry_available", FALSE)))
  geometry_complete <- geometry_available && all(vapply(
    c("missing_admin_units", "unexpected_geometry_units", "invalid_geometries"),
    function(metric) isTRUE(as.numeric(geometry_value(metric, Inf)) == 0),
    logical(1)
  ))

  data.frame(
    step = seq_len(6L),
    work_item = c(
      "Adjudicate every NSS district identity",
      "Resolve open fuzzy identities and evidence requests",
      "Validate reviewed allocation weights",
      "Construct and validate Census 2001 geometry",
      "Build the conservative, primary, and full reviewed crosswalks",
      "Verify complete accepted-identity coverage"
    ),
    complete = c(
      length(roster_ids) > 0L && setequal(resolved_ids, roster_ids),
      !any(fuzzy_open) && nrow(evidence) == 0L,
      allocation_status$coverage_resolved[[1]],
      geometry_complete,
      nrow(conservative) > 0L && nrow(primary) >= nrow(conservative) &&
        nrow(full_reviewed) >= nrow(primary),
      mapping_status$coverage_complete[[1L]]
    ),
    observed = c(
      paste0(length(intersect(resolved_ids, roster_ids)), "/", length(roster_ids), " resolved"),
      paste0(sum(fuzzy_open), " fuzzy identities and ", nrow(evidence), " evidence requests open"),
      paste0(allocation_status$n_unresolved[[1]], " unresolved source-unit allocations"),
      if (geometry_available) paste0(
        geometry_value("geometry_rows", 0), "/",
        geometry_value("expected_admin_units", 0), " expected districts; ",
        geometry_value("missing_admin_units", 0), " missing; ",
        geometry_value("unexpected_geometry_units", 0), " unexpected; ",
        geometry_value("invalid_geometries", 0), " invalid"
      ) else "geometry not constructed",
      paste0(nrow(conservative), " conservative; ", nrow(primary), " primary; ", nrow(full_reviewed), " full reviewed rows"),
      paste0(mapping_status$n_mapped[[1L]], "/", mapping_status$n_accepted[[1L]], " accepted identities mapped")
    ),
    next_action = c(
      "Resolve any remaining rows in the adjudication ledger.",
      "Use registered official evidence to close the remaining review queue.",
      "Correct or explicitly reject any incomplete allocation.",
      "Inspect geometry_2001_unit_coverage.csv for missing, unexpected, or invalid units.",
      "Keep panel-role definitions monotone and evidence based.",
      "Record an explicit exclusion for any accepted identity that cannot be mapped."
    ),
    stringsAsFactors = FALSE
  )
}
