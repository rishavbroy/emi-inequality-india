# Completion workflow for district-lineage v2.
#
# This module converts diagnostic candidates into review-ready artifacts,
# assembles preferred and sensitivity crosswalks from accepted metadata, and
# reports the remaining methodological work. It never promotes a candidate to
# accepted status without a tracked adjudication.

empty_adjudication_draft_v2 <- function() {
  data.frame(
    source_row_id = character(), wave = character(), raw_state = character(),
    raw_district = character(), unit_id = character(), method = character(),
    source_id = character(), status = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

build_adjudication_draft_v2 <- function(source_roster, adjudication_queue, candidates) {
  roster <- safe_df(source_roster)
  queue <- safe_df(adjudication_queue)
  candidates <- safe_df(candidates)
  if (!nrow(roster)) return(empty_adjudication_draft_v2())

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
  if (!nrow(unresolved_queue)) return(empty_adjudication_draft_v2())

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

empty_sensitivity_crosswalk_v2 <- function() {
  data.frame(
    source_row_id = character(), wave = character(), source_code = character(),
    target_unit_2001 = character(), weight = numeric(), basis = character(),
    source_id = character(), panel_variant = character(),
    stringsAsFactors = FALSE
  )
}

build_sensitivity_crosswalk_v2 <- function(primary_crosswalk, allocation_weights) {
  primary <- safe_df(primary_crosswalk)
  allocations <- safe_df(allocation_weights)
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
  if (nrow(allocations) && any(accepted)) {
    a <- allocations[accepted, , drop = FALSE]
    pieces[[length(pieces) + 1L]] <- data.frame(
      source_row_id = a$source_unit,
      wave = NA_character_,
      source_code = NA_character_,
      target_unit_2001 = a$target_2001,
      weight = a$weight,
      basis = a$basis,
      source_id = a$source_id,
      panel_variant = "population_allocation",
      stringsAsFactors = FALSE
    )
  }

  out <- safe_bind_rows(pieces)
  if (!nrow(out)) return(empty_sensitivity_crosswalk_v2())
  if (any(!is.finite(out$weight) | out$weight < 0)) {
    stop("Sensitivity crosswalk weights must be finite and nonnegative.", call. = FALSE)
  }
  out
}

empty_production_comparison_v2 <- function() {
  data.frame(
    source_row_id = character(), wave = character(), source_code = character(),
    v2_target_unit_2001 = character(), production_target_unit_2001 = character(),
    comparison_status = character(), stringsAsFactors = FALSE
  )
}

build_production_crosswalk_comparison_v2 <- function(primary_crosswalk, production_panel) {
  x <- safe_df(primary_crosswalk)
  panel <- safe_df(production_panel)
  if (!nrow(x)) return(empty_production_comparison_v2())

  production_for_wave <- function(wave) {
    code_col <- if (identical(wave, "nss_2007_08")) {
      "district_code_0708"
    } else {
      "district_code_1718"
    }
    if (!all(c(code_col, "district_panel_id") %in% names(panel))) {
      return(data.frame(
        source_code = character(),
        production_target_unit_2001 = character(),
        production_mapping_count = integer(),
        stringsAsFactors = FALSE
      ))
    }
    raw <- unique(data.frame(
      source_code = plain_chr(panel[[code_col]]),
      production_target_unit_2001 =
        sub("^2001__", "pc2001__", plain_chr(panel$district_panel_id)),
      stringsAsFactors = FALSE
    ))
    raw <- raw[!is.na(raw$source_code) & nzchar(raw$source_code), , drop = FALSE]
    if (!nrow(raw)) {
      return(data.frame(
        source_code = character(),
        production_target_unit_2001 = character(),
        production_mapping_count = integer(),
        stringsAsFactors = FALSE
      ))
    }
    groups <- split(raw$production_target_unit_2001, raw$source_code)
    data.frame(
      source_code = names(groups),
      production_target_unit_2001 = vapply(
        groups,
        function(z) if (length(unique(z)) == 1L) unique(z) else NA_character_,
        character(1)
      ),
      production_mapping_count = vapply(
        groups, function(z) length(unique(z)), integer(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  groups <- split(seq_len(nrow(x)), x$wave)
  out <- safe_bind_rows(lapply(names(groups), function(wave) {
    rows <- x[groups[[wave]], , drop = FALSE]
    current <- production_for_wave(wave)
    rows <- merge(rows, current, by = "source_code", all.x = TRUE, sort = FALSE)
    rows$production_mapping_count[
      is.na(rows$production_mapping_count)
    ] <- 0L
    data.frame(
      source_row_id = rows$source_row_id,
      wave = rows$wave,
      source_code = rows$source_code,
      v2_target_unit_2001 = rows$target_unit_2001,
      production_target_unit_2001 = rows$production_target_unit_2001,
      comparison_status = ifelse(
        rows$production_mapping_count > 1L,
        "ambiguous_production_mapping",
        ifelse(
          is.na(rows$production_target_unit_2001),
          "missing_from_production_panel",
          ifelse(
            rows$target_unit_2001 == rows$production_target_unit_2001,
            "same_target",
            "changed_target"
          )
        )
      ),
      stringsAsFactors = FALSE
    )
  }))
  out
}

read_zipped_gpkg_v2 <- function(path) {
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

save_lineage_geometry_2001_v2 <- function(
  geometry_2001, admin_2001,
  path = "outputs/derived/district_lineage_v2/district_2001.gpkg"
) {
  need_pkg("sf", "Census 2001 district geometry output")
  if (!inherits(geometry_2001, "sf") || !nrow(geometry_2001)) {
    stop("Census 2001 geometry is empty or not an sf object.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) unlink(path)
  sf::st_write(geometry_2001, path, quiet = TRUE)
  qa <- geometry_qa_v2(geometry_2001, admin_2001)
  qa_path <- file.path(dirname(path), "district_2001_qa.csv")
  utils::write.csv(qa, qa_path, row.names = FALSE, na = "")
  c(path, qa_path)
}

make_valid_sf_v2 <- function(x) {
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

dissolve_shrid_geometry_2001_v2 <- function(shrid_geometry, bridge) {
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
  joined <- make_valid_sf_v2(joined)
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
  out <- make_valid_sf_v2(out)
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

geometry_qa_v2 <- function(geometry_2001, admin_2001) {
  need_pkg("sf", "Census 2001 district geometry validation")
  if (!inherits(geometry_2001, "sf")) {
    return(data.frame(
      metric = "geometry_available", value = FALSE, stringsAsFactors = FALSE
    ))
  }
  expected <- unique(plain_chr(safe_df(admin_2001)$unit_id))
  observed <- unique(plain_chr(geometry_2001$unit_id))
  valid <- sf::st_is_valid(geometry_2001)
  data.frame(
    metric = c(
      "geometry_available", "geometry_rows", "expected_admin_units",
      "missing_admin_units", "unexpected_geometry_units", "invalid_geometries"
    ),
    value = c(
      TRUE, nrow(geometry_2001), length(expected),
      length(setdiff(expected, observed)), length(setdiff(observed, expected)),
      sum(!valid)
    ),
    stringsAsFactors = FALSE
  )
}

lineage_completion_steps_v2 <- function(
  source_roster, source_matches, adjudication_queue, evidence_requests,
  allocation_validation, allocation_weights, primary_crosswalk,
  sensitivity_crosswalk, production_comparison, geometry_qa, readiness
) {
  roster <- safe_df(source_roster)
  matches <- safe_df(source_matches)
  queue <- safe_df(adjudication_queue)
  evidence <- safe_df(evidence_requests)
  allocation_validation <- safe_df(allocation_validation)
  allocation_weights <- safe_df(allocation_weights)
  primary <- safe_df(primary_crosswalk)
  sensitivity <- safe_df(sensitivity_crosswalk)
  comparison <- safe_df(production_comparison)
  geometry_qa <- safe_df(geometry_qa)
  readiness <- safe_df(readiness)

  resolved_ids <- unique(matches$source_row_id[
    matches$status %in% c("accepted", "excluded")
  ])
  roster_ids <- unique(roster$source_row_id)
  fuzzy_class <- queue$review_class %in% c(
    "high_precision_fuzzy_candidate", "fuzzy_candidates", "no_candidate"
  )
  fuzzy_open <- fuzzy_class &
    !(queue$adjudication_status %in% c("accepted", "excluded"))
  incomplete_source_keys <- allocation_validation$source_key[
    !(allocation_validation$coverage_complete %in% TRUE)
  ]
  accepted_allocation_keys <- unique(allocation_weights$source_unit[
    allocation_weights$status %in% "accepted"
  ])
  allocation_gaps_resolved <- nrow(allocation_validation) > 0L &&
    (
      !length(incomplete_source_keys) ||
        !length(setdiff(incomplete_source_keys, accepted_allocation_keys))
    )
  geometry_value <- function(metric, default = NA_real_) {
    value <- geometry_qa$value[geometry_qa$metric == metric]
    if (length(value)) value[[1]] else default
  }
  geometry_available <- isTRUE(as.logical(geometry_value(
    "geometry_available", FALSE
  )))
  geometry_complete <- geometry_available &&
    all(vapply(
      c("missing_admin_units", "unexpected_geometry_units", "invalid_geometries"),
      function(metric) isTRUE(as.numeric(geometry_value(metric, Inf)) == 0),
      logical(1)
    ))
  geometry_observed <- if (!geometry_available) {
    "not constructed from local SHRID polygons"
  } else {
    paste0(
      geometry_value("geometry_rows", 0), "/",
      geometry_value("expected_admin_units", 0), " expected districts; ",
      geometry_value("missing_admin_units", 0), " missing; ",
      geometry_value("unexpected_geometry_units", 0), " unexpected; ",
      geometry_value("invalid_geometries", 0), " invalid"
    )
  }
  migration_ready <- any(
    readiness$gate == "production_crosswalk_migration_ready" &
      readiness$passed %in% TRUE
  )

  data.frame(
    step = seq_len(9L),
    work_item = c(
      "Review deterministic identities and populate source adjudications",
      "Resolve fuzzy source identities",
      "Review targeted administrative-event evidence",
      "Resolve incomplete SHRID coverage and sensitivity allocations",
      "Construct and validate Census 2001 geometry",
      "Build preferred and sensitivity source crosswalks",
      "Compare v2 mappings with the production panel",
      "Review changed observations and estimates",
      "Migrate the production crosswalk deliberately"
    ),
    complete = c(
      length(roster_ids) > 0L && setequal(resolved_ids, roster_ids),
      !any(fuzzy_open & !(queue$adjudication_status %in% c("accepted", "excluded"))),
      nrow(evidence) == 0L,
      allocation_gaps_resolved,
      geometry_complete,
      nrow(primary) > 0L && nrow(sensitivity) >= nrow(primary),
      nrow(primary) > 0L && nrow(comparison) == nrow(primary),
      nrow(comparison) > 0L &&
        all(comparison$comparison_status %in% "same_target"),
      migration_ready
    ),
    observed = c(
      paste0(length(intersect(resolved_ids, roster_ids)), "/", length(roster_ids), " resolved"),
      paste0(sum(fuzzy_open), " fuzzy or missing candidates open"),
      paste0(nrow(evidence), " targeted evidence requests"),
      paste0(sum(!allocation_validation$coverage_complete), " incomplete source allocations"),
      geometry_observed,
      paste0(nrow(primary), " preferred; ", nrow(sensitivity), " total sensitivity rows"),
      paste0(nrow(comparison), " source mappings compared"),
      paste0(
        sum(comparison$comparison_status != "same_target"),
        " changed, missing, or ambiguous mappings require review"
      ),
      if (migration_ready) "all gates pass" else "migration gates remain blocked"
    ),
    next_action = c(
      "Review adjudication_draft.csv and copy verified decisions into district_adjudications_v2.csv.",
      "Use official rename or boundary evidence; accept, exclude, or retain needs_review.",
      "Record accepted or rejected edges in district_admin_events_v2.csv with registered source IDs.",
      "Investigate unmapped mass and enter reviewed allocations in district_allocation_weights_v2.csv.",
      "Load local SHRID polygons, dissolve with dissolve_shrid_geometry_2001_v2(), and save a derived GeoPackage.",
      "Regenerate the diagnostic after accepted source decisions and allocation weights are tracked.",
      "Inspect production_crosswalk_comparison.csv after preferred mappings exist.",
      "Rebuild measures and models only after mapping comparisons are complete.",
      "Replace the inherited crosswalk only after every migration gate passes and changes are reviewed."
    ),
    stringsAsFactors = FALSE
  )
}
