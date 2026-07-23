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

  top <- candidates[
    candidates$candidate_unit %in% queue$recommended_unit &
      candidates$source_row_id %in% queue$source_row_id,
    c("source_row_id", "candidate_unit", "candidate_source_id"),
    drop = FALSE
  ]
  top <- top[!duplicated(top$source_row_id), , drop = FALSE]
  names(top) <- c("source_row_id", "unit_id", "source_id")

  out <- merge(
    roster[c("source_row_id", "wave", "raw_state", "raw_district")],
    queue[c(
      "source_row_id", "recommended_unit", "recommended_method",
      "review_class", "recommended_vintage"
    )],
    by = "source_row_id", all.x = TRUE, sort = FALSE
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
      return(data.frame(source_code = character(), production_target_unit_2001 = character()))
    }
    out <- unique(data.frame(
      source_code = plain_chr(panel[[code_col]]),
      production_target_unit_2001 = sub("^2001__", "pc2001__", plain_chr(panel$district_panel_id)),
      stringsAsFactors = FALSE
    ))
    out[!is.na(out$source_code) & nzchar(out$source_code), , drop = FALSE]
  }

  groups <- split(seq_len(nrow(x)), x$wave)
  out <- safe_bind_rows(lapply(names(groups), function(wave) {
    rows <- x[groups[[wave]], , drop = FALSE]
    current <- production_for_wave(wave)
    rows <- merge(rows, current, by = "source_code", all.x = TRUE, sort = FALSE)
    data.frame(
      source_row_id = rows$source_row_id,
      wave = rows$wave,
      source_code = rows$source_code,
      v2_target_unit_2001 = rows$target_unit_2001,
      production_target_unit_2001 = rows$production_target_unit_2001,
      comparison_status = ifelse(
        is.na(rows$production_target_unit_2001),
        "missing_from_production_panel",
        ifelse(
          rows$target_unit_2001 == rows$production_target_unit_2001,
          "same_target",
          "changed_target"
        )
      ),
      stringsAsFactors = FALSE
    )
  }))
  out
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
  joined$unit_id <- paste0(
    "pc2001__", joined$state_code_2001, "__", joined$district_code_2001
  )
  aggregate(
    joined["geometry"],
    by = list(unit_id = joined$unit_id),
    FUN = length,
    do_union = TRUE
  )["unit_id"]
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

  resolved <- matches$status %in% c("accepted", "excluded")
  fuzzy_open <- queue$review_class %in% c(
    "high_precision_fuzzy_candidate", "fuzzy_candidates", "no_candidate"
  )
  coverage_complete <- nrow(allocation_validation) > 0L &&
    all(allocation_validation$coverage_complete)
  geometry_complete <- nrow(geometry_qa) > 0L &&
    any(geometry_qa$metric == "geometry_available" & geometry_qa$value %in% TRUE) &&
    all(geometry_qa$value[geometry_qa$metric %in% c(
      "missing_admin_units", "unexpected_geometry_units", "invalid_geometries"
    )] == 0)
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
      nrow(roster) > 0L && length(unique(matches$source_row_id[resolved])) == nrow(roster),
      !any(fuzzy_open & !(queue$adjudication_status %in% c("accepted", "excluded"))),
      nrow(evidence) == 0L,
      coverage_complete || any(allocation_weights$status %in% "accepted"),
      geometry_complete,
      nrow(primary) > 0L && nrow(sensitivity) >= nrow(primary),
      nrow(primary) > 0L && nrow(comparison) == nrow(primary),
      nrow(comparison) > 0L && !any(comparison$comparison_status == "changed_target"),
      migration_ready
    ),
    observed = c(
      paste0(length(unique(matches$source_row_id[resolved])), "/", nrow(roster), " resolved"),
      paste0(sum(fuzzy_open), " fuzzy or missing candidates open"),
      paste0(nrow(evidence), " targeted evidence requests"),
      paste0(sum(!allocation_validation$coverage_complete), " incomplete source allocations"),
      if (geometry_complete) "complete" else "not constructed from local SHRID polygons",
      paste0(nrow(primary), " preferred; ", nrow(sensitivity), " total sensitivity rows"),
      paste0(nrow(comparison), " source mappings compared"),
      paste0(sum(comparison$comparison_status == "changed_target"), " changed targets available for review"),
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
