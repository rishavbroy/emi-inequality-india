# This file is part of the EMI inequality research pipeline.
# It builds the reviewed district lineage panel used by legacy and diagnostics.

lineage_source_code <- function(x) {
  raw <- gsub("[^0-9]", "", plain_chr(x))
  raw[nchar(raw) == 0L] <- NA_character_
  suppressWarnings(as.character(as.integer(raw)))
}

lineage_target_codes <- function(unit_id) {
  parts <- strsplit(plain_chr(unit_id), "__", fixed = TRUE)
  data.frame(
    state_code_2001 = vapply(
      parts, function(x) if (length(x) >= 3L) x[[2L]] else NA_character_,
      character(1)
    ),
    district_code_2001 = vapply(
      parts, function(x) if (length(x) >= 3L) x[[3L]] else NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

lineage_wave_measure_spec <- function(wave) {
  if (identical(wave, "nss_2007_08")) {
    return(list(
      code_col = "district_code_0708",
      count_cols = c("npeople_0708", "nhouses_0708", "n"),
      weight_col = "nhouses_0708"
    ))
  }
  if (identical(wave, "nss_2017_18")) {
    return(list(
      code_col = "district_code_1718",
      count_cols = c("npeople_1718", "nhouses_1718", "n"),
      weight_col = "nhouses_1718"
    ))
  }
  stop("Unsupported district lineage wave: ", wave, call. = FALSE)
}

first_nonmissing <- function(x) {
  keep <- !is.na(x)
  if (is.character(x)) keep <- keep & nzchar(x)
  hit <- which(keep)
  if (length(hit)) x[[hit[[1L]]]] else x[[1L]][NA_integer_]
}

weighted_mean <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

collapse_lineage_measure_rows <- function(mapped, spec) {
  mapped <- safe_df(mapped)
  if (!nrow(mapped)) return(data.frame())
  if (!"weight" %in% names(mapped)) mapped$weight <- 1
  if (!"panel_variant" %in% names(mapped)) {
    mapped$panel_variant <- "deterministic"
  }

  raw_households <- if (spec$weight_col %in% names(mapped)) {
    suppressWarnings(as.numeric(mapped[[spec$weight_col]]))
  } else {
    rep(1, nrow(mapped))
  }
  mapped$.aggregation_mass <- raw_households * mapped$weight
  for (nm in intersect(spec$count_cols, names(mapped))) {
    if (is.numeric(mapped[[nm]])) {
      mapped[[nm]] <- mapped[[nm]] * mapped$weight
    }
  }

  groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
  safe_bind_rows(lapply(groups, function(i) {
    rows <- mapped[i, , drop = FALSE]
    out <- rows[1L, c("target_unit_2001"), drop = FALSE]
    source_ids <- if ("source_row_id" %in% names(rows)) {
      unique(rows$source_row_id[
        !is.na(rows$source_row_id) & nzchar(rows$source_row_id)
      ])
    } else {
      character()
    }
    out$lineage_source_count <- if (length(source_ids)) {
      length(source_ids)
    } else {
      nrow(rows)
    }
    allocated <- any(
      rows$panel_variant %in% "population_allocation" |
        abs(rows$weight - 1) > 1e-8
    )
    out$lineage_aggregation_status <- if (
      nrow(rows) == 1L && !allocated
    ) {
      "one_to_one"
    } else if (allocated && out$lineage_source_count == 1L) {
      "source_split_population_allocated"
    } else if (allocated) {
      "district_aggregate_population_weighted"
    } else {
      "district_aggregate_weighted"
    }

    value_cols <- setdiff(
      names(rows),
      c(
        "target_unit_2001", "source_code", "source_row_id", "wave",
        "mapping_class", "basis", "source_id", "panel_variant",
        "weight", ".aggregation_mass", "lineage_source_count",
        "lineage_aggregation_status"
      )
    )
    for (nm in value_cols) {
      x <- rows[[nm]]
      if (nm %in% spec$count_cols && is.numeric(x)) {
        out[[nm]] <- sum(x, na.rm = TRUE)
      } else if (is.numeric(x)) {
        out[[nm]] <- weighted_mean(x, rows$.aggregation_mass)
      } else {
        out[[nm]] <- first_nonmissing(x)
      }
    }
    out
  }))
}

map_lineage_measures <- function(measures, crosswalk, wave) {
  measures <- safe_df(measures)
  crosswalk <- safe_df(crosswalk)
  spec <- lineage_wave_measure_spec(wave)
  if (!nrow(measures) || !nrow(crosswalk) || !spec$code_col %in% names(measures)) {
    return(data.frame())
  }

  map <- crosswalk[
    crosswalk$wave %in% wave,
    intersect(
      c(
        "source_row_id", "wave", "source_code", "target_unit_2001",
        "mapping_class", "weight", "basis", "source_id", "panel_variant"
      ),
      names(crosswalk)
    ),
    drop = FALSE
  ]
  if (!nrow(map)) return(data.frame())
  if (!"weight" %in% names(map)) map$weight <- 1
  if (!"panel_variant" %in% names(map)) {
    map$panel_variant <- "deterministic"
  }
  map$source_code_key <- lineage_source_code(map$source_code)
  measures$source_code_key <- lineage_source_code(measures[[spec$code_col]])

  source_weight <- aggregate(
    map$weight,
    list(source_row_id = map$source_row_id),
    sum
  )
  if (any(abs(source_weight$x - 1) > 1e-8)) {
    stop(
      "District lineage crosswalk weights must sum to one within source row.",
      call. = FALSE
    )
  }
  source_code_rows <- unique(map[c("source_row_id", "source_code_key")])
  if (anyDuplicated(
    source_code_rows$source_code_key[
      !is.na(source_code_rows$source_code_key)
    ]
  )) {
    stop(
      "Each wave-specific source code must identify one NSS source row.",
      call. = FALSE
    )
  }

  mapped <- merge(
    map,
    measures,
    by = "source_code_key",
    all.x = TRUE,
    sort = FALSE
  )
  mapped$source_code_key <- NULL
  collapse_lineage_measure_rows(mapped, spec)
}

lineage_household_consumption <- function(inputs, wave) {
  inputs <- as_input_list(inputs)
  if (identical(wave, "nss_2007_08")) {
    df <- standardize_nss_2007_district_code(std(safe_df(
      select_input_frame(inputs, c("nss0708edu_block3", "block3"))
    ), 2007L))
    code_col <- "district_code_0708"
    value_col <- first_col(df, c("MPCE", "mpce", "consumption_pc", "consumption_per_capita"))
    total_col <- first_col(df, c("TOTAL", "total", "HH_Con_exp_rs", "consumption"))
    size_col <- first_col(df, c("HH_SIZE", "HH_Size", "household_size"))
    weight_col <- first_col(df, c("weight", "WEIGHT", "Multiplier", "multiplier"))
    household_col <- first_col(df, c("HHID", "HH_ID", "household_id"))
    value <- if (!is.null(value_col)) {
      num(df[[value_col]])
    } else if (!is.null(total_col) && !is.null(size_col)) {
      num(df[[total_col]]) / num(df[[size_col]])
    } else {
      numeric()
    }
  } else if (identical(wave, "nss_2017_18")) {
    df <- normalize_2017_district_code(std(safe_df(
      select_input_frame_2017(inputs, c("nss1718edu_block3", "block3", "block"))
    ), 2017L))
    code_col <- "district_code_1718"
    total_col <- first_col(df, c("HH_Con_exp_rs", "MPCE", "mpce", "consumption", "hh_cons"))
    size_col <- first_col(df, c("Household_size", "HH_SIZE", "household_size"))
    weight_col <- first_col(df, c("MULT_Combined", "weight", "WEIGHT", "multiplier"))
    household_col <- first_col(df, c("HHID", "HH_ID", "household_id"))
    value <- if (!is.null(total_col) && !is.null(size_col)) {
      num(df[[total_col]]) / num(df[[size_col]])
    } else if (!is.null(total_col)) {
      num(df[[total_col]])
    } else {
      numeric()
    }
  } else {
    stop("Unsupported district lineage wave: ", wave, call. = FALSE)
  }

  if (!nrow(df) || !code_col %in% names(df) || is.null(weight_col) ||
      length(value) != nrow(df)) {
    return(data.frame())
  }
  out <- data.frame(
    source_code_key = lineage_source_code(df[[code_col]]),
    consumption = value,
    household_size = if (!is.null(size_col)) num(df[[size_col]]) else NA_real_,
    survey_weight = num(df[[weight_col]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(household_col)) {
    out$household_key <- paste(
      out$source_code_key, canon(df[[household_col]]), sep = "__"
    )
    out <- collapse_identical_key_rows(
      out, "household_key", context = paste(wave, "lineage household consumption")
    )
  }
  out[
    !is.na(out$source_code_key) & is.finite(out$consumption) &
      is.finite(out$household_size) & out$household_size > 0 &
      is.finite(out$survey_weight) & out$survey_weight > 0,
    , drop = FALSE
  ]
}

reconstruct_lineage_pooled_ginis <- function(
  panel, crosswalk, nss_2007_education, nss_2017_education
) {
  if (!is.data.frame(panel)) panel <- safe_df(panel)
  crosswalk <- safe_df(crosswalk)
  audit <- list()

  for (wave in c("nss_2007_08", "nss_2017_18")) {
    gini_col <- if (wave == "nss_2007_08") "gini_cons_0708" else "gini_cons_1718"
    status_col <- paste0(gini_col, "_reconstruction_status")
    panel[[status_col]] <- "not_required"
    map <- crosswalk[crosswalk$wave %in% wave, , drop = FALSE]
    if (!nrow(map)) next
    if (!"weight" %in% names(map)) map$weight <- 1
    map$source_code_key <- lineage_source_code(map$source_code)
    source_targets <- aggregate(
      map$target_unit_2001,
      list(source_row_id = map$source_row_id),
      function(x) length(unique(x))
    )
    names(source_targets)[[2L]] <- "n_targets"
    whole <- merge(map, source_targets, by = "source_row_id", all.x = TRUE, sort = FALSE)
    whole <- whole[
      is.finite(num(whole$weight)) & abs(num(whole$weight) - 1) <= 1e-8 &
        whole$n_targets == 1L,
      , drop = FALSE
    ]
    target_sources <- aggregate(
      whole$source_row_id,
      list(target_unit_2001 = whole$target_unit_2001),
      function(x) length(unique(x))
    )
    names(target_sources)[[2L]] <- "source_count"
    targets <- target_sources$target_unit_2001[target_sources$source_count > 1L]
    if (!length(targets)) next

    households <- lineage_household_consumption(
      if (wave == "nss_2007_08") nss_2007_education else nss_2017_education,
      wave
    )
    mapped <- merge(
      whole[whole$target_unit_2001 %in% targets,
            c("source_code_key", "target_unit_2001", "source_row_id"), drop = FALSE],
      households,
      by = "source_code_key", all.x = TRUE, sort = FALSE
    )
    groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
    rows <- safe_bind_rows(lapply(names(groups), function(target) {
      x <- mapped[groups[[target]], , drop = FALSE]
      valid <- is.finite(x$consumption) & is.finite(x$household_size) & x$household_size > 0 &
        is.finite(x$survey_weight) & x$survey_weight > 0
      observed_sources <- unique(x$source_row_id[valid])
      expected_sources <- unique(whole$source_row_id[whole$target_unit_2001 == target])
      complete <- length(observed_sources) == length(expected_sources) &&
        setequal(observed_sources, expected_sources)
      data.frame(
        target_unit_2001 = target,
        wave = wave,
        source_count = length(expected_sources),
        household_count = sum(valid),
        pooled_gini = if (complete && any(valid)) {
          wgini(
            x$consumption[valid],
            x$survey_weight[valid] * x$household_size[valid]
          )
        } else {
          NA_real_
        },
        status = if (complete && any(valid)) "reconstructed" else "missing_household_source",
        stringsAsFactors = FALSE
      )
    }))
    audit[[wave]] <- rows
    hit <- match(panel$target_unit_2001, rows$target_unit_2001)
    matched <- which(!is.na(hit))
    reconstructed <- matched[rows$status[hit[matched]] %in% "reconstructed"]
    panel[[gini_col]][reconstructed] <- rows$pooled_gini[hit[reconstructed]]
    panel[[status_col]][matched] <- rows$status[hit[matched]]
  }

  list(panel = panel, audit = safe_bind_rows(audit))
}

attach_lineage_instrument <- function(panel, linguistic_distance_iv) {
  panel <- safe_df(panel)
  iv <- safe_df(linguistic_distance_iv)
  if (!nrow(panel) || !nrow(iv)) return(panel)

  codes <- lineage_target_codes(panel$target_unit_2001)
  panel$state_code_2001 <- codes$state_code_2001
  panel$district_code_2001 <- codes$district_code_2001

  state_col <- first_col(iv, c("state_code", "state"))
  district_col <- first_col(iv, c("district_code", "district"))
  if (is.null(state_col) || is.null(district_col)) return(panel)

  iv$state_code_2001 <- sprintf("%02d", as.integer(num(iv[[state_col]])))
  iv$district_code_2001 <- sprintf("%02d", as.integer(num(iv[[district_col]])))
  duplicate <- intersect(
    setdiff(names(iv), c("state_code_2001", "district_code_2001")),
    names(panel)
  )
  iv <- iv[setdiff(names(iv), duplicate)]
  merge(
    panel,
    iv,
    by = c("state_code_2001", "district_code_2001"),
    all.x = TRUE,
    sort = FALSE
  )
}

attach_lineage_geometry <- function(panel, geometry_2001) {
  panel <- safe_df(panel)
  if (!inherits(geometry_2001, "sf") || !nrow(geometry_2001) ||
      !"unit_id" %in% names(geometry_2001)) {
    return(panel)
  }

  unit_id <- plain_chr(geometry_2001$unit_id)
  if (anyDuplicated(unit_id)) {
    stop("Census-2001 geometry must be unique by unit_id.", call. = FALSE)
  }
  if (!"target_unit_2001" %in% names(panel)) {
    stop("Lineage panel is missing target_unit_2001.", call. = FALSE)
  }

  match_index <- match(plain_chr(panel$target_unit_2001), unit_id)
  source_geometry <- sf::st_geometry(geometry_2001)
  panel_geometry <- sf::st_sfc(
    lapply(match_index, function(i) {
      if (is.na(i)) sf::st_geometrycollection() else source_geometry[[i]]
    }),
    crs = sf::st_crs(geometry_2001)
  )
  sf::st_sf(panel, geometry = panel_geometry)
}

build_lineage_district_panel <- function(
  primary_crosswalk, measures_2007, measures_2017,
  linguistic_distance_iv, geometry_2001 = data.frame(), cfg = list()
) {
  m07 <- map_lineage_measures(
    measures_2007, primary_crosswalk, "nss_2007_08"
  )
  m17 <- map_lineage_measures(
    measures_2017, primary_crosswalk, "nss_2017_18"
  )
  if (!nrow(m07) || !nrow(m17)) return(empty_panel())

  duplicate <- intersect(
    setdiff(names(m17), c("target_unit_2001")),
    names(m07)
  )
  m17 <- m17[setdiff(names(m17), duplicate)]
  panel <- merge(m07, m17, by = "target_unit_2001", all = FALSE, sort = FALSE)
  panel <- attach_lineage_instrument(panel, linguistic_distance_iv)
  panel$district_panel_id <- sub(
    "^pc2001__", "2001__", panel$target_unit_2001
  )
  panel <- add_panel_standardized_names(panel)
  panel <- add_panel_regions(panel)
  panel <- compute_consumption_pct_change(panel)
  panel <- compute_log_consumption_difference(panel)
  panel <- compute_real_consumption_outcomes(panel)
  panel <- compute_gini_change(panel)
  panel <- panel[panel_has_analysis_core(panel), , drop = FALSE]
  rownames(panel) <- NULL
  validate_analysis_district_panel(
    attach_lineage_geometry(panel, geometry_2001),
    cfg,
    strict = FALSE
  )
}

lineage_panel_unit_id <- function(panel) {
  panel <- safe_df(panel)
  if ("target_unit_2001" %in% names(panel)) {
    return(plain_chr(panel$target_unit_2001))
  }
  if ("district_panel_id" %in% names(panel)) {
    return(sub("^2001__", "pc2001__", plain_chr(panel$district_panel_id)))
  }
  rep(NA_character_, nrow(panel))
}

compare_lineage_panels <- function(legacy_panel, lineage_panel) {
  legacy <- safe_df(legacy_panel)
  candidate <- safe_df(lineage_panel)
  legacy$target_unit_2001 <- lineage_panel_unit_id(legacy)
  candidate$target_unit_2001 <- lineage_panel_unit_id(candidate)

  legacy_units <- unique(stats::na.omit(legacy$target_unit_2001))
  candidate_units <- unique(stats::na.omit(candidate$target_unit_2001))
  units <- sort(unique(c(legacy_units, candidate_units)))
  data.frame(
    target_unit_2001 = units,
    in_legacy = units %in% legacy_units,
    in_lineage = units %in% candidate_units,
    comparison_status = ifelse(
      units %in% legacy_units & units %in% candidate_units,
      "shared",
      ifelse(units %in% candidate_units, "lineage_only", "legacy_only")
    ),
    stringsAsFactors = FALSE
  )
}



lineage_admin_2001_labels <- function(admin_2001) {
  admin <- safe_df(admin_2001)
  required <- c("unit_id", "state_std", "district_std")
  if (!nrow(admin) || !all(required %in% names(admin))) {
    return(data.frame(
      target_unit_2001 = character(),
      state_label_2001 = character(),
      district_label_2001 = character(),
      label_source_id = character(),
      stringsAsFactors = FALSE
    ))
  }

  admin <- admin[
    is.na(admin$level) | admin$level %in% "district",
    ,
    drop = FALSE
  ]
  out <- data.frame(
    target_unit_2001 = plain_chr(admin$unit_id),
    state_label_2001 = plain_chr(admin$state_std),
    district_label_2001 = plain_chr(admin$district_std),
    label_source_id = if ("source_id" %in% names(admin)) {
      plain_chr(admin$source_id)
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
  out <- out[
    !is.na(out$target_unit_2001) & nzchar(out$target_unit_2001),
    ,
    drop = FALSE
  ]
  if (anyDuplicated(out$target_unit_2001)) {
    stop(
      "Canonical Census-2001 district labels must be unique by unit_id.",
      call. = FALSE
    )
  }
  out
}

build_lineage_nonoverlap_queue <- function(
  panel_membership, admin_2001
) {
  membership <- safe_df(panel_membership)
  membership <- membership[
    membership$comparison_status %in% c("legacy_only", "lineage_only"),
    ,
    drop = FALSE
  ]
  if (!nrow(membership)) return(data.frame())

  labels <- lineage_admin_2001_labels(admin_2001)
  out <- merge(
    membership,
    labels,
    by = "target_unit_2001",
    all.x = TRUE,
    sort = FALSE
  )
  codes <- lineage_target_codes(out$target_unit_2001)
  out$state_code_2001 <- codes$state_code_2001
  out$district_code_2001 <- codes$district_code_2001
  out$canonical_label_available <-
    !is.na(out$district_label_2001) & nzchar(out$district_label_2001)
  out$review_scope <- ifelse(
    out$comparison_status == "legacy_only",
    "inherited_panel_only",
    "lineage_panel_only"
  )
  out$next_action <- ifelse(
    !out$canonical_label_available,
    paste0(
      "Resolve the canonical Census-2001 registry label before reviewing ",
      "panel membership."
    ),
    ifelse(
      out$comparison_status == "legacy_only",
      paste0(
        "Check whether the inherited row is a duplicate, obsolete target, ",
        "or a district missing from the reviewed lineage bridge."
      ),
      paste0(
        "Confirm that the recovered lineage unit has valid two-wave measures and ",
        "is not an allocation artifact."
      )
    )
  )
  out[
    order(
      out$comparison_status,
      out$state_code_2001,
      out$district_code_2001
    ),
    ,
    drop = FALSE
  ]
}

build_lineage_unmapped_identity_queue <- function(
  conservative_eligibility, full_reviewed_crosswalk
) {
  eligibility <- safe_df(conservative_eligibility)
  crosswalk <- safe_df(full_reviewed_crosswalk)
  if (!nrow(eligibility)) return(data.frame())

  mapped_ids <- unique(stats::na.omit(crosswalk$source_row_id))
  queue <- eligibility[
    eligibility$status %in% "accepted" &
      !(eligibility$source_row_id %in% mapped_ids),
    ,
    drop = FALSE
  ]
  if (!nrow(queue)) return(data.frame())

  keep <- intersect(
    c(
      "source_row_id", "wave", "source_code",
      "raw_state", "raw_district", "state_std", "district_std",
      "terminal_unit", "terminal_vintage", "resolution_status",
      "lineage_path", "mapping_class", "exclusion_reason"
    ),
    names(queue)
  )
  queue <- queue[keep]
  queue$review_scope <- "accepted_identity_without_full_reviewed_mapping"
  queue$next_action <- paste0(
    "Trace the accepted terminal district through reviewed LGD, SHRUG, ",
    "tracker, and manual district-history evidence to Census 2001."
  )
  queue[
    order(queue$wave, queue$state_std, queue$district_std),
    ,
    drop = FALSE
  ]
}



build_lineage_panel_membership_adjudication <- function(
  panel_membership, legacy_duplicates = data.frame(),
  identity_coverage_complete = FALSE
) {
  membership <- safe_df(panel_membership)
  duplicates <- safe_df(legacy_duplicates)
  if (!nrow(membership)) return(data.frame())

  duplicate_units <- unique(plain_chr(
    duplicates$target_unit_2001 %||% character()
  ))
  out <- membership
  ready <- isTRUE(identity_coverage_complete)
  out$decision <- if (!ready) {
    "defer_until_identity_coverage_complete"
  } else ifelse(
    out$target_unit_2001 %in% duplicate_units & out$in_lineage %in% TRUE,
    "replace_inherited_duplicate_with_unique",
    ifelse(
      out$comparison_status == "shared",
      "retain_shared_support",
      ifelse(
        out$comparison_status == "lineage_only",
        "accept_lineage_coverage_addition",
        ifelse(
          out$target_unit_2001 %in% duplicate_units,
          "exclude_inherited_duplicate",
          "exclude_inherited_only_support"
        )
      )
    )
  )
  out$status <- if (!ready) {
    "needs_review"
  } else ifelse(
    out$comparison_status %in% c("shared", "lineage_only"),
    "accepted",
    "excluded"
  )
  out$method <- if (!ready) {
    "coverage_prerequisite"
  } else {
    "panel_support_invariant"
  }
  out$note <- if (!ready) {
    paste0(
      "Panel membership is not adjudicated until every accepted identity is ",
      "mapped or explicitly excluded."
    )
  } else ifelse(
    out$target_unit_2001 %in% duplicate_units & out$in_lineage %in% TRUE,
    paste0(
      "Retained once through the unique district lineage row, replacing duplicated ",
      "support in the inherited legacy panel."
    ),
    ifelse(
      out$comparison_status == "shared",
      "Retained because the canonical Census-2001 unit is present in both panels.",
      ifelse(
        out$comparison_status == "lineage_only",
        paste0(
          "Accepted as a district lineage coverage addition because the canonical ",
          "Census-2001 unit has a unique rebuilt row and no inherited counterpart."
        ),
        ifelse(
          out$target_unit_2001 %in% duplicate_units,
          paste0(
            "Excluded from review because inherited support is duplicated and ",
            "the rebuilt district lineage panel does not retain this inherited-only row."
          ),
          paste0(
            "Excluded from review because the unit is supported only by the ",
            "inherited panel after accepted-identity coverage is complete."
          )
        )
      )
    )
  )
  out[
    order(out$status, out$comparison_status, out$target_unit_2001),
    ,
    drop = FALSE
  ]
}

build_lineage_unmapped_terminal_queue <- function(
  identity_queue, allocation_weights = data.frame()
) {
  identities <- safe_df(identity_queue)
  allocations <- safe_df(allocation_weights)
  if (!nrow(identities)) return(data.frame())

  for (nm in setdiff(
    c(
      "source_row_id", "source_code", "wave", "terminal_unit",
      "state_std", "district_std"
    ),
    names(identities)
  )) {
    identities[[nm]] <- rep(NA_character_, nrow(identities))
  }
  identities <- identities[
    !is.na(identities$terminal_unit) &
      nzchar(identities$terminal_unit),
    ,
    drop = FALSE
  ]
  if (!nrow(identities)) return(data.frame())

  for (nm in setdiff(
    c(
      "source_unit", "target_2001", "weight", "basis",
      "source_id", "status", "note"
    ),
    names(allocations)
  )) {
    allocations[[nm]] <- rep(NA_character_, nrow(allocations))
  }

  collapse_values <- function(x) {
    x <- unique(plain_chr(x))
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) paste(sort(x), collapse = " | ") else NA_character_
  }

  groups <- split(
    seq_len(nrow(identities)),
    identities$terminal_unit
  )
  safe_bind_rows(lapply(groups, function(i) {
    rows <- identities[i, , drop = FALSE]
    terminal <- rows$terminal_unit[[1L]]
    evidence <- allocations[
      allocations$source_unit %in% terminal,
      ,
      drop = FALSE
    ]
    statuses <- unique(plain_chr(evidence$status))
    statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
    accepted <- sum(statuses %in% "accepted")
    rejected <- sum(statuses %in% "rejected")
    needs_review <- sum(statuses %in% "needs_review")
    evidence_class <- if (!nrow(evidence)) {
      "no_allocation_record"
    } else if (length(statuses) == 1L && rejected == 1L) {
      "rejected_allocation_record"
    } else if (length(statuses) == 1L && accepted == 1L) {
      "accepted_allocation_not_connected"
    } else if (needs_review > 0L) {
      "allocation_record_needs_review"
    } else {
      "mixed_allocation_records"
    }
    priority <- switch(
      evidence_class,
      no_allocation_record = 1L,
      accepted_allocation_not_connected = 1L,
      allocation_record_needs_review = 2L,
      mixed_allocation_records = 2L,
      rejected_allocation_record = 3L,
      3L
    )
    action <- switch(
      evidence_class,
      no_allocation_record = paste0(
        "Create and source a Census-2011-to-2001 allocation decision for ",
        "this terminal unit."
      ),
      accepted_allocation_not_connected = paste0(
        "Repair the connected full-reviewed crosswalk: an accepted allocation ",
        "exists but these identities remain unmapped."
      ),
      allocation_record_needs_review = paste0(
        "Resolve the tracked allocation review, then regenerate the ",
        "full-reviewed crosswalk."
      ),
      mixed_allocation_records = paste0(
        "Reconcile conflicting allocation records before regenerating the ",
        "full-reviewed crosswalk."
      ),
      rejected_allocation_record = paste0(
        "Replace the rejected zero-weight proposal with supported allocation ",
        "evidence or explicitly exclude the affected identities."
      ),
      paste0(
        "Review the terminal-unit allocation evidence and regenerate the ",
        "full-reviewed crosswalk."
      )
    )
    data.frame(
      terminal_unit = terminal,
      wave = collapse_values(rows$wave),
      state_std = plain_chr(first_nonmissing(rows$state_std)),
      district_std = plain_chr(first_nonmissing(rows$district_std)),
      identity_count = nrow(rows),
      source_codes = collapse_values(rows$source_code),
      source_row_ids = collapse_values(rows$source_row_id),
      allocation_record_count = nrow(evidence),
      allocation_statuses = collapse_values(evidence$status),
      proposed_targets = collapse_values(evidence$target_2001),
      allocation_basis = collapse_values(evidence$basis),
      allocation_source_ids = collapse_values(evidence$source_id),
      allocation_notes = collapse_values(evidence$note),
      evidence_class = evidence_class,
      review_priority = priority,
      review_scope = "unmapped_terminal_unit",
      next_action = action,
      stringsAsFactors = FALSE
    )
  })) |>
    (\(out) out[
      order(
        out$review_priority,
        out$state_std,
        out$district_std,
        out$terminal_unit
      ),
      ,
      drop = FALSE
    ])()
}

lineage_panel_duplicates <- function(panel, variant) {
  panel <- safe_df(panel)
  unit_id <- lineage_panel_unit_id(panel)
  duplicate <- !is.na(unit_id) & (duplicated(unit_id) | duplicated(unit_id, fromLast = TRUE))
  if (!any(duplicate)) {
    return(data.frame(
      panel_variant = character(),
      target_unit_2001 = character(),
      row_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  counts <- table(unit_id[duplicate])
  data.frame(
    panel_variant = variant,
    target_unit_2001 = names(counts),
    row_count = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

summarize_lineage_downstream_coverage <- function(
  full_reviewed_crosswalk, conservative_eligibility, legacy_panel, lineage_panel
) {
  crosswalk <- safe_df(full_reviewed_crosswalk)
  eligibility <- safe_df(conservative_eligibility)
  for (nm in setdiff(
    c("source_row_id", "wave", "target_unit_2001"),
    names(crosswalk)
  )) {
    crosswalk[[nm]] <- rep(NA_character_, nrow(crosswalk))
  }
  for (nm in setdiff(
    c("source_row_id", "wave", "status"),
    names(eligibility)
  )) {
    eligibility[[nm]] <- rep(NA_character_, nrow(eligibility))
  }

  waves <- sort(unique(c(
    plain_chr(crosswalk$wave),
    plain_chr(eligibility$wave)
  )))
  wave_rows <- safe_bind_rows(lapply(waves, function(wave) {
    eligible <- eligibility[
      eligibility$wave %in% wave &
        eligibility$status %in% "accepted",
      ,
      drop = FALSE
    ]
    mapped <- crosswalk[crosswalk$wave %in% wave, , drop = FALSE]
    mapped_identities <- length(unique(stats::na.omit(
      mapped$source_row_id
    )))
    data.frame(
      scope = "wave",
      wave = wave,
      accepted_identities = nrow(eligible),
      mapped_identities = mapped_identities,
      crosswalk_rows = nrow(mapped),
      mapped_targets = length(unique(stats::na.omit(
        mapped$target_unit_2001
      ))),
      unmapped_identities = max(nrow(eligible) - mapped_identities, 0L),
      identity_coverage_share = if (nrow(eligible)) {
        mapped_identities / nrow(eligible)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }))

  legacy_units <- unique(stats::na.omit(
    lineage_panel_unit_id(legacy_panel)
  ))
  lineage_units <- unique(stats::na.omit(lineage_panel_unit_id(lineage_panel)))
  shared_units <- intersect(legacy_units, lineage_units)
  overall <- data.frame(
    scope = "panel",
    wave = "two_wave_panel",
    accepted_identities = NA_integer_,
    mapped_identities = NA_integer_,
    crosswalk_rows = NA_integer_,
    mapped_targets = length(lineage_units),
    unmapped_identities = NA_integer_,
    identity_coverage_share = NA_real_,
    legacy_unique_units = length(legacy_units),
    candidate_unique_units = length(lineage_units),
    shared_unique_units = length(shared_units),
    legacy_only_units = length(setdiff(legacy_units, lineage_units)),
    candidate_only_units = length(setdiff(lineage_units, legacy_units)),
    shared_share_of_legacy = if (length(legacy_units)) {
      length(shared_units) / length(legacy_units)
    } else {
      NA_real_
    },
    shared_share_of_candidate = if (length(lineage_units)) {
      length(shared_units) / length(lineage_units)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
  safe_bind_rows(list(wave_rows, overall))
}

lineage_downstream_review_gates <- function(
  coverage, legacy_panel, lineage_panel,
  panel_membership_adjudication = data.frame(),
  gini_reconstruction_queue = data.frame(),
  identity_coverage_complete = FALSE
) {
  coverage <- safe_df(coverage)
  legacy_duplicates <- lineage_panel_duplicates(
    legacy_panel, "legacy"
  )
  lineage_duplicates <- lineage_panel_duplicates(lineage_panel, "lineage")
  panel_row <- coverage[
    coverage$scope %in% "panel" &
      coverage$wave %in% "two_wave_panel",
    ,
    drop = FALSE
  ]
  primary_panel_available <- nrow(safe_df(lineage_panel)) > 0L

  adjudication <- safe_df(panel_membership_adjudication)
  gini_queue <- safe_df(gini_reconstruction_queue)
  membership_complete <-
    isTRUE(identity_coverage_complete) &&
    nrow(adjudication) == nrow(safe_df(compare_lineage_panels(
      legacy_panel, lineage_panel
    ))) &&
    nrow(adjudication) > 0L &&
    all(adjudication$status %in% c("accepted", "excluded"))
  shared_available <- nrow(panel_row) &&
    panel_row$shared_unique_units[[1L]] > 0L

  data.frame(
    gate = c(
      "inherited_legacy_duplicates_identified",
      "lineage_panel_unique_by_2001_unit",
      "panel_membership_adjudicated",
      "primary_panel_constructed_from_reviewed_sources",
      "shared_support_comparison_available",
      "multi_source_ginis_reconstructed",
      "legacy_reviewable"
    ),
    passed = c(
      nrow(legacy_duplicates) == 0L || all(
        legacy_duplicates$target_unit_2001 %in%
          adjudication$target_unit_2001[
            adjudication$decision %in% c(
              "replace_inherited_duplicate_with_unique",
              "exclude_inherited_duplicate"
            ) & adjudication$status %in% c("accepted", "excluded")
          ]
      ),
      nrow(lineage_duplicates) == 0L,
      membership_complete,
      primary_panel_available && isTRUE(identity_coverage_complete),
      shared_available,
      nrow(gini_queue) == 0L,
      membership_complete && primary_panel_available && shared_available &&
        nrow(lineage_duplicates) == 0L && nrow(gini_queue) == 0L
    ),
    next_action = c(
      paste0(
        "Confirm every inherited duplicate is explicitly excluded from the ",
        "review candidate."
      ),
      "Resolve duplicated Census-2001 units in the district lineage panel.",
      paste0(
        "Complete accepted-identity coverage and review the generated panel-",
        "membership adjudication."
      ),
      paste0(
        "Require a nonempty primary panel constructed only from adjudicated ",
        "NSS source identities and reviewed Census-2001 mappings. Differences ",
        "from the legacy-panel support are diagnostic, not vetoes."
      ),
      paste0(
        "Use the shared, unique Census-2001 support for interpretable model ",
        "comparisons."
      ),
      paste0(
        "Recompute every queued Gini from pooled household microdata before ",
        "review."
      ),
      paste0(
        "Record the downstream-results decision only after membership and ",
        "pooled-Gini gates pass."
      )
    ),
    stringsAsFactors = FALSE
  )
}

build_lineage_shared_support <- function(legacy_panel, lineage_panel) {
  legacy <- safe_df(legacy_panel)
  candidate <- safe_df(lineage_panel)
  legacy$target_unit_2001 <- lineage_panel_unit_id(legacy)
  candidate$target_unit_2001 <- lineage_panel_unit_id(candidate)

  legacy_counts <- table(legacy$target_unit_2001)
  candidate_counts <- table(candidate$target_unit_2001)
  legacy_unique <- names(legacy_counts[legacy_counts == 1L])
  candidate_unique <- names(candidate_counts[candidate_counts == 1L])
  shared <- sort(intersect(legacy_unique, candidate_unique))
  legacy <- legacy[
    legacy$target_unit_2001 %in% shared,
    ,
    drop = FALSE
  ]
  candidate <- candidate[
    candidate$target_unit_2001 %in% shared,
    ,
    drop = FALSE
  ]
  legacy$state_2001_cluster <- lineage_target_codes(
    legacy$target_unit_2001
  )$state_code_2001
  candidate$state_2001_cluster <- lineage_target_codes(
    candidate$target_unit_2001
  )$state_code_2001

  list(
    legacy = legacy,
    lineage = candidate,
    units = data.frame(
      target_unit_2001 = shared,
      state_2001_cluster = lineage_target_codes(shared)$state_code_2001,
      stringsAsFactors = FALSE
    )
  )
}


empty_lineage_gini_reconstruction_queue <- function() {
  data.frame(
    target_unit_2001 = character(),
    lineage_source_count = integer(),
    lineage_aggregation_status = character(),
    gini_cons_0708_reconstruction_status = character(),
    gini_cons_1718_reconstruction_status = character(),
    gini_cons_0708 = numeric(),
    gini_cons_1718 = numeric(),
    review_scope = character(),
    status = character(),
    next_action = character(),
    stringsAsFactors = FALSE
  )
}

build_lineage_gini_reconstruction_queue <- function(lineage_panel) {
  panel <- safe_df(lineage_panel)
  required <- c("target_unit_2001", "lineage_source_count", "lineage_aggregation_status")
  if (!nrow(panel) || !all(required %in% names(panel))) {
    return(empty_lineage_gini_reconstruction_queue())
  }

  status_cols <- intersect(
    c("gini_cons_0708_reconstruction_status", "gini_cons_1718_reconstruction_status"),
    names(panel)
  )
  if (!length(status_cols)) {
    queue <- panel[num(panel$lineage_source_count) > 1, required, drop = FALSE]
  } else {
    needs <- Reduce(`|`, lapply(status_cols, function(nm) {
      !panel[[nm]] %in% c("not_required", "reconstructed")
    }))
    queue <- panel[needs, c(required, status_cols), drop = FALSE]
  }
  if (!nrow(queue)) return(empty_lineage_gini_reconstruction_queue())
  queue <- queue[!duplicated(queue$target_unit_2001), , drop = FALSE]
  gini_cols <- intersect(c("gini_cons_0708", "gini_cons_1718"), names(panel))
  for (nm in gini_cols) {
    queue[[nm]] <- panel[[nm]][match(queue$target_unit_2001, panel$target_unit_2001)]
  }
  queue$review_scope <- "pooled_household_gini_reconstruction"
  queue$status <- "needs_reconstruction"
  queue$next_action <- paste0(
    "Pool the contributing household records for each wave and recompute the ",
    "survey-weighted Gini; do not average source-district Ginis."
  )
  queue[order(queue$target_unit_2001), , drop = FALSE]
}

lineage_model_summary <- function(iv_models, first_stage_tests, panel, variant) {
  panel_df <- safe_df(panel)
  coefficients <- tidy_iv_models(iv_models, panel_df)
  if (nrow(coefficients)) coefficients$panel_variant <- variant

  first_stage <- safe_df(first_stage_tests)
  if (nrow(first_stage)) first_stage$panel_variant <- variant

  coefficient_inference_available <- if (nrow(coefficients)) {
    all(
      is.finite(coefficients$std.error[coefficients$status == "estimated"]) &
        is.finite(coefficients$p.value[coefficients$status == "estimated"])
    )
  } else {
    FALSE
  }
  panel_summary <- data.frame(
    panel_variant = variant,
    panel_rows = nrow(panel_df),
    unique_districts = length(unique(stats::na.omit(
      lineage_panel_unit_id(panel_df)
    ))),
    complete_iv_rows = if (nrow(panel_df)) {
      sum(panel_has_analysis_core(panel_df), na.rm = TRUE)
    } else {
      0L
    },
    multi_source_rows = if (
      "lineage_source_count" %in% names(panel_df)
    ) {
      sum(num(panel_df$lineage_source_count) > 1, na.rm = TRUE)
    } else {
      0L
    },
    coefficient_inference_available = coefficient_inference_available,
    stringsAsFactors = FALSE
  )
  list(
    coefficients = coefficients,
    first_stage = first_stage,
    panel_summary = panel_summary
  )
}

compare_lineage_model_summaries <- function(
  legacy, candidate,
  comparison_scope = "different_panel_composition",
  comparable = FALSE
) {
  prod_coef <- safe_df(legacy$coefficients)
  cand_coef <- safe_df(candidate$coefficients)
  keys <- intersect(c("model", "term"), intersect(names(prod_coef), names(cand_coef)))
  coefficient_comparison <- if (length(keys) == 2L) {
    merge(
      prod_coef[c(keys, "estimate", "std.error", "p.value")],
      cand_coef[c(keys, "estimate", "std.error", "p.value")],
      by = keys,
      all = TRUE,
      suffixes = c("_legacy", ""),
      sort = FALSE
    )
  } else {
    data.frame()
  }
  if (nrow(coefficient_comparison)) {
    coefficient_comparison$estimate_change <-
      coefficient_comparison$estimate -
      coefficient_comparison$estimate_legacy
    coefficient_comparison$std_error_change <-
      coefficient_comparison$std.error -
      coefficient_comparison$std.error_legacy
    coefficient_comparison$inference_available <-
      is.finite(coefficient_comparison$std.error_legacy) &
      is.finite(coefficient_comparison$p.value_legacy) &
      is.finite(coefficient_comparison$std.error) &
      is.finite(coefficient_comparison$p.value)
    coefficient_comparison$comparison_scope <- comparison_scope
    coefficient_comparison$comparable <-
      comparable & coefficient_comparison$inference_available
  }

  prod_fs <- safe_df(legacy$first_stage)
  cand_fs <- safe_df(candidate$first_stage)
  fs_keys <- intersect(c("model", "term"), intersect(names(prod_fs), names(cand_fs)))
  first_stage_comparison <- if (length(fs_keys) == 2L) {
    merge(
      prod_fs[c(
        fs_keys, "estimate", "std.error", "partial_f", "partial_p", "nobs"
      )],
      cand_fs[c(
        fs_keys, "estimate", "std.error", "partial_f", "partial_p", "nobs"
      )],
      by = fs_keys,
      all = TRUE,
      suffixes = c("_legacy", ""),
      sort = FALSE
    )
  } else {
    data.frame()
  }

  if (nrow(first_stage_comparison)) {
    first_stage_comparison$comparison_scope <- comparison_scope
    first_stage_comparison$comparable <- comparable
  }

  list(
    panel_summary = safe_bind_rows(list(
      legacy$panel_summary,
      candidate$panel_summary
    )),
    coefficient_comparison = coefficient_comparison,
    first_stage_comparison = first_stage_comparison
  )
}

build_lineage_downstream_review <- function(
  legacy_panel, lineage_panel, legacy_models, lineage_models,
  legacy_first_stage, lineage_first_stage,
  full_reviewed_crosswalk = data.frame(),
  conservative_eligibility = data.frame(),
  legacy_shared_models = NULL,
  lineage_shared_models = NULL,
  legacy_shared_first_stage = data.frame(),
  lineage_shared_first_stage = data.frame(),
  legacy_shared_panel = data.frame(),
  lineage_shared_panel = data.frame(),
  admin_2001 = data.frame(),
  allocation_weights = data.frame(),
  gini_reconstruction_audit = data.frame()
) {
  legacy <- lineage_model_summary(
    legacy_models, legacy_first_stage, legacy_panel, "legacy"
  )
  candidate <- lineage_model_summary(
    lineage_models, lineage_first_stage, lineage_panel, "lineage"
  )
  comparison <- compare_lineage_model_summaries(legacy, candidate)
  comparison$panel_membership <- compare_lineage_panels(
    legacy_panel, lineage_panel
  )
  comparison$panel_nonoverlap_queue <-
    build_lineage_nonoverlap_queue(
      comparison$panel_membership,
      admin_2001
    )
  comparison$unmapped_identity_queue <-
    build_lineage_unmapped_identity_queue(
      conservative_eligibility,
      full_reviewed_crosswalk
    )
  comparison$unmapped_terminal_queue <-
    build_lineage_unmapped_terminal_queue(
      comparison$unmapped_identity_queue,
      allocation_weights
    )
  comparison$crosswalk_coverage <-
    summarize_lineage_downstream_coverage(
      full_reviewed_crosswalk,
      conservative_eligibility,
      legacy_panel,
      lineage_panel
    )
  comparison$panel_duplicates <- safe_bind_rows(list(
    lineage_panel_duplicates(legacy_panel, "legacy"),
    lineage_panel_duplicates(lineage_panel, "lineage")
  ))
  accepted_coverage <- accepted_mapping_status(
    conservative_eligibility,
    full_reviewed_crosswalk
  )
  comparison$panel_membership_adjudication <-
    build_lineage_panel_membership_adjudication(
      comparison$panel_membership,
      comparison$panel_duplicates[
        comparison$panel_duplicates$panel_variant %in% "legacy",
        ,
        drop = FALSE
      ],
      accepted_coverage$coverage_complete[[1L]]
    )
  comparison$gini_reconstruction_audit <- safe_df(gini_reconstruction_audit)
  comparison$gini_reconstruction_queue <-
    build_lineage_gini_reconstruction_queue(lineage_panel)
  comparison$review_gates <- lineage_downstream_review_gates(
    comparison$crosswalk_coverage,
    legacy_panel,
    lineage_panel,
    comparison$panel_membership_adjudication,
    comparison$gini_reconstruction_queue,
    accepted_coverage$coverage_complete[[1L]]
  )

  if (!is.null(legacy_shared_models) && !is.null(lineage_shared_models)) {
    shared_legacy <- lineage_model_summary(
      legacy_shared_models,
      legacy_shared_first_stage,
      legacy_shared_panel,
      "legacy_shared"
    )
    shared_candidate <- lineage_model_summary(
      lineage_shared_models,
      lineage_shared_first_stage,
      lineage_shared_panel,
      "lineage_shared"
    )
    shared <- compare_lineage_model_summaries(
      shared_legacy,
      shared_candidate,
      comparison_scope = "shared_unique_2001_support",
      comparable = TRUE
    )
    comparison$shared_panel_summary <- shared$panel_summary
    comparison$shared_coefficient_comparison <- shared$coefficient_comparison
    comparison$shared_first_stage_comparison <- shared$first_stage_comparison
  }
  comparison
}

save_lineage_downstream_review <- function(
  review, dir = "outputs/diagnostics/extended/district_lineage"
) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  gini_queue <- review$gini_reconstruction_queue
  if (is.null(gini_queue)) {
    gini_queue <- empty_lineage_gini_reconstruction_queue()
  }
  output_manifest(c(
    downstream_panel_summary = write_diagnostic_csv(
      review$panel_summary %||% data.frame(),
      file.path(dir, "downstream_panel_summary.csv")
    ),
    downstream_panel_membership = write_diagnostic_csv(
      review$panel_membership %||% data.frame(),
      file.path(dir, "downstream_panel_membership.csv")
    ),
    downstream_panel_nonoverlap_queue = write_diagnostic_csv(
      review$panel_nonoverlap_queue %||% data.frame(),
      file.path(dir, "downstream_panel_nonoverlap_queue.csv")
    ),
    downstream_panel_membership_adjudication = write_diagnostic_csv(
      review$panel_membership_adjudication %||% data.frame(),
      file.path(dir, "downstream_panel_membership_adjudication.csv")
    ),
    downstream_unmapped_identity_queue = write_diagnostic_csv(
      review$unmapped_identity_queue %||% data.frame(),
      file.path(dir, "downstream_unmapped_identity_queue.csv")
    ),
    downstream_unmapped_terminal_queue = write_diagnostic_csv(
      review$unmapped_terminal_queue %||% data.frame(),
      file.path(dir, "downstream_unmapped_terminal_queue.csv")
    ),
    downstream_coefficient_comparison = write_diagnostic_csv(
      review$coefficient_comparison %||% data.frame(),
      file.path(dir, "downstream_coefficient_comparison.csv")
    ),
    downstream_first_stage_comparison = write_diagnostic_csv(
      review$first_stage_comparison %||% data.frame(),
      file.path(dir, "downstream_first_stage_comparison.csv")
    ),
    downstream_crosswalk_coverage = write_diagnostic_csv(
      review$crosswalk_coverage %||% data.frame(),
      file.path(dir, "downstream_crosswalk_coverage.csv")
    ),
    downstream_panel_duplicates = write_diagnostic_csv(
      review$panel_duplicates %||% data.frame(),
      file.path(dir, "downstream_panel_duplicates.csv")
    ),
    downstream_review_gates = write_diagnostic_csv(
      review$review_gates %||% data.frame(),
      file.path(dir, "downstream_review_gates.csv")
    ),
    downstream_gini_reconstruction_audit = write_diagnostic_csv(
      review$gini_reconstruction_audit %||% data.frame(),
      file.path(dir, "downstream_gini_reconstruction_audit.csv")
    ),
    downstream_gini_reconstruction_queue = write_diagnostic_csv(
      gini_queue,
      file.path(dir, "downstream_gini_reconstruction_queue.csv")
    ),
    downstream_shared_panel_summary = write_diagnostic_csv(
      review$shared_panel_summary %||% data.frame(),
      file.path(dir, "downstream_shared_panel_summary.csv")
    ),
    downstream_shared_coefficient_comparison = write_diagnostic_csv(
      review$shared_coefficient_comparison %||% data.frame(),
      file.path(dir, "downstream_shared_coefficient_comparison.csv")
    ),
    downstream_shared_first_stage_comparison = write_diagnostic_csv(
      review$shared_first_stage_comparison %||% data.frame(),
      file.path(dir, "downstream_shared_first_stage_comparison.csv")
    )
  ))
}

build_lineage_panel_variant_review <- function(
  panels, models, first_stage_tests, gini_audits
) {
  variants <- names(panels)
  variant_sets <- lapply(
    list(models, first_stage_tests, gini_audits),
    function(x) sort(names(x))
  )
  if (
    !length(variants) || anyDuplicated(variants) ||
      !all(vapply(variant_sets, identical, logical(1), sort(variants)))
  ) {
    stop(
      "Panel-variant review requires the same unique names for panels, models, first stages, and Gini audits.",
      call. = FALSE
    )
  }
  summaries <- lapply(variants, function(variant) {
    lineage_model_summary(
      models[[variant]], first_stage_tests[[variant]], panels[[variant]], variant
    )
  })
  names(summaries) <- variants
  gini <- safe_bind_rows(lapply(variants, function(variant) {
    x <- safe_df(gini_audits[[variant]])
    if (nrow(x)) x$panel_variant <- variant
    x
  }))
  list(
    panel_summary = safe_bind_rows(lapply(summaries, `[[`, "panel_summary")),
    coefficients = safe_bind_rows(lapply(summaries, `[[`, "coefficients")),
    first_stage = safe_bind_rows(lapply(summaries, `[[`, "first_stage")),
    gini_reconstruction = gini
  )
}

save_lineage_panel_variant_review <- function(
  review,
  dir = "outputs/diagnostics/extended/district_lineage"
) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    panel_variant_model_summary = write_diagnostic_csv(
      review$panel_summary %||% data.frame(),
      file.path(dir, "panel_variant_model_summary.csv")
    ),
    panel_variant_coefficients = write_diagnostic_csv(
      review$coefficients %||% data.frame(),
      file.path(dir, "panel_variant_coefficients.csv")
    ),
    panel_variant_first_stage = write_diagnostic_csv(
      review$first_stage %||% data.frame(),
      file.path(dir, "panel_variant_first_stage.csv")
    ),
    panel_variant_gini_reconstruction = write_diagnostic_csv(
      review$gini_reconstruction %||% data.frame(),
      file.path(dir, "panel_variant_gini_reconstruction.csv")
    )
  )
  output_manifest(paths)
}
