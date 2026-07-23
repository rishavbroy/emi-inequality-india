# This file is part of the EMI inequality research pipeline.
# It builds a diagnostic-only panel from the reviewed lineage-v2 crosswalk.
# Public targets continue to use the inherited production panel until migration
# is explicitly reviewed and accepted.

lineage_v2_source_code <- function(x) {
  raw <- gsub("[^0-9]", "", plain_chr(x))
  raw[nchar(raw) == 0L] <- NA_character_
  suppressWarnings(as.character(as.integer(raw)))
}

lineage_v2_target_codes <- function(unit_id) {
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

lineage_v2_wave_measure_spec <- function(wave) {
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
  stop("Unsupported lineage-v2 wave: ", wave, call. = FALSE)
}

first_nonmissing_v2 <- function(x) {
  keep <- !is.na(x)
  if (is.character(x)) keep <- keep & nzchar(x)
  hit <- which(keep)
  if (length(hit)) x[[hit[[1L]]]] else x[[1L]][NA_integer_]
}

weighted_mean_v2 <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

collapse_lineage_v2_measure_rows <- function(mapped, spec) {
  mapped <- safe_df(mapped)
  if (!nrow(mapped)) return(data.frame())
  groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
  safe_bind_rows(lapply(groups, function(i) {
    rows <- mapped[i, , drop = FALSE]
    out <- rows[1L, c("target_unit_2001"), drop = FALSE]
    out$lineage_source_count <- nrow(rows)
    out$lineage_aggregation_status <- if (nrow(rows) == 1L) {
      "one_to_one"
    } else {
      "district_aggregate_weighted"
    }

    value_cols <- setdiff(
      names(rows),
      c(
        "target_unit_2001", "source_code", "source_row_id", "wave",
        "mapping_class", "lineage_source_count",
        "lineage_aggregation_status"
      )
    )
    weight <- if (spec$weight_col %in% names(rows)) {
      rows[[spec$weight_col]]
    } else {
      rep(1, nrow(rows))
    }

    for (nm in value_cols) {
      x <- rows[[nm]]
      if (nm %in% spec$count_cols && is.numeric(x)) {
        out[[nm]] <- sum(x, na.rm = TRUE)
      } else if (is.numeric(x)) {
        out[[nm]] <- if (nrow(rows) == 1L) x[[1L]] else weighted_mean_v2(x, weight)
      } else {
        out[[nm]] <- first_nonmissing_v2(x)
      }
    }
    out
  }))
}

map_lineage_v2_measures <- function(measures, crosswalk, wave) {
  measures <- safe_df(measures)
  crosswalk <- safe_df(crosswalk)
  spec <- lineage_v2_wave_measure_spec(wave)
  if (!nrow(measures) || !nrow(crosswalk) || !spec$code_col %in% names(measures)) {
    return(data.frame())
  }

  map <- crosswalk[
    crosswalk$wave %in% wave,
    intersect(
      c(
        "source_row_id", "wave", "source_code", "target_unit_2001",
        "mapping_class"
      ),
      names(crosswalk)
    ),
    drop = FALSE
  ]
  map$source_code_key <- lineage_v2_source_code(map$source_code)
  measures$source_code_key <- lineage_v2_source_code(measures[[spec$code_col]])

  if (anyDuplicated(map$source_code_key[!is.na(map$source_code_key)])) {
    stop(
      "The preferred lineage-v2 crosswalk must map each wave-specific source ",
      "code once before measure construction.",
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
  collapse_lineage_v2_measure_rows(mapped, spec)
}

attach_lineage_v2_instrument <- function(panel, linguistic_distance_iv) {
  panel <- safe_df(panel)
  iv <- safe_df(linguistic_distance_iv)
  if (!nrow(panel) || !nrow(iv)) return(panel)

  codes <- lineage_v2_target_codes(panel$target_unit_2001)
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

attach_lineage_v2_geometry <- function(panel, geometry_2001) {
  panel <- safe_df(panel)
  if (!inherits(geometry_2001, "sf") || !nrow(geometry_2001) ||
      !"unit_id" %in% names(geometry_2001)) {
    return(panel)
  }
  geometry <- geometry_2001[
    !duplicated(plain_chr(geometry_2001$unit_id)),
    c("unit_id", attr(geometry_2001, "sf_column")),
    drop = FALSE
  ]
  names(geometry)[names(geometry) == "unit_id"] <- "target_unit_2001"
  merge(geometry, panel, by = "target_unit_2001", all.y = TRUE, sort = FALSE)
}

build_lineage_v2_district_panel <- function(
  primary_crosswalk, measures_2007, measures_2017,
  linguistic_distance_iv, geometry_2001 = data.frame(), cfg = list()
) {
  m07 <- map_lineage_v2_measures(
    measures_2007, primary_crosswalk, "nss_2007_08"
  )
  m17 <- map_lineage_v2_measures(
    measures_2017, primary_crosswalk, "nss_2017_18"
  )
  if (!nrow(m07) || !nrow(m17)) return(empty_panel())

  duplicate <- intersect(
    setdiff(names(m17), c("target_unit_2001")),
    names(m07)
  )
  m17 <- m17[setdiff(names(m17), duplicate)]
  panel <- merge(m07, m17, by = "target_unit_2001", all = FALSE, sort = FALSE)
  panel <- attach_lineage_v2_instrument(panel, linguistic_distance_iv)
  panel$district_panel_id <- sub(
    "^pc2001__", "2001__", panel$target_unit_2001
  )
  panel <- add_panel_standardized_names(panel)
  panel <- add_panel_regions(panel)
  panel <- compute_consumption_pct_change(panel)
  panel <- compute_log_consumption_difference(panel)
  panel <- compute_gini_change(panel)
  panel <- panel[panel_has_analysis_core(panel), , drop = FALSE]
  rownames(panel) <- NULL
  validate_analysis_district_panel(
    attach_lineage_v2_geometry(panel, geometry_2001),
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

compare_lineage_v2_panels <- function(production_panel, v2_panel) {
  production <- safe_df(production_panel)
  candidate <- safe_df(v2_panel)
  production$target_unit_2001 <- lineage_panel_unit_id(production)
  candidate$target_unit_2001 <- lineage_panel_unit_id(candidate)

  production_units <- unique(stats::na.omit(production$target_unit_2001))
  candidate_units <- unique(stats::na.omit(candidate$target_unit_2001))
  units <- sort(unique(c(production_units, candidate_units)))
  data.frame(
    target_unit_2001 = units,
    in_production = units %in% production_units,
    in_v2 = units %in% candidate_units,
    comparison_status = ifelse(
      units %in% production_units & units %in% candidate_units,
      "shared",
      ifelse(units %in% candidate_units, "v2_only", "production_only")
    ),
    stringsAsFactors = FALSE
  )
}

lineage_v2_model_summary <- function(iv_models, first_stage_tests, panel, variant) {
  coefficients <- tidy_iv_models(iv_models)
  if (nrow(coefficients)) coefficients$panel_variant <- variant

  first_stage <- safe_df(first_stage_tests)
  if (nrow(first_stage)) first_stage$panel_variant <- variant

  panel_df <- safe_df(panel)
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
    stringsAsFactors = FALSE
  )
  list(
    coefficients = coefficients,
    first_stage = first_stage,
    panel_summary = panel_summary
  )
}

compare_lineage_v2_model_summaries <- function(production, candidate) {
  prod_coef <- safe_df(production$coefficients)
  cand_coef <- safe_df(candidate$coefficients)
  keys <- intersect(c("model", "term"), intersect(names(prod_coef), names(cand_coef)))
  coefficient_comparison <- if (length(keys) == 2L) {
    merge(
      prod_coef[c(keys, "estimate", "std.error", "p.value")],
      cand_coef[c(keys, "estimate", "std.error", "p.value")],
      by = keys,
      all = TRUE,
      suffixes = c("_production", "_v2"),
      sort = FALSE
    )
  } else {
    data.frame()
  }
  if (nrow(coefficient_comparison)) {
    coefficient_comparison$estimate_change <-
      coefficient_comparison$estimate_v2 -
      coefficient_comparison$estimate_production
    coefficient_comparison$std_error_change <-
      coefficient_comparison$std.error_v2 -
      coefficient_comparison$std.error_production
  }

  prod_fs <- safe_df(production$first_stage)
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
      suffixes = c("_production", "_v2"),
      sort = FALSE
    )
  } else {
    data.frame()
  }

  list(
    panel_summary = safe_bind_rows(list(
      production$panel_summary,
      candidate$panel_summary
    )),
    coefficient_comparison = coefficient_comparison,
    first_stage_comparison = first_stage_comparison
  )
}

build_lineage_v2_downstream_review <- function(
  production_panel, v2_panel, production_models, v2_models,
  production_first_stage, v2_first_stage
) {
  production <- lineage_v2_model_summary(
    production_models, production_first_stage, production_panel, "production"
  )
  candidate <- lineage_v2_model_summary(
    v2_models, v2_first_stage, v2_panel, "lineage_v2"
  )
  comparison <- compare_lineage_v2_model_summaries(production, candidate)
  comparison$panel_membership <- compare_lineage_v2_panels(
    production_panel, v2_panel
  )
  comparison
}

save_lineage_v2_downstream_review <- function(
  review, dir = "outputs/diagnostics/extended/district_lineage_v2"
) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  output_manifest(c(
    downstream_panel_summary = write_diagnostic_csv(
      review$panel_summary %||% data.frame(),
      file.path(dir, "downstream_panel_summary.csv")
    ),
    downstream_panel_membership = write_diagnostic_csv(
      review$panel_membership %||% data.frame(),
      file.path(dir, "downstream_panel_membership.csv")
    ),
    downstream_coefficient_comparison = write_diagnostic_csv(
      review$coefficient_comparison %||% data.frame(),
      file.path(dir, "downstream_coefficient_comparison.csv")
    ),
    downstream_first_stage_comparison = write_diagnostic_csv(
      review$first_stage_comparison %||% data.frame(),
      file.path(dir, "downstream_first_stage_comparison.csv")
    )
  ))
}
