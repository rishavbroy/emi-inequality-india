# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose weak instruments
#'
diagnose_weak_instruments <- function(iv_models, district_panel, cfg) {
  estimate_first_stage(iv_models, district_panel, cfg)
}

candidate_iv_diagnostic_specifications <- function() {
  registry <- iv_diagnostic_specification_registry()
  candidates <- registry[
    registry$adjustment_id %in% iv_candidate_design_adjustments() &
      registry$construction_id == "nonzero_mean",
    ,
    drop = FALSE
  ]
  expected <- paste(
    iv_candidate_design_adjustments(), "nonzero_mean", sep = "__"
  )
  if (!setequal(candidates$specification_id, expected) ||
      anyDuplicated(candidates$adjustment_id)) {
    stop(
      "Expected one nonzero-mean IV specification for each candidate main design.",
      call. = FALSE
    )
  }
  candidates[
    match(iv_candidate_design_adjustments(), candidates$adjustment_id),
    ,
    drop = FALSE
  ]
}

diagnose_candidate_anderson_rubin <- function(
    district_panel, level = 0.95, points = 401L) {
  panel <- as.data.frame(district_panel)
  specifications <- candidate_iv_diagnostic_specifications()
  safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    specification <- specifications[i, , drop = FALSE]
    out <- estimate_anderson_rubin_spec(
      panel,
      specification,
      level = level,
      points = points
    )$summary
    out$adjustment_id <- specification$adjustment_id[[1L]]
    out$construction_id <- specification$construction_id[[1L]]
    out$fixed_effect <- specification$fixed_effect[[1L]]
    out
  }))
}

save_candidate_anderson_rubin <- function(
    diagnostic,
    path = "outputs/diagnostics/public/anderson_rubin_candidate_designs.csv"
) {
  write_diagnostic_csv(diagnostic, path)
}


#' diagnose instrument exploration
#'
#' Build a district-level view of the preferred public treatment and instrument,
#' while retaining the archived prose notes that motivated the original check.
diagnose_instrument_exploration <- function(district_panel, cfg = list()) {
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else as.data.frame(district_panel, stringsAsFactors = FALSE)
  if (!nrow(panel)) {
    return(list(
      dotplot_data = data.frame(),
      legacy_notes = data.frame(note = "No active district-panel rows available for IV-strength exploration.", stringsAsFactors = FALSE)
    ))
  }

  spec <- preferred_iv_variables()
  code_col <- if ("district_code_0708" %in% names(panel)) "district_code_0708" else if ("district_panel_id" %in% names(panel)) "district_panel_id" else NA_character_
  state_col <- if ("state_07" %in% names(panel)) "state_07" else if ("state_std" %in% names(panel)) "state_std" else NA_character_
  district_col <- if ("district_07" %in% names(panel)) "district_07" else if ("district_std" %in% names(panel)) "district_std" else NA_character_

  dot <- data.frame(
    district_order = seq_len(nrow(panel)),
    district_code = if (!is.na(code_col)) as.character(panel[[code_col]]) else as.character(seq_len(nrow(panel))),
    state = if (!is.na(state_col)) as.character(panel[[state_col]]) else NA_character_,
    district = if (!is.na(district_col)) as.character(panel[[district_col]]) else NA_character_,
    emi_exposure_all_children_0708 = if (spec$treatment %in% names(panel)) suppressWarnings(as.numeric(panel[[spec$treatment]])) else NA_real_,
    ling_distance_nonzero_mean = if (spec$instrument %in% names(panel)) suppressWarnings(as.numeric(panel[[spec$instrument]])) else NA_real_,
    region = if ("region" %in% names(panel)) as.character(panel$region) else NA_character_,
    stringsAsFactors = FALSE
  )
  dot$state_prefix <- substr(dot$district_code, 1L, 2L)
  dot <- dot[order(dot$district_code), , drop = FALSE]
  dot$district_order <- seq_len(nrow(dot))

  notes <- data.frame(
    diagnostic = c("legacy_emie_dotplot", "legacy_peak_comment", "smaller_units_question", "district_count_check"),
    legacy_note = c(
      "The historical code plotted EMI among enrolled children by district code.",
      "Legacy notes described high EMI-among-enrolled values in several geographically distant regions.",
      "Legacy comments asked whether smaller units of analysis would be useful.",
      "Legacy code checked that the number of districts did not change while constructing weighted linguistic distance."
    ),
    current_status = c(
      "the current plot uses all-child EMI exposure from the active district panel",
      "use current treatment and instrument diagnostics rather than the legacy visual impression",
      "retained as exploratory rationale, not a final-paper claim",
      "final panel match summaries are rendered in this analysis note"
    ),
    stringsAsFactors = FALSE
  )
  list(dotplot_data = dot, legacy_notes = notes)
}

save_instrument_exploration_plot <- function(dotplot_data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(dotplot_data, stringsAsFactors = FALSE)
  treatment <- preferred_iv_variables()$treatment
  grDevices::png(path, width = 1300, height = 800, res = 140)
  old <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(old); grDevices::dev.off() }, add = TRUE)
  if (!nrow(df) || !treatment %in% names(df) || all(is.na(df[[treatment]]))) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No EMI-exposure dotplot data available")
    return(normalizePath(path, mustWork = FALSE))
  }
  graphics::par(mar = c(5, 5, 4, 2))
  prefix <- as.character(df$state_prefix)
  prefix[is.na(prefix) | !nzchar(prefix)] <- "unknown"
  groups <- as.factor(prefix)
  cols <- grDevices::hcl.colors(max(3L, length(levels(groups))), palette = "Dark 3")
  graphics::plot(
    df$district_order,
    df[[treatment]],
    pch = 19,
    col = cols[as.integer(groups)],
    xlab = "Districts ordered by active 2007-08 district code",
    ylab = "All-child EMI exposure (percent)",
    main = "All-child EMI exposure by district"
  )
  normalizePath(path, mustWork = FALSE)
}

save_instrument_exploration_diagnostics <- function(x, dir = "outputs/diagnostics/extended/instrument_exploration") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.list(x)) x <- list(dotplot_data = data.frame(), legacy_notes = data.frame())
  output_manifest(c(
    dotplot_data = write_diagnostic_csv(x$dotplot_data %||% data.frame(), file.path(dir, "instrument_strength_dotplot_data.csv")),
    legacy_notes = write_diagnostic_csv(x$legacy_notes %||% data.frame(), file.path(dir, "instrument_exploration_legacy_notes.csv")),
    emie_dotplot = save_instrument_exploration_plot(x$dotplot_data %||% data.frame(), file.path(dir, "emie_by_district_dotplot.png"))
  ))
}
