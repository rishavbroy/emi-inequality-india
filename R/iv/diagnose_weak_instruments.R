# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose weak instruments
#'
diagnose_weak_instruments <- function(iv_models, district_panel, cfg) {
  estimate_first_stage(iv_models, district_panel, cfg)
}

#' jackknife first stage by state
#'
#' @return Explicit inactive status until state jackknife relevance checks are activated.
jackknife_first_stage_by_state <- function(...) {
  data.frame(
    status = "out_of_active_pipeline",
    reason = "State jackknife first-stage checks are documented as future relevance diagnostics and are not active.",
    stringsAsFactors = FALSE
  )
}

#' jackknife first stage by region
#'
#' @return Explicit inactive status until region jackknife relevance checks are activated.
jackknife_first_stage_by_region <- function(...) {
  data.frame(
    status = "out_of_active_pipeline",
    reason = "Region jackknife first-stage checks are documented as future relevance diagnostics and are not active.",
    stringsAsFactors = FALSE
  )
}

#' summarize weak iv metrics
#'
#' @return First non-empty diagnostics table, or explicit inactive status.
summarize_weak_iv_metrics <- function(...) {
  pieces <- list(...)
  pieces <- pieces[vapply(pieces, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]
  if (length(pieces)) return(safe_bind_rows(pieces))
  data.frame(
    status = "out_of_active_pipeline",
    reason = "No weak-IV diagnostics were supplied for summarization.",
    stringsAsFactors = FALSE
  )
}

#' diagnose instrument exploration
#'
#' Preserve the legacy Chunk 15 exploratory IV-strength dotplot as an opt-in
#' analysis artifact. The current analog uses the active district panel rather
#' than the pre-refactor `df0708` object and returns both the data used for the
#' dotplot and the legacy prose notes which motivated the check.
diagnose_instrument_exploration <- function(district_panel, cfg = list()) {
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else as.data.frame(district_panel, stringsAsFactors = FALSE)
  if (!nrow(panel)) {
    return(list(
      dotplot_data = data.frame(),
      legacy_notes = data.frame(note = "No active district-panel rows available for IV-strength exploration.", stringsAsFactors = FALSE)
    ))
  }

  code_col <- if ("district_code_0708" %in% names(panel)) "district_code_0708" else if ("district_panel_id" %in% names(panel)) "district_panel_id" else NA_character_
  state_col <- if ("state_07" %in% names(panel)) "state_07" else if ("state_std" %in% names(panel)) "state_std" else NA_character_
  district_col <- if ("district_07" %in% names(panel)) "district_07" else if ("district_std" %in% names(panel)) "district_std" else NA_character_

  dot <- data.frame(
    district_order = seq_len(nrow(panel)),
    district_code = if (!is.na(code_col)) as.character(panel[[code_col]]) else as.character(seq_len(nrow(panel))),
    state = if (!is.na(state_col)) as.character(panel[[state_col]]) else NA_character_,
    district = if (!is.na(district_col)) as.character(panel[[district_col]]) else NA_character_,
    EMIE = if ("EMIE" %in% names(panel)) suppressWarnings(as.numeric(panel$EMIE)) else NA_real_,
    wavg_ling_degrees = if ("wavg_ling_degrees" %in% names(panel)) suppressWarnings(as.numeric(panel$wavg_ling_degrees)) else NA_real_,
    region = if ("region" %in% names(panel)) as.character(panel$region) else NA_character_,
    stringsAsFactors = FALSE
  )
  dot$state_prefix <- substr(dot$district_code, 1L, 2L)
  dot <- dot[order(dot$district_code), , drop = FALSE]
  dot$district_order <- seq_len(nrow(dot))

  notes <- data.frame(
    diagnostic = c("emie_dotplot", "legacy_peak_comment", "smaller_units_question", "district_count_check"),
    legacy_note = c(
      "Dotplot of EMIE values by district_code.",
      "EMIE had visible peaks in Jammu and Kashmir; in several Northeast states; and in southern/coastal districts historically furthest from Hindi.",
      "Many districts outside peaks had low EMIE values; legacy comments asked whether smaller units of analysis would be useful.",
      "Legacy code checked that the number of districts did not change while constructing weighted linguistic distance."
    ),
    current_status = c(
      "rendered from active district_panel as a target-backed figure",
      "use current dotplot/table rather than the legacy hard-coded visual impression",
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
  grDevices::png(path, width = 1300, height = 800, res = 140)
  old <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(old); grDevices::dev.off() }, add = TRUE)
  if (!nrow(df) || !"EMIE" %in% names(df) || all(is.na(df$EMIE))) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No EMIE dotplot data available")
    return(normalizePath(path, mustWork = FALSE))
  }
  graphics::par(mar = c(5, 5, 4, 2))
  prefix <- as.character(df$state_prefix)
  prefix[is.na(prefix) | !nzchar(prefix)] <- "unknown"
  groups <- as.factor(prefix)
  cols <- grDevices::hcl.colors(max(3L, length(levels(groups))), palette = "Dark 3")
  graphics::plot(
    df$district_order,
    df$EMIE,
    pch = 19,
    col = cols[as.integer(groups)],
    xlab = "Districts ordered by active 2007-08 district code",
    ylab = "EMI exposure (percent)",
    main = "Percentage of students in EMI by district, current panel"
  )
  normalizePath(path, mustWork = FALSE)
}

save_instrument_exploration_diagnostics <- function(x, dir = "outputs/diagnostics/extended/instrument_exploration") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.list(x)) x <- list(dotplot_data = data.frame(), legacy_notes = data.frame())
  legacy_output_manifest(c(
    dotplot_data = write_diagnostic_csv(x$dotplot_data %||% data.frame(), file.path(dir, "instrument_strength_dotplot_data.csv")),
    legacy_notes = write_diagnostic_csv(x$legacy_notes %||% data.frame(), file.path(dir, "instrument_exploration_legacy_notes.csv")),
    emie_dotplot = save_instrument_exploration_plot(x$dotplot_data %||% data.frame(), file.path(dir, "emie_by_district_dotplot.png"))
  ))
}
