# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-spatial-weights

#' build spatial weights
#'
#' Build the rook-contiguity spatial weights used by the legacy Rmd.
#'
#' The legacy chunk first removed rows with missing analysis values, then called
#' `poly2nb(queen = FALSE)`, `nb2mat(style = "B")`, and `nb2listw(style = "W")`.
#' The target stores all three objects so downstream diagnostics can reproduce
#' both the binary adjacency matrix and the row-standardized listw object.
#'
#' @return A list containing neighbor, matrix, and listw objects, or an explicit
#' inactive status when geometry is unavailable.
build_spatial_weights <- function(district_panel, cfg) {
  if (!inherits(district_panel, "sf")) {
    return(list(status = "out_of_active_pipeline", reason = "Requires sf geometry."))
  }
  need_pkg("spdep", "spatial weights")
  need_pkg("sf", "spatial weights")

  row_index <- spatial_weight_final_panel_rows(district_panel)
  coverage <- length(row_index) / max(nrow(district_panel), 1L)
  if (!is.finite(coverage) || coverage < 0.75) {
    return(list(
      status = "out_of_active_pipeline",
      reason = paste0(
        "Requires validated non-empty geometry for at least 75% of district-panel rows; current coverage is ",
        round(100 * coverage, 1), "%.")
    ))
  }

  weights <- build_spatial_weights_for_rows(district_panel, row_index, queen = FALSE)
  weights$coverage <- coverage
  weights$panel_scope <- "current_final_matched_panel_non_empty_geometry"
  weights
}

#' rows used by final-panel spatial diagnostics
#'
#' Spatial diagnostics operate on the active `district_panel` target, not on the
#' legacy exploratory geometry object.  The final-panel scope is therefore all
#' rows in the current matched panel with non-empty geometry.
spatial_weight_final_panel_rows <- function(district_panel) {
  if (!inherits(district_panel, "sf")) return(integer())
  geom <- sf::st_geometry(district_panel)
  which(!sf::st_is_empty(geom))
}

#' build spatial weights for selected district-panel rows
#'
#' @return A list with nb, binary matrix, and row-standardized listw objects.
build_spatial_weights_for_rows <- function(district_panel, rows, queen = FALSE) {
  if (!inherits(district_panel, "sf")) {
    return(list(status = "out_of_active_pipeline", reason = "Requires sf geometry."))
  }
  need_pkg("spdep", "spatial weights")
  need_pkg("sf", "spatial weights")

  rows <- as.integer(rows)
  rows <- rows[is.finite(rows) & rows >= 1L & rows <= nrow(district_panel)]
  if (!length(rows)) {
    return(list(status = "out_of_active_pipeline", reason = "No rows with usable geometry."))
  }
  geom <- sf::st_geometry(district_panel)
  rows <- rows[!sf::st_is_empty(geom[rows])]
  if (!length(rows)) {
    return(list(status = "out_of_active_pipeline", reason = "Selected rows have empty geometry."))
  }

  spatial_warnings <- character()
  capture_expected_spatial_warning <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        msg <- conditionMessage(w)
        expected <- grepl("no neighbours|sub-graphs", msg, ignore.case = TRUE)
        if (expected) {
          spatial_warnings <<- unique(c(spatial_warnings, msg))
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  panel <- district_panel[rows, , drop = FALSE]
  nb <- capture_expected_spatial_warning(spdep::poly2nb(panel, queen = queen))
  W <- capture_expected_spatial_warning(spdep::nb2mat(nb, style = "B", zero.policy = TRUE))
  listw <- capture_expected_spatial_warning(spdep::nb2listw(nb, style = "W", zero.policy = TRUE))

  out <- list(
    status = "constructed",
    contiguity = if (isTRUE(queen)) "queen" else "rook",
    style = "W",
    matrix_style = "B",
    zero_policy = TRUE,
    row_index = rows,
    nb = nb,
    W = W,
    listw = listw,
    neighbor_counts = lengths(nb),
    n = length(rows),
    n_islands = sum(lengths(nb) == 0L),
    mean_neighbors = mean(lengths(nb)),
    warnings = spatial_warnings
  )
  class(out) <- c("emi_spatial_weights", class(out))
  out
}

#' diagnose spatial weights
#'
diagnose_spatial_weights <- function(district_panel, spatial_weights, cfg) {
  if (!inherits(district_panel, "sf")) {
    return(data.frame(
      diagnostic = "spatial_weights",
      status = "out_of_active_pipeline",
      reason = "Requires sf geometry.",
      stringsAsFactors = FALSE
    ))
  }

  comparison <- compare_rook_queen_contiguity(district_panel)
  base <- data.frame(
    diagnostic = "spatial_weights",
    status = spatial_weights$status %||% "constructed",
    contiguity = spatial_weights$contiguity %||% NA_character_,
    style = spatial_weights$style %||% NA_character_,
    matrix_style = spatial_weights$matrix_style %||% NA_character_,
    n = spatial_weights$n %||% NA_real_,
    n_islands = spatial_weights$n_islands %||% NA_real_,
    mean_neighbors = spatial_weights$mean_neighbors %||% NA_real_,
    panel_scope = spatial_weights$panel_scope %||% "current_final_matched_panel_non_empty_geometry",
    warnings = paste(spatial_weights$warnings %||% attr(spatial_weights, "spatial_warnings") %||% character(), collapse = "; "),
    stringsAsFactors = FALSE
  )
  attr(base, "rook_queen_comparison") <- add_legacy_spatial_weight_reference(comparison)
  attr(base, "legacy_reference") <- legacy_spatial_weight_reference()
  base
}

#' compare rook and queen contiguity
#'
#' The legacy Rmd commented out an sfExtras rook-vs-queen check and recorded
#' nearly identical mean neighbor counts (rook 4.780165, queen 4.783471) plus
#' similar run times.  To avoid an extra sfExtras dependency, this project uses
#' the same `spdep::poly2nb()` implementation used by the final rook weights and
#' records elapsed time and average neighbor counts for both choices.
compare_rook_queen_contiguity <- function(district_panel) {
  if (!inherits(district_panel, "sf")) return(tibble::tibble())
  need_pkg("spdep", "rook/queen contiguity comparison")
  need_pkg("sf", "rook/queen contiguity comparison")
  panel <- district_panel[spatial_weight_final_panel_rows(district_panel), , drop = FALSE]
  if (!nrow(panel)) return(tibble::tibble())

  one <- function(queen) {
    warnings <- character()
    elapsed <- system.time({
      nb <- withCallingHandlers(
        spdep::poly2nb(panel, queen = queen),
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("no neighbours|sub-graphs", msg, ignore.case = TRUE)) {
            warnings <<- unique(c(warnings, msg))
            invokeRestart("muffleWarning")
          }
        }
      )
    })[["elapsed"]]
    tibble::tibble(
      contiguity = if (isTRUE(queen)) "queen" else "rook",
      n = length(nb),
      mean_neighbors = mean(lengths(nb)),
      n_islands = sum(lengths(nb) == 0L),
      panel_scope = "current_final_matched_panel_non_empty_geometry",
      elapsed_seconds = unname(elapsed),
      warnings = paste(warnings, collapse = "; ")
    )
  }

  safe_bind_rows(list(one(FALSE), one(TRUE)))
}


legacy_spatial_weight_reference <- function() {
  data.frame(
    contiguity = c("rook", "queen"),
    legacy_method = c(
      "sfExtras::st_rook() timing comment; final weights use spdep::poly2nb(queen = FALSE)",
      "sfExtras::st_queen() timing comment; benchmark uses spdep::poly2nb(queen = TRUE)"
    ),
    legacy_mean_neighbors = c(4.780165, 4.783471),
    legacy_elapsed_note = c("legacy comment recorded similar run time to queen", "legacy comment recorded similar run time to rook"),
    interpretation = "Current means are intentionally computed on the active final matched panel with non-empty geometry; they may differ from legacy exploratory sfExtras objects, but they now match the panel used for current spatial diagnostics.",
    stringsAsFactors = FALSE
  )
}

add_legacy_spatial_weight_reference <- function(comparison) {
  comparison <- as.data.frame(comparison, stringsAsFactors = FALSE)
  if (!nrow(comparison)) return(comparison)
  ref <- legacy_spatial_weight_reference()[c("contiguity", "legacy_mean_neighbors")]
  out <- merge(comparison, ref, by = "contiguity", all.x = TRUE, sort = FALSE)
  out$mean_neighbor_delta_from_legacy <- out$mean_neighbors - out$legacy_mean_neighbors
  out$pct_delta_from_legacy <- 100 * out$mean_neighbor_delta_from_legacy / out$legacy_mean_neighbors
  out
}

#' summarize islands
#'
summarize_islands <- function(spatial_weights) {
  if (!inherits(spatial_weights, "emi_spatial_weights")) return(tibble::tibble())
  islands <- which(spatial_weights$neighbor_counts == 0L)
  tibble::tibble(row_index = spatial_weights$row_index[islands], n_neighbors = 0L)
}

#' summarize neighbor counts
#'
summarize_neighbor_counts <- function(spatial_weights) {
  if (!inherits(spatial_weights, "emi_spatial_weights")) return(tibble::tibble())
  tibble::tibble(
    row_index = spatial_weights$row_index,
    n_neighbors = spatial_weights$neighbor_counts
  )
}

#' save spatial weight diagnostics
#'
save_spatial_weight_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/spatial") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    summary = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "spatial_weights_summary.csv")),
    rook_queen_comparison = write_diagnostic_csv(attr(diagnostics, "rook_queen_comparison") %||% data.frame(), file.path(dir, "rook_queen_contiguity_comparison.csv")),
    legacy_reference = write_diagnostic_csv(attr(diagnostics, "legacy_reference") %||% data.frame(), file.path(dir, "spatial_weights_legacy_reference.csv"))
  )
  legacy_output_manifest(paths)
}

# sample-end: code-spatial-weights
