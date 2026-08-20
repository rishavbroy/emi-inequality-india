# Audit final-mode public output artifacts for required files and diagnostic leftovers.

source("scripts/public_output_contract.R", local = TRUE)

if (!file.exists(".pipeline-final-ok")) {
  stop("Final output audit requires a successful current final pipeline run. Run `make pipeline-final` first.", call. = FALSE)
}

failures <- character()
add_failure <- function(...) failures <<- c(failures, paste0(...))

required_files <- required_final_artifacts()
missing_required <- missing_or_empty_files(required_files)
if (length(missing_required)) {
  add_failure("Missing required public output files: ", paste(missing_required, collapse = ", "))
}

report_has_geometry_blocker <- FALSE
if (file.exists("paper/report.qmd")) {
  report_text <- paste(readLines("paper/report.qmd", warn = FALSE), collapse = "\n")
  report_has_geometry_blocker <- grepl("Final district map figures are withheld", report_text, fixed = TRUE)
}

figure_dir <- "outputs/figures/main"
if (dir.exists(figure_dir)) {
  manifest_path <- file.path(figure_dir, "figure_manifest.csv")
  if (file.exists(manifest_path)) {
    manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
    if (report_has_geometry_blocker && "name" %in% names(manifest)) {
      diagnostic_names <- grep("^(map_|collage_.*maps)", manifest$name, value = TRUE)
      if (length(diagnostic_names)) {
        add_failure("Final figure manifest lists map/collage outputs while final maps are withheld: ", paste(diagnostic_names, collapse = ", "))
      }
    }
  }
  if (report_has_geometry_blocker) {
    map_files <- list.files(figure_dir, pattern = "^(map_|collage_.*maps)", full.names = TRUE)
    if (length(map_files)) add_failure("Final figure directory contains map-like files despite withheld final maps: ", paste(basename(map_files), collapse = ", "))
  }
}

table_dir <- "outputs/tables/main"
if (dir.exists(table_dir)) {
  csv_files <- list.files(table_dir, pattern = "\\.csv$", full.names = TRUE)
  public_tables <- setdiff(basename(csv_files), c("selection_n.csv", "ame_results.csv", "first_stage.csv"))
  for (path in file.path(table_dir, public_tables)) {
    if (!file.exists(path)) next
    tab <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
    bad_cols <- intersect(c("status", "reason"), names(tab))
    if (length(bad_cols)) add_failure(basename(path), " contains diagnostic columns in a public table: ", paste(bad_cols, collapse = ", "))
  }

  ame_path <- file.path(table_dir, "ame_results.csv")
  if (file.exists(ame_path)) {
    ame <- utils::read.csv(ame_path, stringsAsFactors = FALSE, check.names = FALSE)
    estimated <- if ("status" %in% names(ame)) ame$status == "estimated" else rep(TRUE, nrow(ame))
    if (any(estimated, na.rm = TRUE)) {
      required <- c("std.error", "statistic", "p.value", "s.value", "conf.low", "conf.high")
      missing_cols <- setdiff(required, names(ame))
      if (length(missing_cols)) add_failure("AME results are missing required columns: ", paste(missing_cols, collapse = ", "))
      for (col in intersect(required, names(ame))) {
        if (all(is.na(ame[[col]][estimated]))) add_failure("AME results have no final values in column: ", col)
      }
      if ("method" %in% names(ame) && any(ame$method[estimated] %in% c("coefficient_fallback"), na.rm = TRUE)) {
        add_failure("Final AME results still use draft/fallback methods: ", paste(unique(ame$method[estimated]), collapse = ", "))
      }
    }
  }
}

multicollinearity_path <- "outputs/diagnostics/public/multicollinearity_diagnostics.csv"
if (file.exists(multicollinearity_path)) {
  multi <- tryCatch(
    utils::read.csv(multicollinearity_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
  vif <- if (all(c("diagnostic", "status") %in% names(multi))) {
    multi[multi$diagnostic == "vif", , drop = FALSE]
  } else {
    data.frame()
  }
  if (!nrow(vif)) {
    add_failure("Public multicollinearity diagnostics contain no VIF/GVIF rows.")
  } else if (any(vif$status != "estimated" | is.na(vif$status))) {
    reasons <- if ("reason" %in% names(vif)) unique(stats::na.omit(vif$reason[vif$status != "estimated" | is.na(vif$status)])) else character()
    suffix <- if (length(reasons)) paste0(" Reasons: ", paste(reasons, collapse = "; ")) else ""
    add_failure("Public VIF/GVIF diagnostics are unavailable for one or more final IV models.", suffix)
  } else if (!"gvif_scaled" %in% names(vif) || any(!is.finite(vif$gvif_scaled))) {
    add_failure("Public VIF/GVIF diagnostics contain non-finite scaled GVIF values.")
  }
}

anderson_rubin_path <- "outputs/diagnostics/public/anderson_rubin_preferred.csv"
if (file.exists(anderson_rubin_path)) {
  ar <- tryCatch(
    utils::read.csv(anderson_rubin_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
  required_ar <- c(
    "status", "anderson_rubin_p_beta0", "ar_95_lower", "ar_95_upper",
    "ar_95_n_components", "ar_95_disconnected", "ar_95_contains_zero",
    "ar_95_left_truncated", "ar_95_right_truncated", "ar_95_components"
  )
  if (!nrow(ar) || !all(required_ar %in% names(ar))) {
    add_failure("Preferred Anderson-Rubin diagnostic is malformed.")
  } else if (!identical(ar$status[[1]], "estimated") || !is.finite(ar$anderson_rubin_p_beta0[[1]])) {
    reason <- if ("reason" %in% names(ar) && !is.na(ar$reason[[1]])) paste0(" Reason: ", ar$reason[[1]]) else ""
    add_failure("Preferred Anderson-Rubin diagnostic is unavailable in final mode.", reason)
  } else {
    null_accepted <- ar$anderson_rubin_p_beta0[[1]] >= 0.05
    if (!identical(as.logical(ar$ar_95_contains_zero[[1]]), null_accepted)) {
      add_failure("Preferred Anderson-Rubin confidence-set inversion disagrees with the beta=0 test.")
    }
    noninterval <- isTRUE(ar$ar_95_disconnected[[1]]) ||
      isTRUE(ar$ar_95_left_truncated[[1]]) ||
      isTRUE(ar$ar_95_right_truncated[[1]])
    if (noninterval && (is.finite(ar$ar_95_lower[[1]]) || is.finite(ar$ar_95_upper[[1]]))) {
      add_failure("Preferred Anderson-Rubin output reports ordinary interval bounds for a disconnected or grid-truncated confidence set.")
    }
  }
}


if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Final output artifact audit failed.", call. = FALSE)
}

message("Final output artifact audit passed.")
