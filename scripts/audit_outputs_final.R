# Audit final-mode public output artifacts for required files and diagnostic leftovers.

if (!file.exists(".pipeline-final-ok")) {
  stop("Final output audit requires a successful current final pipeline run. Run `make pipeline-final` first.", call. = FALSE)
}

failures <- character()
add_failure <- function(...) failures <<- c(failures, paste0(...))

required_files <- c(
  "paper/references.bib",
  "outputs/tables/main/sum_tbl_probit_quant.csv",
  "outputs/tables/main/sum_tbl_probit_cat.csv",
  "outputs/tables/main/probit_mfx.csv",
  "outputs/tables/main/sum_tbl_iv.csv",
  "outputs/tables/main/fs_cons.csv",
  "outputs/tables/main/cons_iv.csv",
  "outputs/figures/main/fig_ilo_trends.png",
  "outputs/figures/main/district_carveouts_shifts.png"
)
missing_required <- required_files[!file.exists(required_files) | file.info(required_files)$size <= 0]
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
    ame <- utils::read.csv(ame_path, stringsAsFactors = FALSE)
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

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Final output artifact audit failed.", call. = FALSE)
}

message("Final output artifact audit passed.")
