# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' Read project configuration
read_config <- function(path = Sys.getenv("EMI_CONFIG", "config/fast.yml")) {
  cfg <- yaml::read_yaml(path)
  cfg$.config_path <- path
  validate_config(cfg)
  cfg
}

#' Validate project configuration
validate_config <- function(cfg) {
  required <- c(
    "mode", "run_full_ame", "strict_district_panel_validation",
    "strict_analysis_panel_validation", "run_diagnostics", "output_formats",
    "overidentification"
  )
  missing <- setdiff(required, names(cfg))
  if (length(missing)) {
    stop("Config is missing required fields: ", paste(missing, collapse = ", "))
  }
  if (!cfg$mode %in% c("fast", "final")) stop("Unknown config mode: ", cfg$mode)
  invisible(TRUE)
}

#' Is a named within-analysis diagnostic enabled?
diagnostic_enabled <- function(cfg, name) {
  isTRUE(cfg$run_diagnostics[[name]])
}

#' Is the active configuration the final research specification?
is_final_mode <- function(cfg) {
  identical(cfg$mode, "final")
}
