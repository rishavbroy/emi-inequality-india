# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' read config
#'
#' @return Function-specific return value.
read_config <- function(path = Sys.getenv("EMI_CONFIG", "config/draft.yml")) {
  cfg <- yaml::read_yaml(path)
  cfg$.config_path <- path
  validate_config(cfg)
  cfg
}

#' validate config
#'
#' @return Function-specific return value.
validate_config <- function(cfg) {
  required <- c("mode", "run_full_ame", "run_diagnostics", "sample_rows", "output_formats")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) stop("Config is missing required fields: ", paste(missing, collapse = ", "))
  if (!cfg$mode %in% c("draft", "final", "diagnostics")) stop("Unknown config mode: ", cfg$mode)
  invisible(TRUE)
}

#' cfg get
#'
#' @return Function-specific return value.
cfg_get <- function(cfg, key, default = NULL) {
  if (!key %in% names(cfg)) return(default)
  cfg[[key]]
}

#' diagnostic enabled
#'
#' @return Function-specific return value.
diagnostic_enabled <- function(cfg, name) {
  isTRUE(cfg$run_diagnostics[[name]])
}

#' is final mode
#'
#' @return Function-specific return value.
is_final_mode <- function(cfg) {
  identical(cfg$mode, "final")
}

#' is diagnostics mode
#'
#' @return Function-specific return value.
is_diagnostics_mode <- function(cfg) {
  identical(cfg$mode, "diagnostics")
}

