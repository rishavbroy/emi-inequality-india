# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' read config
#'
read_config <- function(path = Sys.getenv("EMI_CONFIG", "config/draft.yml")) {
  cfg <- yaml::read_yaml(path)
  cfg$.config_path <- path
  validate_config(cfg)
  cfg
}

#' validate config
#'
validate_config <- function(cfg) {
  required <- c("mode", "run_full_ame", "run_diagnostics", "sample_rows", "output_formats")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) stop("Config is missing required fields: ", paste(missing, collapse = ", "))
  if (!cfg$mode %in% c("draft", "final", "diagnostics")) stop("Unknown config mode: ", cfg$mode)
  invisible(TRUE)
}

#' cfg get
#'
cfg_get <- function(cfg, key, default = NULL) {
  if (!key %in% names(cfg)) return(default)
  cfg[[key]]
}

#' diagnostic enabled
#'
diagnostic_enabled <- function(cfg, name) {
  isTRUE(cfg$run_diagnostics[[name]])
}

#' is final mode
#'
is_final_mode <- function(cfg) {
  identical(cfg$mode, "final")
}

#' is diagnostics mode
#'
is_diagnostics_mode <- function(cfg) {
  identical(cfg$mode, "diagnostics")
}

