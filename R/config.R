# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' read config
#'
#' @return A tibble, model object, list, or file path depending on context.
read_config <- function(path = Sys.getenv("EMI_CONFIG", "config/draft.yml")) {
  cfg <- yaml::read_yaml(path)
  cfg$.config_path <- path
  validate_config(cfg)
  cfg
}

#' validate config
#'
#' @return A tibble, model object, list, or file path depending on context.
validate_config <- function(cfg) {
  required <- c("mode", "run_full_ame", "run_diagnostics", "sample_rows", "output_formats")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) stop("Config is missing required fields: ", paste(missing, collapse = ", "))
  if (!cfg$mode %in% c("draft", "final", "diagnostics")) stop("Unknown config mode: ", cfg$mode)
  invisible(TRUE)
}

#' cfg get
#'
#' @return A tibble, model object, list, or file path depending on context.
cfg_get <- function(cfg, key, default = NULL) {
  if (!key %in% names(cfg)) return(default)
  cfg[[key]]
}

#' diagnostic enabled
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnostic_enabled <- function(cfg, name) {
  isTRUE(cfg$run_diagnostics[[name]])
}

#' is final mode
#'
#' @return A tibble, model object, list, or file path depending on context.
is_final_mode <- function(cfg) {
  identical(cfg$mode, "final")
}

#' is diagnostics mode
#'
#' @return A tibble, model object, list, or file path depending on context.
is_diagnostics_mode <- function(cfg) {
  identical(cfg$mode, "diagnostics")
}

