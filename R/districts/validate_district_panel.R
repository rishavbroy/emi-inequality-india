# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' validate district panel
#'
validate_district_panel <- function(panel, join_map = NULL, strict = FALSE) {
  issues <- safe_bind_rows(list(
    check_unique_district_units(panel),
    check_no_unintended_many_to_many(panel, join_map = join_map),
    check_core_variables_present(panel),
    check_panel_variable_ranges(panel)
  ))
  attr(panel, "district_panel_validation") <- issues
  if (nrow(issues) && isTRUE(strict)) {
    stop(paste(issues$message, collapse = "\n"), call. = FALSE)
  }
  panel
}

validation_issue <- function(check, severity, message, n = NA_integer_) {
  data.frame(check = check, severity = severity, message = message, n = n, stringsAsFactors = FALSE)
}

#' check unique district units
#'
check_unique_district_units <- function(panel) {
  panel <- as.data.frame(if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else panel, stringsAsFactors = FALSE)
  if (!nrow(panel) || !"district_panel_id" %in% names(panel)) return(data.frame())
  dup <- duplicated(panel$district_panel_id) | duplicated(panel$district_panel_id, fromLast = TRUE)
  if (any(dup, na.rm = TRUE)) {
    return(validation_issue("unique_district_units", "error", "district_panel_id is not unique.", sum(dup, na.rm = TRUE)))
  }
  data.frame()
}

#' check no unintended many to many
#'
check_no_unintended_many_to_many <- function(panel, join_map = NULL) {
  panel <- as.data.frame(if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else panel, stringsAsFactors = FALSE)
  issues <- list()
  key_sets <- list(
    source_2001 = c("state_01", "district_01"),
    source_2007 = c("state_07", "district_07"),
    source_2008 = c("state_08", "district_08"),
    source_2017 = c("state_17", "district_17"),
    source_2018 = c("state_18", "district_18"),
    source_2020 = c("state_20", "district_20")
  )
  for (nm in names(key_sets)) {
    cols <- key_sets[[nm]]
    if (!all(cols %in% names(panel))) next
    key <- paste(canon(panel[[cols[[1]]]]), canon(panel[[cols[[2]]]]), sep = "\r")
    key <- key[!is.na(key) & nzchar(key) & key != "__"]
    dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(dup, na.rm = TRUE)) {
      issues[[length(issues) + 1L]] <- validation_issue("many_to_many", "warning", paste0(nm, " has duplicated state/district keys; verify these are documented split/merge rows."), sum(dup, na.rm = TRUE))
    }
  }
  join_map <- safe_df(join_map %||% data.frame())
  if (nrow(join_map) && "many_to_many" %in% names(join_map)) {
    many <- !is.na(join_map$many_to_many) & join_map$many_to_many %in% TRUE
    allowed <- if ("many_to_many_allowed" %in% names(join_map)) join_map$many_to_many_allowed %in% TRUE else rep(FALSE, nrow(join_map))
    bad <- many & !allowed
    if (any(bad, na.rm = TRUE)) {
      issues[[length(issues) + 1L]] <- validation_issue("many_to_many", "error", "join_map contains unintended many-to-many matches.", sum(bad, na.rm = TRUE))
    }
  }
  safe_bind_rows(issues)
}

#' check core variables present
#'
check_core_variables_present <- function(panel) {
  required <- c("EMIE", "wavg_ling_degrees")
  missing <- setdiff(required, names(panel))
  if (length(missing)) {
    return(validation_issue("core_variables_present", "warning", paste("Panel missing variables:", paste(missing, collapse = ", ")), length(missing)))
  }
  data.frame()
}

#' check panel variable ranges
#'
check_panel_variable_ranges <- function(panel) {
  panel <- as.data.frame(if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else panel, stringsAsFactors = FALSE)
  issues <- list()
  if ("EMIE" %in% names(panel)) {
    bad <- is.finite(num(panel$EMIE)) & (num(panel$EMIE) < 0 | num(panel$EMIE) > 100)
    if (any(bad, na.rm = TRUE)) issues[[length(issues) + 1L]] <- validation_issue("panel_variable_ranges", "error", "EMIE must be on a 0-100 percentage scale.", sum(bad, na.rm = TRUE))
  }
  if ("dependency_ratio" %in% names(panel)) {
    bad <- is.finite(num(panel$dependency_ratio)) & num(panel$dependency_ratio) < 0
    if (any(bad, na.rm = TRUE)) issues[[length(issues) + 1L]] <- validation_issue("panel_variable_ranges", "error", "dependency_ratio cannot be negative.", sum(bad, na.rm = TRUE))
  }
  safe_bind_rows(issues)
}
