# Source adapter for documented SHRUG Economic Census district products.

read_shrug_ec05_district <- function(path) {
  raw <- read_shrug_district_archive(
    path,
    "ec05_pc01dist.csv",
    source = "SHRUG 2005 Economic Census district archive"
  )
  required <- c(
    "pc01_state_id", "pc01_district_id",
    "ec05_emp_all", "ec05_emp_f", "ec05_emp_hired", "ec05_emp_priv",
    "ec05_emp_inf", "ec05_emp_manuf", "ec05_emp_services", "ec05_count_all"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      "SHRUG EC05 district source is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data.frame(
    state_code = normalize_census_code(raw$pc01_state_id, 2L),
    district_code = normalize_census_code(raw$pc01_district_id, 2L),
    nonfarm_employment = num(raw$ec05_emp_all),
    female_employment = num(raw$ec05_emp_f),
    hired_employment = num(raw$ec05_emp_hired),
    private_employment = num(raw$ec05_emp_priv),
    informal_employment = num(raw$ec05_emp_inf),
    manufacturing_employment = num(raw$ec05_emp_manuf),
    services_employment = num(raw$ec05_emp_services),
    firms_total = num(raw$ec05_count_all),
    stringsAsFactors = FALSE
  )

  keys <- c("state_code", "district_code")
  if (any(!stats::complete.cases(out[keys])) || anyDuplicated(out[keys])) {
    stop("SHRUG EC05 district source must be unique by complete Census-2001 keys.", call. = FALSE)
  }
  count_columns <- setdiff(names(out), keys)
  invalid <- vapply(count_columns, function(column) {
    value <- num(out[[column]])
    any(!is.finite(value) | value < 0)
  }, logical(1))
  if (any(invalid)) {
    stop(
      "SHRUG EC05 district source has missing, non-finite, or negative core counts: ",
      paste(count_columns[invalid], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(out$nonfarm_employment <= 0) || any(out$firms_total <= 0)) {
    stop("SHRUG EC05 district source requires positive employment and firm denominators.", call. = FALSE)
  }
  out
}
