# Core helpers for named public report values.

report_value_status <- function(reason, status = "unavailable_in_draft", value = NA, display = "—") {
  list(status = status, reason = reason, value = value, display = display)
}

is_report_value_status <- function(value) {
  is.list(value) && !is.null(value$status) && !is.null(value$reason)
}

value_or_status <- function(value, reason) {
  if (length(value) == 0L || all(is.na(value))) report_value_status(reason) else value
}

as_plain_data_frame <- function(x) {
  tryCatch(as.data.frame(x), error = function(e) data.frame())
}

first_available_number <- function(x, candidates) {
  if (is.null(x)) return(NA_real_)
  x <- as_plain_data_frame(x)
  hit <- intersect(candidates, names(x))
  if (!length(hit) || !nrow(x)) return(NA_real_)
  suppressWarnings(as.numeric(x[[hit[[1]]]][[1]]))
}

set_report_value <- function(values, name, value, reason) {
  values[[name]] <- value_or_status(value, reason)
  values
}

format_report_value <- function(value, fun) {
  if (is_report_value_status(value)) return(value)
  fun(value)
}
