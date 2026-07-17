# Spatial diagnostic helpers for public report values.

spatial_p_value <- function(diag, legacy_name = NULL, pattern = NULL) {
  x <- as_plain_data_frame(diag)
  if (!nrow(x)) return(NA_real_)
  if ("status" %in% names(x) && any(x$status == "estimated", na.rm = TRUE)) {
    x <- x[x$status == "estimated", , drop = FALSE]
  }
  if (!is.null(legacy_name) && "legacy_name" %in% names(x)) {
    exact <- x[as.character(x$legacy_name) == legacy_name, , drop = FALSE]
    out <- first_available_number(exact, c("p.value", "p_value", "p", "pval"))
    if (is.finite(out)) return(signif(out, 3))
  }
  if (!is.null(pattern)) {
    text_cols <- intersect(c("legacy_name", "estimand", "model", "target", "outcome", "name", "source", "variable", "test"), names(x))
    if (length(text_cols)) {
      keep <- Reduce(`|`, lapply(text_cols, function(col) grepl(pattern, x[[col]], ignore.case = TRUE)))
      if (any(keep)) x <- x[keep, , drop = FALSE]
    }
  }
  out <- first_available_number(x, c("p.value", "p_value", "p", "pval"))
  if (is.finite(out)) signif(out, 3) else NA_real_
}
