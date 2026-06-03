# Shared low-level helpers for raw readers, cleaning modules, and bridge code.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

need_pkg <- function(pkg, why = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required",
      if (!is.null(why)) paste0(" for ", why) else "",
      ". Run `make init-renv`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

safe_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(x)) {
    if (is.list(x[[nm]]) && !inherits(x[[nm]], "data.frame")) {
      x[[nm]] <- vapply(x[[nm]], function(z) paste(z, collapse = "; "), character(1))
    }
  }
  x
}

safe_bind_rows <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && length(x) > 0L, xs)
  xs <- lapply(xs, safe_df)
  xs <- Filter(function(x) length(names(x)) > 0L, xs)
  if (!length(xs)) return(data.frame())

  all_cols <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(all_cols, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[all_cols]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

canon <- function(x) {
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", tolower(gsub("&", " and ", as.character(x))))))
}

first_col <- function(df, candidates) {
  if (!length(names(df))) return(NULL)
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) return(hit[[1]])

  canon_names <- canon(names(df))
  canon_candidates <- canon(candidates)
  idx <- match(canon_candidates, canon_names, nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx)) names(df)[idx[[1]]] else NULL
}

num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

wmean <- function(x, w = NULL) {
  x <- num(x)
  if (is.null(w)) w <- rep(1, length(x)) else w <- num(w)
  ok <- is.finite(x) & is.finite(w) & w >= 0
  if (!any(ok) || sum(w[ok]) == 0) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

wgini <- function(x, w = NULL) {
  x <- num(x)
  if (is.null(w)) w <- rep(1, length(x)) else w <- num(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) < 2) return(NA_real_)

  order <- order(x)
  x <- x[order]
  w <- w[order]
  total_weight <- sum(w)
  mean_x <- sum(w * x) / total_weight
  if (!is.finite(mean_x) || mean_x == 0) return(NA_real_)

  sum(w * (2 * cumsum(w) - w - total_weight) * x) / (total_weight^2 * mean_x)
}
