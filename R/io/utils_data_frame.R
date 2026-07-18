# Shared low-level helpers for raw readers, cleaning modules, and measures.

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

empty_panel <- function() {
  data.frame(
    district_panel_id = character(),
    state_std = character(),
    district_std = character(),
    stringsAsFactors = FALSE
  )
}

std <- function(df, year) {
  df <- safe_df(df)
  s <- first_col(df, c("state", "STATE", "state_0708", "state_1718", "state_20", "stname", "ST_NM", "State", "state name", "Name of State", "state_name"))
  d <- first_col(df, c("district", "DISTRICT", "district_0708", "district_1718", "district_20", "dtname", "DT_NM", "District", "district name", "district_name", "Name of District"))
  if (!is.null(s)) df$state_std <- canonicalize_state_name(df[[s]])
  if (!is.null(d)) df$district_std <- canonicalize_district_name(df[[d]])
  df$source_year <- rep(as.integer(year), nrow(df))
  df
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

plain_chr <- function(x) {
  tryCatch(
    as.character(x),
    error = function(e) as.character(unclass(x))
  )
}

canon <- function(x) {
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", tolower(gsub("&", " and ", plain_chr(x))))))
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
  suppressWarnings(as.numeric(plain_chr(x)))
}

wmean <- function(x, w = NULL) {
  x <- num(x)
  if (is.null(w)) w <- rep(1, length(x)) else w <- num(w)
  ok <- is.finite(x) & is.finite(w) & w >= 0
  if (!any(ok) || sum(w[ok]) == 0) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

bydist <- function(df, value, weight = NULL, name = "value", fun = wmean) {
  g <- intersect(c("state_std", "district_std"), names(df))
  if (length(g) < 2 || is.null(value) || !nrow(df)) {
    return(data.frame(state_std = character(), district_std = character()))
  }
  split_i <- split(seq_len(nrow(df)), interaction(df[g], drop = TRUE))
  safe_bind_rows(lapply(split_i, function(i) {
    z <- df[i[1], g, drop = FALSE]
    z[[name]] <- fun(df[[value]][i], if (!is.null(weight)) df[[weight]][i] else NULL)
    z$n <- length(i)
    z
  }))
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

#' Stable NSS 2007 household key
#'
#' The household number is not globally unique, so combine it with the survey
#' state, first-stage unit, stratum, and substratum identifiers.
nss_2007_household_key <- function(df) {
  df <- safe_df(df)
  key_cols <- c("STATE", "FSU_SL_NO", "STRATUM", "SUB_STRATUM_NO", "HHID")
  for (nm in key_cols) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  do.call(paste, c(lapply(df[key_cols], function(x) canon(plain_chr(x))), sep = "__"))
}
