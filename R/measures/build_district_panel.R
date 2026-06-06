# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build district panel
#'
#' @return A district panel; an sf object when validated boundary geometry joins.
build_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg) {
  out <- safe_df(measures_2007)
  if (!nrow(out)) return(empty_panel())
  if (all(c("state_std", "district_std") %in% names(measures_2017))) {
    out <- merge(out, measures_2017, by = c("state_std", "district_std"), all.x = TRUE, suffixes = c("_2007", "_2017"))
  }
  if (all(c("state_std", "district_std") %in% names(linguistic_distance_iv))) {
    out <- merge(out, linguistic_distance_iv, by = c("state_std", "district_std"), all.x = TRUE)
  }
  if (!"district_panel_id" %in% names(out)) {
    out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  }
  out <- compute_consumption_growth_pct(out)
  out <- compute_log_consumption_difference(out)
  out <- compute_gini_change(out)
  attach_panel_geometry(out, boundaries_2020)
}

attach_panel_geometry <- function(panel, boundaries_2020) {
  if (!inherits(boundaries_2020, "sf")) return(panel)
  if (!all(c("state_std", "district_std") %in% names(panel)) ||
      !all(c("state_std", "district_std") %in% names(boundaries_2020))) {
    return(panel)
  }

  geom_col <- attr(boundaries_2020, "sf_column")
  boundary_keys <- boundaries_2020[c("state_std", "district_std", geom_col)]
  boundary_keys <- boundary_keys[!duplicated(as.data.frame(boundary_keys[c("state_std", "district_std")])), ]
  panel_key <- paste(normalize_panel_geometry_key(panel$state_std), normalize_panel_geometry_key(panel$district_std), sep = "\r")
  boundary_key <- paste(normalize_panel_geometry_key(boundary_keys$state_std), normalize_panel_geometry_key(boundary_keys$district_std), sep = "\r")
  boundary_index <- match(panel_key, boundary_key)
  boundary_geometry <- sf::st_geometry(boundary_keys)
  geometry <- lapply(boundary_index, function(i) {
    if (is.na(i)) sf::st_geometrycollection() else boundary_geometry[[i]]
  })

  out <- panel
  out[[geom_col]] <- sf::st_sfc(geometry, crs = sf::st_crs(boundaries_2020))
  sf::st_as_sf(out, sf_column_name = geom_col, crs = sf::st_crs(boundaries_2020))
}

normalize_panel_geometry_key <- function(x) {
  x <- trimws(as.character(x))
  numeric <- grepl("^[0-9]+$", x)
  x[numeric] <- as.character(as.integer(x[numeric]))
  canon(x)
}

#' compute consumption growth pct
#'
#' @return Function-specific return value.
compute_consumption_growth_pct <- function(df) {
  if (all(c("consumption_2007", "consumption_2017") %in% names(df))) {
    base <- num(df$consumption_2007)
    follow <- num(df$consumption_2017)
    df$consumption_growth_pct <- ifelse(is.finite(base) & base != 0, 100 * (follow - base) / base, NA_real_)
  }
  df
}

#' compute log consumption difference
#'
#' @return Function-specific return value.
compute_log_consumption_difference <- function(df) {
  if (all(c("consumption_2007", "consumption_2017") %in% names(df))) {
    base <- num(df$consumption_2007)
    follow <- num(df$consumption_2017)
    df$log_consumption_difference <- ifelse(base > 0 & follow > 0, log(follow) - log(base), NA_real_)
  }
  df
}

#' compute gini change
#'
#' @return Function-specific return value.
compute_gini_change <- function(df) {
  if (all(c("gini_consumption_2007", "gini_consumption_2017") %in% names(df))) {
    df$gini_change <- num(df$gini_consumption_2017) - num(df$gini_consumption_2007)
  }
  df
}

#' attach baseline controls
#'
#' @return Function-specific return value.
attach_baseline_controls <- function(df) {
  df
}

#' attach iv measures
#'
#' @return Function-specific return value.
attach_iv_measures <- function(df) {
  df
}

#' save processed district panel
#'
#' @return Function-specific return value.
save_processed_district_panel <- function(district_panel, path = "data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  out <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else as.data.frame(district_panel)
  utils::write.csv(out, path, row.names = FALSE)
  path
}

#' save processed district tracker
#'
#' @return Function-specific return value.
save_processed_district_tracker <- function(district_tracker, path = "data/processed/district_tracker_2001_2007_2017_2020.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(district_tracker), path, row.names = FALSE)
  path
}
