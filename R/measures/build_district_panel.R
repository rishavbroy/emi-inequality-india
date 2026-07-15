# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' build district panel
#'
#' @return A district panel; an sf object when validated boundary geometry joins.
build_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg) {
  tracker <- legacy_tracker_frame(district_tracker)
  if (nrow(tracker) && legacy_named_measures_available(measures_2007, measures_2017, linguistic_distance_iv)) {
    out <- build_tracker_based_district_panel(tracker, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020)
    if (nrow(out)) return(validate_legacy_district_panel(out, cfg, join_map = district_join_map))
  }

  # Fallback for tests and draft diagnostics when named tracker inputs are not
  # available.  This preserves the previous key merge but still emits legacy
  # aliases expected by the report/table code.
  out <- safe_df(measures_2007)
  if (!nrow(out)) return(empty_panel())
  if (all(c("state_std", "district_std") %in% names(measures_2017))) {
    out <- merge(out, measures_2017, by = c("state_std", "district_std"), all.x = TRUE, suffixes = c("_2007", "_2017"))
  }
  if (all(c("state_std", "district_std") %in% names(linguistic_distance_iv))) {
    out <- merge(out, linguistic_distance_iv, by = c("state_std", "district_std"), all.x = TRUE)
  }
  out <- add_legacy_panel_aliases(out)
  out <- add_legacy_regions(out)
  if (!"district_panel_id" %in% names(out)) out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  out <- compute_consumption_growth_pct(out)
  out <- compute_log_consumption_difference(out)
  out <- compute_gini_change(out)
  validate_legacy_district_panel(attach_panel_geometry(out, boundaries_2020), cfg, join_map = district_join_map)
}

legacy_tracker_frame <- function(district_tracker) {
  tracker <- safe_df(district_tracker)
  if (all(c("state_01", "district_01", "state_07", "district_07", "state_17", "district_17", "state_20", "district_20") %in% names(tracker))) {
    return(tracker)
  }
  path <- "data/processed/district_tracker_legacy.csv"
  if (file.exists(path)) return(utils::read.csv(path, stringsAsFactors = FALSE))
  data.frame()
}

legacy_named_measures_available <- function(...) {
  dfs <- list(...)
  any(vapply(dfs, function(df) any(grepl("^state_(01|07|08|17|18)$", names(safe_df(df)))), logical(1)))
}

legacy_panel_has_analysis_core <- function(out) {
  required <- c("EMIE", "wavg_ling_degrees", "consumption_0708", "consumption_1718")
  present <- intersect(required, names(out))
  if (!length(present)) return(rep(TRUE, nrow(out)))
  vals <- lapply(out[present], function(x) {
    if (is.list(x)) {
      vapply(x, function(value) {
        if (length(value) == 0L || all(is.na(value))) NA_character_ else paste(as.character(value), collapse = "; ")
      }, character(1))
    } else {
      x
    }
  })
  stats::complete.cases(as.data.frame(vals, stringsAsFactors = FALSE))
}

build_tracker_based_district_panel <- function(tracker, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020) {
  tracker$.tracker_row <- seq_len(nrow(tracker))
  out <- tracker
  out <- legacy_attach_source(out, safe_df(linguistic_distance_iv), legacy_suffix_chain("01"), source_label = "2001")
  out <- legacy_attach_source_one_to_one(out, safe_df(measures_2007), legacy_suffix_chain("08"), source_label = "2007")
  out <- legacy_attach_source(out, safe_df(measures_2017), legacy_suffix_chain("18"), source_label = "2017")

  # Some rebuilt measure targets only expose numeric standardized district keys
  # (state_std/district_std) and no longer carry the legacy name columns used by
  # the tracker.  Attach those sources after the 2007 join has supplied the
  # panel's standardized district keys; otherwise the panel silently lacks the
  # 2017 outcome and 2001 instrument required by maps and IV models.
  out <- legacy_attach_source_by_standard_keys(out, safe_df(linguistic_distance_iv), source_label = "2001")
  out <- legacy_attach_source_by_standard_keys(out, safe_df(measures_2017), source_label = "2017")

  out <- add_legacy_panel_aliases(out)
  out <- add_legacy_regions(out)
  out <- compute_consumption_growth_pct(out)
  out <- compute_log_consumption_difference(out)
  out <- compute_gini_change(out)
  if (!"district_panel_id" %in% names(out)) out$district_panel_id <- paste0("legacy_tracker_", out$.tracker_row)
  out <- attach_panel_geometry(out, boundaries_2020)
  out <- out[legacy_panel_has_analysis_core(out), , drop = FALSE]
  rownames(out) <- NULL
  out
}


add_legacy_panel_aliases <- function(out) {
  alias <- function(new, old) if (old %in% names(out) && !new %in% names(out)) out[[new]] <<- out[[old]]
  alias("EMIE", "emie_2007")
  alias("emie_2007", "EMIE")
  alias("consumption_0708", "consumption_2007")
  alias("consumption_2007", "consumption_0708")
  alias("gini_cons_0708", "gini_consumption_2007")
  alias("gini_consumption_2007", "gini_cons_0708")
  alias("consumption_1718", "consumption_2017")
  alias("consumption_2017", "consumption_1718")
  alias("gini_cons_1718", "gini_consumption_2017")
  alias("gini_consumption_2017", "gini_cons_1718")
  alias("pucca_share_2007", "pct_pucca")
  alias("pct_pucca", "pucca_share_2007")
  alias("head_secondary_plus_2007", "pct_head_secondary_plus")
  alias("npeople_0708", "n_2007")
  alias("n_2007", "npeople_0708")
  alias("npeople_1718", "n_2017")
  alias("n_2017", "npeople_1718")
  if (!"nhouses_0708" %in% names(out) && "n_households_2007" %in% names(out)) out$nhouses_0708 <- out$n_households_2007
  if (!"nhouses_1718" %in% names(out) && "n_households_2017" %in% names(out)) out$nhouses_1718 <- out$n_households_2017
  if (!"state_std" %in% names(out) && "state_20" %in% names(out)) out$state_std <- canonicalize_state_name(out$state_20)
  if (!"district_std" %in% names(out) && "district_20" %in% names(out)) out$district_std <- canonicalize_district_name(out$district_20)
  out
}

add_legacy_regions <- function(out) {
  valid_regions <- c("North", "Central", "East", "West", "South")
  if ("region" %in% names(out)) {
    current <- as.character(out$region)
    current_nonmissing <- stats::na.omit(current)
    if (length(current_nonmissing) && all(current_nonmissing %in% valid_regions)) return(out)
  }

  state <- NULL
  for (nm in c("state_20", "state_17", "state_07", "state_01", "state_std")) {
    if (nm %in% names(out)) { state <- canon(out[[nm]]); break }
  }
  if (is.null(state)) return(out)
  north <- canon(c("Jammu & Kashmir", "Himachal Pradesh", "Punjab", "Chandigarh", "Uttarakhand", "Haryana", "Delhi", "Rajasthan"))
  central <- canon(c("Uttar Pradesh", "Chhattisgarh", "Madhya Pradesh"))
  east <- canon(c("Bihar", "Sikkim", "Arunachal Pradesh", "Nagaland", "Manipur", "Mizoram", "Tripura", "Meghalaya", "Assam", "West Bengal", "Jharkhand", "Odisha"))
  west <- canon(c("Gujarat", "Daman & Diu", "Dadra & Nagar Haveli", "Maharashtra", "Goa"))
  south <- canon(c("Andhra Pradesh", "Karnataka", "Lakshadweep", "Kerala", "Tamil Nadu", "Puducherry", "Andaman & Nicobar Islands", "Telangana"))
  out$region <- ifelse(state %in% north, "North",
    ifelse(state %in% central, "Central",
      ifelse(state %in% east, "East",
        ifelse(state %in% west, "West",
          ifelse(state %in% south, "South", NA_character_)))))
  out$region <- factor(out$region, levels = valid_regions)
  out
}

compute_consumption_growth_pct <- function(panel) {
  if (all(c("consumption_1718", "consumption_0708") %in% names(panel))) {
    growth <- (num(panel$consumption_1718) - num(panel$consumption_0708)) / num(panel$consumption_0708) * 100
    panel$consumption_pct_change <- growth
    panel$consumption_growth_pct <- growth
  } else if (all(c("consumption_2017", "consumption_2007") %in% names(panel))) {
    growth <- (num(panel$consumption_2017) - num(panel$consumption_2007)) / num(panel$consumption_2007) * 100
    panel$consumption_growth_pct <- growth
    panel$consumption_pct_change <- growth
  }
  panel
}

compute_log_consumption_difference <- function(panel) {
  if (all(c("consumption_1718", "consumption_0708") %in% names(panel))) {
    panel$log_consumption_difference <- log(num(panel$consumption_1718)) - log(num(panel$consumption_0708))
  } else if (all(c("consumption_2017", "consumption_2007") %in% names(panel))) {
    panel$log_consumption_difference <- log(num(panel$consumption_2017)) - log(num(panel$consumption_2007))
  }
  panel
}

compute_gini_change <- function(panel) {
  if (all(c("gini_cons_1718", "gini_cons_0708") %in% names(panel))) {
    panel$gini_change <- num(panel$gini_cons_1718) - num(panel$gini_cons_0708)
  } else if (all(c("gini_consumption_2017", "gini_consumption_2007") %in% names(panel))) {
    panel$gini_change <- num(panel$gini_consumption_2017) - num(panel$gini_consumption_2007)
  }
  panel
}

legacy_panel_validation_failures <- function(out) {
  df <- as.data.frame(out)
  failures <- character()
  add <- function(...) failures <<- c(failures, paste0(...))

  required <- c("EMIE", "wavg_ling_degrees", "npeople_0708", "consumption_0708", "gini_cons_0708", "consumption_1718", "gini_cons_1718", "consumption_pct_change", "gini_change")
  missing <- setdiff(required, names(df))
  if (length(missing)) add("district_panel is missing required legacy columns: ", paste(missing, collapse = ", "))

  present_required <- intersect(required, names(df))
  if (length(present_required)) {
    incomplete <- !stats::complete.cases(df[present_required])
    if (any(incomplete)) add("district_panel has ", sum(incomplete), " rows with missing core IV analysis values.")
  }

  if ("district_panel_id" %in% names(df) && anyDuplicated(df$district_panel_id)) {
    add("district_panel_id is not unique after tracker/source matching.")
  }

  for (flag in c(".matched_2001", ".matched_2007", ".matched_2017")) {
    if (flag %in% names(df) && any(!isTRUEish(df[[flag]]), na.rm = TRUE)) {
      add("district_panel contains rows without ", flag, " after final core filtering.")
    }
  }

  if ("EMIE" %in% names(df)) {
    emie <- num(df$EMIE)
    if (mean(emie, na.rm = TRUE) < 10 || max(emie, na.rm = TRUE) < 90) add("EMIE scale is inconsistent with legacy 0-100 percentage scale.")
  }
  if ("npeople_0708" %in% names(df) && mean(num(df$npeople_0708), na.rm = TRUE) < 10000) {
    add("npeople_0708 looks like a sample count rather than weighted population.")
  }
  if ("dependency_ratio" %in% names(df) && mean(num(df$dependency_ratio), na.rm = TRUE) > 80) {
    add("dependency_ratio mean is implausibly high relative to the legacy table; verify weighted numerator/denominator construction.")
  }
  failures
}

isTRUEish <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true", "1")
}

validate_legacy_district_panel <- function(out, cfg = list(), join_map = NULL, strict = isTRUE(cfg$strict_legacy_panel_validation)) {
  out <- validate_district_panel(out, join_map = join_map, strict = isTRUE(cfg$strict_district_panel_validation))
  if (!identical(cfg$mode, "final")) return(out)
  failures <- legacy_panel_validation_failures(out)
  attr(out, "legacy_panel_validation_failures") <- failures
  if (length(failures) && isTRUE(strict)) {
    stop(paste(failures, collapse = "\n"), call. = FALSE)
  }
  out
}
