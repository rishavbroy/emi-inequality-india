# Final-paper migration and extended local-development summaries.
#
# This module only reshapes canonical diagnostic objects into publication exhibits.
# Estimation, sample construction, multiplicity adjustment, and geography remain
# owned by their domain modules.

appendix_d_mechanism_rows <- function(x, source_label, expected_rows = NULL) {
  result <- extract_posttreatment_mechanism_result(x)
  reduced <- safe_df(result$reduced_form)
  required <- c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect", "term",
    "estimate", "std.error", "p.value", "p_holm_within_spec", "n", "status"
  )
  missing <- setdiff(required, names(reduced))
  if (length(missing)) {
    stop("Local-development appendix table is missing fields: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(expected_rows) && nrow(reduced) != expected_rows) {
    stop("Local-development appendix expected ", expected_rows, " registered rows for ", source_label,
         " but found ", nrow(reduced), ".", call. = FALSE)
  }
  if (!nrow(reduced) || any(plain_chr(reduced$status) != "estimated") ||
      any(!is.finite(num(reduced$estimate))) || any(!is.finite(num(reduced$std.error))) ||
      any(!is.finite(num(reduced$p.value))) || any(!is.finite(num(reduced$p_holm_within_spec))) ||
      any(!is.finite(num(reduced$n)))) {
    stop("Local-development appendix requires complete estimated mechanism rows for ", source_label, ".", call. = FALSE)
  }
  reduced$source <- source_label
  reduced[, c("source", required), drop = FALSE]
}

appendix_d_mechanism_display <- function(csv) {
  adjustment <- c(region_main = "Region + controls", state_main = "State + controls")
  construction <- paper_linguistic_distance_display_labels()[c(
    "nonzero_mean", "glottolog_mean", "dyen_noncognate"
  )]
  data.frame(
    Outcome = gsub("_", " ", plain_chr(csv$outcome_id)),
    Adjustment = unname(adjustment[plain_chr(csv$adjustment_id)]),
    `Distance measure` = unname(construction[plain_chr(csv$construction_id)]),
    Estimate = sprintf("%+.4f", num(csv$estimate)),
    SE = sprintf("%.4f", num(csv$std.error)),
    `Raw p` = ifelse(num(csv$p.value) < 0.001, "<0.001", sprintf("%.3f", num(csv$p.value))),
    `Holm p` = ifelse(num(csv$p_holm_within_spec) < 0.001, "<0.001", sprintf("%.3f", num(csv$p_holm_within_spec))),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

appendix_migration_summary <- function(migration) {
  reduced <- appendix_d_mechanism_rows(migration, "Census migration", 48L)
  preferred_ids <- c(
    "interstate_migrant_composition",
    "work_migration_reason",
    "education_migration_reason",
    "outside_state_recent_work_migration",
    "skilled_recent_work_migration"
  )
  labels <- c(
    interstate_migrant_composition = "Interstate share among migrants",
    work_migration_reason = "Work/employment share among migrants",
    education_migration_reason = "Education share among migrants",
    outside_state_recent_work_migration = "Outside-state share among recent work migrants",
    skilled_recent_work_migration = "Graduate/technical-degree share among recent work migrants"
  )
  national <- reduced[
    plain_chr(reduced$outcome_id) %in% preferred_ids &
      plain_chr(reduced$adjustment_id) == "state_main" &
      plain_chr(reduced$construction_id) == "nonzero_mean",
    , drop = FALSE
  ]
  national <- national[match(preferred_ids, plain_chr(national$outcome_id)), , drop = FALSE]
  if (nrow(national) != length(preferred_ids) || anyNA(national$outcome_id)) {
    stop("Migration appendix requires the five registered preferred national comparisons.", call. = FALSE)
  }

  hindi <- safe_df(migration$hindi_belt_skilled_migration %||% data.frame())
  needed <- c(
    "outcome_id", "estimate", "std.error", "p.value", "n", "n_states",
    "adjustment_id", "construction_id", "fixed_effect", "status"
  )
  missing <- setdiff(needed, names(hindi))
  if (length(missing) || nrow(hindi) != 1L ||
      plain_chr(hindi$status)[[1L]] != "estimated" ||
      plain_chr(hindi$outcome_id)[[1L]] != "skilled_recent_work_migration") {
    stop("Migration appendix requires the registered Hindi-belt skilled-migration restriction.", call. = FALSE)
  }

  csv <- safe_bind_rows(list(
    data.frame(
      panel = c(rep("All migrants", 3L), rep("Recent work migrants", 2L)),
      sample = "All states",
      outcome_id = plain_chr(national$outcome_id),
      outcome = unname(labels[plain_chr(national$outcome_id)]),
      estimate = num(national$estimate),
      std.error = num(national$std.error),
      p.value = num(national$p.value),
      p.value_for_stars = num(national$p_holm_within_spec),
      p.value_basis = "Holm-adjusted within preferred migration family",
      n = as.integer(national$n),
      n_states = NA_integer_,
      adjustment_id = plain_chr(national$adjustment_id),
      construction_id = plain_chr(national$construction_id),
      fixed_effect = plain_chr(national$fixed_effect),
      stringsAsFactors = FALSE
    ),
    data.frame(
      panel = "Recent work migrants",
      sample = "Hindi-belt states",
      outcome_id = plain_chr(hindi$outcome_id),
      outcome = unname(labels[plain_chr(hindi$outcome_id)]),
      estimate = num(hindi$estimate),
      std.error = num(hindi$std.error),
      p.value = num(hindi$p.value),
      p.value_for_stars = num(hindi$p.value),
      p.value_basis = "Raw p-value for predeclared single restriction",
      n = as.integer(hindi$n),
      n_states = as.integer(hindi$n_states),
      adjustment_id = plain_chr(hindi$adjustment_id),
      construction_id = plain_chr(hindi$construction_id),
      fixed_effect = plain_chr(hindi$fixed_effect),
      stringsAsFactors = FALSE
    )
  ))
  if (any(!is.finite(csv$estimate)) || any(!is.finite(csv$std.error)) ||
      any(!is.finite(csv$p.value_for_stars)) || any(!is.finite(csv$n))) {
    stop("Migration appendix requires finite estimates, inference, and sample sizes.", call. = FALSE)
  }
  out <- data.frame(Term = "Linguistic distance from Hindi", stringsAsFactors = FALSE)
  attr(out, "csv_data") <- csv
  out
}

appendix_d3_housing_assets <- function(housing) {
  csv <- appendix_d_mechanism_rows(housing, "Census housing/assets", 48L)
  out <- appendix_d_mechanism_display(csv)
  attr(out, "csv_data") <- csv
  out
}

appendix_d4_economic_census <- function(economic_census) {
  csv <- appendix_d_mechanism_rows(economic_census, "Economic Census", 36L)
  out <- appendix_d_mechanism_display(csv)
  attr(out, "csv_data") <- csv
  out
}

appendix_d5_labor <- function(nss66, plfs, plfs_conservative) {
  csv <- safe_bind_rows(list(
    appendix_d_mechanism_rows(nss66, "NSS 2009-10", 12L),
    appendix_d_mechanism_rows(plfs, "PLFS 2017-18 primary", 12L),
    appendix_d_mechanism_rows(plfs_conservative, "PLFS 2017-18 conservative lineage", 12L)
  ))
  if (nrow(csv) != 36L) stop("Labor appendix table must reconcile to the registered 36-model labor family.", call. = FALSE)
  out <- cbind(Survey = csv$source, appendix_d_mechanism_display(csv))
  attr(out, "csv_data") <- csv
  out
}

appendix_d6_household_capacity <- function(household_capacity) {
  csv <- safe_df(household_capacity$estimates %||% data.frame())
  required <- c(
    "outcome_id", "predictor_id", "predictor_role", "estimand", "estimate",
    "std_error_state_clustered", "p_value_state_clustered",
    "p_value_holm_predictor_family", "n", "n_states", "status"
  )
  if (length(setdiff(required, names(csv))) || nrow(csv) != 8L ||
      any(plain_chr(csv$status) != "estimated")) {
    stop("Household-capacity appendix table requires the eight registered household-capacity trajectory models.", call. = FALSE)
  }
  out <- data.frame(
    Outcome = gsub("_", " ", plain_chr(csv$outcome_id)),
    Predictor = ifelse(plain_chr(csv$predictor_id) == "linguistic_opportunity", "Linguistic distance", "Observed all-child EMI"),
    Role = gsub("_", " ", plain_chr(csv$predictor_role)),
    Estimate = sprintf("%+.4f", num(csv$estimate)),
    SE = sprintf("%.4f", num(csv$std_error_state_clustered)),
    `Raw p` = sprintf("%.3f", num(csv$p_value_state_clustered)),
    `Holm p` = sprintf("%.3f", num(csv$p_value_holm_predictor_family)),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_d7_social_heterogeneity <- function(schooling_access, st_heterogeneity) {
  social <- safe_df(schooling_access$estimates %||% data.frame())
  st <- safe_df(st_heterogeneity$estimates %||% data.frame())
  social_required <- c("sample", "social_group", "outcome", "estimate", "std_error_state_clustered", "p_value_state_clustered", "p_value_holm_family", "n_districts")
  st_required <- c("sample", "outcome_id", "heterogeneity", "term", "estimate", "std_error_state_clustered", "p_value_state_clustered", "p_value_holm_family", "n_districts", "status")
  if (length(setdiff(social_required, names(social))) || !nrow(social) ||
      any(!is.finite(num(social$estimate))) || any(!is.finite(num(social$std_error_state_clustered))) ||
      any(!is.finite(num(social$p_value_state_clustered))) || any(!is.finite(num(social$p_value_holm_family))) ||
      any(!is.finite(num(social$n_districts)))) {
    stop("Heterogeneity appendix table requires the registered social-group distance-heterogeneity family.", call. = FALSE)
  }
  if (length(setdiff(st_required, names(st))) || !nrow(st) || any(plain_chr(st$status) != "estimated")) {
    stop("Heterogeneity appendix table requires the registered ST-concentration heterogeneity family.", call. = FALSE)
  }
  social_csv <- data.frame(
    family = "Social-group gap x distance", sample = plain_chr(social$sample),
    outcome = plain_chr(social$outcome), subgroup = plain_chr(social$social_group),
    design = "gap slope", estimate = num(social$estimate),
    std.error = num(social$std_error_state_clustered), p.value = num(social$p_value_state_clustered),
    p.value_holm = num(social$p_value_holm_family), n = as.integer(social$n_districts), stringsAsFactors = FALSE
  )
  st_csv <- data.frame(
    family = "ST concentration", sample = plain_chr(st$sample), outcome = plain_chr(st$outcome_id),
    subgroup = "ST concentration", design = plain_chr(st$heterogeneity), estimate = num(st$estimate),
    std.error = num(st$std_error_state_clustered), p.value = num(st$p_value_state_clustered),
    p.value_holm = num(st$p_value_holm_family), n = as.integer(st$n_districts), stringsAsFactors = FALSE
  )
  csv <- safe_bind_rows(list(social_csv, st_csv))
  out <- data.frame(
    Family = csv$family, Sample = gsub("_", " ", csv$sample),
    Outcome = gsub("_", " ", csv$outcome), Subgroup = csv$subgroup,
    Design = gsub("_", " ", csv$design), Estimate = sprintf("%+.4f", csv$estimate),
    SE = sprintf("%.4f", csv$std.error), `Holm p` = sprintf("%.3f", csv$p.value_holm),
    N = formatC(csv$n, format = "d", big.mark = ","), check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}


appendix_d8_raw_spatial_geography <- function(spatial) {
  x <- safe_df(spatial)
  ids <- c("linguistic_distance", "emie", "real_consumption_growth")
  labels <- c(
    linguistic_distance = "Linguistic distance",
    emie = "All-child EMI exposure",
    real_consumption_growth = "Real log consumption change"
  )
  map_names <- c(
    linguistic_distance = "map_linguistic_distance",
    emie = "map_emi_exposure",
    real_consumption_growth = "map_consumption_growth"
  )
  required <- c("estimand", "estimate", "p.value", "n", "contiguity", "weights_style", "status")
  if (length(setdiff(required, names(x)))) {
    stop("Raw-spatial appendix inputs are missing required Moran fields.", call. = FALSE)
  }
  x <- x[match(ids, plain_chr(x$estimand)), , drop = FALSE]
  if (nrow(x) != length(ids) || any(is.na(x$estimand)) ||
      any(plain_chr(x$status) != "estimated") ||
      any(!is.finite(num(x$estimate))) || any(!is.finite(num(x$p.value))) ||
      length(unique(as.integer(x$n))) != 1L ||
      length(unique(plain_chr(x$contiguity))) != 1L ||
      length(unique(plain_chr(x$weights_style))) != 1L) {
    stop("Raw-spatial appendix requires the three preferred raw Moran diagnostics on one spatial-support contract.", call. = FALSE)
  }
  data.frame(
    panel = c("A", "B", "C"),
    estimand = ids,
    label = unname(labels[ids]),
    map_name = unname(map_names[ids]),
    moran_i = num(x$estimate),
    p.value = num(x$p.value),
    n = as.integer(x$n),
    contiguity = plain_chr(x$contiguity),
    weights_style = plain_chr(x$weights_style),
    stringsAsFactors = FALSE
  )
}

appendix_d8_map_paths <- function(figure_files, map_names) {
  figure_files <- plain_chr(figure_files)
  wanted <- paste0(map_names, ".png")
  out <- vapply(wanted, function(file) {
    hits <- figure_files[basename(figure_files) == file & file.exists(figure_files)]
    if (length(hits) != 1L) {
      stop("Raw-spatial appendix requires exactly one rendered source map: ", file, ".", call. = FALSE)
    }
    hits[[1L]]
  }, character(1))
  unname(out)
}

appendix_d8_labeled_map_image <- function(path, panel, label, moran_i) {
  need_pkg("magick", "raw spatial map panel")
  image <- magick::image_read(path)
  image <- magick::image_background(image, "white", flatten = TRUE)
  image <- magick::image_scale(image, "800")
  info <- magick::image_info(image)
  image <- magick::image_extent(
    image,
    geometry = sprintf("%dx%d", info$width[[1L]], info$height[[1L]] + 90L),
    gravity = "south",
    color = "white"
  )
  magick::image_annotate(
    image,
    text = sprintf("%s. %s\nMoran's I = %.3f", panel, label, moran_i),
    gravity = "north",
    location = "+0+10",
    size = 24
  )
}

save_appendix_d8_raw_spatial_geography <- function(summary, figure_files, cfg) {
  if (!identical(cfg$mode, "final")) return(character())
  if (!is.data.frame(summary) || nrow(summary) != 3L) {
    stop("Raw-spatial appendix requires its three-row summary.", call. = FALSE)
  }
  paths <- appendix_d8_map_paths(figure_files, summary$map_name)
  images <- Map(
    appendix_d8_labeled_map_image,
    paths, summary$panel, summary$label, summary$moran_i
  )
  panel <- magick::image_append(magick::image_join(images), stack = FALSE)
  base <- appendix_figure_path_base("appendix_d8_raw_spatial_geography")
  formats <- figure_formats(cfg)
  written <- save_magick_formats(panel, base, formats)
  csv_path <- paste0(base, ".csv")
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, csv_path, row.names = FALSE, na = "")
  c(written, csv_path)
}

appendix_d9_residual_spatial_diagnostics <- function(spatial) {
  x <- safe_df(spatial)
  ids <- c("consumption_iv_residual", "consumption_first_stage_residual")
  x <- x[match(ids, plain_chr(x$estimand)), , drop = FALSE]
  required <- c("estimand", "estimate", "p.value", "n", "contiguity", "weights_style", "status")
  if (length(setdiff(required, names(x))) || nrow(x) != length(ids) || any(is.na(x$estimand)) ||
      any(plain_chr(x$status) != "estimated") || any(!is.finite(num(x$estimate))) || any(!is.finite(num(x$p.value)))) {
    stop("Residual-spatial appendix table requires the preferred first- and second-stage residual Moran diagnostics.", call. = FALSE)
  }
  labels <- c(
    consumption_iv_residual = "Consumption IV residual",
    consumption_first_stage_residual = "Consumption first-stage residual"
  )
  csv <- x[, required, drop = FALSE]
  out <- data.frame(
    Residual = unname(labels[plain_chr(csv$estimand)]),
    `Moran I` = sprintf("%+.4f", num(csv$estimate)),
    `p-value` = sprintf("%.3f", num(csv$p.value)),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    Contiguity = plain_chr(csv$contiguity), `Weight style` = plain_chr(csv$weights_style),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_local_development_exhibits <- function(
    migration, housing, economic_census, nss66_labor, plfs_labor,
    plfs_conservative_labor, household_capacity, schooling_access,
    st_heterogeneity, spatial_autocorrelation) {
  list(
    appendix_migration_summary = appendix_migration_summary(migration),
    appendix_d3_housing_assets = appendix_d3_housing_assets(housing),
    appendix_d4_economic_census = appendix_d4_economic_census(economic_census),
    appendix_d5_labor = appendix_d5_labor(nss66_labor, plfs_labor, plfs_conservative_labor),
    appendix_d6_household_capacity = appendix_d6_household_capacity(household_capacity),
    appendix_d7_social_heterogeneity = appendix_d7_social_heterogeneity(schooling_access, st_heterogeneity),
    appendix_d8_raw_spatial_geography = appendix_d8_raw_spatial_geography(spatial_autocorrelation),
    appendix_d9_residual_spatial_diagnostics = appendix_d9_residual_spatial_diagnostics(spatial_autocorrelation)
  )
}

save_appendix_local_development_exhibits <- function(exhibits, figure_files, cfg) {
  written <- save_appendix_tables(
    exhibits,
    c(
      "appendix_migration_summary", "appendix_d3_housing_assets", "appendix_d4_economic_census",
      "appendix_d5_labor", "appendix_d6_household_capacity",
      "appendix_d7_social_heterogeneity", "appendix_d9_residual_spatial_diagnostics"
    ),
    cfg
  )
  written <- c(
    written,
    save_appendix_d8_raw_spatial_geography(
      exhibits$appendix_d8_raw_spatial_geography, figure_files, cfg
    )
  )
  unique(normalizePath(written, mustWork = FALSE))
}
