# Final-paper Appendix A exhibit builders.
#
# Appendix A is prose-led. These helpers retain only the compact lineage-source
# table and the adjacent-HCES consistency figure used by the manuscript; detailed
# measurement and validation files remain available as machine-readable outputs.

appendix_a3_lineage_source_hierarchy <- function(district_lineage) {
  if (!is.list(district_lineage)) {
    stop("District-lineage source summary requires registered district lineage output.", call. = FALSE)
  }
  registry <- safe_df(district_lineage$source_registry)
  if (!all(c("source_id", "citation") %in% names(registry)) || !nrow(registry)) {
    stop("District-lineage source summary requires the lineage source registry.", call. = FALSE)
  }

  source_families <- list(
    c("datameet_census_2001_districts", "census_registry_2001_2011_continuity",
      "nss64_education_district_codes", "nss75_official_district_list_census2011_exact"),
    c("lgd_districts", "lgd_mod_districts_2001_2011", "lgd_mod_districts"),
    "kumar_somanathan_2016",
    c("isded_1951_2024", "india_district_tracker"),
    c("shrug_pc_keys", "shrug_pc11_district_geometry"),
    c("concordance_plfs_nss", "concordance_census_plfs")
  )
  required_ids <- unique(unlist(source_families, use.names = FALSE))
  missing_ids <- setdiff(required_ids, plain_chr(registry$source_id))
  if (length(missing_ids)) {
    stop(
      "District-lineage source summary is missing registered evidence: ",
      paste(missing_ids, collapse = ", "),
      call. = FALSE
    )
  }

  csv <- data.frame(
    source_family = c(
      "Census and NSS district records",
      "Local Government Directory",
      "Kumar and Somanathan district histories",
      "India State Stories / District Changes Tracker",
      "SHRUG locality and district keys",
      "Published survey concordances"
    ),
    coverage = c(
      "Census 2001 and 2011; NSS 64th and 75th rounds",
      "2001 onward",
      "1961-2001",
      "1951 onward / 2001-2020",
      "Population Censuses 1991-2011",
      "Census, NSS, and PLFS district codes"
    ),
    role = c(
      "Define reference districts and resolve survey identities",
      "Verify district creation, renaming, and Census-code continuity",
      "Benchmark historical partitions and population transfers",
      "Cross-check district histories and predecessor relationships",
      "Measure locality-based overlap when later districts cross 2001 boundaries",
      "Cross-check survey-to-Census district links"
    ),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Source = csv$source_family, Coverage = csv$coverage, Role = csv$role,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

hces_cross_round_consistency_data <- function(welfare) {
  x <- safe_df(welfare)
  required <- c("district_2001", "round_id", "outcome_id", "estimate", "preferred_eligible")
  if (length(setdiff(required, names(x)))) stop("HCES cross-round consistency requires district welfare outputs.", call. = FALSE)
  outcomes <- c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce")
  x <- x[x$round_id %in% c("hces_2022_23", "hces_2023_24") & x$outcome_id %in% outcomes & x$preferred_eligible %in% TRUE, required, drop = FALSE]
  if (anyDuplicated(x[c("district_2001", "round_id", "outcome_id")])) {
    stop("HCES cross-round consistency requires unique district-round-outcome welfare estimates.", call. = FALSE)
  }
  rows <- lapply(outcomes, function(outcome) {
    z <- x[x$outcome_id == outcome, , drop = FALSE]
    a <- z[z$round_id == "hces_2022_23", c("district_2001", "estimate"), drop = FALSE]
    b <- z[z$round_id == "hces_2023_24", c("district_2001", "estimate"), drop = FALSE]
    names(a)[2] <- "estimate_2022_23"; names(b)[2] <- "estimate_2023_24"
    m <- merge(a, b, by = "district_2001", all = FALSE, sort = FALSE)
    if (nrow(m) < 2L) stop("HCES cross-round consistency requires common eligible districts in both HCES rounds.", call. = FALSE)
    m$outcome_id <- outcome
    m$pearson <- stats::cor(num(m$estimate_2022_23), num(m$estimate_2023_24))
    m
  })
  safe_bind_rows(rows)
}

appendix_consumption_hces_consistency_plot <- function(welfare) {
  need_pkg("ggplot2", "HCES cross-round consistency figure")
  d <- hces_cross_round_consistency_data(welfare)
  labels <- c(
    real_mean_mpce = "Real mean MPCE",
    mean_log_real_mpce = "Mean log real MPCE",
    weighted_median_real_mpce = "Weighted median real MPCE"
  )
  summary <- safe_bind_rows(lapply(names(labels), function(id) {
    x <- d[d$outcome_id == id, , drop = FALSE]
    data.frame(
      outcome_id = id, outcome = unname(labels[[id]]), n = nrow(x),
      pearson = stats::cor(num(x$estimate_2022_23), num(x$estimate_2023_24)),
      stringsAsFactors = FALSE
    )
  }))
  facet_labels <- setNames(
    sprintf("%s\nN = %d; r = %.3f", summary$outcome, summary$n, summary$pearson),
    summary$outcome_id
  )
  d$outcome <- unname(facet_labels[d$outcome_id])
  ggplot2::ggplot(d, ggplot2::aes(x = estimate_2022_23, y = estimate_2023_24)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.35, linetype = 2) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap(~ outcome, scales = "free", nrow = 1) +
    ggplot2::labs(x = "2022-23 estimate", y = "2023-24 estimate") +
    ggplot2::theme_minimal(base_size = 10)
}


make_appendix_data_construction_exhibits <- function(district_lineage, consumption_district_welfare) {
  list(
    appendix_a3_lineage_source_hierarchy = appendix_a3_lineage_source_hierarchy(district_lineage),
    appendix_consumption_hces_consistency = appendix_consumption_hces_consistency_plot(consumption_district_welfare)
  )
}

save_appendix_data_construction_exhibits <- function(exhibits, cfg) {
  if (!is.list(exhibits) || !all(c("appendix_a3_lineage_source_hierarchy", "appendix_consumption_hces_consistency") %in% names(exhibits))) {
    stop("Appendix A exhibit bundle is incomplete.", call. = FALSE)
  }
  written <- save_appendix_tables(exhibits, "appendix_a3_lineage_source_hierarchy", cfg)
  written <- c(
    written,
    save_plot_formats(
      exhibits$appendix_consumption_hces_consistency,
      appendix_figure_path_base("appendix_consumption_hces_consistency"),
      figure_formats(cfg), width = 8.2, height = 3.8
    )
  )
  unique(normalizePath(written, mustWork = FALSE))
}
