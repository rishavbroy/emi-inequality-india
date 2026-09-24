poster_map_fixture <- function(n = 2L) {
  out <- data.frame(
    EMIE = seq_len(n),
    emi_exposure_all_children_0708 = seq_len(n),
    ling_distance_nonzero_mean = seq_len(n),
    real_log_consumption_change = seq_len(n) + 1,
    pct_pucca = seq_len(n) + 2,
    pct_head_secondary_plus = seq_len(n) + 3,
    region = rep("Northern", n),
    state_code_2001 = sprintf("%02d", seq_len(n)),
    wavg_ling_degrees = seq_len(n) + 4,
    emi_share_enrolled_0708 = seq_len(n),
    public_emi_exposure_all_children_0708 = seq_len(n),
    private_emi_exposure_all_children_0708 = seq_len(n),
    stringsAsFactors = FALSE
  )
  for (v in census_2001_absorption_controls()) out[[v]] <- seq_len(n)
  out
}

test_that("poster residualization resolves factor terms to source columns", {
  panel <- poster_map_fixture(20L)
  panel$region <- rep(panel_region_levels()[1:2], each = 10L)
  panel$emi_exposure_all_children_0708 <- seq_len(20L)
  panel$ling_distance_nonzero_mean <- rev(seq_len(20L))

  residuals <- poster_residual_pair(panel, fixed_effect = "region")

  expect_equal(dim(residuals), c(20L, 2L))
  expect_true(all(is.finite(residuals)))
  expect_identical(
    colnames(residuals),
    c("emi_exposure_all_children_0708", "ling_distance_nonzero_mean")
  )
})


test_that("final figures degrade to status specs without real sf geometry", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- poster_map_fixture(1L)

  figures <- make_figures(panel, character(), cfg)
  expect_identical(figures$map_emi_exposure$kind, "status")
  expect_true(any(grepl("Geometry coverage", attr(figures, "map_input_failures"), fixed = TRUE)))
})

test_that("district carve-out figure data uses pct_91in01 values", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    'Anantapur,"3,183,814",Anantapur,74.5,75.5',
    ',,Sri Sathya Sai,25.5,24.5'
  ), path)

  carveouts <- read_carveout_shift_data(path)

  expect_equal(nrow(carveouts), 2L)
  expect_true("pct_91in01" %in% names(carveouts))
  expect_equal(carveouts$pct_91in01, c(75.5, 24.5))
})

test_that("public-map regions use the six-region RBI classification", {
  panel <- data.frame(
    state_20 = c(
      "Punjab", "Assam", "Uttaranchal", "Orissa", "Maharashtra",
      "Pondicherry", "Delhi", "Andaman & Nicobar Islands"
    ),
    region = seq_len(8),
    stringsAsFactors = FALSE
  )

  out <- add_panel_regions(panel)

  expect_equal(
    as.character(out$region),
    c("Northern", "North Eastern", "Central", "Eastern", "Western", "Southern", "Northern", "Eastern")
  )
  expect_identical(levels(out$region), panel_region_levels())
})


test_that("RBI region crosswalk covers every Census-2001 state and union territory", {
  states <- census_2001_state_name(sprintf("%02d", 1:35))
  mapped <- add_panel_regions(data.frame(state_01 = states, stringsAsFactors = FALSE))

  expect_false(anyNA(mapped$region))
  expect_setequal(as.character(unique(mapped$region)), panel_region_levels())
  expect_equal(anyDuplicated(panel_state_region_crosswalk()$state_key), 0L)
})


test_that("public maps retain the production panel's Census-2001 geometry and attributes", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 4326
  )
  panel <- sf::st_sf(
    target_unit_2001 = "pc2001__01__01",
    emi_exposure_all_children_0708 = 10,
    geometry = geometry
  )
  spec <- figure_spec(
    "map_emi_exposure",
    "map_emi_exposure.png",
    "EMI Exposure",
    kind = "map",
    variable = "emi_exposure_all_children_0708"
  )

  fill <- public_map_fill(panel, "emi_exposure_all_children_0708", public_map_style("emi_exposure_all_children_0708"))
  expect_s3_class(fill$data, "sf")
  expect_identical(fill$data$target_unit_2001, panel$target_unit_2001)
  expect_equal(sum(map_overlay_rows(fill$data, ".map_fill")), 1L)
  expect_equal(sf::st_geometry(fill$data), sf::st_geometry(panel))
})

test_that("public map rendering refuses all-grey data layers", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 4326
  )
  panel <- sf::st_sf(
    state_20 = "A",
    district_20 = "missing",
    emi_exposure_all_children_0708 = NA_real_,
    geometry = geometry
  )
  spec <- figure_spec("map_emi_exposure", "map_emi_exposure.png", "EMI Exposure", kind = "map", variable = "emi_exposure_all_children_0708")

  expect_error(
    build_public_ggplot_map(panel, spec),
    "no non-missing overlay districts",
    fixed = TRUE
  )
})


test_that("continuous and diverging map styles keep numeric fills", {
  df <- data.frame(real_log_consumption_change = c(-0.5, 0, 0.5, NA))
  fill <- public_map_fill(df, "real_log_consumption_change", public_map_style("real_log_consumption_change"))

  expect_true(isTRUE(fill$continuous))
  expect_true(is.numeric(fill$data$.map_value))

  residual <- data.frame(resid_emi_exposure_region_expanded = c(-2, 0, 3, NA))
  resid_fill <- public_map_fill(residual, "resid_emi_exposure_region_expanded", public_map_style("resid_emi_exposure_region_expanded"))
  expect_true(isTRUE(resid_fill$continuous))
  expect_equal(resid_fill$limits[1], -resid_fill$limits[2])
})



test_that("figure residual variables use their registered adjustment sets", {
  panel <- poster_map_fixture(30L)
  panel$region <- rep(panel_region_levels()[1:3], length.out = nrow(panel))
  panel$state_code_2001 <- rep(sprintf("%02d", 1:6), each = 5L)
  panel$emi_exposure_all_children_0708[[2]] <- NA_real_
  panel$ling_distance_nonzero_mean[[3]] <- NA_real_

  out <- add_poster_residual_variables(panel)
  region_emi <- is.finite(out$resid_emi_exposure_region_expanded)
  region_iv <- is.finite(out$resid_ling_distance_region_expanded)
  expect_identical(region_emi, region_iv)
  expect_false(region_emi[[2]])
  expect_false(region_emi[[3]])

  state_main <- iv_adjustment_sets()[["state_main"]]
  expected <- poster_residual_pair(
    panel,
    variables = "ling_distance_nonzero_mean",
    fixed_effect = state_main$fixed_effect,
    controls = state_main$controls
  )[, "ling_distance_nonzero_mean"]
  expect_equal(out$resid_ling_distance_state_main, expected)
})


test_that("poster residualization omits fixed effects with one observed level", {
  panel <- poster_map_fixture(30L)
  panel$region <- factor(rep(panel_region_levels()[[1]], nrow(panel)), levels = panel_region_levels())

  residuals <- poster_residual_pair(panel, fixed_effect = "region")

  expect_equal(nrow(residuals), nrow(panel))
  expect_true(all(is.finite(residuals)))
  expect_equal(unname(colMeans(residuals)), c(0, 0), tolerance = 1e-10)
})


test_that("poster first-stage specifications use one common sample", {
  skip_if_not_installed("sandwich")
  set.seed(23)
  panel <- poster_map_fixture(90L)
  panel$state_code_2001 <- rep(sprintf("%02d", 1:9), each = 10L)
  panel$region <- factor(rep(panel_region_levels()[1:6], length.out = 90L), levels = panel_region_levels())
  panel$ling_distance_nonzero_mean <- stats::rnorm(90L)
  panel$emi_exposure_all_children_0708 <- 3 * panel$ling_distance_nonzero_mean + stats::rnorm(90L)
  controls <- poster_spec_controls(poster_first_stage_specs())
  for (v in controls) panel[[v]] <- stats::rnorm(90L)
  panel[[controls[[1L]]]][[1L]] <- NA_real_

  plot_data <- poster_first_stage_spec_data(panel)

  expect_equal(unique(plot_data$n), 89L)
  expect_equal(length(unique(plot_data$specification_id)), length(poster_first_stage_specs()))
})


test_that("poster first-stage residualization omits one-level fixed effects", {
  panel <- poster_map_fixture(30L)
  panel$region <- factor(rep(panel_region_levels()[[1]], nrow(panel)), levels = panel_region_levels())

  residuals <- poster_residualize_for_spec(
    panel,
    "ling_distance_nonzero_mean",
    fixed_effect = "region",
    controls = census_2001_absorption_controls()
  )

  expect_equal(length(residuals), nrow(panel))
  expect_true(all(is.finite(residuals)))
  expect_equal(unname(mean(residuals)), 0, tolerance = 1e-10)
})


test_that("poster first-stage figure is a binned view of registered common-support specifications", {
  skip_if_not_installed("sandwich")
  set.seed(24)
  panel <- poster_map_fixture(120L)
  panel$state_code_2001 <- rep(sprintf("%02d", 1:12), each = 10L)
  panel$region <- factor(rep(panel_region_levels(), length.out = 120L), levels = panel_region_levels())
  panel$ling_distance_nonzero_mean <- stats::rnorm(120L)
  panel$emi_exposure_all_children_0708 <- 4 * panel$ling_distance_nonzero_mean + stats::rnorm(120L)
  controls <- poster_spec_controls(poster_first_stage_specs())
  for (v in controls) panel[[v]] <- stats::rnorm(120L)

  plot_data <- poster_first_stage_spec_data(panel, bins = 12L)
  specs <- poster_first_stage_specs()

  expect_setequal(unique(plot_data$adjustment_id), c("unadjusted", "region_main", "state_main"))
  expect_equal(length(unique(plot_data$n)), 1L)
  expect_true(all(is.finite(plot_data$x)))
  expect_true(all(is.finite(plot_data$y)))
  expect_true(all(is.finite(plot_data$f_stat)))
  expect_true(all(table(plot_data$specification_id) <= 12L))
  expect_identical(vapply(specs, `[[`, character(1), "adjustment_id"), c(raw = "unadjusted", region = "region_main", state = "state_main"))
})


test_that("continuous map limits use rounded central quantiles rather than extreme outliers", {
  values <- c(seq(-1, 1, length.out = 100L), 1000)
  limits <- map_continuous_limits(values, public_map_style("real_log_consumption_change"))

  expect_true(limits[[2]] < 1000)
  expect_true(limits[[1]] <= stats::quantile(values, 0.02))
  expect_true(limits[[2]] >= stats::quantile(values, 0.98))
})

test_that("poster inference restores ivreg sandwich methods for serialized models", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")

  dat <- data.frame(
    y = c(1.0, 2.2, 2.8, 4.1, 5.2, 5.9, 7.1, 8.2),
    x = c(0.7, 1.3, 1.8, 2.5, 3.2, 3.7, 4.5, 5.1),
    z = 1:8,
    state = rep(c("a", "b"), each = 4)
  )
  model <- ivreg::ivreg(y ~ x | z, data = dat, model = TRUE, x = TRUE, y = TRUE)
  attr(model, "cluster_state") <- dat$state
  path <- tempfile(fileext = ".rds")
  saveRDS(model, path)

  try(unloadNamespace("ivreg"), silent = TRUE)
  restored <- readRDS(path)
  covariance <- poster_prediction_vcov(restored)

  expect_true("ivreg" %in% loadedNamespaces())
  expect_equal(dim(covariance), c(length(stats::coef(restored)), length(stats::coef(restored))))
  expect_true(all(is.finite(covariance)))
  expect_equal(covariance, t(covariance), tolerance = 1e-12)
})

test_that("poster expected-value predictions preserve serialized state fixed-effect levels", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("marginaleffects")

  set.seed(27)
  n <- 36L
  state_code_2001 <- rep(c("01", "02", "10"), each = 12L)
  z <- stats::rnorm(n)
  exposure <- 15 + 4 * z + stats::rnorm(n)
  y <- 2 + 0.3 * exposure + as.numeric(factor(state_code_2001)) + stats::rnorm(n)
  panel <- data.frame(
    y = y, emi_exposure_all_children_0708 = exposure, z = z, state_code_2001 = state_code_2001,
    state_2001_cluster = state_code_2001, stringsAsFactors = FALSE
  )
  formula <- stats::as.formula(
    "y ~ emi_exposure_all_children_0708 + factor(state_code_2001) | z + factor(state_code_2001)"
  )
  model <- estimate_2sls(panel, list(model = formula), list())$model
  restored <- unserialize(serialize(model, NULL))
  grid <- data.frame(emi_exposure_all_children_0708 = stats::quantile(panel$emi_exposure_all_children_0708, c(0.25, 0.75), names = FALSE))

  predictions <- poster_expected_value_predictions(restored, grid)

  expect_equal(nrow(predictions), 2L)
  expect_equal(as.numeric(predictions$emi_exposure_all_children_0708), grid$emi_exposure_all_children_0708)
  expect_true(all(is.finite(predictions$estimate)))
  expect_identical(
    unique(attr(restored, "prediction_data")$state_code_2001),
    c("01", "02", "10")
  )
})

test_that("complete Census-2001 map geometry keeps state identity for districts absent from the panel", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 4326
  )
  universe <- sf::st_sf(
    target_unit_2001 = c("pc2001__01__01", "pc2001__02__01"),
    geometry = geometry
  )
  panel <- sf::st_sf(
    target_unit_2001 = "pc2001__01__01",
    state_code_2001 = "01",
    emi_exposure_all_children_0708 = 10,
    geometry = geometry[1]
  )

  complete <- complete_public_map_geometry(panel, universe)
  fill <- public_map_fill(
    complete,
    "emi_exposure_all_children_0708",
    public_map_style("emi_exposure_all_children_0708")
  )

  expect_identical(complete$state_code_2001, c("01", "02"))
  expect_equal(nrow(fill$data), 2L)
  expect_equal(as.character(fill$data$.map_fill), c("2.5-10", "No data"))
  expect_equal(unname(fill$colors[["No data"]]), map_no_data_colour())
  expect_true(all(!sf::st_is_empty(fill$data)))
})


test_that("poster model specifications share fixed-effect definitions", {
  specs <- poster_model_specs()
  expect_identical(poster_first_stage_specs(), specs)
  expect_identical(poster_second_stage_specs(), specs)
  expect_identical(poster_fixed_effect_term("none"), character())
  expect_identical(poster_fixed_effect_term("region"), "factor(region)")
  expect_identical(poster_fixed_effect_term("state"), "factor(state_code_2001)")
  expect_error(poster_fixed_effect_term("district"), "Unknown poster fixed-effect")
})

test_that("poster second-stage specifications use preferred variables and one sample", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("sandwich")

  set.seed(623)
  panel <- poster_map_fixture(120L)
  panel$state_code_2001 <- rep(sprintf("%02d", 1:12), each = 10)
  panel$region <- rep(panel_region_levels(), length.out = nrow(panel))
  controls <- poster_spec_controls(poster_second_stage_specs())
  for (control in controls) panel[[control]] <- stats::rnorm(nrow(panel))
  panel$ling_distance_nonzero_mean <- stats::rnorm(nrow(panel))
  panel$emi_exposure_all_children_0708 <-
    4 + 2 * panel$ling_distance_nonzero_mean + 0.5 * panel[[controls[[1]]]] +
    stats::rnorm(nrow(panel), sd = 0.5)
  panel$real_log_consumption_change <-
    0.02 * panel$emi_exposure_all_children_0708 + 0.1 * panel[[controls[[2]]]] +
    stats::rnorm(nrow(panel), sd = 0.2)

  expect_warning(out <- poster_second_stage_spec_data(panel), NA)
  expect_setequal(unique(out$specification_id), c("raw", "region", "state"))
  expect_length(unique(out$n), 1L)
  expect_true(all(is.finite(out$estimate)))
})

test_that("paper welfare maps use preferred common support and the paper's long-run horizon", {
  panel <- data.frame(target_unit_2001 = c("d1", "d2", "d3"), stringsAsFactors = FALSE)
  welfare <- expand.grid(
    district_2001 = c("d1", "d2", "d3"),
    round_id = c("nss_2004_05", "hces_2022_23"),
    outcome_id = "real_mean_mpce",
    stringsAsFactors = FALSE
  )
  welfare$estimate <- c(100, 200, 300, 150, 400, 600)
  welfare$preferred_eligible <- TRUE
  welfare$preferred_eligible[welfare$district_2001 == "d3" & welfare$round_id == "nss_2004_05"] <- FALSE

  out <- add_paper_welfare_map_variables(panel, welfare)

  expect_equal(out$paper_real_mean_mpce_2022_23, c(150, 400, NA))
  expect_equal(
    out$paper_real_log_mpce_change_2004_05_2022_23[1:2],
    log(c(150 / 100, 400 / 200))
  )
  expect_true(is.na(out$paper_real_log_mpce_change_2004_05_2022_23[[3L]]))
})

test_that("dynamic welfare figure compares registered reduced-form estimands only", {
  rounds <- c(
    "nss_2009_10_type2", "nss_2011_12_type2",
    "hces_2022_23", "hces_2023_24"
  )
  summary <- expand.grid(
    outcome_round = rounds,
    estimand = c("ancova", "change"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  summary$reduced_form_estimate <- seq(-0.02, 0.05, length.out = nrow(summary))
  summary$reduced_form_std.error <- rep(0.02, nrow(summary))

  out <- consumption_iv_dynamic_figure_data(list(summary = summary))

  expect_equal(nrow(out), 8L)
  expect_setequal(as.character(out$estimand), c("ANCOVA", "Long change"))
  expect_identical(levels(out$horizon), c("2009-10", "2011-12", "2022-23", "2023-24"))
  expect_false(any(c("second_stage_estimate", "partial_f", "effective_f") %in% names(out)))
  expect_true(all(out$conf.low < out$estimate))
  expect_true(all(out$conf.high > out$estimate))
  expect_equal(
    out$conf.high - out$estimate,
    stats::qnorm(0.975) * out$std.error,
    tolerance = 1e-12
  )
})

test_that("shared figure registry carries validated dynamic welfare diagnostics", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- poster_map_fixture(1L)
  dynamics <- list(summary = data.frame(
    outcome_round = rep(c(
      "nss_2009_10_type2", "nss_2011_12_type2",
      "hces_2022_23", "hces_2023_24"
    ), each = 2L),
    estimand = rep(c("ancova", "change"), 4L),
    reduced_form_estimate = 0,
    reduced_form_std.error = 1,
    stringsAsFactors = FALSE
  ))

  figures <- make_figures(
    panel, character(), cfg,
    iv_models = list(),
    consumption_iv_dynamics = dynamics
  )
  expect_identical(
    figures$consumption_iv_dynamics$kind,
    "consumption_iv_dynamics"
  )
  expect_identical(
    attr(figures, "consumption_iv_dynamics"),
    dynamics
  )
})


test_that("paper EMI maps use one comparable percentage scale", {
  variables <- c(
    "emi_exposure_all_children_0708",
    "public_emi_exposure_all_children_0708",
    "private_emi_exposure_all_children_0708"
  )
  styles <- lapply(variables, public_map_style)

  expect_true(all(vapply(styles, function(x) identical(x$style, "fixed"), logical(1))))
  expect_true(all(vapply(styles, function(x) identical(x$breaks, styles[[1]]$breaks), logical(1))))
  expect_true(all(vapply(styles, function(x) grepl("%", x$title, fixed = TRUE), logical(1))))
})

test_that("state outlines dissolve intrastate district edges", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 3857
  )
  panel <- sf::st_sf(
    state_code_2001 = c("01", "01"),
    target_unit_2001 = c("a", "b"),
    geometry = geometry
  )
  intrastate_edge <- sf::st_sfc(
    sf::st_linestring(rbind(c(1, 0), c(1, 1))), crs = 3857
  )

  outlines <- public_map_state_outlines(panel)

  expect_s3_class(outlines, "sf")
  expect_equal(as.numeric(sf::st_length(sf::st_union(outlines))), 6, tolerance = 1e-8)
  expect_true(all(sf::st_is_empty(
    sf::st_intersection(sf::st_union(outlines), intrastate_edge)
  )))
})

schooling_access_figure_fixture <- function() {
  groups <- c("Other Backward Class", "Scheduled Caste", "Scheduled Tribe")
  outcomes <- paper_schooling_access_outcome_registry()$outcome
  access <- expand.grid(
    social_group = groups,
    outcome = outcomes,
    stringsAsFactors = FALSE
  )
  access$reference_group <- "Other"
  access$mean_district_gap_percentage_points <- -seq_len(nrow(access))

  cross_outcomes <- paper_schooling_access_outcome_registry()$outcome[
    paper_schooling_access_outcome_registry()$panel_b
  ]
  cross <- expand.grid(
    social_group = groups,
    outcome = cross_outcomes,
    stratum = c("Rural", "Urban"),
    stringsAsFactors = FALSE
  )
  cross$reference_group <- "Other"
  cross$crosscut <- "sector"
  cross$mean_district_gap_percentage_points <- -seq_len(nrow(cross)) / 2
  list(access_summary = access, access_crosscuts = cross)
}

test_that("Natural Earth map reference requires only the disputed-area shapefile bundle", {
  spec <- natural_earth_map_reference_spec()
  expect_identical(names(spec), "disputed_areas")

  files <- unname(paste0(
    "/tmp/", spec[["disputed_areas"]],
    c(".dbf", ".shp", ".prj", ".shx", ".cpg")
  ))
  layers <- natural_earth_reference_shapefiles(files)

  expect_identical(names(layers), "disputed_areas")
  expect_identical(
    basename(unname(layers)),
    paste0(unname(spec), ".shp")
  )

  tracked <- natural_earth_map_reference_paths()
  expect_setequal(
    tools::file_ext(tracked),
    c("shp", "dbf", "shx", "prj", "cpg")
  )
})

test_that("disputed-area registry selects only explicitly registered Natural Earth polygons", {
  registry <- data.frame(
    area_id = c("aksai", "azad", "gilgit", "trans", "siachen"),
    natural_earth_brk_name = c(
      "Aksai Chin", "Azad Kashmir", "Gilgit-Baltistan",
      "Shaksam Valley", "Siachen Glacier"
    ),
    display_label = c(
      "Aksai Chin", "Azad Kashmir", "Gilgit-Baltistan",
      "Trans-Karakoram Tract", "Siachen Glacier"
    ),
    include = TRUE,
    stringsAsFactors = FALSE
  )
  source <- data.frame(
    BRK_NAME = c(registry$natural_earth_brk_name, "Junagadh", "Demchok"),
    stringsAsFactors = FALSE
  )

  selected <- select_registered_disputed_areas(source, registry)

  expect_identical(selected$area_id, registry$area_id)
  expect_identical(selected$BRK_NAME, registry$natural_earth_brk_name)
  expect_identical(selected$display_label, registry$display_label)
  expect_false(any(selected$BRK_NAME %in% c("Junagadh", "Demchok")))
  expect_error(
    select_registered_disputed_areas(
      source[source$BRK_NAME != "Aksai Chin", , drop = FALSE], registry
    ),
    "Aksai Chin",
    fixed = TRUE
  )
})

test_that("state outlines retain complete interstate seams", {
  skip_if_not_installed("sf")
  square <- function(x0, x1, y0 = 0, y1 = 1) sf::st_polygon(list(rbind(
    c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0)
  )))
  districts <- sf::st_sf(
    target_unit_2001 = c("d1", "d2", "d3"),
    state_code_2001 = c("01", "01", "02"),
    geometry = sf::st_sfc(
      square(0, 1), square(1, 2), square(2, 3), crs = 3857
    )
  )
  interstate_edge <- sf::st_sfc(
    sf::st_linestring(rbind(c(2, 0), c(2, 1))), crs = 3857
  )

  outlines <- public_map_state_outlines(districts)

  expect_s3_class(outlines, "sf")
  expect_equal(
    as.numeric(sf::st_length(sf::st_intersection(sf::st_union(outlines), interstate_edge))),
    1,
    tolerance = 1e-8
  )
})

test_that("state outlines omit holes in dissolved source geometry", {
  skip_if_not_installed("sf")
  polygon_with_hole <- sf::st_polygon(list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)),
    rbind(c(0.5, 0.5), c(1, 0.5), c(1, 1), c(0.5, 1), c(0.5, 0.5))
  ))
  districts <- sf::st_sf(
    target_unit_2001 = "d1",
    state_code_2001 = "01",
    geometry = sf::st_sfc(polygon_with_hole, crs = 3857)
  )
  hole_boundary <- sf::st_sfc(sf::st_linestring(rbind(
    c(0.5, 0.5), c(1, 0.5), c(1, 1), c(0.5, 1), c(0.5, 0.5)
  )), crs = 3857)

  outlines <- public_map_state_outlines(districts)

  expect_equal(as.numeric(sf::st_length(sf::st_union(outlines))), 8, tolerance = 1e-8)
  expect_true(all(sf::st_is_empty(
    sf::st_intersection(sf::st_union(outlines), hole_boundary)
  )))
})

test_that("disputed display unions Natural Earth masks with the DataMeet scaffold", {
  skip_if_not_installed("sf")
  square <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 1), c(x0, 1), c(x0, 0)
  )))
  districts <- sf::st_sf(
    target_unit_2001 = c("d1", "d2"),
    state_code_2001 = c("01", "01"),
    geometry = sf::st_sfc(square(0, 2), square(2, 4), crs = 3857)
  )
  disputed <- sf::st_sf(
    area_id = "registered_dispute",
    geometry = sf::st_sfc(square(1.5, 3.5), crs = 3857)
  )
  scaffold <- sf::st_sf(
    scaffold_id = "datameet_2001_99_99",
    geometry = sf::st_sfc(square(3.5, 4.5), crs = 3857)
  )
  before <- sf::st_geometry(districts)
  reference <- build_public_map_boundary_reference(
    list(disputed_areas = disputed),
    scaffold
  )

  expect_identical(names(reference), "disputed_display")
  disputed_display <- public_map_disputed_display(districts, reference)
  expect_silent(
    display <- mask_public_map_disputed_areas(districts, disputed_display)
  )

  expect_true(isTRUE(all.equal(sf::st_geometry(districts), before)))
  expect_equal(nrow(display), 1L)
  expect_identical(display$target_unit_2001, "d1")
  expect_equal(
    as.numeric(sf::st_area(sf::st_union(disputed_display))),
    3,
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(sf::st_area(sf::st_union(display))),
    1.5,
    tolerance = 1e-8
  )
  restored <- sf::st_union(c(
    sf::st_geometry(display),
    sf::st_geometry(disputed_display)
  ))
  expected <- sf::st_union(c(
    sf::st_geometry(districts),
    sf::st_geometry(disputed_display)
  ))
  expect_true(all(sf::st_is_empty(
    sf::st_sym_difference(restored, expected)
  )))
})

test_that("registered disputed areas are polygons with a distinct no-estimate treatment", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  square <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 1), c(x0, 1), c(x0, 0)
  )))
  districts <- sf::st_sf(
    target_unit_2001 = c("d1", "d2"),
    state_code_2001 = c("01", "01"),
    ling_distance_nonzero_mean = c(1, 2),
    geometry = sf::st_sfc(square(0, 1), square(1, 2), crs = 3857)
  )
  disputed <- sf::st_sf(
    area_id = "aksai_chin",
    display_label = "Aksai Chin",
    geometry = sf::st_sfc(square(1.5, 2.5), crs = 3857)
  )
  spec <- list(name = "fixture", variable = "ling_distance_nonzero_mean")

  scaffold <- sf::st_sf(
    scaffold_id = "datameet_2001_99_99",
    geometry = sf::st_sfc(square(2.5, 3), crs = 3857)
  )
  reference <- build_public_map_boundary_reference(
    list(disputed_areas = disputed),
    scaffold
  )
  plot <- build_public_ggplot_map(districts, spec, reference)

  district_layers <- Filter(
    function(layer) "target_unit_2001" %in% names(layer$data),
    plot$layers
  )
  disputed_layers <- Filter(
    function(layer) "area_id" %in% names(layer$data),
    plot$layers
  )

  expect_length(district_layers, 1L)
  expect_length(disputed_layers, 1L)
  expect_lt(
    as.numeric(sf::st_area(sf::st_union(district_layers[[1]]$data))),
    as.numeric(sf::st_area(sf::st_union(districts)))
  )
  expect_true(all(
    as.character(sf::st_geometry_type(disputed_layers[[1]]$data)) %in%
      c("POLYGON", "MULTIPOLYGON")
  ))
  expect_false(identical(map_disputed_no_data_colour(), map_no_data_colour()))
})

test_that("disputed masks cut state polygons before ordinary outlines are derived", {
  skip_if_not_installed("sf")
  state <- sf::st_sf(
    target_unit_2001 = "d1",
    state_code_2001 = "01",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(2, 0), c(2, 1), c(0, 1), c(0, 0)
    ))), crs = 3857)
  )
  mask <- sf::st_sf(
    area_id = "aksai_chin",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(1.5, 0), c(2.5, 0), c(2.5, 1), c(1.5, 1), c(1.5, 0)
    ))), crs = 3857)
  )
  expected_seam <- sf::st_sfc(
    sf::st_linestring(rbind(c(1.5, 0), c(1.5, 1))), crs = 3857
  )

  outlines <- public_map_state_outlines(state, mask)

  expect_equal(
    as.numeric(sf::st_length(sf::st_intersection(sf::st_union(outlines), expected_seam))),
    1,
    tolerance = 1e-8
  )
})

test_that("district boundaries stay thinner than state and disputed boundaries", {
  expect_gt(map_major_boundary_linewidth(), map_district_boundary_linewidth())
  expect_gt(map_disputed_boundary_linewidth(), map_district_boundary_linewidth())
  expect_lt(map_disputed_boundary_linewidth(), map_major_boundary_linewidth())
})

test_that("paper consumption level map uses the positive half of the change palette", {
  level_colors <- map_palette_values("poster.consumption.positive", 3L)
  change_colors <- map_palette_values("poster.diverging.emi", 5L)

  expect_identical(level_colors, change_colors[3:5])
  expect_identical(
    public_map_style("paper_real_mean_mpce_2022_23")$palette,
    "poster.consumption.positive"
  )
})
