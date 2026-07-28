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

test_that("final figures include public map collages when geometry is validated", {
  skip_if_not_installed("sf")
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 4326
  )
  panel_df <- poster_map_fixture(2L)
  panel <- sf::st_sf(panel_df, geometry = geometry)

  figures <- make_figures(panel, character(), cfg)
  expect_true(all(c("map_emi_exposure", "map_consumption_growth", "map_residual_emi_exposure", "map_residual_linguistic_distance", "collage_main_maps", "collage_iv_region_maps") %in% names(figures)))
})

test_that("district carve-out figure data uses pct_91in01 values", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    'Anantapur,"3,183,814",Anantapur,100,75.5',
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

test_that("map collage order matches public captions", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- poster_map_fixture(1L)

  figs <- make_figures(panel, character(), cfg)

  expect_equal(figs$collage_main_maps$inputs, c("map_emi_exposure", "map_consumption_growth", "map_pucca", "map_education"))
  expect_equal(
    figs$collage_iv_region_maps$inputs,
    c("map_region", "map_linguistic_distance", "map_residual_linguistic_distance", "map_residual_emi_exposure")
  )
})

test_that("linguistic-distance map labels begin at zero and no-data uses visible grey", {
  df <- data.frame(wavg_ling_degrees = c(0.0001089, 1.5, 5, NA))
  fill <- public_map_fill(df, "wavg_ling_degrees", public_map_style("wavg_ling_degrees"))

  expect_true(startsWith(levels(fill$data$.map_fill)[[1]], "0-"))
  expect_equal(unname(fill$colors[["No data"]]), "#bdbdbd")
})

test_that("district carve-out figure uses unbordered bars", {
  path <- file.path("R", "output", "save_figures.R")
  if (!file.exists(path)) path <- file.path("..", "..", "R", "output", "save_figures.R")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(src, "geom_histogram\\(binwidth = binwidth, fill = \"goldenrod\", color = NA\\)")
})


test_that("public no-data map colour is a visible ggplot2 scale na.value", {
  expect_equal(map_no_data_colour(), "#bdbdbd")
})

test_that("public maps retain the production panel's Census-2001 geometry and attributes", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 4326
  )
  panel <- sf::st_sf(
    target_unit_2001 = "pc2001__01__01",
    EMIE = 10,
    geometry = geometry
  )
  spec <- figure_spec(
    "map_emi_exposure",
    "map_emi_exposure.png",
    "EMI Exposure",
    kind = "map",
    variable = "EMIE"
  )

  fill <- public_map_fill(panel, "EMIE", public_map_style("EMIE"))
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
    EMIE = NA_real_,
    geometry = geometry
  )
  spec <- figure_spec("map_emi_exposure", "map_emi_exposure.png", "EMI Exposure", kind = "map", variable = "EMIE")

  expect_error(
    build_public_ggplot_map(panel, spec),
    "no non-missing overlay districts",
    fixed = TRUE
  )
})


test_that("public map range labels do not contain padded spaces", {
  labels <- map_cut_labels(c(0, 20, 40, 60, 80, 100))
  expect_equal(labels, c("0-20", "20-40", "40-60", "60-80", "80-100"))
  expect_false(any(grepl("\\s+-|-[[:space:]]+", labels)))
})

test_that("No data is mapped through the fill scale so its legend key is grey", {
  colors <- c("0-20" = "#123456", "No data" = map_no_data_colour())
  override <- map_legend_override(colors)

  expect_equal(override$fill, unname(colors))
  expect_equal(tail(override$fill, 1L), map_no_data_colour())
  expect_equal(override$alpha, rep(1, length(colors)))
})


test_that("main map legends use rounded publication bounds", {
  cons <- public_map_style("real_log_consumption_change")
  educ <- public_map_style("pct_head_secondary_plus")

  expect_equal(cons$title, "Real Log Consumption Change")
  expect_equal(cons$style, "continuous")
  expect_null(cons$breaks)
  expect_null(cons$labels)
  expect_equal(educ$breaks, c(0, 20, 40, 60, 80))
  expect_equal(educ$labels, c("0-20", "20-40", "40-60", "60-80"))
  expect_equal(map_no_data_colour(), "#bdbdbd")
})

test_that("continuous and diverging map styles keep numeric fills", {
  df <- data.frame(real_log_consumption_change = c(-0.5, 0, 0.5, NA))
  fill <- public_map_fill(df, "real_log_consumption_change", public_map_style("real_log_consumption_change"))

  expect_true(isTRUE(fill$continuous))
  expect_true(is.numeric(fill$data$.map_value))
  expect_equal(grDevices::col2rgb(unname(fill$colors[1])), grDevices::col2rgb("#f7fbff"))

  residual <- data.frame(resid_emi_exposure_region_expanded = c(-2, 0, 3, NA))
  resid_fill <- public_map_fill(residual, "resid_emi_exposure_region_expanded", public_map_style("resid_emi_exposure_region_expanded"))
  expect_true(isTRUE(resid_fill$continuous))
  expect_equal(resid_fill$limits[1], -resid_fill$limits[2])
})



test_that("poster residual maps use one common complete-case sample", {
  panel <- poster_map_fixture(30L)
  panel$emi_exposure_all_children_0708[[2]] <- NA_real_
  panel$ling_distance_nonzero_mean[[3]] <- NA_real_

  out <- add_poster_residual_variables(panel)
  emi_observed <- is.finite(out$resid_emi_exposure_region_expanded)
  iv_observed <- is.finite(out$resid_ling_distance_region_expanded)

  expect_identical(emi_observed, iv_observed)
  expect_false(emi_observed[[2]])
  expect_false(emi_observed[[3]])
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
  for (v in census_2001_absorption_controls()) panel[[v]] <- stats::rnorm(90L)
  panel[[census_2001_absorption_controls()[[1]]]][[1]] <- NA_real_

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


test_that("poster first-stage ribbons remain ordered for negative residualized values", {
  skip_if_not_installed("sandwich")
  set.seed(24)
  panel <- poster_map_fixture(90L)
  panel$state_code_2001 <- rep(sprintf("%02d", 1:9), each = 10L)
  panel$region <- factor(rep(panel_region_levels()[1:6], length.out = 90L), levels = panel_region_levels())
  panel$ling_distance_nonzero_mean <- stats::rnorm(90L)
  panel$emi_exposure_all_children_0708 <- 4 * panel$ling_distance_nonzero_mean + stats::rnorm(90L)
  for (v in census_2001_absorption_controls()) panel[[v]] <- stats::rnorm(90L)

  plot_data <- poster_first_stage_spec_data(panel)

  expect_true(any(plot_data$z_resid < 0))
  expect_true(all(plot_data$conf.low <= plot_data$estimate))
  expect_true(all(plot_data$estimate <= plot_data$conf.high))
})


test_that("continuous map limits use rounded central quantiles rather than extreme outliers", {
  values <- c(seq(-1, 1, length.out = 100L), 1000)
  limits <- map_continuous_limits(values, public_map_style("real_log_consumption_change"))

  expect_true(limits[[2]] < 1000)
  expect_true(limits[[1]] <= stats::quantile(values, 0.02))
  expect_true(limits[[2]] >= stats::quantile(values, 0.98))
})

test_that("poster EMIE grid uses observed percentiles", {
  panel <- data.frame(EMIE = 0:100)
  grid <- poster_emie_percentiles(panel, probs = c(0.05, 0.50, 0.95))

  expect_equal(grid$percentile, c(0.05, 0.50, 0.95))
  expect_equal(grid$EMIE, c(5, 50, 95))
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
  EMIE <- 15 + 4 * z + stats::rnorm(n)
  y <- 2 + 0.3 * EMIE + as.numeric(factor(state_code_2001)) + stats::rnorm(n)
  panel <- data.frame(
    y = y, EMIE = EMIE, z = z, state_code_2001 = state_code_2001,
    state_2001_cluster = state_code_2001, stringsAsFactors = FALSE
  )
  formula <- stats::as.formula(
    "y ~ EMIE + factor(state_code_2001) | z + factor(state_code_2001)"
  )
  model <- estimate_2sls(panel, list(model = formula), list())$model
  restored <- unserialize(serialize(model, NULL))
  grid <- data.frame(EMIE = stats::quantile(panel$EMIE, c(0.25, 0.75), names = FALSE))

  predictions <- poster_expected_value_predictions(restored, grid)

  expect_equal(nrow(predictions), 2L)
  expect_equal(as.numeric(predictions$EMIE), grid$EMIE)
  expect_true(all(is.finite(predictions$estimate)))
  expect_identical(
    unique(attr(restored, "prediction_data")$state_code_2001),
    c("01", "02", "10")
  )
})

test_that("poster expected-values figure is generated with the main figures", {
  cfg <- list(mode = "final", output_formats = list(figures = c("pdf", "png")))
  panel <- poster_map_fixture(1L)

  figures <- make_figures(panel, character(), cfg, iv_models = list())

  expect_identical(figures$poster_emie_expected_values$kind, "emie_expected_values")
  expect_identical(figures$poster_first_stage_specs$kind, "poster_first_stage_specs")
  expect_identical(figures$poster_second_stage_specs$kind, "poster_second_stage_specs")
  expect_identical(figures$map_preferred_linguistic_distance$variable, "ling_distance_nonzero_mean")
  expect_identical(attr(figures, "iv_models"), list())
})

test_that("complete Census-2001 map geometry shows missing panel districts as grey polygons", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 4326
  )
  universe <- sf::st_sf(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    geometry = geometry
  )
  panel <- sf::st_sf(
    target_unit_2001 = "pc2001__01__01",
    EMIE = 10,
    geometry = geometry[1]
  )

  complete <- complete_public_map_geometry(panel, universe)
  fill <- public_map_fill(complete, "EMIE", public_map_style("EMIE"))

  expect_equal(nrow(fill$data), 2L)
  expect_equal(as.character(fill$data$.map_fill), c("2.5-10", "No data"))
  expect_equal(unname(fill$colors[["No data"]]), map_no_data_colour())
  expect_true(all(!sf::st_is_empty(fill$data)))
})


test_that("poster map legends reserve readable vertical space", {
  skip_if_not_installed("ggplot2")
  dimensions <- public_map_colorbar_dimensions()
  expect_s3_class(dimensions$height, "unit")
  expect_s3_class(dimensions$width, "unit")
  expect_equal(grid::convertHeight(dimensions$height, "pt", valueOnly = TRUE), 92)
  expect_equal(grid::convertWidth(dimensions$width, "pt", valueOnly = TRUE), 10)
  expect_s3_class(public_map_colorbar_guide(), "GuideColourbar")
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
  panel <- poster_map_fixture(120L)
  panel$real_log_consumption_change <- 0.02 * panel$emi_exposure_all_children_0708 + rnorm(nrow(panel), sd = 0.2)
  panel$state_code_2001 <- rep(sprintf("%02d", 1:12), each = 10)
  panel$region <- rep(panel_region_levels(), length.out = nrow(panel))
  out <- poster_second_stage_spec_data(panel)
  expect_setequal(unique(out$specification_id), c("raw", "region", "state"))
  expect_length(unique(out$n), 1L)
  expect_true(all(is.finite(out$estimate)))
})
