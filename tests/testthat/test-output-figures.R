test_that("final figures degrade to status specs without real sf geometry", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- data.frame(
    EMIE = 1,
    real_log_consumption_change = 2,
    pct_pucca = 3,
    pct_head_secondary_plus = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

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
  panel <- sf::st_sf(
    EMIE = c(1, 2),
    real_log_consumption_change = c(2, 3),
    pct_pucca = c(3, 4),
    pct_head_secondary_plus = c(4, 5),
    region = c("North", "North"),
    wavg_ling_degrees = c(5, 6),
    geometry = geometry
  )

  figures <- make_figures(panel, character(), cfg)
  expect_true(all(c("map_emi_exposure", "map_consumption_growth", "collage_main_maps", "collage_iv_region_maps") %in% names(figures)))
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

test_that("public-map regions overwrite numeric source codes with named categories", {
  panel <- data.frame(
    state_20 = c("Punjab", "Tamil Nadu"),
    region = c(1, 5),
    stringsAsFactors = FALSE
  )

  out <- add_panel_regions(panel)

  expect_equal(as.character(out$region), c("North", "South"))
})

test_that("map collage order matches public captions", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- data.frame(
    EMIE = 1,
    real_log_consumption_change = 2,
    pct_pucca = 3,
    pct_head_secondary_plus = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

  figs <- make_figures(panel, character(), cfg)

  expect_equal(figs$collage_main_maps$inputs, c("map_emi_exposure", "map_consumption_growth", "map_pucca", "map_education"))
  expect_equal(figs$collage_iv_region_maps$inputs, c("map_region", "map_linguistic_distance"))
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
  expect_equal(cons$style, "quantile")
  expect_null(cons$breaks)
  expect_null(cons$labels)
  expect_equal(educ$breaks, c(0, 20, 40, 60, 80))
  expect_equal(educ$labels, c("0-20", "20-40", "40-60", "60-80"))
  expect_equal(map_no_data_colour(), "#bdbdbd")
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

test_that("poster expected-values figure is generated with the main figures", {
  cfg <- list(mode = "final", output_formats = list(figures = c("pdf", "png")))
  panel <- data.frame(
    EMIE = 1,
    real_log_consumption_change = 2,
    pct_pucca = 3,
    pct_head_secondary_plus = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

  figures <- make_figures(panel, character(), cfg, iv_models = list())

  expect_identical(figures$poster_emie_expected_values$kind, "emie_expected_values")
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
