test_that("final figures degrade to status specs without real sf geometry", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- data.frame(
    EMIE = 1,
    consumption_pct_change = 2,
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
    consumption_pct_change = c(2, 3),
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
    consumption_pct_change = 2,
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

test_that("complete map geometry keeps grey boundaries and non-missing overlay rows", {
  skip_if_not_installed("sf")
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 4326
  )
  boundaries <- sf::st_sf(
    state_20 = c("A", "A"),
    district_20 = c("matched", "unmatched"),
    geometry = geometry
  )
  panel <- sf::st_sf(
    state_20 = "A",
    district_20 = "matched",
    EMIE = 10,
    geometry = geometry[1]
  )

  out <- complete_map_geometry(panel, boundaries, "EMIE")
  fill <- public_map_fill(out, "EMIE", public_map_style("EMIE"))

  expect_equal(nrow(out), 2L)
  expect_true("No data" %in% as.character(fill$data$.map_fill))
  expect_equal(sum(map_overlay_rows(fill$data, ".map_fill")), 1L)
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
  src <- paste(deparse(build_public_ggplot_map), collapse = "\n")
  expect_match(src, "geom_sf(data = plot_data, ggplot2::aes(fill = .data[[fill$fill]])", fixed = TRUE)
  expect_match(src, "guide_legend", fixed = TRUE)
  expect_match(src, "map_no_data_colour()", fixed = TRUE)
})


test_that("main map legends use rounded publication bounds", {
  cons <- public_map_style("consumption_pct_change")
  educ <- public_map_style("pct_head_secondary_plus")

  expect_equal(cons$title, "Consumption Growth (%)")
  expect_equal(cons$breaks, c(10, 100, 200, 300, 400, 450))
  expect_equal(cons$labels, c("10-100", "100-200", "200-300", "300-400", "400-450"))
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

test_that("poster expected-values figure is generated with the main figures", {
  cfg <- list(mode = "final", output_formats = list(figures = c("pdf", "png")))
  panel <- data.frame(
    EMIE = 1,
    consumption_pct_change = 2,
    pct_pucca = 3,
    pct_head_secondary_plus = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

  figures <- make_figures(panel, character(), cfg, iv_models = list())

  expect_identical(figures$poster_emie_expected_values$kind, "emie_expected_values")
  expect_identical(attr(figures, "iv_models"), list())
})
