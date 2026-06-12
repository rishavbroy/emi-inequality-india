test_that("final figures degrade to status specs without real sf geometry", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- data.frame(
    emie_2007 = 1,
    consumption_growth_pct = 2,
    pucca_share_2007 = 3,
    head_secondary_plus_2007 = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

  figures <- make_figures(panel, character(), cfg)
  expect_identical(figures$map_emi_exposure$kind, "status")
  expect_true(any(grepl("Geometry coverage", attr(figures, "legacy_map_input_failures"), fixed = TRUE)))
})

test_that("final figures include legacy map collages when geometry is validated", {
  skip_if_not_installed("sf")
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 4326
  )
  panel <- sf::st_sf(
    emie_2007 = c(1, 2),
    consumption_growth_pct = c(2, 3),
    pucca_share_2007 = c(3, 4),
    head_secondary_plus_2007 = c(4, 5),
    region = c("North", "North"),
    wavg_ling_degrees = c(5, 6),
    geometry = geometry
  )

  figures <- make_figures(panel, character(), cfg)
  expect_true(all(c("map_emi_exposure", "map_consumption_growth", "collage_main_maps", "collage_iv_region_maps") %in% names(figures)))
})

test_that("district carve-out figure data uses pct_91in01 values", {
  carveouts <- read_carveout_shift_data()

  expect_true(nrow(carveouts) > 0)
  expect_true("pct_91in01" %in% names(carveouts))
  expect_true(all(is.finite(carveouts$pct_91in01)))
})

test_that("legacy regions overwrite numeric source codes with named categories", {
  panel <- data.frame(
    state_20 = c("Punjab", "Tamil Nadu"),
    region = c(1, 5),
    stringsAsFactors = FALSE
  )

  out <- add_legacy_regions(panel)

  expect_equal(as.character(out$region), c("North", "South"))
})

test_that("map collage order matches public captions", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- data.frame(
    emie_2007 = 1,
    consumption_growth_pct = 2,
    pucca_share_2007 = 3,
    head_secondary_plus_2007 = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

  figs <- make_figures(panel, character(), cfg)

  expect_equal(figs$collage_main_maps$inputs, c("map_emi_exposure", "map_consumption_growth", "map_pucca", "map_education"))
  expect_equal(figs$collage_iv_region_maps$inputs, c("map_region", "map_linguistic_distance"))
})

test_that("linguistic-distance map labels begin at zero and no-data uses visible grey", {
  df <- data.frame(wavg_ling_degrees = c(0.0001089, 1.5, 5, NA))
  fill <- legacy_map_fill(df, "wavg_ling_degrees", legacy_map_style("wavg_ling_degrees"))

  expect_true(startsWith(levels(fill$data$.map_fill)[[1]], "0-"))
  expect_equal(unname(fill$colors[["No data"]]), "#bdbdbd")
})

test_that("district carve-out figure uses unbordered legacy-style bars", {
  path <- file.path("R", "output", "save_figures.R")
  if (!file.exists(path)) path <- file.path("..", "..", "R", "output", "save_figures.R")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(src, "geom_histogram\\(binwidth = binwidth, fill = \"goldenrod\", color = NA\\)")
})


test_that("legacy no-data map colour is a visible ggplot2 scale na.value", {
  expect_equal(legacy_no_data_colour(), "#bdbdbd")
})
