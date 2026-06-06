test_that("final figures require real sf geometry", {
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  panel <- data.frame(
    emie_2007 = 1,
    consumption_growth_pct = 2,
    pucca_share_2007 = 3,
    head_secondary_plus_2007 = 4,
    region = "North",
    wavg_ling_degrees = 5
  )

  expect_error(
    make_figures(panel, character(), cfg),
    "Final map generation requires an sf district_panel"
  )
})

test_that("final figures require sufficient non-empty geometry coverage", {
  skip_if_not_installed("sf")
  cfg <- list(mode = "final", output_formats = list(figures = "png"))
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_geometrycollection(),
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

  expect_error(
    make_figures(panel, character(), cfg),
    "covering at least 75%"
  )
})

test_that("district carve-out figure data uses pct_91in01 values", {
  carveouts <- read_carveout_shift_data()

  expect_true(nrow(carveouts) > 0)
  expect_true("pct_91in01" %in% names(carveouts))
  expect_true(all(is.finite(carveouts$pct_91in01)))
})
