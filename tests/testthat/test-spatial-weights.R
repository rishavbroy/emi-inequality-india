test_that("spatial weights diagnostics can be skipped on non-sf objects", {
  out <- build_spatial_weights(data.frame(x = 1), list())
  expect_type(out, "list")
  expect_equal(out$status, "out_of_active_pipeline")
})

test_that("spatial weights store legacy rook listw and binary adjacency matrix", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("spdep")

  polys <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    sf::st_polygon(list(rbind(c(0, 1), c(1, 1), c(1, 2), c(0, 2), c(0, 1)))),
    crs = 4326
  )
  panel <- sf::st_sf(id = 1:3, geometry = polys)

  weights <- build_spatial_weights(panel, list())

  expect_equal(weights$status, "constructed")
  expect_equal(weights$contiguity, "rook")
  expect_equal(weights$matrix_style, "B")
  expect_equal(weights$style, "W")
  expect_true(inherits(weights$listw, "listw"))
  expect_equal(dim(weights$W), c(3L, 3L))
})

test_that("spatial weights explicitly use current final matched panel rows", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("spdep")

  polys <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    sf::st_polygon(list(rbind(c(0, 1), c(1, 1), c(1, 2), c(0, 2), c(0, 1)))),
    sf::st_sfc(sf::st_geometrycollection(), crs = 4326)[[1]],
    crs = 4326
  )
  panel <- sf::st_sf(id = 1:4, geometry = polys)

  expect_equal(spatial_weight_final_panel_rows(panel), 1:3)
  weights <- build_spatial_weights(panel, list())
  expect_equal(weights$row_index, 1:3)
  expect_equal(weights$panel_scope, "current_final_matched_panel_non_empty_geometry")
  comp <- compare_rook_queen_contiguity(panel)
  expect_true(all(comp$panel_scope == "current_final_matched_panel_non_empty_geometry"))
})
