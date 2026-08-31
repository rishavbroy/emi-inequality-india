make_census_c17_mechanism_fixture <- function() {
  languages <- data.frame(
    native_language_code = c("006000", "005000", "007000", "002000", "011000"),
    native_language = c("Hindi", "Gujarati", "Marathi", "Bengali", "Kannada"),
    degree = c(0, 1, 2, 3, 5),
    stringsAsFactors = FALSE
  )
  states <- c("09", "19", "24")
  safe_bind_rows(lapply(seq_along(states), function(s) {
    speakers <- c(70, 55, 40, 30, 20) + c(0, 3, 7, 11, 14) * (s - 1L)
    multilingual <- pmax(5, round(speakers * c(.30, .35, .40, .45, .50)))
    english <- pmin(multilingual, round(multilingual * c(.70, .20, .30, .45, .65)))
    hindi <- pmin(multilingual, round(multilingual * c(.10, .65, .55, .40, .20)))
    data.frame(
      state_code = states[[s]], state_name = paste0("State ", states[[s]]),
      native_language_code = languages$native_language_code,
      native_language = languages$native_language,
      sex = "Persons", native_speakers = speakers,
      multilingual_speakers = multilingual,
      english_multilingual_speakers = english,
      hindi_multilingual_speakers = hindi,
      multilingual_share_native = 100 * multilingual / speakers,
      english_share_multilingual = 100 * english / multilingual,
      hindi_share_multilingual = 100 * hindi / multilingual,
      english_share_native = 100 * english / speakers,
      stringsAsFactors = FALSE
    )
  }))
}

test_that("C-17 mechanism data uses the shared Shastry language identity", {
  out <- prepare_census_c17_mechanism_data(make_census_c17_mechanism_fixture())

  expect_equal(unique(out$shastry_degree[out$native_language == "Hindi"]), 0)
  expect_equal(unique(out$shastry_degree[out$native_language == "Gujarati"]), 1)
  expect_equal(unique(out$shastry_degree[out$native_language == "Bengali"]), 3)
  expect_true(all(is.finite(out$shastry_degree)))
  expect_equal(unique(out$hindi_urdu_reference[out$native_language == "Hindi"]), 1L)
  expect_equal(unique(out$hindi_urdu_reference[out$native_language == "Gujarati"]), 0L)
  expect_equal(sum(out$state_modal_language), 3L)

  state_share <- tapply(out$native_share_state, out$state_code, sum)
  expect_equal(as.numeric(state_share), rep(1, 3), tolerance = 1e-12)
})

test_that("C-17 preferred mechanism model identifies within-state language variation", {
  data <- prepare_census_c17_mechanism_data(make_census_c17_mechanism_fixture())
  registry <- census_c17_mechanism_registry()
  specification <- registry[registry$preferred, , drop = FALSE]
  out <- fit_census_c17_mechanism(data, specification)

  expect_equal(out$summary$status, "estimated")
  expect_equal(out$summary$n_states, 3L)
  expect_true("shastry_degree" %in% out$coefficients$term)
  expect_true(all(out$coefficients$status == "estimated"))
  preferred <- out$coefficients[out$coefficients$term == "shastry_degree", , drop = FALSE]
  expect_true(is.finite(preferred$partial_r_squared))
  expect_true(preferred$partial_r_squared >= 0 && preferred$partial_r_squared <= 1)
  expect_equal(
    preferred$signed_partial_correlation^2,
    preferred$partial_r_squared,
    tolerance = 1e-12
  )
  formula_terms <- attr(
    stats::terms(census_c17_mechanism_formula(specification)),
    "term.labels"
  )
  expect_true("factor(state_code)" %in% formula_terms)
})

test_that("C-17 coefficient partial R-squared matches the nested-model identity", {
  data <- data.frame(
    y = c(1.2, 2.1, 3.7, 5.4, 6.6, 8.3),
    x = c(0, 1, 1, 2, 2, 3),
    z = c(1, 0, 1, 0, 1, 0),
    w = c(1, 2, 1, 2, 1, 2)
  )
  full <- stats::lm(y ~ x + z, data = data, weights = w)
  restricted <- stats::lm(y ~ z, data = data, weights = w)
  expected <- 1 - stats::deviance(full) / stats::deviance(restricted)

  expect_equal(
    census_c17_coefficient_partial_r_squared(full, "x"),
    expected,
    tolerance = 1e-12
  )
  expect_true(is.na(census_c17_coefficient_partial_r_squared(full, "not_a_term")))
})

test_that("C-17 partial R-squared covers expanded distance-bin coefficients", {
  data <- prepare_census_c17_mechanism_data(make_census_c17_mechanism_fixture())
  registry <- census_c17_mechanism_registry()
  specification <- registry[registry$specification_id == "english_bins", , drop = FALSE]
  out <- fit_census_c17_mechanism(data, specification)

  bin_rows <- startsWith(out$coefficients$term, "distance_bin")
  expect_true(any(bin_rows))
  expect_true(all(is.finite(out$coefficients$partial_r_squared[bin_rows])))
  expect_true(all(out$coefficients$partial_r_squared[bin_rows] >= 0))
  expect_true(all(out$coefficients$partial_r_squared[bin_rows] <= 1))
})

test_that("C-17 alternative bases reuse reviewed language identities", {
  languoids <- data.frame(
    id = c("indo1319", "dravidian", "hind1269", "gujarati", "kannada"),
    family_id = c("", "", "indo1319", "indo1319", "dravidian"),
    parent_id = c("", "", "indo1319", "indo1319", "dravidian"),
    name = c("Indo-European", "Dravidian", "Hindi", "Gujarati", "Kannada"),
    bookkeeping = FALSE,
    level = c("family", "family", "language", "language", "language"),
    iso639P3code = c("", "", "hin", "guj", "kan"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = c("006118", "005011", "007013"),
    mother_tongue = c("Hindi", "Gujarati", "Kannada"),
    canonical_language = c("Hindi", "Gujarati", "Kannada"),
    language_glottocode = c("hind1269", "gujarati", "kannada"),
    family_id = c("indo1319", "indo1319", "dravidian"),
    match_basis = "manual",
    review_status = "accepted_manual",
    stringsAsFactors = FALSE
  )
  historical <- list(
    dyen_hindi = data.frame(
      list_name = c("Hindi", "Gujarati"),
      percent_cognates_with_hindi = c(100, 64.5502645502645),
      stringsAsFactors = FALSE
    )
  )
  lexical <- data.frame(
    language = c("Hindi", "Gujarati", "Kannada"),
    dyen_list_name = c("Hindi", "Gujarati", NA_character_),
    kogan_code = NA_character_,
    match_basis = "test", source_note = "test",
    stringsAsFactors = FALSE
  )

  out <- prepare_census_c17_mechanism_data(
    make_census_c17_mechanism_fixture(),
    glottolog = list(languoids = languoids),
    glottolog_crosswalk = crosswalk,
    historical_linguistics = historical,
    lexical_index = lexical
  )

  expect_equal(unique(out$glottolog_edge_distance[out$native_language == "Hindi"]), 0)
  expect_true(all(is.finite(out$glottolog_edge_distance[out$native_language == "Gujarati"])))
  expect_equal(
    unique(out$dyen_noncognate_pct[out$native_language == "Gujarati"]),
    100 - 64.5502645502645
  )
  expect_equal(unique(out$dyen_noncognate_pct[out$native_language == "Kannada"]), 95)

  coverage <- summarize_census_c17_mapping_coverage(out)
  expect_setequal(unique(coverage$distance_basis), c("shastry", "glottolog", "dyen"))
})

test_that("C-17 alternative distance specifications remain separate robustness models", {
  registry <- census_c17_mechanism_registry()
  expect_identical(
    registry$distance_variable[registry$specification_id == "english_glottolog"],
    "glottolog_edge_distance"
  )
  expect_identical(
    registry$distance_variable[registry$specification_id == "english_dyen"],
    "dyen_noncognate_pct"
  )
  expect_false(any(registry$preferred[registry$distance_basis != "shastry"]))
})

test_that("C-17 mechanism registry stays deliberately small", {
  registry <- census_c17_mechanism_registry()

  expect_equal(nrow(registry), 9L)
  expect_equal(sum(registry$preferred), 1L)
  expect_setequal(unique(registry$distance_basis), c("shastry", "glottolog", "dyen"))
  expect_setequal(unique(registry$distance_form), c("linear", "bins", "distant"))
  expect_setequal(
    unique(registry$outcome),
    c("english_share_multilingual", "hindi_share_multilingual", "multilingual_share_native")
  )
})

test_that("C-17 diagnostic saver returns the files it writes", {
  diagnostics <- diagnose_census_c17_mechanism(
    make_census_c17_mechanism_fixture()
  )
  dir <- file.path(tempdir(), paste0("c17-diagnostics-", Sys.getpid()))
  unlink(dir, recursive = TRUE)

  paths <- save_census_c17_mechanism_diagnostics(diagnostics, dir = dir)

  expect_type(paths, "character")
  expect_length(paths, 4L)
  expect_true(all(file.exists(paths)))
  expect_setequal(
    basename(paths),
    c(
      "c17_mechanism_registry.csv",
      "c17_language_mapping_coverage.csv",
      "c17_mechanism_coefficients.csv",
      "c17_mechanism_model_summary.csv"
    )
  )
})
