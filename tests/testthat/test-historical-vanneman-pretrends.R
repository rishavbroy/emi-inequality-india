test_that("Vanneman pretrend reader follows the archived fixed-width count contract", {
  path <- tempfile(fileext = ".data.gz")
  con <- gzfile(path, "wt")
  records <- vanneman_pretrend_record_registry()$record_id
  lines <- character()
  for (year in c(61, 71, 81, 91)) {
    for (record in records) {
      total <- if (record == "100") 1000 + year else if (record == "111") 400 else if (record == "112") 300 else 200
      rural <- if (record == "100") 800 else round(total * .8)
      lines <- c(lines, sprintf("0201%s%02d5%9d%9d%9d%9d", record, year, total, rural, round(total*.6), round(rural*.6)))
    }
  }
  writeLines(lines, con); close(con)
  on.exit(unlink(path), add = TRUE)

  out <- read_vanneman_panel4_pretrend_counts(path)
  expect_equal(nrow(out), 4L * nrow(vanneman_pretrend_record_registry()))
  expect_setequal(out$year, c(1961L, 1971L, 1981L, 1991L))
  expect_true(all(out$panel_unit_id == "0201"))
})

test_that("Vanneman pretrend reader ignores short label records before fixed-width validation", {
  path <- tempfile(fileext = ".data.gz")
  con <- gzfile(path, "wt")
  records <- vanneman_pretrend_record_registry()$record_id
  lines <- "0201000615Short label"
  for (year in c(61, 71, 81, 91)) for (record in records) {
    lines <- c(lines, sprintf(
      "0201%s%02d5%9d%9d%9d%9d",
      record, year, 1000, 800, 600, 480
    ))
  }
  writeLines(lines, con); close(con)
  on.exit(unlink(path), add = TRUE)

  out <- read_vanneman_panel4_pretrend_counts(path)
  expect_equal(nrow(out), 4L * nrow(vanneman_pretrend_record_registry()))
})

test_that("Vanneman pretrend reader requires version-5 stable-panel records", {
  path <- tempfile(fileext = ".data.gz")
  con <- gzfile(path, "wt")
  records <- vanneman_pretrend_record_registry()$record_id
  lines <- character()
  for (year in c(61, 71, 81, 91)) for (record in records) {
    version <- if (year == 81 && record == "140") 6 else 5
    lines <- c(lines, sprintf(
      "0201%s%02d%d%9d%9d%9d%9d",
      record, year, version, 1000, 800, 600, 480
    ))
  }
  writeLines(lines, con); close(con)
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_vanneman_panel4_pretrend_counts(path),
    "version-5 contract"
  )
})

test_that("Vanneman pretrend semantic contract is tied to the archived SAS reader", {
  path <- tempfile(fileext = ".sas")
  writeLines(c(
    "/* Record ID # 100 - Total population */",
    "/* Record ID # 111 - Total main workers */",
    "/ WORK6 11-19 WORKr6 20-28 /* estimated */",
    "/* Record ID # 112 - Farm workers (main) */",
    "/ FARM6 11-19 FARMr6 20-28 /* estimated */",
    "/* Record ID # 140 - Literates (ages 5+) */",
    "/* Record ID # 151 - Primary School or higher */",
    "/* Record ID # 153 - Matriculates or higher */"
  ), path)
  expect_true(validate_vanneman_pretrend_sas_contract(path))

  writeLines("/* Record ID # 100 - Total population */", path)
  expect_error(
    validate_vanneman_pretrend_sas_contract(path),
    "semantic contract"
  )
})

test_that("Vanneman pretrend reader treats negative count sentinels as unavailable", {
  path <- tempfile(fileext = ".data.gz")
  con <- gzfile(path, "wt")
  records <- vanneman_pretrend_record_registry()$record_id
  lines <- character()
  for (year in c(61, 71, 81, 91)) for (record in records) {
    total <- if (record == "151" && year == 81) -2 else 100
    lines <- c(lines, sprintf("0201%s%02d5%9d%9d%9d%9d", record, year, total, 80, 60, 50))
  }
  writeLines(lines, con); close(con)
  on.exit(unlink(path), add = TRUE)

  out <- read_vanneman_panel4_pretrend_counts(path)
  expect_true(is.na(out$total[out$record_id == "151" & out$year == 1981L]))
})

test_that("Vanneman pretrend levels enforce accounting identities and reviewed geography", {
  rows <- lapply(c(1961L, 1971L, 1981L, 1991L), function(year) {
    safe_bind_rows(list(
      data.frame(panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01", record_id="100", year=year, version=5, total=1000, rural=700, male=600, rural_male=420),
      data.frame(panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01", record_id="111", year=year, version=5, total=400, rural=300, male=300, rural_male=220),
      data.frame(panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01", record_id="112", year=year, version=5, total=300, rural=250, male=230, rural_male=190),
      data.frame(panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01", record_id="140", year=year, version=5, total=500, rural=350, male=320, rural_male=220),
      data.frame(panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01", record_id="151", year=year, version=5, total=200, rural=150, male=140, rural_male=100),
      data.frame(panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01", record_id="153", year=year, version=5, total=100, rural=70, male=80, rural_male=55)
    ))
  })
  counts <- safe_bind_rows(rows)
  geography <- data.frame(
    panel_unit_id="0201", dist91_state_id="02", dist91_district_id="01",
    state_code_2001="28", district_code_2001="01",
    pretrend_geography_status="preferred_single_target",
    preferred_vanneman_pretrend_eligible=TRUE,
    stringsAsFactors=FALSE
  )
  out <- build_vanneman_pretrend_levels(counts, geography)
  expect_equal(unique(out$urban_share), .3)
  expect_equal(unique(out$main_worker_share), .4)
  expect_equal(unique(out$nonfarm_worker_share_main_workers), .25)
  expect_equal(unique(out$literate_share_population), .5)
  expect_equal(unique(out$primary_plus_share_population), .2)
  expect_equal(unique(out$matriculate_plus_share_population), .1)

  missing_geo <- geography
  missing_geo$panel_unit_id <- "9999"
  expect_error(
    build_vanneman_pretrend_levels(counts, missing_geo),
    "stable-ID universes differ"
  )

  bad <- counts
  bad$total[bad$record_id == "112" & bad$year == 1961L] <- 500
  expect_error(build_vanneman_pretrend_levels(bad, geography), "accounting identities")
})

test_that("Vanneman pretrend changes use stable units and predetermined 1961 weights", {
  levels <- safe_bind_rows(lapply(c(1961L, 1971L, 1981L, 1991L), function(year) {
    data.frame(
      panel_unit_id="0201", vanneman_state_id="02", vanneman_district_id="01",
      year=year, version=5, population=1000 + year-1961,
      log_population=log(1000 + year-1961), urban_share=(year-1960)/100,
      main_worker_share=.4, nonfarm_worker_share_main_workers=.7,
      literate_share_population=.5,
      primary_plus_share_population=.2, matriculate_plus_share_population=.1,
      dist91_state_id="02", dist91_district_id="01",
      state_code_2001="28", district_code_2001="01",
      pretrend_geography_status="preferred_single_target",
      preferred_vanneman_pretrend_eligible=TRUE, stringsAsFactors=FALSE
    )
  }))
  out <- build_vanneman_pretrend_changes(levels)
  row <- out[out$period_id == "1961_1981" & out$measure_id == "urban_share", , drop=FALSE]
  expect_equal(row$change, .20)
  expect_equal(row$population_1961, 1000)

  labor <- out[
    out$period_id == "1961_1981" &
      out$measure_id == "main_worker_share",
    , drop = FALSE
  ]
  later_labor <- out[
    out$period_id == "1971_1981" &
      out$measure_id == "main_worker_share",
    , drop = FALSE
  ]
  expect_true(labor$contains_estimated_source)
  expect_false(later_labor$contains_estimated_source)
})

test_that("Vanneman pretrend registries remain narrow and entirely pre-treatment", {
  expect_true(all(vanneman_pretrend_period_registry()$end_year <= 1991L))
  measures <- vanneman_pretrend_measure_registry()
  expect_setequal(unique(measures$domain), c("demography", "labor", "education"))
  expect_equal(nrow(measures), 7L)
  expect_false(any(grepl("migration|religion|school_supply", measures$measure_id)))
})

test_that("Vanneman pretrend regressions honor reviewed geography and state-FE inference", {
  set.seed(58)
  n <- 60L
  ids <- sprintf("%04d", seq_len(n))
  states <- rep(sprintf("%02d", 1:6), each = 10)
  districts <- sprintf("%02d", seq_len(n))
  changes <- data.frame(
    panel_unit_id = ids,
    dist91_state_id = states,
    dist91_district_id = districts,
    state_code_2001 = states,
    district_code_2001 = districts,
    preferred_vanneman_pretrend_eligible = TRUE,
    pretrend_geography_status = "preferred_single_target",
    period_id = "1961_1981",
    start_year = 1961L,
    end_year = 1981L,
    measure_id = "urban_share",
    domain = "demography",
    label = "Urban share",
    start_value = 0.2,
    end_value = 0.2 + stats::rnorm(n, sd = 0.03),
    change = stats::rnorm(n, sd = 0.03),
    population_1961 = 1000 + seq_len(n),
    stringsAsFactors = FALSE
  )
  district_panel <- data.frame(
    state_code_2001 = states,
    district_code_2001 = districts,
    emi_exposure_all_children_0708 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  distance <- data.frame(
    state_code_1991 = states,
    district_code_1991 = districts,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )

  panel <- build_vanneman_pretrend_predictor_panel(changes, district_panel, distance)
  emie <- estimate_vanneman_pretrend_association(
    panel, "emie_exposure", "1961_1981", "urban_share", "state"
  )
  ld <- estimate_vanneman_pretrend_association(
    panel, "ling_distance_nonzero_mean_1991", "1961_1981", "urban_share", "state"
  )

  expect_equal(emie$status, "estimated")
  expect_equal(ld$status, "estimated")
  expect_equal(emie$n, n)
  expect_equal(ld$n, n)
  expect_true(is.finite(emie$p.value))
  expect_true(is.finite(ld$p.value))
})


test_that("Vanneman pretrend specification registry makes common support explicit", {
  panel <- data.frame(
    historical_ld_eligible = c(TRUE, FALSE),
    ling_distance_nonzero_mean_1991 = c(1, NA_real_),
    stringsAsFactors = FALSE
  )
  registry <- vanneman_pretrend_specification_registry(panel)

  expect_equal(nrow(registry), 3L)
  expect_true(any(
    registry$predictor_id == "eventual_emie" &
      registry$sample_id == "full_pretrend"
  ))
  expect_true(any(
    registry$predictor_id == "eventual_emie" &
      registry$sample_id == "historical_ld_support"
  ))
  expect_true(any(
    registry$predictor_id == "historical_ld_1991" &
      registry$sample_id == "historical_ld_support"
  ))
  expect_false(any(
    registry$predictor_id == "historical_ld_1991" &
      registry$sample_id == "full_pretrend"
  ))
})

test_that("Vanneman pretrend common support restricts EMIE and LD to identical units", {
  panel <- data.frame(
    panel_unit_id = rep(c("a", "b", "c"), each = 2),
    preferred_vanneman_pretrend_eligible = TRUE,
    period_id = "1961_1981",
    measure_id = rep(c("log_population", "urban_share"), times = 3),
    change = 1:6,
    population_1961 = rep(c(100, 200, 300), each = 2),
    state_code_2001 = rep(c("01", "01", "02"), each = 2),
    emie_exposure = rep(c(1, 2, 3), each = 2),
    historical_ld_eligible = rep(c(TRUE, FALSE, TRUE), each = 2),
    ling_distance_nonzero_mean_1991 = rep(c(.2, NA, .8), each = 2),
    stringsAsFactors = FALSE
  )
  emie <- vanneman_pretrend_sample(
    panel, "emie_exposure", "1961_1981", "urban_share",
    "historical_ld_support"
  )
  ld <- vanneman_pretrend_sample(
    panel, "ling_distance_nonzero_mean_1991", "1961_1981", "urban_share",
    "historical_ld_support"
  )

  expect_setequal(emie$panel_unit_id, c("a", "c"))
  expect_setequal(ld$panel_unit_id, c("a", "c"))
})

test_that("Vanneman pretrend coverage reports the cost of historical-LD support", {
  panel <- data.frame(
    panel_unit_id = rep(c("a", "b", "c"), each = 2),
    preferred_vanneman_pretrend_eligible = TRUE,
    state_code_2001 = rep(c("01", "01", "02"), each = 2),
    population_1961 = rep(c(100, 200, 300), each = 2),
    emie_exposure = rep(c(1, 2, 3), each = 2),
    ling_distance_nonzero_mean_2001 = rep(c(.3, .5, .9), each = 2),
    historical_ld_eligible = rep(c(TRUE, FALSE, TRUE), each = 2),
    stringsAsFactors = FALSE
  )
  out <- vanneman_pretrend_sample_coverage(panel)
  full <- out[out$sample_id == "full_pretrend", , drop = FALSE]
  common <- out[out$sample_id == "historical_ld_support", , drop = FALSE]

  expect_equal(full$n_units, 3L)
  expect_equal(common$n_units, 2L)
  expect_equal(common$share_of_full_units, 2 / 3)
  expect_equal(common$population_1961, 400)
})


test_that("historical-parent predictors aggregate EMIE and LD2001 from components", {
  bridge <- data.frame(
    panel_unit_id = c("a", "a", "b"),
    dist91_state_id = c("02", "02", "02"),
    dist91_district_id = c("01", "01", "02"),
    state_code_2001 = c("28", "28", NA),
    district_code_2001 = c("11", "12", NA),
    parent_bridge_status = c(
      "preferred_historical_parent_split",
      "preferred_historical_parent_split",
      "merger_requires_amalgamation"
    ),
    preferred_vanneman_parent_eligible = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  panel <- data.frame(
    state_code_2001 = c("28", "28"),
    district_code_2001 = c("11", "12"),
    eligible_child_weight_0708 = c(100, 300),
    emi_enrolled_child_weight_0708 = c(20, 120),
    emi_exposure_all_children_0708 = c(.2, .4),
    ling_total_speakers = c(1000, 3000),
    ling_distance_nonzero_mean = c(2, 4),
    ling_share_distance_1 = c(0, 0),
    ling_share_distance_2 = c(100, 0),
    ling_share_distance_3 = c(0, 0),
    ling_share_distance_4 = c(0, 100),
    ling_share_distance_5 = c(0, 0),
    stringsAsFactors = FALSE
  )

  out <- aggregate_vanneman_parent_predictors(bridge, panel)
  a <- out[out$panel_unit_id == "a", , drop = FALSE]
  b <- out[out$panel_unit_id == "b", , drop = FALSE]

  expect_equal(a$emie_exposure, .35)
  expect_equal(a$ling_distance_nonzero_mean_2001, 3.5)
  expect_equal(a$n_descendant_2001_districts, 2L)
  expect_true(a$pretrend_analysis_eligible)
  expect_false(b$pretrend_analysis_eligible)
})

test_that("pretrend registry adds Census-2001 LD without changing historical common support", {
  panel <- data.frame(
    historical_ld_eligible = c(TRUE, FALSE),
    ling_distance_nonzero_mean_1991 = c(1, NA_real_),
    ling_distance_nonzero_mean_2001 = c(2, 3),
    stringsAsFactors = FALSE
  )
  registry <- vanneman_pretrend_specification_registry(panel)

  expect_equal(nrow(registry), 5L)
  expect_true(any(
    registry$predictor_id == "census_2001_ld" &
      registry$sample_id == "full_pretrend"
  ))
  expect_true(any(
    registry$predictor_id == "census_2001_ld" &
      registry$sample_id == "historical_ld_support"
  ))
})


test_that("external historical LD attaches by exact Census-1991 code", {
  changes <- data.frame(panel_unit_id="a",dist91_state_id="02",dist91_district_id="03",year=1961L,stringsAsFactors=FALSE)
  predictors <- data.frame(panel_unit_id="a",state_code_2001="28",district_code_2001="11",pretrend_analysis_eligible=TRUE,pretrend_analysis_geography_status="preferred_single_target",emie_exposure=.25,stringsAsFactors=FALSE)
  external <- data.frame(state_code_1991="02",district_code_1991="03",linguistic_distance_1991_helms_lim=4.75,stringsAsFactors=FALSE)
  out <- attach_vanneman_pretrend_predictors(changes,predictors,external_historical_distance=external)
  expect_equal(out$ling_distance_helms_lim_1991,4.75); expect_true(out$helms_lim_ld_eligible)
})

test_that("pretrend registry keeps Atlas primary while exposing Helms-Lim robustness", {
  panel <- data.frame(historical_ld_eligible=c(TRUE,FALSE),ling_distance_nonzero_mean_1991=c(1,NA_real_),ling_distance_nonzero_mean_2001=c(2,3),ling_distance_helms_lim_1991=c(1.1,3.1),stringsAsFactors=FALSE)
  registry <- vanneman_pretrend_specification_registry(panel)
  expect_true(any(registry$predictor_id=="helms_lim_ld_1991" & registry$sample_id=="full_pretrend"))
  expect_true(any(registry$predictor_id=="historical_ld_1991" & registry$sample_id=="historical_ld_support"))
  expect_false(any(registry$predictor_id=="historical_ld_1991" & registry$sample_id=="full_pretrend"))
})


test_that("optional Helms-Lim source is truly optional for predictor attachment", {
  changes <- data.frame(
    panel_unit_id = c("a", "b"),
    dist91_state_id = c("02", "02"),
    dist91_district_id = c("01", "02"),
    stringsAsFactors = FALSE
  )
  predictors <- data.frame(
    panel_unit_id = c("a", "b"),
    state_code_2001 = c("28", "28"),
    district_code_2001 = c("11", "12"),
    pretrend_analysis_eligible = TRUE,
    pretrend_analysis_geography_status = "preferred_single_target",
    emie_exposure = c(.2, .4),
    stringsAsFactors = FALSE
  )

  out <- attach_vanneman_pretrend_predictors(changes, predictors)

  expect_identical(out$helms_lim_ld_eligible, c(FALSE, FALSE))
  expect_false("ling_distance_helms_lim_1991" %in% names(out))
})

test_that("Atlas common support is not redefined by optional Helms-Lim coverage", {
  panel <- data.frame(
    panel_unit_id = rep(c("a", "b", "c"), each = 2),
    preferred_vanneman_pretrend_eligible = TRUE,
    period_id = "1961_1981",
    measure_id = rep(c("log_population", "urban_share"), times = 3),
    change = 1:6,
    population_1961 = rep(c(100, 200, 300), each = 2),
    state_code_2001 = rep(c("01", "01", "02"), each = 2),
    emie_exposure = rep(c(1, 2, 3), each = 2),
    ling_distance_nonzero_mean_2001 = rep(c(.3, .5, .9), each = 2),
    historical_ld_eligible = rep(c(TRUE, TRUE, FALSE), each = 2),
    ling_distance_nonzero_mean_1991 = rep(c(.2, .4, NA), each = 2),
    ling_distance_helms_lim_1991 = rep(c(.25, NA, .8), each = 2),
    stringsAsFactors = FALSE
  )

  emie_common <- vanneman_pretrend_sample(
    panel, "emie_exposure", "1961_1981", "urban_share",
    "historical_ld_support"
  )
  atlas_common <- vanneman_pretrend_sample(
    panel, "ling_distance_nonzero_mean_1991",
    "1961_1981", "urban_share", "historical_ld_support"
  )
  helms_common <- vanneman_pretrend_sample(
    panel, "ling_distance_helms_lim_1991",
    "1961_1981", "urban_share", "historical_ld_support"
  )

  expect_setequal(emie_common$panel_unit_id, c("a", "b"))
  expect_setequal(atlas_common$panel_unit_id, c("a", "b"))
  expect_identical(helms_common$panel_unit_id, "a")
})

test_that("pretrend specification registry is additive across optional sources", {
  base <- data.frame(
    historical_ld_eligible = c(TRUE, FALSE),
    ling_distance_nonzero_mean_1991 = c(1, NA_real_),
    stringsAsFactors = FALSE
  )
  base_registry <- vanneman_pretrend_specification_registry(base)
  expect_setequal(
    paste(base_registry$predictor_id, base_registry$sample_id, sep = "__"),
    c(
      "eventual_emie__full_pretrend",
      "eventual_emie__historical_ld_support",
      "historical_ld_1991__historical_ld_support"
    )
  )

  expanded <- base
  expanded$ling_distance_nonzero_mean_2001 <- c(2, 3)
  expanded$ling_distance_helms_lim_1991 <- c(1.1, 3.1)
  registry <- vanneman_pretrend_specification_registry(expanded)
  expect_equal(nrow(registry), 7L)
  expect_true(any(
    registry$predictor_id == "helms_lim_ld_1991" &
      registry$sample_id == "full_pretrend"
  ))
  expect_true(any(
    registry$predictor_id == "census_2001_ld" &
      registry$sample_id == "historical_ld_support"
  ))
})

test_that("strict and historical-parent support comparison reports sample gain", {
  strict <- list(sample_coverage = data.frame(
    sample_id = c(
      "full_pretrend", "census_2001_ld_support",
      "helms_lim_ld_support", "historical_ld_support"
    ),
    n_units = c(160L, 158L, 159L, 60L),
    n_states = c(17L, 17L, 17L, 12L),
    population_1961 = c(1000, 990, 995, 400),
    share_of_full_units = c(1, 158/160, 159/160, 60/160),
    stringsAsFactors = FALSE
  ))
  parent <- list(sample_coverage = data.frame(
    sample_id = c(
      "full_pretrend", "census_2001_ld_support",
      "helms_lim_ld_support", "historical_ld_support"
    ),
    n_units = c(190L, 188L, 189L, 70L),
    n_states = c(18L, 18L, 18L, 13L),
    population_1961 = c(1200, 1180, 1190, 500),
    share_of_full_units = c(1, 188/190, 189/190, 70/190),
    stringsAsFactors = FALSE
  ))

  out <- build_vanneman_pretrend_support_comparison(strict, parent)
  parent_full <- out[
    out$analysis_id == "historical_parent" &
      out$sample_id == "full_pretrend",
    , drop = FALSE
  ]

  expect_equal(parent_full$gain_vs_strict_full_n, 30L)
  expect_equal(parent_full$gain_vs_strict_full_share, 30 / 160)
  expect_error(
    build_vanneman_pretrend_support_comparison(parent, strict),
    "cannot be smaller"
  )
})


test_that("eventual EMIE remains a core pretrend registry contract", {
  panel <- data.frame(
    historical_ld_eligible = TRUE,
    ling_distance_nonzero_mean_1991 = 2,
    stringsAsFactors = FALSE
  )
  registry <- vanneman_pretrend_specification_registry(panel)
  expect_true(any(
    registry$predictor_id == "eventual_emie" &
      registry$sample_id == "full_pretrend"
  ))
  expect_true(any(
    registry$predictor_id == "eventual_emie" &
      registry$sample_id == "historical_ld_support"
  ))
})
