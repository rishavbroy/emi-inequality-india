git_attribute <- function(path, attribute) {
  root <- dirname(repo_file(".gitattributes"))
  output <- suppressWarnings(system2(
    "git",
    c(
      "-C", shQuote(root),
      "check-attr", shQuote(attribute),
      "--", shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "git check-attr failed for ", path, ": ",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }
  if (length(output) != 1L) {
    stop("git check-attr returned unexpected output for ", path, call. = FALSE)
  }
  sub("^.*: [^:]+: ", "", output[[1]])
}

test_that("rendered and archived artifacts are treated as binary by Git", {
  artifacts <- c(
    "posters/2026_predoc_conference/poster.pdf",
    "outputs/figures/main/map_emi_exposure.png",
    "docs/plan/TO-DO Research Paper ECON 623.docx"
  )

  expect_identical(
    unname(vapply(artifacts, git_attribute, character(1), attribute = "diff")),
    rep("unset", length(artifacts))
  )
})


test_that("current public build helper scripts parse", {
  expect_silent(parse(repo_file("_targets.R")))
  for (file in repo_pipeline_target_files()) {
    expect_silent(parse(file))
  }
  expect_silent(parse(repo_file("scripts", "check_required_outputs.R")))
  expect_silent(parse(repo_file("scripts", "check_targets_process.R")))
  expect_silent(parse(repo_file("scripts", "run_targets_checked.R")))
  expect_silent(parse(repo_file("scripts", "run_targets_strict.R")))
  expect_silent(parse(repo_file("scripts", "target_metadata_helpers.R")))
  expect_silent(parse(repo_file("R", "output", "output_hygiene.R")))
  expect_silent(parse(repo_file("scripts", "check_rendered_text.R")))
  expect_silent(parse(repo_file("scripts", "audit_outputs_final.R")))
  expect_silent(parse(repo_file("scripts", "public_output_contract.R")))
  expect_silent(parse(repo_file("scripts", "check_report_values.R")))
  expect_silent(parse(repo_file("R", "output", "public_qmd_helpers.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_core.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_coefficients.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_selection_ame.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_spatial.R")))
  expect_silent(parse(repo_file("R", "application_samples", "writing_sample_sections.R")))
})

test_that("raw source registry is a tracked target input", {
  manifest <- repo_target_manifest()
  row <- manifest[manifest$name == "raw_file_manifest_file", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_identical(row$format[[1L]], "file")

  command <- parse(text = repo_target_command("raw_manifest"))[[1L]]
  expect_true("raw_file_manifest_file" %in% all.names(command))
})

test_that("paper render has one current manuscript target", {
  manifest <- repo_target_manifest()
  paper_targets <- manifest$name[manifest$name %in% c(
    "paper_qmd", "paper", "paper_new_qmd", "paper_new"
  )]

  expect_setequal(paper_targets, c("paper_qmd", "paper"))
  expect_match(repo_target_command("paper_qmd"), "paper/paper.qmd", fixed = TRUE)
  expect_false(grepl("paper-new", repo_target_definition_text(), fixed = TRUE))
})

test_that("conference poster requirements are opt-in", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "public_output_contract.R"), envir = env)

  ordinary <- env$required_final_documents(
    require_application_samples = FALSE, require_poster = FALSE
  )
  with_poster <- env$required_final_documents(
    require_application_samples = FALSE, require_poster = TRUE
  )

  expect_false(any(grepl("posters/", ordinary, fixed = TRUE)))
  expect_true("posters/2026_predoc_conference/poster.pdf" %in% with_poster)
  expect_true("posters/2026_predoc_conference/RishavRoy-Education.png" %in% with_poster)

  ordinary_inputs <- env$required_public_render_inputs(require_poster = FALSE)
  poster_inputs <- env$required_public_render_inputs(require_poster = TRUE)
  expect_false("posters/2026_predoc_conference/generated/poster_second_stage_specs.pdf" %in% ordinary_inputs)
  expect_true("posters/2026_predoc_conference/generated/poster_second_stage_specs.pdf" %in% poster_inputs)
  expect_false(any(grepl("poster_emie_expected_values", c(ordinary_inputs, poster_inputs), fixed = TRUE)))
})

test_that("public QMD helper loads its table-formatting dependencies", {
  env <- new.env(parent = baseenv())
  sys.source(repo_file("R", "output", "public_qmd_helpers.R"), envir = env)

  rows <- env$public_probit_ame_rows(data.frame(
    Term = "Fixture",
    estimate = 0.1,
    std.error = 0.05,
    p.value = 0.08,
    check.names = FALSE
  ))

  expect_equal(
    rows$Estimate,
    c(paste0("0.100", env$significance_stars(0.08)), "(0.050)")
  )
})

test_that("audit workspace cleanup removes transient state and preserves optional outputs", {
  root <- tempfile("audit-clean-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  transient <- c(
    "outputs/build/build.csv",
    "outputs/diagnostics/public/public.csv",
    "outputs/diagnostics/root.csv",
    "outputs/diagnostics/extended/district_lineage_v2/stale.csv",
    "data/processed/geography_v2/stale.gpkg",
    "paper/references_bibertool.bib"
  )
  preserved <- c(
    "outputs/diagnostics/extended/current.csv",
    "outputs/benchmarking/current.csv",
    "data/processed/geography/current.gpkg"
  )

  for (path in c(transient, preserved)) {
    full <- file.path(root, path)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    writeLines("fixture", full)
  }

  status <- system2(
    "bash",
    c(
      shQuote(repo_file("scripts", "clean_audit_workspace.sh")),
      shQuote(root)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(status, "status"))
  expect_false(any(file.exists(file.path(root, transient))))
  expect_true(all(file.exists(file.path(root, preserved))))
  expect_true(dir.exists(file.path(root, "outputs/build")))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/public")))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/extended")))
  expect_true(dir.exists(file.path(root, "outputs/benchmarking")))
})



test_that("debug review archives retain intermediate diagnostics but exclude raw data", {
  skip_if(Sys.which("git") == "")
  skip_if(Sys.which("zip") == "")
  root <- tempfile("review-archive-intermediates-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(root, "scripts"), recursive = TRUE)
  dir.create(
    file.path(root, "outputs", "diagnostics", "extended"),
    recursive = TRUE
  )
  dir.create(file.path(root, "outputs", "benchmarking"), recursive = TRUE)
  dir.create(file.path(root, "outputs", "replication", "processed"), recursive = TRUE)
  dir.create(file.path(root, "data", "processed"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw"), recursive = TRUE)
  dir.create(file.path(root, "application-samples", "filters"), recursive = TRUE)
  dir.create(file.path(root, "application-samples", "output"), recursive = TRUE)
  file.copy(
    repo_file("scripts", "make_review_archive.sh"),
    file.path(root, "scripts", "make_review_archive.sh")
  )
  writeLines("tracked", file.path(root, "README.md"))
  writeLines(
    "diagnostic",
    file.path(root, "outputs", "diagnostics", "extended", "intermediate.csv")
  )
  writeLines(
    "benchmark",
    file.path(root, "outputs", "benchmarking", "runtime.csv")
  )
  writeLines(
    "target,status,detail\nexample,value_mismatch,example detail",
    file.path(root, "outputs", "replication", "processed", "verification.csv")
  )
  writeLines("processed", file.path(root, "data", "processed", "panel.csv"))
  writeLines("raw", file.path(root, "data", "raw", "private.csv"))
  writeLines("schema_version: 1", file.path(root, "application-samples", "samples.yml"))
  writeLines("-- filter", file.path(root, "application-samples", "filters", "select-sections.lua"))
  writeLines("generated", file.path(root, "application-samples", "output", "sample.pdf"))

  system2("git", c("-C", shQuote(root), "init", "-q"))
  system2(
    "git",
    c(
      "-C", shQuote(root), "add", "README.md", "scripts/make_review_archive.sh",
      "application-samples/samples.yml", "application-samples/filters/select-sections.lua"
    )
  )

  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  writeLines("stale archive", "review.zip")
  output <- system2(
    "bash",
    c(
      "scripts/make_review_archive.sh",
      "--without-samples",
      "--allow-incomplete",
      "--output", "review.zip"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"))
  expect_true(file.info("review.zip")$size > nchar("stale archive"))
  listing <- utils::unzip("review.zip", list = TRUE)$Name
  expect_true(
    "outputs/diagnostics/extended/intermediate.csv" %in% listing
  )
  expect_true("outputs/benchmarking/runtime.csv" %in% listing)
  expect_true("outputs/replication/processed/verification.csv" %in% listing)
  expect_true("data/processed/panel.csv" %in% listing)
  expect_true("application-samples/samples.yml" %in% listing)
  expect_true("application-samples/filters/select-sections.lua" %in% listing)
  expect_false(any(grepl("^application-samples/output/", listing)))
  expect_false("data/raw/private.csv" %in% listing)
})

test_that("selected target warning scope includes executed dependencies", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  progress <- data.frame(
    name = c("selected", "dependency", "unrelated"),
    progress = c("skipped", "completed", "skipped"),
    stringsAsFactors = FALSE
  )

  scope <- env$target_run_metadata_scope("selected", progress)

  expect_setequal(scope, c("selected", "dependency"))
  expect_false("unrelated" %in% scope)
})

test_that("programmatic metadata selection stays inside tidyselect context", {
  skip_if_not_installed("rlang")
  skip_if_not_installed("tidyselect")

  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  selection <- env$target_metadata_selection(c("selected", "missing"))
  columns <- data.frame(
    selected = logical(),
    unrelated = logical(),
    check.names = FALSE
  )

  resolved <- tidyselect::eval_select(selection, columns)

  expect_identical(names(resolved), "selected")
})

test_that("target warning metadata normalizes list columns and consolidates runs", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  meta <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  meta$warnings <- I(list(c("first", "second"), character()))
  meta$error <- I(list(character(), character()))
  path <- tempfile(fileext = ".csv")

  normalized <- env$normalize_target_metadata(meta)
  env$record_target_warnings(normalized, "optional", path)

  expect_equal(normalized$warnings[[1]], "first; second")
  recorded <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(recorded$name, "a")
  expect_equal(recorded$run_label, "optional")
})

test_that("new-machine setup restores the tracked renv lockfile without rewriting it", {
  skip_if(Sys.which("make") == "")
  output <- system2(
    "make",
    c("-n", "-f", shQuote(repo_file("Makefile")), "init-renv"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(attr(output, "status") %||% 0L, 0L)
  expect_true(any(grepl("renv::restore", output, fixed = TRUE)))
  expect_false(any(grepl("renv::snapshot|renv::install|renv::init", output)))

  scripts <- list.files("scripts", pattern = "\\.[Rr]$", full.names = TRUE)
  guidance <- unlist(lapply(scripts, readLines, warn = FALSE), use.names = FALSE)
  expect_false(any(grepl("make init-renv", guidance, fixed = TRUE)))
})

test_that("Makefile cache roots honor TMPDIR instead of assuming macOS paths", {
  skip_if(Sys.which("make") == "")
  temp_root <- tempdir()
  output <- system2(
    "make",
    c("-n", "-f", shQuote(repo_file("Makefile")), "pipeline"),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("TMPDIR=", temp_root)
  )

  expect_identical(attr(output, "status") %||% 0L, 0L)
  expect_true(any(grepl(temp_root, output, fixed = TRUE)))
  expect_false(any(grepl("/private/tmp", output, fixed = TRUE)))
})

test_that("target issue printer selects columns without data-frame drop warnings", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  rows <- data.frame(
    name = "district_panel",
    error = "example failure",
    extra = "ignored",
    stringsAsFactors = FALSE
  )

  expect_warning(
    output <- capture.output(env$print_target_issues(rows, "error", "Errored targets:")),
    NA
  )
  expect_match(paste(output, collapse = "\n"), "district_panel", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "example failure", fixed = TRUE)
  expect_false(grepl("ignored", paste(output, collapse = "\n"), fixed = TRUE))
})

test_that("active QMD citations resolve through the project bibliography", {
  source(repo_file("scripts", "public_output_contract.R"), local = TRUE)
  qmd_sources <- public_qmd_sources()
  bibliography <- readLines(repo_file("paper", "references.bib"), warn = FALSE)
  bibliography_keys <- sub(
    "^@[[:alpha:]]+\\{([^,]+),.*$",
    "\\1",
    grep("^@[[:alpha:]]+\\{[^,]+,", bibliography, value = TRUE)
  )

  expect_identical(anyDuplicated(bibliography_keys), 0L)

  for (path in qmd_sources) {
    text <- repo_text(path)
    citation_tokens <- unique(unlist(regmatches(
      text,
      gregexpr("(?<![[:alnum:]_.+-])@[[:alnum:]_:.#$%&+?/-]+", text, perl = TRUE)
    )))
    citation_keys <- sub("^@", "", citation_tokens)
    citation_keys <- sub("\\.$", "", citation_keys)
    citation_keys <- citation_keys[!grepl(
      "^(fig|tbl|sec|eq|app|thm|lem|cor)-",
      citation_keys
    )]
    missing_keys <- setdiff(citation_keys, bibliography_keys)

    expect_identical(
      missing_keys,
      character(),
      info = paste(path, "has unresolved bibliography keys:", paste(missing_keys, collapse = ", "))
    )
  }

})


poster_renderer_test_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "output", "render_public_artifacts.R"), envir = env)
  env
}

test_that("poster PNG rendering supports paths containing spaces", {
  skip_if(!nzchar(Sys.which("pdftoppm")), "pdftoppm is unavailable")
  renderer <- poster_renderer_test_env()
  dir <- file.path(tempdir(), "poster preview with spaces")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  pdf <- file.path(dir, "poster source.pdf")
  png <- file.path(dir, "poster preview.png")
  grDevices::pdf(pdf, width = 2, height = 2)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "poster")
  grDevices::dev.off()

  out <- renderer$render_poster_png(pdf, png, dpi = 72)

  expect_identical(out, png)
  expect_true(file.exists(png))
  expect_gt(file.info(png)$size, 0)
})


test_that("Census downloader discovers year manifests and skips present files", {
  root <- tempfile("census-download-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)

  years <- c(1991, 2001, 2011)
  manifests <- lapply(years, function(year) {
    data.frame(
      table = "C13", state_code = "01",
      relative_path = sprintf("data/raw/census_%d/age/C13/table.xls", year),
      url = sprintf("https://censusindia.gov.in/%d.xls", year),
      stringsAsFactors = FALSE
    )
  })
  for (i in seq_along(manifests)) {
    write.table(
      manifests[[i]],
      file.path(
        root, "data", "metadata",
        sprintf("census_%d_download_manifest.tsv", years[[i]])
      ),
      sep = "\t", quote = FALSE, row.names = FALSE
    )
  }

  present <- file.path(root, manifests[[2]]$relative_path)
  dir.create(dirname(present), recursive = TRUE)
  writeLines("existing", present)

  log_path <- file.path(root, "curl.log")
  fake_curl <- file.path(root, "curl")
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "out=''",
    "url=''",
    "while [[ $# -gt 0 ]]; do",
    "  case \"$1\" in",
    "    --output) out=\"$2\"; shift 2 ;;",
    "    --retry|--retry-delay|--connect-timeout) shift 2 ;;",
    "    --fail|--location) shift ;;",
    "    *) url=\"$1\"; shift ;;",
    "  esac",
    "done",
    "printf '%s\\n' \"$url\" >> \"$FAKE_CURL_LOG\"",
    "printf 'downloaded\\n' > \"$out\""
  ), fake_curl)
  Sys.chmod(fake_curl, mode = "0755")

  old <- Sys.getenv(c("EMI_PROJECT_ROOT", "CURL_BIN", "FAKE_CURL_LOG"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, setNames(list(old[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.setenv(EMI_PROJECT_ROOT = root, CURL_BIN = fake_curl, FAKE_CURL_LOG = log_path)

  output <- system2(
    "bash",
    shQuote(repo_file("scripts", "download_census_tables.sh")),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_null(attr(output, "status"))
  expect_identical(readLines(present), "existing")
  downloaded <- vapply(manifests[c(1, 3)], function(x) {
    file.path(root, x$relative_path)
  }, character(1))
  expect_true(all(file.exists(downloaded)))
  expect_true(all(vapply(downloaded, function(path) {
    identical(readLines(path), "downloaded")
  }, logical(1))))
  expect_setequal(readLines(log_path), vapply(manifests[c(1, 3)], `[[`, character(1), "url"))
  expect_true(any(grepl("2 downloaded, 1 already present", output, fixed = TRUE)))
})

test_that("Census 1991 acquisition manifest preserves source-specific published scope", {
  manifest <- utils::read.delim(
    repo_file("data", "metadata", "census_1991_download_manifest.tsv"),
    stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
  )
  validation_states <- sprintf("%02d", c(2:9, 11:33))

  expect_identical(names(manifest), c("table", "state_code", "relative_path", "url"))
  expect_identical(
    as.integer(table(manifest$table)[c("B01S", "C02T", "C02U", "C06T", "C09T")]),
    c(31L, 31L, 31L, 31L, 1L)
  )
  for (table_id in c("B01S", "C02T", "C02U", "C06T")) {
    expect_setequal(manifest$state_code[manifest$table == table_id], validation_states)
  }

  st16_states <- sprintf("%02d", c(2:4, 6:7, 9, 11:19, 21:27, 29:30, 32))
  st16 <- manifest[manifest$table == "ST16T", , drop = FALSE]
  expect_setequal(st16$state_code, st16_states)
  expect_true(all(startsWith(
    st16$relative_path,
    file.path("data", "raw", "census_1991", "scheduled_tribes", "ST16T")
  )))

  st17_state_codes <- sprintf("%02d", c(2:7, 9, 11:19, 21:23, 25:27, 29:30, 32))
  st17_district_codes <- sort(unique(c(st17_state_codes, "24")))
  st17_state <- manifest[manifest$table == "ST17T_STATE", , drop = FALSE]
  st17_district <- manifest[manifest$table == "ST17T_DISTRICT", , drop = FALSE]
  expect_equal(nrow(st17_state), 25L)
  expect_equal(nrow(st17_district), 416L)
  expect_setequal(st17_state$state_code, st17_state_codes)
  expect_setequal(unique(st17_district$state_code), st17_district_codes)
  expect_true(all(grepl("00\\.xlsx$", st17_state$relative_path)))
  expect_false(any(grepl("00\\.xlsx$", st17_district$relative_path)))
  expect_true(all(startsWith(
    st17_state$relative_path,
    file.path("data", "raw", "census_1991", "scheduled_tribes", "ST17T", "state")
  )))
  expect_true(all(startsWith(
    st17_district$relative_path,
    file.path("data", "raw", "census_1991", "scheduled_tribes", "ST17T", "district")
  )))

  religion <- manifest[manifest$table == "C09T", , drop = FALSE]
  expect_identical(religion$state_code, "01")
  expect_match(religion$relative_path, "^data/raw/census_1991/religion/C09T/")
  expect_true(all(grepl("^https://censusindia\\.gov\\.in/", manifest$url)))
  expect_identical(anyDuplicated(manifest$relative_path), 0L)
  expect_identical(anyDuplicated(manifest$url), 0L)
})



test_that("output hygiene distinguishes malformed empties from typed empties", {
  root <- tempfile("output-hygiene-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  utils::write.csv(
    data.frame(value = character()), file.path(root, "typed.csv"), row.names = FALSE
  )
  writeLines('""', file.path(root, "schema-less.csv"))

  empty_root <- tempfile("output-hygiene-empty-")
  dir.create(empty_root)
  expect_identical(
    basename(schema_less_csv_files(c(root, empty_root))),
    "schema-less.csv"
  )
})

test_that("duplicate-output warnings ignore cross-directory and replication equality", {
  root <- tempfile("output-duplicates-")
  dir.create(file.path(root, "diagnostics", "a"), recursive = TRUE)
  dir.create(file.path(root, "diagnostics", "b"), recursive = TRUE)
  dir.create(file.path(root, "replication", "processed"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  x <- data.frame(value = 1)
  utils::write.csv(x, file.path(root, "diagnostics", "a", "one.csv"), row.names = FALSE)
  utils::write.csv(x, file.path(root, "diagnostics", "a", "two.csv"), row.names = FALSE)
  utils::write.csv(x, file.path(root, "diagnostics", "b", "three.csv"), row.names = FALSE)
  utils::write.csv(x, file.path(root, "replication", "processed", "copy.csv"), row.names = FALSE)

  duplicates <- output_hygiene_duplicate_candidates(root)
  expect_equal(nrow(duplicates), 1L)
  expect_setequal(
    basename(c(duplicates$path_a, duplicates$path_b)), c("one.csv", "two.csv")
  )
})


test_that("output hygiene ignores byte-identical header-only tables", {
  root <- tempfile("output-hygiene-empty-duplicates-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.csv(data.frame(id = character()), file.path(root, "a.csv"), row.names = FALSE)
  utils::write.csv(data.frame(id = character()), file.path(root, "b.csv"), row.names = FALSE)

  expect_equal(nrow(output_hygiene_duplicate_candidates(root)), 0L)
})

test_that("full build scopes output hygiene to diagnostics generated in that run", {
  script <- paste(readLines(repo_file("scripts", "run_full_build.sh"), warn = FALSE), collapse = "\n")
  expect_match(
    script,
    'EMI_AUDIT_EXTENDED_DIAGNOSTICS="$with_extended_diagnostics"',
    fixed = TRUE
  )
})
