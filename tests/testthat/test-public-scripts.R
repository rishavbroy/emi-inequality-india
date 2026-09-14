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
  expect_silent(parse(repo_file("R", "output", "render_analysis_notes.R")))
  expect_silent(parse(repo_file("scripts", "check_rendered_text.R")))
  expect_silent(parse(repo_file("scripts", "audit_outputs_final.R")))
  expect_silent(parse(repo_file("scripts", "public_output_contract.R")))
  expect_silent(parse(repo_file("scripts", "check_report_values.R")))
  expect_silent(parse(repo_file("R", "output", "public_qmd_helpers.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_core.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_coefficients.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_selection_ame.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_spatial.R")))
  expect_silent(parse(repo_file("R", "application_samples", "extract_qmd_excerpts.R")))
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
    "outputs/diagnostics/build/build.csv",
    "outputs/diagnostics/public/public.csv",
    "outputs/diagnostics/root.csv",
    "outputs/diagnostics/extended/district_lineage_v2/stale.csv",
    "outputs/derived/district_lineage_v2/stale.gpkg",
    "paper/references_bibertool.bib"
  )
  preserved <- c(
    "outputs/diagnostics/extended/current.csv",
    "outputs/benchmarking/current.csv",
    "outputs/derived/district_lineage/current.gpkg"
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
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/build")))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/public")))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/extended")))
  expect_true(dir.exists(file.path(root, "outputs/benchmarking")))
})



test_that("writing sample YAML includes LaTeX table packages for raw table excerpts", {
  source(repo_file("R", "application_samples", "extract_qmd_excerpts.R"), local = TRUE)
  lines <- c(
    "---",
    "title: Test",
    "format:",
    "  pdf:",
    "    pdf-engine: xelatex",
    "---"
  )

  out <- normalize_sample_yaml(lines)

  expect_true(any(out == "  - \\usepackage{setspace}"))
  expect_true(any(out == "  - \\usepackage{threeparttable}"))
  expect_true(any(out == "  - \\usepackage{booktabs}"))
  expect_true(any(out == "  - \\usepackage{xcolor}"))
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
  dir.create(file.path(root, "data", "processed"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw"), recursive = TRUE)
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
  writeLines("processed", file.path(root, "data", "processed", "panel.csv"))
  writeLines("raw", file.path(root, "data", "raw", "private.csv"))

  system2("git", c("-C", shQuote(root), "init", "-q"))
  system2(
    "git",
    c("-C", shQuote(root), "add", "README.md", "scripts/make_review_archive.sh")
  )

  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
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
  listing <- utils::unzip("review.zip", list = TRUE)$Name
  expect_true(
    "outputs/diagnostics/extended/intermediate.csv" %in% listing
  )
  expect_true("outputs/benchmarking/runtime.csv" %in% listing)
  expect_true("data/processed/panel.csv" %in% listing)
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

audit_script_fixture <- function(manifest_exit = 0L, archive_exit = 0L) {
  root <- tempfile("public-audit-fixture-")
  dir.create(root, recursive = TRUE)
  for (dir in c("paper", "docs", "scripts", "R", "tests", "posters", "config")) {
    dir.create(file.path(root, dir), recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(
    repo_file("scripts", "run_public_build_audit.sh"),
    file.path(root, "scripts", "run_public_build_audit.sh")
  )
  for (script in c("clean_audit_workspace.sh", "check_source_syntax.sh")) {
    writeLines(c("#!/usr/bin/env bash", "set -euo pipefail", "exit 0"), file.path(root, "scripts", script))
  }
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "out=review.zip",
      "incomplete=false",
      "while (($#)); do",
      "  case \"$1\" in",
      "    --allow-incomplete) incomplete=true; shift ;;",
      "    --output) out=\"$2\"; shift 2 ;;",
      "    *) shift ;;",
      "  esac",
      "done",
      "if [[ \"${FAKE_ARCHIVE_EXIT:-0}\" -ne 0 ]]; then exit \"$FAKE_ARCHIVE_EXIT\"; fi",
      "if [[ \"$incomplete\" == true ]]; then printf incomplete > \"$out\"; else printf verified > \"$out\"; fi"
    ),
    file.path(root, "scripts", "make_review_archive.sh")
  )
  bin <- file.path(root, "bin")
  dir.create(bin)
  writeLines(c("#!/usr/bin/env bash", "exit 0"), file.path(bin, "make"))
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "if [[ \"${1:-}\" == scripts/write_output_manifest.R ]]; then",
      "  exit_code=\"${FAKE_MANIFEST_EXIT:-0}\"",
      "  if [[ \"$exit_code\" -eq 0 ]]; then",
      "    mkdir -p outputs/diagnostics/build",
      "    printf 'artifact_id,path\\nfixture,outputs/fixture.csv\\n' > outputs/diagnostics/build/output_manifest.csv",
      "  fi",
      "  exit \"$exit_code\"",
      "fi",
      "exit 0"
    ),
    file.path(bin, "Rscript")
  )
  runner <- file.path(root, "run-audit-fixture.sh")
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "export PATH=\"$PWD/bin:$PATH\"",
      "export FAKE_MANIFEST_EXIT=\"${1:-0}\"",
      "export FAKE_ARCHIVE_EXIT=\"${2:-0}\"",
      "exec bash scripts/run_public_build_audit.sh --incremental --with-extended-diagnostics --with-benchmarks"
    ),
    runner
  )
  Sys.chmod(c(
    file.path(root, "scripts", "run_public_build_audit.sh"),
    file.path(root, "scripts", "clean_audit_workspace.sh"),
    file.path(root, "scripts", "check_source_syntax.sh"),
    file.path(root, "scripts", "make_review_archive.sh"),
    file.path(bin, "make"), file.path(bin, "Rscript"), runner
  ), mode = "0755")
  system2("git", c("-C", shQuote(root), "init", "-q"))
  writeLines("stale", file.path(root, "review.zip"))
  list(
    root = root, runner = runner, manifest_exit = as.integer(manifest_exit),
    archive_exit = as.integer(archive_exit)
  )
}

run_audit_script_fixture <- function(fixture) {
  old <- setwd(fixture$root)
  on.exit(setwd(old), add = TRUE)
  system2(
    unname(Sys.which("bash")),
    c(
      shQuote(fixture$runner), as.character(fixture$manifest_exit),
      as.character(fixture$archive_exit)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
}

test_that("public audit always replaces review.zip with the current run", {
  skip_if(Sys.which("bash") == "")
  skip_if(Sys.which("git") == "")
  skip_if(Sys.which("python3") == "")

  success <- audit_script_fixture(0L)
  on.exit(unlink(success$root, recursive = TRUE, force = TRUE), add = TRUE)
  output <- run_audit_script_fixture(success)
  expect_null(attr(output, "status"))
  expect_identical(readChar(file.path(success$root, "review.zip"), 8L), "verified")
  expect_true(file.exists(file.path(
    success$root, "outputs", "diagnostics", "build", "output_manifest.csv"
  )))
  status <- jsonlite::read_json(file.path(
    success$root, "outputs", "diagnostics", "build", "audit_status.json"
  ))
  expect_identical(status$status, "passed")
  expect_identical(status$archive_mode, "verified")

  failed <- audit_script_fixture(7L)
  on.exit(unlink(failed$root, recursive = TRUE, force = TRUE), add = TRUE)
  output <- suppressWarnings(run_audit_script_fixture(failed))
  expect_identical(attr(output, "status"), 7L)
  expect_identical(readChar(file.path(failed$root, "review.zip"), 10L), "incomplete")
  status <- jsonlite::read_json(file.path(
    failed$root, "outputs", "diagnostics", "build", "audit_status.json"
  ))
  expect_identical(status$status, "failed")
  expect_identical(status$stage, "output-manifest")
  expect_equal(status$exit_code, 7L)
  expect_identical(status$archive_mode, "incomplete")
  expect_false("archive_on_failure" %in% names(status$options))

  packaging_failure <- audit_script_fixture(7L, archive_exit = 9L)
  on.exit(unlink(packaging_failure$root, recursive = TRUE, force = TRUE), add = TRUE)
  output <- suppressWarnings(run_audit_script_fixture(packaging_failure))
  expect_identical(attr(output, "status"), 7L)
  expect_false(file.exists(file.path(packaging_failure$root, "review.zip")))
  status <- jsonlite::read_json(file.path(
    packaging_failure$root, "outputs", "diagnostics", "build", "audit_status.json"
  ))
  expect_identical(status$archive_mode, "archive_failed")
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
  qmd_sources <- unique(c(public_qmd_sources(), "paper/paper-new.qmd"))
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


test_that("coding-sample specifications use one valid nonempty marker pair", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "application_samples", "extract_code_excerpts.R"), envir = env)
  specs <- list.files(
    repo_file("application-samples", "specs"),
    pattern = "^coding-.*\\.yml$",
    full.names = TRUE
  )
  expect_gt(length(specs), 0L)

  for (spec_path in specs) {
    spec <- yaml::read_yaml(spec_path)
    for (excerpt in spec$excerpts) {
      lines <- env$extract_between_sample_markers(
        repo_file(excerpt$file),
        excerpt$id
      )
      expect_true(
        sum(nzchar(trimws(lines))) > 1L,
        info = excerpt$id
      )
    }
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
