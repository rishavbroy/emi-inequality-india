repo_file <- function(...) {
  candidates <- c(
    file.path(getwd(), ...),
    file.path(getwd(), "..", ...),
    file.path(getwd(), "..", "..", ...)
  )
  hits <- candidates[file.exists(candidates)]
  if (!length(hits)) {
    stop("Could not locate repository file: ", file.path(...), call. = FALSE)
  }
  normalizePath(hits[[1]], mustWork = TRUE)
}

repo_text <- function(...) {
  paste(readLines(repo_file(...), warn = FALSE), collapse = "\n")
}

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
  expect_silent(parse(repo_file("scripts", "check_required_outputs.R")))
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

test_that("public render targets own final report, notes, and sample rendering", {
  targets <- repo_text("_targets.R")
  renderer <- repo_text("R", "output", "render_public_artifacts.R")
  samples <- repo_text("R", "application_samples", "render_writing_sample.R")

  expect_match(targets, 'tar_target(report_qmd, "paper/report.qmd", format = "file")', fixed = TRUE)
  expect_match(targets, 'tar_target(district_matching_qmd, "docs/district-matching.qmd", format = "file")', fixed = TRUE)
  expect_match(targets, 'render_public_html(district_matching_qmd, dependencies = list(report_values))', fixed = TRUE)
  expect_match(targets, 'tar_target(report, render_report_pdf(report_qmd, report_values, figure_files, table_files), format = "file")', fixed = TRUE)
  expect_match(targets, 'tar_target(application_sample_inputs, application_sample_input_files(), format = "file")', fixed = TRUE)
  expect_match(renderer, 'system2("quarto", c("render", report_qmd, "--to", "pdf"))', fixed = TRUE)
  expect_match(renderer, 'render_public_html <- function', fixed = TRUE)
  expect_match(samples, "application_sample_input_files", fixed = TRUE)
  expect_false(grepl("tar_render\\(report|tar_quarto\\(report", targets, perl = TRUE))
})

test_that("audit workspace cleanup removes transient state and preserves optional outputs", {
  root <- tempfile("audit-clean-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  transient <- c(
    "outputs/diagnostics/build/build.csv",
    "outputs/diagnostics/public/public.csv",
    "outputs/diagnostics/root.csv",
    "outputs/diagnostics/extended/district_lineage_v2/stale.csv",
    "outputs/derived/district_lineage_v2/stale.gpkg"
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


test_that("targets graph separates public diagnostics, extended diagnostics, and benchmarks", {
  src <- repo_text("_targets.R")

  expect_match(src, "core_pipeline_targets <- list", fixed = TRUE)
  expect_match(src, "extended_diagnostic_targets <- list", fixed = TRUE)
  expect_match(src, "benchmark_targets <- list", fixed = TRUE)
  expect_match(src, "diag_public_iv_panel", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation_files", fixed = TRUE)
  expect_match(src, "save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = \"file\"", fixed = TRUE)
  expect_match(src, "diag_ext_missingness", fixed = TRUE)
  expect_match(src, "bench_ame_methods", fixed = TRUE)
  expect_match(src, "EMI_RUN_EXTENDED_DIAGNOSTICS", fixed = TRUE)
  expect_match(src, "EMI_RUN_BENCHMARKS", fixed = TRUE)
  public_spatial_line <- grep(
    "tar_target\\(diag_public_spatial_autocorrelation,",
    strsplit(src, "\n", fixed = TRUE)[[1]],
    value = TRUE
  )
  expect_length(public_spatial_line, 1L)
  expect_false(grepl('tar_cue(mode = "always")', public_spatial_line, fixed = TRUE))
})

test_that("target warning metadata is written to build diagnostics", {
  strict <- repo_text("scripts", "run_targets_strict.R")
  helper <- repo_text("scripts", "target_metadata_helpers.R")
  audit <- repo_text("scripts", "run_public_build_audit.sh")

  expect_match(strict, "write_target_run_metadata(meta_active, \"strict\")", fixed = TRUE)
  expect_match(helper, "target_meta_after_", fixed = TRUE)
  expect_match(helper, "outputs/diagnostics/build/target_warnings.csv", fixed = TRUE)
  expect_match(audit, "outputs/diagnostics/build/target_warnings.csv", fixed = TRUE)
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

test_that("current QMD sources load shared public rendering helpers", {
  report <- repo_text("paper", "report.qmd")
  appendix <- repo_text("paper", "appendix.qmd")
  docs_note <- repo_text("docs", "district-matching.qmd")
  helper <- repo_text("R", "output", "public_qmd_helpers.R")

  expect_match(report, "public-output-table-helper", fixed = TRUE)
  expect_match(report, "source_public_qmd_helpers", fixed = TRUE)
  expect_match(appendix, "source_public_qmd_helpers", fixed = TRUE)
  expect_match(docs_note, "source_public_qmd_helpers", fixed = TRUE)
  expect_match(helper, "render_public_tex", fixed = TRUE)
  expect_match(helper, "knitr::asis_output(paste0", fixed = TRUE)
  expect_match(report, "\\usepackage{xcolor}", fixed = TRUE)
  expect_match(report, "\\definecolor{gray35}{gray}{0.35}", fixed = TRUE)
  expect_match(report, "\\usepackage{pdflscape}", fixed = TRUE)
})

test_that("report values use current named keys", {
  report <- repo_text("paper", "report.qmd")
  docs_note <- repo_text("docs", "district-matching.qmd")
  appendix <- repo_text("paper", "appendix.qmd")
  builder <- repo_text("R", "output", "build_report_values.R")
  spatial_values <- repo_text("R", "output", "report_value_spatial.R")
  checker <- repo_text("scripts", "check_report_values.R")

  expect_match(report, "report_value(\"ame_edu_free_pct\")", fixed = TRUE)
  expect_match(docs_note, "report_value(\"moran_iv_residual_p\")", fixed = TRUE)
  expect_match(builder, "moran_iv_residual_p", fixed = TRUE)
  expect_match(builder, "moran_consumption_growth_p", fixed = TRUE)
  expect_match(spatial_values, "spatial_p_value", fixed = TRUE)
  expect_match(checker, "public_report_value_sources", fixed = TRUE)
  expect_match(checker, "pattern <-", fixed = TRUE)
  expect_match(checker, "gregexpr(pattern", fixed = TRUE)
})

test_that("public-output checks share one file contract", {
  contract <- repo_text("scripts", "public_output_contract.R")
  required <- repo_text("scripts", "check_required_outputs.R")
  final <- repo_text("scripts", "check_public_final.R")
  audit <- repo_text("scripts", "audit_outputs_final.R")

  expect_match(contract, "required_public_render_inputs", fixed = TRUE)
  expect_match(contract, "required_final_documents", fixed = TRUE)
  expect_match(contract, "required_final_artifacts", fixed = TRUE)
  expect_match(contract, "spatial_moran_tests.csv", fixed = TRUE)
  expect_match(contract, "spatial_moran_mc_reference.csv", fixed = TRUE)
  expect_match(required, "required_public_render_inputs()", fixed = TRUE)
  expect_match(final, "required_final_documents(require_application_samples)", fixed = TRUE)
  expect_match(audit, "required_final_artifacts()", fixed = TRUE)
})

test_that("optional diagnostics and benchmarking targets use checked targets wrapper", {
  makefile <- repo_text("Makefile")
  strict <- repo_text("scripts", "run_targets_checked.R")
  targets <- repo_text("_targets.R")

  expect_match(makefile, "run_targets_checked.R --starts-with diag_ext_", fixed = TRUE)
  expect_match(makefile, "run_targets_checked.R --starts-with bench_", fixed = TRUE)
  expect_match(strict, "tar_make", fixed = TRUE)
  expect_match(strict, "--starts-with", fixed = TRUE)
  expect_match(targets, "if (extended_diagnostics_enabled())", fixed = TRUE)
  expect_match(targets, "legacy_comparison_targets <- list(", fixed = TRUE)
  expect_match(
    targets,
    "legacy_comparison_targets, extended_diagnostic_targets",
    fixed = TRUE
  )
  expect_match(targets, "if (benchmarks_enabled())", fixed = TRUE)
  expect_match(targets, "selected_targets <- c(selected_targets, benchmark_targets)", fixed = TRUE)
  expect_false(grepl("rerun.*flag", targets))
})

test_that("fuzzy matching benchmarks use the canonical district fuzzy distance helpers", {
  source_attachment <- repo_text("R", "districts", "source_attachment.R")
  benchmark <- repo_text("R", "benchmarking", "benchmark_fuzzy_matching.R")
  fuzzy_distance <- repo_text("R", "districts", "fuzzy_distance.R")

  expect_match(source_attachment, "district_source_match_methods", fixed = TRUE)
  expect_match(benchmark, "district_fuzzy_match_methods", fixed = TRUE)
  expect_match(fuzzy_distance, "stringdist::stringdist", fixed = TRUE)
  expect_false(grepl("utils::adist", source_attachment, fixed = TRUE))
})

test_that("analysis notebooks render only to GitHub-flavored Markdown", {
  renderer <- repo_text("R", "output", "render_analysis_notes.R")
  wrapper <- repo_text("scripts", "render_analysis_notes.R")
  qmd <- repo_text("analysis", "benchmarking", "ame-benchmark.qmd")
  archive <- repo_text("scripts", "make_review_archive.sh")

  expect_match(renderer, "--to", fixed = TRUE)
  expect_match(renderer, "gfm", fixed = TRUE)
  expect_match(wrapper, "targets::tar_make", fixed = TRUE)
  expect_match(wrapper, "analysis_markdown_files", fixed = TRUE)
  expect_match(qmd, "format: gfm", fixed = TRUE)
  expect_false(grepl("pdf: default", qmd, fixed = TRUE))
  expect_false(grepl("html: default", qmd, fixed = TRUE))
  expect_match(archive, "GitHub-flavored Markdown", fixed = TRUE)
  expect_match(archive, "-name '*.html' -o -name '*.pdf' -o -name '*.tex' -o -name '*.log'", fixed = TRUE)
})

test_that("analysis long-path note contains runnable current code analogs", {
  qmd <- repo_text("analysis", "io", "long-paths-and-8-3-filenames.qmd")
  expect_match(qmd, 'source(analysis_path("R", "io", "read_long_paths.R"))', fixed = TRUE)
  expect_match(qmd, "read_csv_short(tmp)", fixed = TRUE)
  expect_match(qmd, "get_windows_short_path(tmp)", fixed = TRUE)
  expect_match(qmd, "LongPathsEnabled", fixed = TRUE)
  expect_match(qmd, "readr:::standardise_path", fixed = TRUE)
  expect_match(qmd, "vroom", fixed = TRUE)
})

test_that("review archives do not carry stale root-level diagnostic CSVs", {
  archive <- repo_text("scripts", "make_review_archive.sh")

  expect_match(archive, "find \"$tmpdir/outputs/diagnostics\" -maxdepth 1 -type f -name '*.csv' -delete", fixed = TRUE)
})

test_that("targets sources only R scripts from source directories", {
  targets <- repo_text("_targets.R")

  expect_match(targets, "tar_source_r <- function", fixed = TRUE)
  expect_match(targets, "list.files(path, pattern = \"\\\\.[Rr]$\", recursive = TRUE, full.names = TRUE)", fixed = TRUE)
  expect_false(grepl('tar_source\\("R/', targets))
  root <- dirname(repo_file("README.md"))
  expect_false(file.exists(file.path(root, "R", "districts", "join_district_panel.R")))
  expect_false(grepl("join_district_panel", targets, fixed = TRUE))
})

test_that("selected target runner uses the programmatic targets API and shared warning metadata", {
  runner <- repo_text("scripts", "run_targets_checked.R")
  helper <- repo_text("scripts", "target_metadata_helpers.R")

  expect_match(runner, "rlang::inject", fixed = TRUE)
  expect_match(runner, "tidyselect::all_of(!!selected_target_names)", fixed = TRUE)
  expect_false(grepl("eval(parse", runner, fixed = TRUE))
  expect_match(runner, "record_target_warnings", fixed = TRUE)
  expect_match(runner, "targets::tar_progress", fixed = TRUE)
  expect_match(runner, "target_run_metadata_scope", fixed = TRUE)
  expect_match(helper, "target_metadata_selection", fixed = TRUE)
  expect_match(helper, "rlang::inject", fixed = TRUE)
  expect_match(helper, "targets" , fixed = TRUE)
  expect_match(helper, "target_warnings.csv", fixed = TRUE)
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

test_that("audit and archive scripts carry machine-readable run status", {
  audit <- repo_text("scripts", "run_public_build_audit.sh")
  archive <- repo_text("scripts", "make_review_archive.sh")

  expect_match(audit, "audit_status.json", fixed = TRUE)
  expect_match(audit, 'write_audit_status "failed"', fixed = TRUE)
  expect_match(audit, 'write_audit_status "passed" "complete"', fixed = TRUE)
  expect_match(archive, "standalone_archive", fixed = TRUE)
  expect_match(archive, 'cp -f "$tmpdir/outputs/diagnostics/build/audit_status.json" "$tmpdir/audit_status.json"', fixed = TRUE)
})

test_that("dependency and target-worker contracts avoid unused attachment machinery", {
  description <- repo_text("DESCRIPTION")
  targets <- repo_text("_targets.R")
  root <- dirname(repo_file("README.md"))

  expect_false(grepl("tarchetypes", description, fixed = TRUE))
  expect_false(grepl("fuzzyjoin", description, fixed = TRUE))
  expect_match(description, "Suggests:", fixed = TRUE)
  expect_false(grepl("pdftools", description, fixed = TRUE))
  expect_match(description, "testthat", fixed = TRUE)
  expect_match(targets, "packages = character()", fixed = TRUE)
  expect_false(file.exists(file.path(root, "R", "packages.R")))
})

test_that("legacy tracker remains an auditable comparison input", {
  targets <- repo_text("_targets.R")
  diagnostics <- repo_text("R", "diagnostics", "diagnose_district_tracker_sources.R")
  root <- dirname(repo_file("README.md"))

  expect_false(grepl("processed_district_tracker_file", targets, fixed = TRUE))
  expect_match(targets, "prepare_district_join_map(district_harmonization_crosswalk)", fixed = TRUE)
  expect_match(
    targets,
    "tar_target\\(\\s*district_panel_legacy\\s*,",
    perl = TRUE
  )
  expect_match(diagnostics, "data/metadata/district_harmonization_crosswalk.csv", fixed = TRUE)
  expect_false(file.exists(file.path(root, "data", "processed", "district_tracker_2001_2007_2017_2020.csv")))
})

test_that("source syntax preflight is centralized and read-only", {
  helper <- repo_text("scripts", "check_source_syntax.sh")
  audit <- repo_text("scripts", "run_public_build_audit.sh")

  expect_match(audit, "bash scripts/check_source_syntax.sh", fixed = TRUE)
  expect_match(helper, "bash -n", fixed = TRUE)
  expect_match(helper, "ast.parse", fixed = TRUE)
  expect_match(helper, "json.loads", fixed = TRUE)
  expect_match(helper, "DESCRIPTION runtime dependencies", fixed = TRUE)
  expect_match(helper, "Rscript -", fixed = TRUE)
  expect_match(helper, "knitr::purl", fixed = TRUE)
  expect_false(grepl("py_compile", helper, fixed = TRUE))
  expect_false(grepl("renv::snapshot", helper, fixed = TRUE))
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

test_that("conference poster is a first-class final output", {
  targets <- repo_text("_targets.R")
  renderer <- repo_text("R", "output", "render_public_artifacts.R")
  contract <- repo_text("scripts", "public_output_contract.R")
  poster <- repo_text("posters", "2026_predoc_conference", "poster.qmd")

  expect_match(targets, "tar_target(poster, render_poster_pdf", fixed = TRUE)
  expect_match(renderer, "render_poster_pdf", fixed = TRUE)
  expect_match(contract, "posters/2026_predoc_conference/poster.pdf", fixed = TRUE)
  expect_match(poster, "poster_emie_expected_values.pdf", fixed = TRUE)
  expect_match(poster, "map_emi_exposure.pdf", fixed = TRUE)
  expect_match(poster, "map_linguistic_distance.pdf", fixed = TRUE)
})

poster_renderer_test_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "output", "render_public_artifacts.R"), envir = env)
  env
}

test_that("poster renderer uses warning-free vector assets", {
  renderer <- poster_renderer_test_env()
  assets <- renderer$poster_required_assets()

  expect_true(all(file.exists(vapply(assets, repo_file, character(1)))))
  expect_true(any(grepl("uw-logo-horizontal-full-color-print\\.svg$", assets)))
  expect_false(any(grepl("uw-logo-horizontal-full-color-print\\.pdf$", assets)))
})

test_that("poster Typst format supplies both standard template partials", {
  poster_qmd <- repo_file("posters", "2026_predoc_conference", "poster.qmd")
  renderer <- poster_renderer_test_env()
  paths <- renderer$validate_poster_typst_templates(poster_qmd)

  expect_named(paths, c("template", "show"))
  expect_true(all(file.exists(paths)))
})

test_that("poster citations resolve through the project bibliography", {
  poster <- paste(
    readLines(repo_file("posters", "2026_predoc_conference", "poster.qmd"), warn = FALSE),
    collapse = "\n"
  )
  bibliography <- readLines(repo_file("paper", "references.bib"), warn = FALSE)

  citation_groups <- regmatches(
    poster,
    gregexpr("\\[@[^]]+\\]", poster, perl = TRUE)
  )[[1]]
  citation_keys <- unique(unlist(regmatches(
    citation_groups,
    gregexpr("@[[:alnum:]_:.#$%&+?/-]+", citation_groups, perl = TRUE)
  )))
  citation_keys <- sub("^@", "", citation_keys)
  bibliography_keys <- sub(
    "^@[[:alpha:]]+\\{([^,]+),.*$",
    "\\1",
    grep("^@[[:alpha:]]+\\{[^,]+,", bibliography, value = TRUE)
  )

  expect_match(poster, "\nciteproc: true\n", fixed = TRUE)
  expect_setequal(intersect(citation_keys, bibliography_keys), citation_keys)
})


test_that("public audit prepares production geometry before public outputs", {
  audit <- readLines(
    repo_file("scripts", "run_public_build_audit.sh"),
    warn = FALSE
  )
  geometry_line <- match(TRUE, grepl(
    "make lineage-geometry-build", audit, fixed = TRUE
  ))
  public_line <- match(TRUE, grepl(
    'make "$check_target"', audit, fixed = TRUE
  ))
  diagnostics_line <- match(TRUE, grepl(
    "make extended-diagnostics", audit, fixed = TRUE
  ))

  expect_true(all(is.finite(c(geometry_line, public_line, diagnostics_line))))
  expect_lt(geometry_line, public_line)
  expect_lt(public_line, diagnostics_line)
})

test_that("reviewed primary lineage is public and alternatives remain diagnostic", {
  target_file <- readLines(repo_file("_targets.R"), warn = FALSE)
  core_start <- match(TRUE, grepl("core_pipeline_targets <- list(", target_file, fixed = TRUE))
  extended_start <- match(
    TRUE,
    grepl("extended_diagnostic_targets <- list(", target_file, fixed = TRUE)
  )
  core <- paste(target_file[core_start:(extended_start - 1L)], collapse = "\n")
  extended <- paste(target_file[extended_start:length(target_file)], collapse = "\n")

  expect_match(
    core,
    "tar_target(district_panel, district_panel_primary)",
    fixed = TRUE
  )
  expect_match(core, "save_processed_district_panel(district_panel)", fixed = TRUE)
  expect_match(core, "estimate_2sls(district_panel, iv_formulas, cfg)", fixed = TRUE)
  expect_match(extended, "estimate_2sls(district_panel_conservative, iv_formulas, cfg)", fixed = TRUE)
  expect_match(extended, "estimate_2sls(district_panel_legacy, iv_formulas, cfg)", fixed = TRUE)
  expect_match(extended, "diag_ext_lineage_downstream", fixed = TRUE)
})


test_that("source checks validate application-sample excerpt markers", {
  checker <- repo_text("scripts", "check_source_syntax.sh")
  expect_match(checker, "Rscript scripts/check_sample_specs.R", fixed = TRUE)
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

test_that("poster logo preprocessing preserves the official PDF source contract", {
  renderer <- poster_renderer_test_env()
  root <- tempfile("poster logo path with spaces ")
  dir.create(root, recursive = TRUE)
  source <- file.path(root, "official print logo.pdf")
  output <- file.path(root, "derived", "flattened print logo.pdf")
  writeBin(charToRaw("%PDF-1.7\nsource\n"), source)

  fake_gs <- file.path(root, "fake gs")
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "out=''",
    "input=''",
    "for arg in \"$@\"; do",
    "  case \"$arg\" in",
    "    -sOutputFile=*) out=${arg#-sOutputFile=} ;;",
    "    *.pdf) input=$arg ;;",
    "  esac",
    "done",
    "test -n \"$out\"",
    "test -n \"$input\"",
    "cp \"$input\" \"$out\""
  ), fake_gs)
  Sys.chmod(fake_gs, mode = "0755")

  result <- renderer$flatten_poster_logo_pdf(source, output, ghostscript = fake_gs)
  expect_identical(result, output)
  expect_true(file.exists(output))
  expect_gt(file.info(output)$size, 0)
})

test_that("poster consumes the flattened official print PDF derivative", {
  poster <- repo_text("posters", "2026_predoc_conference", "poster.qmd")
  targets <- repo_text("_targets.R")

  expect_match(
    poster,
    "outputs/derived/poster/uw-logo-horizontal-full-color-print-flat.pdf",
    fixed = TRUE
  )
  expect_false(grepl(
    "../../assets/uw-logo-horizontal-full-color-print.pdf",
    poster,
    fixed = TRUE
  ))
  expect_match(targets, "tar_target(poster_logo_source", fixed = TRUE)
  expect_match(targets, "flatten_poster_logo_pdf(poster_logo_source)", fixed = TRUE)
})
