# Shared public-output file contracts for final checks.

public_qmd_sources <- function() {
  c(
    "paper/report.qmd",
    "paper/appendix.qmd",
    "docs/district-matching.qmd",
    "docs/long-paths-and-8-3-filenames.qmd",
    "posters/2026_predoc_conference/poster.qmd"
  )
}

public_report_value_sources <- function() {
  c("paper/report.qmd", "paper/appendix.qmd", "docs/district-matching.qmd")
}

required_public_render_inputs <- function() {
  c(
    "paper/references.bib",
    "outputs/tables/main/sum_tbl_probit_quant.csv",
    "outputs/tables/main/sum_tbl_probit_cat.csv",
    "outputs/tables/main/probit_mfx.csv",
    "outputs/tables/main/sum_tbl_iv.csv",
    "outputs/tables/main/fs_cons.csv",
    "outputs/tables/main/cons_iv.csv",
    "outputs/figures/main/fig_ilo_trends.png",
    "outputs/figures/main/district_carveouts_shifts.png",
    "outputs/figures/main/poster_emie_expected_values.pdf",
    "outputs/figures/main/map_emi_exposure.pdf",
    "outputs/figures/main/map_linguistic_distance.pdf",
    "assets/uw-logo-horizontal-full-color-print.pdf",
    "assets/repo-qr.svg"
  )
}

application_sample_outputs <- function() {
  c(
    "application-samples/output/RishavRoy_WritingSample.pdf",
    "application-samples/output/RishavRoy_WritingSample10pg.pdf",
    "application-samples/output/RishavRoy_WritingSample5pg.pdf",
    "application-samples/output/RishavRoy_CodingSample.pdf",
    "application-samples/output/RishavRoy_CodingSample47pg.pdf",
    "application-samples/output/RishavRoy_CodingSample25pg.pdf"
  )
}

required_final_documents <- function(require_application_samples = TRUE) {
  files <- c(
    "paper/report.pdf",
    "docs/district-matching.html",
    "docs/long-paths-and-8-3-filenames.html",
    "posters/2026_predoc_conference/poster.pdf"
  )
  if (isTRUE(require_application_samples)) files <- c(files, application_sample_outputs())
  files
}

required_final_artifacts <- function() {
  c(
    "paper/references.bib",
    "outputs/tables/main/sum_tbl_probit_quant.csv",
    "outputs/tables/main/sum_tbl_probit_cat.csv",
    "outputs/tables/main/probit_mfx.csv",
    "outputs/tables/main/sum_tbl_iv.csv",
    "outputs/tables/main/fs_cons.csv",
    "outputs/tables/main/cons_iv.csv",
    "outputs/figures/main/fig_ilo_trends.png",
    "outputs/figures/main/district_carveouts_shifts.png",
    "outputs/figures/main/collage_main_maps.png",
    "outputs/figures/main/collage_iv_region_maps.png",
    "outputs/figures/main/poster_emie_expected_values.pdf",
    "posters/2026_predoc_conference/poster.pdf",
    "outputs/diagnostics/public/spatial_moran_tests.csv",
    "outputs/diagnostics/public/spatial_moran_mc_reference.csv"
  )
}

missing_or_empty_files <- function(paths) {
  paths[!file.exists(paths) | file.info(paths)$size <= 0]
}
