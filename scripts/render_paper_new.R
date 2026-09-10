# Render the ChatGPT working paper without adding it to the strict public target graph.

source("R/io/utils_data_frame.R")
source("R/config.R")
source("R/output/table_contract.R")
source("R/output/save_tables.R")
source("R/output/paper_new_tables.R")
source("R/output/render_public_artifacts.R")

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required; run `make restore`.", call. = FALSE)
}

paper_qmd <- "paper/paper-new.qmd"
report_values <- targets::tar_read(report_values)
figure_files <- targets::tar_read(figure_files)
table_files <- targets::tar_read(table_files)

# Table 2 uses the non-redistributable archived DISE baseline and therefore stays
# opt-in with paper-new. The Makefile materializes these extended targets before
# this script runs; reading cached targets here is outside the running DAG.
district_mechanisms <- targets::tar_read(english_opportunity_district_mechanisms)
nss_validation <- targets::tar_read(dise_iv_nss_validation)
district_panel <- targets::tar_read(district_panel)
cfg <- targets::tar_read(cfg)
paper_schooling_market <- make_paper_schooling_market_geography_table(
  district_mechanisms, nss_validation, district_panel
)
paper_table_files <- save_tables(
  list(paper_schooling_market_geography = paper_schooling_market), cfg
)
table_files <- unique(c(table_files, paper_table_files))

render_paper_pdf(paper_qmd, report_values, figure_files, table_files)
