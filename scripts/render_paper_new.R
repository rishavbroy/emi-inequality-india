# Render the ChatGPT working paper without adding it to the strict public target graph.

source("R/output/render_public_artifacts.R")

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required; run `make restore`.", call. = FALSE)
}

paper_qmd <- "paper/paper-new.qmd"
report_values <- targets::tar_read(report_values)
figure_files <- targets::tar_read(figure_files)
table_files <- targets::tar_read(table_files)

render_paper_pdf(paper_qmd, report_values, figure_files, table_files)
