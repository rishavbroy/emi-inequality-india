source("R/application_samples/extract_qmd_excerpts.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript scripts/extract_qmd_excerpts.R SOURCE_QMD OUTPUT_QMD EXCERPT_ID [EXCERPT_ID ...]", call. = FALSE)
}
source_qmd <- args[[1]]
output_qmd <- args[[2]]
ids <- args[-c(1, 2)]
excerpts <- extract_qmd_excerpts(source_qmd, ids)
assemble_writing_sample_qmd(NULL, excerpts, output_qmd)
