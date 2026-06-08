source("R/application_samples/extract_code_excerpts.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript scripts/extract_code_excerpts.R SPEC_YML OUTPUT_QMD", call. = FALSE)
}
spec <- yaml::read_yaml(args[[1]])
body <- extract_code_excerpts(spec)
assemble_coding_sample_qmd(NULL, body, args[[2]])
