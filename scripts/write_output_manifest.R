#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
allowed <- c("--with-samples", "--with-analysis-notes")
if (length(setdiff(args, allowed))) stop("Unknown output-manifest argument.", call. = FALSE)
source("R/output/output_artifact_manifest.R", local = TRUE)

path <- "outputs/diagnostics/build/output_manifest.csv"
design_path <- "outputs/diagnostics/extended/iv/analysis_design_registry.csv"
design_registry <- if (file.exists(design_path)) {
  utils::read.csv(design_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  data.frame(analysis_id = character(), family = character())
}
manifest <- build_output_artifact_manifest(
  target_meta = targets::tar_meta(fields = c("format", "path"), targets_only = TRUE),
  design_registry = design_registry,
  roots = output_artifact_roots(
    include_samples = "--with-samples" %in% args,
    include_analysis = "--with-analysis-notes" %in% args
  )
)
dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(manifest, path, row.names = FALSE, na = "")
cat(sprintf("Wrote %s (%d artifacts; %d target-backed; %d with analysis IDs).\n", path, nrow(manifest), sum(nzchar(manifest$target_name)), sum(manifest$analysis_id_count > 0L)))
cat(manifest$path, sep = "\n")
cat("\n")
