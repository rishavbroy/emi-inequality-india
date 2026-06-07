# Strict final-output file checks for the final replication mode.

required_files <- c(
  "paper/report.pdf",
  "docs/district-matching.html",
  "docs/long-paths-and-8-3-filenames.html",
  "application-samples/output/RishavRoy_WritingSample.pdf",
  "application-samples/output/RishavRoy_WritingSample10pg.pdf",
  "application-samples/output/RishavRoy_WritingSample5pg.pdf",
  "application-samples/output/RishavRoy_CodingSample.pdf",
  "application-samples/output/RishavRoy_CodingSample47pg.pdf",
  "application-samples/output/RishavRoy_CodingSample25pg.pdf"
)

missing_or_empty <- required_files[!file.exists(required_files) | file.info(required_files)$size <= 0]

failures <- character()
if (length(missing_or_empty)) failures <- c(failures, paste0("Missing or empty final output: ", missing_or_empty))

if (file.exists("scripts/audit_outputs_final.R")) {
  tryCatch(
    source("scripts/audit_outputs_final.R", local = new.env(parent = globalenv())),
    error = function(e) failures <<- c(failures, paste0("Final output audit failed: ", conditionMessage(e)))
  )
}

if (length(failures)) {
  cat(paste0("- ", failures, collapse = "\n"), "\n")
  stop("Final public checks failed.", call. = FALSE)
}

message("Final public checks passed.")
