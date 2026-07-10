args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1]] else "analysis"
if (!dir.exists(root)) stop("Missing analysis directory: ", root, call. = FALSE)
qmds <- list.files(root, pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
qmds <- qmds[!grepl("(^|/)_[^/]+[.]qmd$", qmds)]
if (!length(qmds)) {
  message("No analysis QMDs found.")
  quit(status = 0L)
}
if (!nzchar(Sys.which("quarto"))) stop("quarto is required to render analysis notebooks.", call. = FALSE)
for (qmd in qmds) {
  message("Rendering ", qmd)
  status <- system2("quarto", c("render", qmd))
  if (!identical(status, 0L)) stop("quarto render failed for ", qmd, " with status ", status, call. = FALSE)
}
