# Shared helpers for final-paper appendix exhibits.
#
# Appendix modules own only presentation and artifact paths. Statistical and
# data-construction logic remains in the domain modules that feed them.

appendix_table_paths <- function(name) {
  base <- file.path("outputs", "tables", "appendix", name)
  c(csv = paste0(base, ".csv"), tex = paste0(base, ".tex"))
}

save_appendix_tables <- function(exhibits, names, cfg) {
  if (!is.list(exhibits) || !all(names %in% names(exhibits))) {
    stop("Appendix exhibit bundle is missing required tables.", call. = FALSE)
  }
  formats <- table_formats(cfg)
  written <- character()
  for (name in names) {
    paths <- appendix_table_paths(name)
    if ("csv" %in% formats) {
      dir.create(dirname(paths[["csv"]]), recursive = TRUE, showWarnings = FALSE)
      written <- c(written, save_table_csv(exhibits[[name]], paths[["csv"]], public = TRUE))
    }
    if ("tex" %in% formats) {
      dir.create(dirname(paths[["tex"]]), recursive = TRUE, showWarnings = FALSE)
      written <- c(written, save_table_tex(
        table = exhibits[[name]],
        path = paths[["tex"]],
        name = name,
        public = TRUE
      ))
    }
  }
  written
}

appendix_figure_path_base <- function(name) {
  file.path("outputs", "figures", "appendix", name)
}
