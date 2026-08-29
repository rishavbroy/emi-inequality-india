# Source readers for predetermined SHRUG Census-1991 district attributes.

shrug_1991_baseline_source_paths <- function(paths = build_paths()) {
  root <- path_project(paths, "data", "raw", "shrug", "census_1991")
  c(
    pca = file.path(root, "shrug-pca91-csv.zip"),
    vd = file.path(root, "shrug-vd91-csv.zip"),
    td = file.path(root, "shrug-td91-csv.zip")
  )
}

read_shrug_1991_baseline_sources <- function(source_paths = shrug_1991_baseline_source_paths()) {
  source_paths <- as.character(source_paths)
  if (length(source_paths) != 3L) stop("SHRUG 1991 baseline requires PCA91, VD91, and TD91 archives.", call. = FALSE)
  names(source_paths) <- c("pca", "vd", "td")
  members <- c(
    pca = "pc91_pca_clean_pc91dist.csv",
    vd = "pc91_vd_clean_pc91dist.csv",
    td = "pc91_td_clean_pc91dist.csv"
  )
  out <- lapply(names(members), function(id) {
    read_shrug_district_archive(
      source_paths[[id]], members[[id]], paste0("SHRUG Census-1991 ", toupper(id), " archive")
    )
  })
  names(out) <- names(members)
  expected <- c(pca = 452L, vd = 445L, td = 441L)
  observed <- vapply(out, nrow, integer(1))
  if (!identical(unname(observed), unname(expected))) {
    bad <- names(expected)[observed != expected]
    stop(
      "SHRUG 1991 district source row counts differ from the documented release: ",
      paste(paste0(bad, "=", observed[bad], " (expected ", expected[bad], ")"), collapse = ", "),
      call. = FALSE
    )
  }
  out
}
