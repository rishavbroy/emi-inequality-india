# This file is part of the EMI inequality research pipeline.

#' Read and validate the versioned Glottolog 5.3 source bundle
#'
#' The direct languoid CSV is the canonical genealogy source used by the
#' project. The CLDF archive is retained for aliases and future reviewed Census
#' crosswalks; the Newick file is retained as an independent representation.
read_glottolog_5_3 <- function(paths = build_paths()) {
  rows <- rbind(
    require_manifest_files(paths, "glottolog_5_3"),
    require_manifest_files(paths, "glottolog_cldf_5_3")
  )
  path_for <- function(file_id) {
    hit <- rows$absolute_path[rows$file_id == file_id]
    if (length(hit) != 1L) {
      stop("Glottolog manifest must contain exactly one row for ", file_id, ".", call. = FALSE)
    }
    hit[[1]]
  }

  languoids <- read_glottolog_languoids(path_for("glottolog53_languoids"))
  cldf_metadata <- read_glottolog_cldf_metadata(path_for("glottolog53_cldf"))
  if (!identical(as.character(cldf_metadata$title), "glottolog/glottolog: Glottolog database 5.3 as CLDF")) {
    stop("Glottolog CLDF archive does not identify itself as release 5.3.", call. = FALSE)
  }
  if (!identical(as.character(cldf_metadata$license), "CC-BY-4.0")) {
    stop("Glottolog CLDF archive does not declare the expected CC-BY-4.0 license.", call. = FALSE)
  }

  validate_glottolog_genealogy(languoids)
  hindi <- languoids[languoids$id == "hind1269", , drop = FALSE]
  if (nrow(hindi) != 1L || hindi$level[[1]] != "language" || hindi$iso639P3code[[1]] != "hin") {
    stop("Glottolog 5.3 Hindi anchor hind1269 is missing or malformed.", call. = FALSE)
  }

  list(
    version = "5.3",
    languoids = languoids,
    cldf_zip = path_for("glottolog53_cldf"),
    geo_path = path_for("glottolog53_geo"),
    tree_path = path_for("glottolog53_newick"),
    license = "CC-BY-4.0"
  )
}

read_glottolog_languoids <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[basename(members) == "languoid.csv"]
  if (length(member) != 1L) {
    stop("Glottolog languoid archive must contain exactly one languoid.csv.", call. = FALSE)
  }
  out <- utils::read.csv(
    unz(path, member), stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
  required <- c("id", "family_id", "parent_id", "name", "bookkeeping", "level", "iso639P3code")
  if (!all(required %in% names(out))) {
    stop("Glottolog languoid.csv has an invalid schema.", call. = FALSE)
  }

  clean_character <- function(x) {
    value <- trimws(plain_chr(x))
    value[is.na(value)] <- ""
    value
  }
  out$id <- clean_character(out$id)
  out$family_id <- clean_character(out$family_id)
  out$parent_id <- clean_character(out$parent_id)
  out$name <- clean_character(out$name)
  out$level <- tolower(clean_character(out$level))
  out$iso639P3code <- tolower(clean_character(out$iso639P3code))
  out
}

read_glottolog_cldf_metadata <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[basename(members) == "metadata.json" & !grepl("/cldf/", members, fixed = TRUE)]
  if (length(member) != 1L) {
    stop("Glottolog CLDF archive must contain exactly one top-level metadata.json.", call. = FALSE)
  }
  need_pkg("jsonlite", "Glottolog CLDF metadata")
  jsonlite::fromJSON(
    paste(readLines(unz(path, member), warn = FALSE), collapse = "\n"),
    simplifyVector = TRUE
  )
}
