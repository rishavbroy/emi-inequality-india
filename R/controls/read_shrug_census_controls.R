# Shared reader for SHRUG Census district products distributed inside ZIP archives.

read_shrug_district_archive <- function(path, member, source = "SHRUG Census archive") {
  if (!file.exists(path)) stop("Missing ", source, ": ", path, call. = FALSE)
  listing <- utils::unzip(path, list = TRUE)$Name
  hit <- listing[basename(listing) == member]
  if (length(hit) != 1L) stop(source, " must contain exactly one ", member, ".", call. = FALSE)
  utils::read.csv(unz(path, hit[[1L]]), stringsAsFactors = FALSE, check.names = FALSE)
}
