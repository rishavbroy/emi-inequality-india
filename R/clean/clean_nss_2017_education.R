# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2017 education
#'
clean_nss_2017_education <- function(raw) {
  out <- lapply(raw, std, year = 2017L)
  class(out) <- c("nss_2017_education_clean", class(out))
  out
}
