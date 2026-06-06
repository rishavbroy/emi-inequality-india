# Dry-run or execute canonical raw-data directory renames.

args <- commandArgs(trailingOnly = TRUE)
execute <- "--execute" %in% args

value_arg <- function(prefix, default) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[length(hit)]], fixed = TRUE) else default
}

root <- normalizePath(value_arg("--root=", "."), mustWork = TRUE)
manifest_path <- file.path(root, value_arg("--manifest=", "data/metadata/file_manifest.csv"))
sources_path <- file.path(root, value_arg("--sources=", "data/metadata/data_sources.csv"))
log_path <- file.path(root, value_arg("--log=", "outputs/diagnostics/raw_data_rename_plan.csv"))

legacy_map <- data.frame(
  source_id = c(
    "nss_2007_education",
    "nss_2007_consumption",
    "nss_2017_education",
    "census_2001_mother_tongue",
    "district_boundaries_2020",
    "district_changes"
  ),
  legacy_relative_path = c(
    "data/raw/NSS 2007-08 Participation and Expenditure in Education 64th Round",
    "data/raw/NSS 2007-08 Household Consumer Expenditure Survey 64th Round",
    "data/raw/NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018",
    "data/raw/Indian Census 2001",
    "data/raw/District Boundaries 2020",
    "data/raw/District Changes Data"
  ),
  canonical_relative_path = c(
    "data/raw/nss_2007_education_64",
    "data/raw/nss_2007_consumption_64",
    "data/raw/nss_2017_education_75",
    "data/raw/census_2001_mother_tongue",
    "data/raw/district_boundaries_2020",
    "data/raw/district_changes"
  ),
  stringsAsFactors = FALSE
)

status_for <- function(old_path, new_path) {
  old_exists <- dir.exists(old_path)
  new_exists <- dir.exists(new_path)
  if (old_exists && new_exists) return("conflict_both_exist")
  if (old_exists && !new_exists) return("move_planned")
  if (!old_exists && new_exists) return("already_canonical")
  "missing"
}

plan <- legacy_map
plan$legacy_absolute_path <- file.path(root, plan$legacy_relative_path)
plan$canonical_absolute_path <- file.path(root, plan$canonical_relative_path)
plan$legacy_exists <- dir.exists(plan$legacy_absolute_path)
plan$canonical_exists <- dir.exists(plan$canonical_absolute_path)
plan$status <- vapply(
  seq_len(nrow(plan)),
  function(i) status_for(plan$legacy_absolute_path[[i]], plan$canonical_absolute_path[[i]]),
  character(1)
)
plan$action <- ifelse(plan$status == "move_planned", "rename directory", "none")

dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
log_plan <- plan[c(
  "source_id",
  "legacy_relative_path",
  "canonical_relative_path",
  "legacy_exists",
  "canonical_exists",
  "status",
  "action"
)]
utils::write.csv(log_plan, log_path, row.names = FALSE, quote = TRUE)

cat(if (execute) "Raw data rename execute plan\n" else "Raw data rename dry-run plan\n")
cat("Log:", log_path, "\n\n")
for (i in seq_len(nrow(plan))) {
  cat(
    sprintf(
      "[%s] %s -> %s (%s)\n",
      plan$status[[i]],
      plan$legacy_relative_path[[i]],
      plan$canonical_relative_path[[i]],
      plan$source_id[[i]]
    )
  )
}

if (any(plan$status == "conflict_both_exist")) {
  stop("Refusing to continue because at least one legacy and canonical directory both exist.", call. = FALSE)
}

if (!execute) {
  cat("\nDry run only. Re-run with --execute after reviewing the plan.\n")
  quit(save = "no", status = 0)
}

planned <- plan[plan$status == "move_planned", , drop = FALSE]
if (!nrow(planned)) {
  cat("\nNo raw directories need to be renamed.\n")
} else {
  for (i in seq_len(nrow(planned))) {
    dir.create(dirname(planned$canonical_absolute_path[[i]]), recursive = TRUE, showWarnings = FALSE)
    ok <- file.rename(planned$legacy_absolute_path[[i]], planned$canonical_absolute_path[[i]])
    if (!ok) {
      stop(
        "Failed to rename ",
        planned$legacy_relative_path[[i]],
        " to ",
        planned$canonical_relative_path[[i]],
        call. = FALSE
      )
    }
  }
  cat("\nRenamed ", nrow(planned), " raw data director", ifelse(nrow(planned) == 1, "y", "ies"), ".\n", sep = "")
}

replace_prefixes <- function(path, mappings) {
  if (!file.exists(path)) return(FALSE)
  before <- readLines(path, warn = FALSE)
  after <- before
  for (i in seq_len(nrow(mappings))) {
    after <- gsub(
      mappings$legacy_relative_path[[i]],
      mappings$canonical_relative_path[[i]],
      after,
      fixed = TRUE
    )
  }
  changed <- !identical(before, after)
  if (changed) writeLines(after, path, useBytes = TRUE)
  changed
}

manifest_changed <- replace_prefixes(manifest_path, legacy_map)
sources_changed <- replace_prefixes(sources_path, legacy_map)
cat("Updated manifest paths:", manifest_changed, "\n")
cat("Updated data source paths:", sources_changed, "\n")
