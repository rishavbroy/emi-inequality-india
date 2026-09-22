library(targets)

source("R/config.R")
source("R/paths.R")

tar_source("R/io")
tar_source("R/measures")
tar_source("R/controls")
tar_source("R/iv")
tar_source("R/diagnostics")
tar_source("R/output")
tar_source("R/replication")
source("R/pipeline/processed_replication_targets.R")

tar_option_set(
  packages = character(),
  format = "rds",
  error = "abridge"
)

processed_replication_target_definitions()
