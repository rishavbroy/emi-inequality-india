#!/usr/bin/env Rscript

source("R/replication/processed_replication.R")

path <- verify_processed_replication()
message("Processed-data replication matches the full-source target outputs: ", path)
