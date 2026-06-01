source("R/packages.R")
source("R/samples/extract_qmd_excerpts.R")
source("R/samples/extract_code_excerpts.R")
source("R/samples/render_writing_sample.R")
source("R/samples/render_coding_sample.R")

load_project_packages()

writing <- render_writing_samples()
coding <- render_coding_samples()

message("Writing samples: ", paste(writing, collapse = ", "))
message("Coding samples: ", paste(coding, collapse = ", "))
