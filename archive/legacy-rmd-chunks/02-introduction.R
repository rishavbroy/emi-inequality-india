knitr::opts_chunk$set(echo = FALSE, # Show code 
                      message=FALSE,
                      warning=FALSE,
                      include=TRUE, # Include code and output
                      cache = TRUE,
                      cache.lazy = FALSE, # Since we have large objects (see https://yihui.org/knitr/options/#chunk-options)
                      tidy = TRUE,
                      tidy.opts=list( # Just tidies code before its sent to Pandoc. See YAML header for code wrapping in PDF.
                        width.cutoff=I(80), # I(80) makes 80 upper bound
                        wrap = TRUE,
                        args.newline = TRUE # Long function call args on new lines, too
                        )
                      )

options(tinytex.verbose = TRUE) # Leave uncommented to help troubleshoot when knitting to PDF

library(this.path) # here() function
library(haven) # Read in SPSS, Stata, and SAS files
library(readxl) # Read in .xls and .xlsx files
library(readODS) # Read in .ods files

library(sf) # For geospatial data in dataframes
library(spdep) # Spatial weights matrices

library(scales) # pretty_breaks(), comma()
library(formatdown) # format_numbers()

library(forcats) # Manipulate factor levels
library(survey) # Sample-weighted probit estimation
# library(sampleSelection) # Extract inverse Mills ratio
library(marginaleffects) # "Quickly" derive average marginal effects from probit

library(broom) # tidy(): glm objects --> tidy df of coefs 
library(parallel) # mclapply(), parLapply() parallelization
# library(future) # Parallelize 
# library(future.apply) # Parallelize more _apply() functions

library(stargazer) # Regression table outputs
# library(xtable) # For function which automates regression table creation

library(DescTools) # Calculate Gini coefficients

library(fuzzyjoin) # Join df's over approximate matches
library(stringdist) # Approximately match strings

library(purrr) # Mapping and manipulating dfs

library(tmap) # Make maps
library(kableExtra) # Make tables
library(modelsummary) # Make more tables

library(AER) # Run 2SLS regressions
library(sandwich) # HC and clustered SEs 
library(lmtest) # Run custom Wald tests

library(magick) # Make collages

library(tidyverse) # To carry me


# From an earlier version of this document, kept just in case:
# library(reticulate) # To access Python and read in weirdly formatted PDF tables
# py_install(c("pandas", "tabula-py", "openpyxl", "JPype1"), pip = TRUE) # Ditto. If this didn't work, uncomment the following lines
# py_install("pandas", pip = TRUE)
# py_install("tabula-py", pip = TRUE)
# py_install("openpyxl", pip = TRUE)
# py_install("JPype1", pip = TRUE)


setwd(here())
