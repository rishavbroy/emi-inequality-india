# This is the .Rmd file which contains all of the code and text used to create my writing sample; please see the abstract above for a refresher and/or quick summary.



# My code can be split into the following sections and chunks
# (ctrl/cmd+F their names (exclude args for the functions) for easier navigation of this sample.):

# Chunk 1: Summary of the code
# Chunk 2: Introduction

# Chunk 3: Define functions to read in short 8.3 filenames
  # read_sav_short(long_path)
  # read_csv_short(long_path)
  # read_excel_short(long_path, sheet)

# Chunk 4: Import key data files
  # District boundaries and regions

# Chunk 5: Match districts: Extract district info from key datasets
  # Diagnose and fix issues
# Chunk 6: Match districts: Construct district tracking dfs
  # Construct district_tracker df
  # Diagnose and correct mistakes


# Chunk 7: Participation model: Cleaning and prep
  # District-level aggregates for imputation
# Chunk 8: Participation model: Are NAs randomly distributed
  # All variables: Variables/Regions with NAs
  # Correlation matrix
  # Logistic per missing variable (parallelized)
    # check_missing_logit_parallel(df, miss_vars, covars, method_p)
    # fit_one(m)
# Chunk 9: Participation model: Estimation and IMR
# Chunk 10: Participation model: Derive AME
# Chunk 11: Participation model: Set up results table
# Chunk 12: Participation model: Make summary tables

# Chunk 13: Construct 2007–08 measures
  # Diagnose issues
# Chunk 14: Construct 2017–18 measures
# Chunk 15: Construct 2001 measure (IV)

# Chunk 16: Match districts: Test joining methods
  # evaluate_distances(pairs, methods, thresholds, col1, col2)
  # Helper vectors
# Chunk 17: Match districts: Define joining functions
  # fuzzy_join_sequence(df1, df2, dist1, state1, dist2, state2, methods, thresholds, mode)
  # merge_dfs_into_tracker(df_names, tracker, years_of_interest, flag)
# Chunk 18: Match districts: Manually fix errors
# Chunk 19: Match districts: Run the joining functions
# Chunk 20: Match districts: Diagnose errors
  # unmatched_rows(df_names, tracker, years_of_interest)
  # Compare outcomes with different tracker dfs
  # Correct even more NAs
# Chunk 21: Match districts: Join with geometry data
  # Compare districts in district_tracker merge vs. district_timeseries merge

# Chunk 22: Geospatial: Make maps
  # Save the maps
# Chunk 23: Geospatial: Make map collages
# Chunk 24: Geospatial: Neighbor list construction

# Chunk 25: 2SLS: Controls and 2SLS formula
  # Control vectors
  # IV Formula
    # make_iv_formula(dep, endog, exog, inst)
# Chunk 26: 2SLS: Baseline 2SLS models
# Chunk 27: 2SLS: Make summary tables
# Chunk 28: Multicollinearity check

# Chunk 29: Geospatial: Spatial autocorrelation tests
# Chunk 30: Spatial–2SLS model attempts

# Chunks 31–40: [Output for paper]



# Note: If reading a PDF, some links in comments did not wrap properly.

# If you are using a sufficiently advanced PDF reader, then you can:
# 1) Still click on any link to get to the correct website, even if the link is cut off.
# 2) Navigate code chunks in the viewer's table of contents.
