# Use automatic differentiation (AD) for SE delta method gradients
autodiff(autodiff = TRUE, install = FALSE) # install = TRUE if self-managing a Python installation
options(marginaleffects_parallel = FALSE) # Make sure it's off

sel_data <- selection_df[comp_cases, , drop = FALSE]

num_samp = 200
set.seed(999)
final_draft = TRUE # TRUE for final draft

newdata_sub <- if (final_draft) sel_data else sel_data %>% slice_sample(n = num_samp)

start.time <- Sys.time()

mfx_all <- avg_slopes(
  model_probit_selection,
  newdata = newdata_sub,
  wts = "weight",
  vcov = TRUE, # Set as FALSE to make this even faster
  type = "response"
)

end.time <- Sys.time()
tdiff <- end.time - start.time
# With newdata = sel_data (no subsampling, as if num_samp = 279180): 6.178544-11.17861 mins when ran for the first time, 4.853338 mins later.
# With num_samp = 20000: 36.13833 secs
# With num_samp = 2000: 4.005744 secs



# The rest of this chunk is saved for quick replications of robustness checks on my final method

# Raw AME derivation is very slow: 
# 9 numeric variables * 279180 observations * 2 calls of predict() per observation per variable (avg_slopes() uses centered finite difference, see https://marginaleffects.com/bonus/uncertainty.html#numerical-derivatives-sensitivity-to-step-size) = 5025240 predict() calls. 

# Method 1: Straight Sampling and Subsampling 
# Note: "slopes() functions will automatically revert to comparisons() for binary or categorical variables" (https://marginaleffects.com/man/r/slopes.html)

# num_samp = 2000
# set.seed(999)
# newdata_sub <- sel_data %>% slice_sample(n = num_samp)
# # newdata_sub <- sel_data
# 
# start.time <- Sys.time()
# 
# mfx_all <- avg_slopes(
#   model_probit_selection,
#   newdata = newdata_sub,
#   wts = "weight",
#   vcov = TRUE # Set as FALSE to make this even faster
# )
# 
# end.time <- Sys.time()
# end.time - start.time

# With newdata = sel_data (no subsampling, as if num_samp = 279180): ~40 minutes!
# With num_samp = 20000: 36.7708 secs
# With num_samp = 2000: 4.429373 secs

# Method 2: Subsampling with forward differences
# Note: Calls predict() once per perturbation instead of twice (a la central differences, "fdcenter")
# Run the above, but add numderiv = "fdforward" into avg_slopes()
# With num_samp = 20000: 37.41933 secs
# With num_samp = 2000: 4.047509 secs
# Cheaper numeric differentiation didn't help at all! Reflective of how many discrete variables I have.
# Centered differences are generally more accurate than forward differences for continuous vars

# Method 3: Split slopes and comparisons, just to be sure
# Ran avg_slopes on numeric_vars, avg_comparisons on factor_vars
# With num_samp = 20000: 38.94813 secs
# With num_samp = 2000: 5.039992 secs
# No. Just use avg_slopes()


# Method 4: Parallelization

# library(future.apply) # For parallelization
# # 
# # Resolve futures in parallel in *forked* R processes. NOT SUPPORTED ON WINDOWS!
# plan(multicore, workers = 4) # Using "multisession" led to the same result.
# 
# options(marginaleffects_parallel = TRUE) # parallelize delta method computation of standard errors
# 
# num_samp = 2000
# set.seed(999)
# newdata_sub <- sel_data %>% slice_sample(n = num_samp)
# 
# start.time <- Sys.time()
# 
# mfx_all <- avg_slopes(
#   model_probit_selection,
#   newdata = newdata_sub,
#   wts = "weight",
#   vcov = TRUE # Set as FALSE to make this even faster
# )
# 
# end.time <- Sys.time()
# end.time - start.time

# Failed! "The total size of the 18 globals exported for future expression (‘FUN()’) is 7.90 GiB." Three largest globals: "‘FUN’ (3.54 GiB of class ‘function’), ‘func’ (3.54 GiB of class ‘function’) and ‘mfx’ (544.70 MiB of class ‘S4’)."

# As documentation states: "There is always considerable overhead when using parallel computation, mainly involved in passing the whole dataset to the different processes." (https://cloud.r-project.org/web/packages/marginaleffects/refman/marginaleffects.html)


# To parallelize for the whole dataset after running `environment(model_probit_selection) <- NULL; model_probit_selection$survey.design <- NULL; model_probit_selection$data <- NULL`: 
# 53.96 GiB. "The three largest globals are ‘FUN’ (24.00 GiB of class ‘function’), ‘func’ (24.00 GiB of class ‘function’) and ‘hi’ (1.82 GiB of class ‘list’)"

# My laptop has 24 GB of RAM! So no parallelization.