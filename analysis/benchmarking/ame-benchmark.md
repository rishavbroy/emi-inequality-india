# Average Marginal Effects Benchmark


## Legacy comments

### Legacy Chunk 10: AME method choice and timing notes

Use automatic differentiation (AD) for SE delta method gradients

``` r
With newdata = sel_data (no subsampling, as if num_samp = 279180): 6.178544-11.17861 mins when ran for the first time, 4.853338 mins later.
With num_samp = 20000: 36.13833 secs
With num_samp = 2000: 4.005744 secs
```

The rest of this chunk is saved for quick replications of robustness
checks on my final method

Raw AME derivation is very slow:

``` r
9 numeric variables * 279180 observations * 2 calls of predict() per observation per variable (avg_slopes() uses centered finite difference, see https://marginaleffects.com/bonus/uncertainty.html#numerical-derivatives-sensitivity-to-step-size) = 5025240 predict() calls. 
```

Method 1: Straight Sampling and Subsampling

``` r
Note: "slopes() functions will automatically revert to comparisons() for binary or categorical variables" (https://marginaleffects.com/man/r/slopes.html)
```

``` r
num_samp = 2000
set.seed(999)
newdata_sub <- sel_data %>% slice_sample(n = num_samp)
newdata_sub <- sel_data
```

``` r
start.time <- Sys.time()
```

``` r
mfx_all <- avg_slopes(
  model_probit_selection,
  newdata = newdata_sub,
  wts = "weight",
  vcov = TRUE # Set as FALSE to make this even faster
)
```

``` r
end.time <- Sys.time()
end.time - start.time
```

``` r
With newdata = sel_data (no subsampling, as if num_samp = 279180): ~40 minutes!
With num_samp = 20000: 36.7708 secs
With num_samp = 2000: 4.429373 secs
```

Method 2: Subsampling with forward differences

``` r
Note: Calls predict() once per perturbation instead of twice (a la central differences, "fdcenter")
Run the above, but add numderiv = "fdforward" into avg_slopes()
With num_samp = 20000: 37.41933 secs
With num_samp = 2000: 4.047509 secs
Cheaper numeric differentiation didn't help at all! Reflective of how many discrete variables I have.
Centered differences are generally more accurate than forward differences for continuous vars
```

Method 3: Split slopes and comparisons, just to be sure Ran avg_slopes
on numeric_vars, avg_comparisons on factor_vars

``` r
With num_samp = 20000: 38.94813 secs
With num_samp = 2000: 5.039992 secs
No. Just use avg_slopes()
```

Method 4: Parallelization

``` r
library(future.apply) # For parallelization
```

Resolve futures in parallel in *forked* R processes. NOT SUPPORTED ON
WINDOWS!

``` r
plan(multicore, workers = 4) # Using "multisession" led to the same result.
```

``` r
options(marginaleffects_parallel = TRUE) # parallelize delta method computation of standard errors
```

``` r
num_samp = 2000
set.seed(999)
newdata_sub <- sel_data %>% slice_sample(n = num_samp)
```

``` r
start.time <- Sys.time()
```

``` r
mfx_all <- avg_slopes(
  model_probit_selection,
  newdata = newdata_sub,
  wts = "weight",
  vcov = TRUE # Set as FALSE to make this even faster
)
```

``` r
end.time <- Sys.time()
end.time - start.time
```

Failed! “The total size of the 18 globals exported for future expression
(‘FUN()’) is 7.90 GiB.” Three largest globals: “‘FUN’ (3.54 GiB of class
‘function’), ‘func’ (3.54 GiB of class ‘function’) and ‘mfx’ (544.70 MiB
of class ‘S4’).”

``` r
As documentation states: "There is always considerable overhead when using parallel computation, mainly involved in passing the whole dataset to the different processes." (https://cloud.r-project.org/web/packages/marginaleffects/refman/marginaleffects.html)
```

``` r
To parallelize for the whole dataset after running `environment(model_probit_selection) <- NULL; model_probit_selection$survey.design <- NULL; model_probit_selection$data <- NULL`: 
53.96 GiB. "The three largest globals are ‘FUN’ (24.00 GiB of class ‘function’), ‘func’ (24.00 GiB of class ‘function’) and ‘hi’ (1.82 GiB of class ‘list’)"
```

My laptop has 24 GB of RAM! So no parallelization.

**Deviation note.** The prose above is rendered from the legacy
comments, with comment markers removed. The tables below replace
manually written timing/status comments with current target outputs;
where the current `marginaleffects` call is incompatible with the active
package version, the incompatibility is shown explicitly rather than
rewritten as a successful timing result.

## Current targets-backed results

| method | sample_size | elapsed_seconds | status |
|:---|---:|---:|:---|
| avg_slopes_centered_default | 200 | 0.869 | estimated_legacy_vcov |
| avg_slopes_fdforward | 200 | 0.704 | estimated_legacy_vcov |
| avg_slopes_centered_default | 2000 | 4.547 | estimated_legacy_vcov |
| avg_slopes_fdforward | 2000 | 4.812 | estimated_legacy_vcov |

Current AME benchmark attempts

### Current-version benchmark note: `avg_slopes_centered_default`, n = 200

``` text
NA
```

``` text
NA
```

### Current-version benchmark note: `avg_slopes_fdforward`, n = 200

``` text
NA
```

``` text
NA
```

### Current-version benchmark note: `avg_slopes_centered_default`, n = 2000

``` text
NA
```

``` text
NA
```

### Current-version benchmark note: `avg_slopes_fdforward`, n = 2000

``` text
NA
```

``` text
NA
```

| topic | legacy_choice |
|:---|:---|
| final_method | avg_slopes(model_probit_selection, newdata = sel_data, wts = ‘weight’, vcov = TRUE, type = ‘response’); marginaleffects_parallel = FALSE |
| sample_seed | set.seed(999) |
| full_data_legacy_timing | legacy notes: full data first run around 6-11 minutes, later around 4.85 minutes; raw derivation once took ~40 minutes |
| subsample_20000_legacy_timing | legacy notes: around 36-39 seconds |
| subsample_2000_legacy_timing | legacy notes: around 4-5 seconds |
| forward_difference_result | fdforward did not materially help; centered differences retained for accuracy on continuous variables |
| split_slopes_comparisons_result | splitting numeric slopes and factor comparisons did not help; use avg_slopes() |
| parallelization_failure | future/marginaleffects parallelization exceeded available memory because exported globals were tens of GiB |

Legacy AME benchmark notes

| method | status | reason |
|:---|:---|:---|
| marginaleffects_parallel_false | legacy_final_choice | Legacy final draft set options(marginaleffects_parallel = FALSE). |
| future_parallel_attempt | documented_not_run_by_default | Legacy commented attempt failed because exported globals exceeded available RAM; benchmark target records this rather than forcing an unsafe run. |

Parallelization notes
