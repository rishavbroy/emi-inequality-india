# Average Marginal Effects Benchmark


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy diagnostic intent

Use automatic differentiation (AD) for standard-error delta-method
gradients, but keep the final method as
`avg_slopes(model_probit_selection, newdata = sel_data, wts = "weight", vcov = TRUE, type = "response")`
with `options(marginaleffects_parallel = FALSE)`. The legacy notebook
noted that raw AME derivation is slow because the count of numeric
variables, observations, and centered finite-difference prediction calls
is large.

``` r
ame_methods <- analysis_target_csv("bench_ame_methods", "ame_methods_benchmark.csv")
ame_notes <- analysis_target_csv("bench_ame_methods", "ame_legacy_benchmark_notes.csv")
ame_parallel <- analysis_target_csv("bench_ame_methods", "ame_parallelization_notes.csv")
full_n <- analysis_value(ame_methods, column = "n_observations")
n_numeric <- analysis_value(ame_methods, column = "n_numeric_variables")
predict_calls <- analysis_value(ame_methods, column = "centered_predict_calls_full_data")
```

The current active model frame has 10 numeric variables and 114,961
observations. Centered finite differences therefore imply 2,299,220
full-data calls of `predict()` if each numeric variable is perturbed up
and down once. This is the current-codebase analog of the legacy note
that `avg_slopes()` uses centered finite differences.

``` r
data.frame(
  current_code_analog = "n_numeric_variables * n_observations * 2 predict() calls",
  n_numeric_variables = n_numeric,
  n_observations = full_n,
  centered_predict_calls = n_numeric * full_n * 2
)
```

                                           current_code_analog n_numeric_variables
    1 n_numeric_variables * n_observations * 2 predict() calls                  10
      n_observations centered_predict_calls
    1         114961                2299220

Method 1 was straight sampling/subsampling. The current benchmark keeps
`set.seed(999)`, samples from the active model frame, and calls the
production `marginaleffects::avg_slopes()` method.

``` r
analysis_table(
  ame_methods[intersect(c("method", "sample_size", "n_observations", "n_numeric_variables", "centered_predict_calls_full_data", "elapsed_seconds", "status"), names(ame_methods))],
  "Current AME benchmark attempts"
)
```

| method | sample_size | n_observations | n_numeric_variables | centered_predict_calls_full_data | elapsed_seconds | status |
|:---|---:|---:|---:|---:|---:|:---|
| avg_slopes_centered_default | 200 | 114961 | 10 | 2299220 | 0.841 | estimated_legacy_vcov |
| avg_slopes_fdforward | 200 | 114961 | 10 | 2299220 | 0.926 | estimated_legacy_vcov |
| avg_slopes_centered_default | 2000 | 114961 | 10 | 2299220 | 5.109 | estimated_legacy_vcov |
| avg_slopes_fdforward | 2000 | 114961 | 10 | 2299220 | 4.791 | estimated_legacy_vcov |

Current AME benchmark attempts

With `newdata = sel_data` (no subsampling, as if `num_samp` were
114,961), the legacy run took several minutes; the default current
benchmark tier intentionally does not rerun the full-data AME timing
every public build. With the current default benchmark tier, the largest
sampled benchmark uses `num_samp` equal to 2,000 and the slowest
recorded current run took 5.109 seconds.

``` r
ame_methods[, intersect(c("method", "sample_size", "elapsed_seconds", "status", "reason"), names(ame_methods)), drop = FALSE]
```

                           method sample_size elapsed_seconds                status
    1 avg_slopes_centered_default         200           0.841 estimated_legacy_vcov
    2        avg_slopes_fdforward         200           0.926 estimated_legacy_vcov
    3 avg_slopes_centered_default        2000           5.109 estimated_legacy_vcov
    4        avg_slopes_fdforward        2000           4.791 estimated_legacy_vcov
      reason
    1     NA
    2     NA
    3     NA
    4     NA

Method 2 was subsampling with forward differences. The current analog is
the row whose method is `avg_slopes_fdforward`.

``` r
analysis_table(
  ame_methods[ame_methods$method == "avg_slopes_fdforward", intersect(c("method", "sample_size", "elapsed_seconds", "status", "reason", "fallback"), names(ame_methods)), drop = FALSE],
  "Forward-difference AME benchmark rows"
)
```

| method | sample_size | elapsed_seconds | status | reason | fallback |
|:---|---:|---:|:---|:---|:---|
| avg_slopes_fdforward | 200 | 0.926 | estimated_legacy_vcov | NA | NA |
| avg_slopes_fdforward | 2000 | 4.791 | estimated_legacy_vcov | NA | NA |

Forward-difference AME benchmark rows

Method 3 split numeric `slopes()` and factor `comparisons()` just to
check whether it improved runtime. The legacy conclusion was no: use
`avg_slopes()`. Because the current production pipeline uses
`avg_slopes()` directly, this note is retained as a benchmark note
rather than a second production path.

Method 4 attempted parallelization with `future.apply`,
`plan(multicore, workers = 4)`, and
`options(marginaleffects_parallel = TRUE)`. The legacy attempt failed
because the exported globals exceeded available RAM. The current
benchmark therefore records the parallelization attempt as
documented-not-run instead of rerunning an unsafe memory-heavy path.

``` r
analysis_table(ame_notes, "Legacy AME benchmark notes retained as target output")
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

Legacy AME benchmark notes retained as target output

``` r
analysis_table(ame_parallel, "Parallelization notes")
```

| method | status | reason |
|:---|:---|:---|
| marginaleffects_parallel_false | legacy_final_choice | Legacy final draft set options(marginaleffects_parallel = FALSE). |
| future_parallel_attempt | documented_not_run_by_default | Legacy commented attempt failed because exported globals exceeded available RAM; benchmark target records this rather than forcing an unsafe run. |

Parallelization notes
