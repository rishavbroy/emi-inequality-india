# Spatial Weights Benchmark


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy diagnostic intent

The legacy geospatial diagnostic compared rook and queen contiguity,
recorded similar mean-neighbor counts, and then kept rook contiguity for
the spatial-autocorrelation path. The current benchmark uses the active
matched panel and `spdep::poly2nb()` rather than the legacy exploratory
`sfExtras` calls.

``` r
spatial_bench <- analysis_target_csv("bench_spatial_weights", "spatial_weights_rook_queen_benchmark.csv")
spatial_diag <- analysis_target_csv("diag_ext_spatial_weights", "rook_queen_contiguity_comparison.csv")
spatial_ref <- analysis_target_csv("diag_ext_spatial_weights", "spatial_weights_legacy_reference.csv")
```

The current rook benchmark has 4.029 mean neighbors; the current queen
benchmark has 4.041 mean neighbors.

``` r
spatial_bench[spatial_bench$contiguity %in% c("rook", "queen"), c("contiguity", "n", "mean_neighbors", "n_islands", "elapsed_seconds"), drop = FALSE]
```

      contiguity   n mean_neighbors n_islands elapsed_seconds
    1       rook 482       4.029046         0          11.402
    2      queen 482       4.041494         0          11.637

``` r
analysis_table(spatial_bench, "Current rook/queen benchmark")
```

| contiguity | n | mean_neighbors | n_islands | elapsed_seconds | warnings |
|:---|---:|---:|---:|---:|:---|
| rook | 482 | 4.029 | 0 | 11.402 | some observations have no neighbours; |

Current rook/queen benchmark

if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \| \|queen \| 482\| 4.041\| 0\|
11.637\|some observations have no neighbours; if this seems unexpected,
try increasing the snap argument.; neighbour object has 21 sub-graphs;
if this sub-graph count seems unexpected, try increasing the snap
argument. \|

``` r
analysis_table(spatial_diag, "Current-vs-legacy rook/queen comparison")
```

| note |
|:---|
| Target output not found: diag_ext_spatial_weights rook_queen_contiguity_comparison.csv |

Current-vs-legacy rook/queen comparison

``` r
analysis_table(spatial_ref, "Legacy spatial-weight reference comments")
```

| note |
|:---|
| Target output not found: diag_ext_spatial_weights spatial_weights_legacy_reference.csv |

Legacy spatial-weight reference comments
