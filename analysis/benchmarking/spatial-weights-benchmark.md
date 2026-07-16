# Spatial Weights Benchmark


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Test for best contiguity measure. Code is based on
@liChapter14Spatial2019. The legacy exploratory comments tried
`st_rook()` and `st_queen()` from `sfExtras`: rook had 4.780165 average
neighbors per district, and queen had 4.783471. Build a rook-contiguity
neighbor list. With `queen = TRUE`: 32.64976 seconds. With
`queen = FALSE`: 31.15051 seconds. Turn that into a binary adjacency
matrix, where `W_2020[i, j] == 1` if districts `i` and `j` share a
border and 0 otherwise.

``` r
analysis_deviation_note("The current benchmark uses the active final matched panel with non-empty geometry and spdep::poly2nb() rather than legacy exploratory sfExtras calls. This makes spatial weights match the current final-panel diagnostics, while the legacy mean-neighbor values remain a reference rather than a target to reproduce.")
```

**Deviation note.** The current benchmark uses the active final matched
panel with non-empty geometry and spdep::poly2nb() rather than legacy
exploratory sfExtras calls. This makes spatial weights match the current
final-panel diagnostics, while the legacy mean-neighbor values remain a
reference rather than a target to reproduce.

``` r
spatial_bench <- analysis_target_csv("bench_spatial_weights", "spatial_weights_rook_queen_benchmark.csv")
spatial_diag <- analysis_target_csv("diag_ext_spatial_weights", "rook_queen_contiguity_comparison.csv")
spatial_ref <- analysis_target_csv("diag_ext_spatial_weights", "spatial_weights_legacy_reference.csv")
```

The current rook benchmark has 4.029 mean neighbors; the current queen
benchmark has 4.041 mean neighbors. These current values are
intentionally computed on the current final matched panel with non-empty
geometry, not on the legacy exploratory `sfExtras` object.

``` r
spatial_bench[spatial_bench$contiguity %in% c("rook", "queen"), c("contiguity", "n", "mean_neighbors", "n_islands", "elapsed_seconds"), drop = FALSE]
```

      contiguity   n mean_neighbors n_islands elapsed_seconds
    1       rook 482       4.029046         0          12.645
    2      queen 482       4.041494         0          12.669

``` r
analysis_table(spatial_bench, "Current rook/queen benchmark")
```

| contiguity | n | mean_neighbors | n_islands | panel_scope | elapsed_seconds | warnings |
|:---|---:|---:|---:|:---|---:|:---|
| rook | 482 | 4.029 | 0 | current_final_matched_panel_non_empty_geometry | 12.645 | some observations have no neighbours; |

Current rook/queen benchmark

if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \| \|queen \| 482\| 4.041\|
0\|current_final_matched_panel_non_empty_geometry \| 12.669\|some
observations have no neighbours; if this seems unexpected, try
increasing the snap argument.; neighbour object has 21 sub-graphs; if
this sub-graph count seems unexpected, try increasing the snap argument.
\|

``` r
analysis_table(spatial_diag, "Current-vs-legacy rook/queen comparison")
```

| contiguity | n | mean_neighbors | n_islands | panel_scope | elapsed_seconds | warnings | legacy_mean_neighbors | mean_neighbor_delta_from_legacy | pct_delta_from_legacy |
|:---|---:|---:|---:|:---|---:|:---|---:|---:|---:|
| rook | 482 | 4.029 | 0 | current_final_matched_panel_non_empty_geometry | 12.834 | some observations have no neighbours; |  |  |  |

Current-vs-legacy rook/queen comparison

if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \| 4.780\| -0.751\| -15.713\| \|queen \|
482\| 4.041\| 0\|current_final_matched_panel_non_empty_geometry \|
14.601\|some observations have no neighbours; if this seems unexpected,
try increasing the snap argument.; neighbour object has 21 sub-graphs;
if this sub-graph count seems unexpected, try increasing the snap
argument. \| 4.783\| -0.742\| -15.511\|

``` r
analysis_table(spatial_ref, "Legacy spatial-weight reference comments")
```

| contiguity | legacy_method | legacy_mean_neighbors | legacy_elapsed_note | interpretation |
|:---|:---|---:|:---|:---|
| rook | sfExtras::st_rook() timing comment; final weights use spdep::poly2nb(queen = FALSE) | 4.780 | legacy comment recorded similar run time to queen | Current means are intentionally computed on the active final matched panel with non-empty geometry; they may differ from legacy exploratory sfExtras objects, but they now match the panel used for current spatial diagnostics. |
| queen | sfExtras::st_queen() timing comment; benchmark uses spdep::poly2nb(queen = TRUE) | 4.783 | legacy comment recorded similar run time to rook | Current means are intentionally computed on the active final matched panel with non-empty geometry; they may differ from legacy exploratory sfExtras objects, but they now match the panel used for current spatial diagnostics. |

Legacy spatial-weight reference comments
