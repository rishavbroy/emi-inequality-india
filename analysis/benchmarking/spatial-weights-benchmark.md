# Spatial Weights Benchmark


## Legacy comments

### Legacy Chunk 24: rook/queen tuning notes

Test for best contiguity measure Code is based on
@liChapter14Spatial2019

``` r
remotes::install_github("spatialanalysis/sfExtras")
library(sfExtras)
rook_neighbors <- joined_geom %>% st_rook()
rook_neighbors %>% lengths() %>% mean()
4.780165 average neighbors per district
queen_neighbors <- joined_geom %>% st_queen()
queen_neighbors %>% lengths() %>% mean()
4.783471
```

Build a rook‐contiguity neighbor list

``` r
start.time <- Sys.time()
end.time <- Sys.time()
end.time - start.time
With queen = TRUE: 32.64976 secs
With queen = FALSE: 31.15051 secs
```

Turn that into a binary adjacency matrix

``` r
   W_2020[i,j] == 1 if districts i and j share a border, 0 otherwise
```

**Deviation note.** The tables below use the current active panel and
geometry. Current-vs-legacy neighbor-count differences are documented
under `docs/refactor/` rather than in the paper or README.

## Current targets-backed results

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

| contiguity | n | mean_neighbors | n_islands | elapsed_seconds | warnings | legacy_mean_neighbors | mean_neighbor_delta_from_legacy | pct_delta_from_legacy |
|:---|---:|---:|---:|---:|:---|---:|---:|---:|
| rook | 482 | 4.029 | 0 | 12.259 | some observations have no neighbours; |  |  |  |

Current-vs-legacy rook/queen comparison

if this seems unexpected, try increasing the snap argument.; neighbour
object has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \| 4.780\| -0.751\| -15.713\| \|queen \|
482\| 4.041\| 0\| 11.701\|some observations have no neighbours; if this
seems unexpected, try increasing the snap argument.; neighbour object
has 21 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \| 4.783\| -0.742\| -15.511\|
