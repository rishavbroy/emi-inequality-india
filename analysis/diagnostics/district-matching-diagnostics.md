# District Matching Diagnostics


## Legacy comments

### Legacy Chunk 20: district-matching error diagnosis

Errors are fixed in the chunk “Match districts: Manually fix errors”

Changed for diagnostic purposes

To extract mismatches: Compute two‐digit suffixes

Run merge_dfs_into_tracker on each df, extract unmatched_df, add source

``` r
Add source column of length nrow(out)
```

Bind their rows

Coalesce the district\_/state\_ columns

### Compare outcomes with different tracker dfs

Identify errors for each tracker dr Combine all errors

Create a df with all rows from original data, so I can search for close
matches easily

### Correct even more NAs

``` r
flagged_df_timeseries %>% .[!complete.cases(.),] %>% nrow()
155 rows
```

``` r
joined_df_timeseries %>% .[!complete.cases(.),] %>% nrow()
305 rows
```

``` r
flagged_df_tracker %>% .[!complete.cases(.),] %>% nrow()
92 rows
```

``` r
joined_df_tracker %>% .[!complete.cases(.),] %>% nrow()
250 rows
```

\*\*\*Why the gigantic discrepancy???

**Deviation note.** The legacy comments above diagnose unmatched rows
from `merge_dfs_into_tracker()` and compare tracker alternatives. The
current outputs below keep true unmatched rows, fallback source-key
inventory rows, and panel-vs-join key comparisons separate so that
source-key inventory is not misreported as a failed final-panel join.

## Current targets-backed results

| n_panel_rows | n_join_rows | n_unmatched_rows | n_source_key_inventory_rows | n_many_to_many_cases | n_panel_unmatched_by_key | n_join_unmatched_by_key |
|---:|---:|---:|---:|---:|---:|---:|
| 482 | 3175 | 0 | 3175 | 0 | 0 | 1523 |

District-matching diagnostic summary

| diagnostic | legacy_chunk | current_value | interpretation |
|:---|:---|---:|:---|
| unmatched_rows | Chunk 20 Match districts: Diagnose errors | 0 | Counts true legacy unmatched_df rows when they are present; source-key inventory rows are excluded. |
| source_key_inventory | Chunk 20 Match districts: Diagnose errors | 3175 | Rows marked source_key_unmatched by the fallback key-map path are preserved separately because they are not the legacy unmatched_df diagnostic. |
| many_to_many_cases | Chunk 20 Match districts: Diagnose errors | 0 | Uses explicit many_to_many attributes/flags rather than treating every source-key row as a many-to-many case. |
| all_rows_search | Chunk 20 Match districts: Diagnose errors | NA | Search table preserves the legacy View()-style close-match inspection in a CSV artifact. |

Legacy Chunk 20 diagnostic reference

| object               | n_rows | n_complete_rows |
|:---------------------|-------:|----------------:|
| district_panel       |    482 |             482 |
| district_join_map    |   3175 |            3175 |
| unmatched_rows       |      0 |              NA |
| source_key_inventory |   3175 |              NA |
| many_to_many_cases   |      0 |              NA |

Tracker/panel/intermediate row-count comparison

| panel_key_status | join_key_status | Freq |
|:-----------------|:----------------|-----:|
| in_panel         | in_join_map     |  482 |
| not_in_panel     | in_join_map     | 1523 |

Panel-vs-join key-status summary

| key_role | n_keys | interpretation |
|:---|---:|:---|
| shared_panel_join_key | 482 | Shared by active final panel and join map. |
| source_key_inventory_only | 1523 | Observed only in the fallback source-key inventory; not a true failed final-panel join. |

Panel/join key roles and interpretation

| state_std | district_std | source_year | district_key | source | match_status | possible_false_positive | many_to_many | diagnostic_role | legacy_note |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 01 | 01 | 2001 | 2001\_\_01\_\_01 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 02 | 2001 | 2001\_\_01\_\_02 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 03 | 2001 | 2001\_\_01\_\_03 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 04 | 2001 | 2001\_\_01\_\_04 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 05 | 2001 | 2001\_\_01\_\_05 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 06 | 2001 | 2001\_\_01\_\_06 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 07 | 2001 | 2001\_\_01\_\_07 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 08 | 2001 | 2001\_\_01\_\_08 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 09 | 2001 | 2001\_\_01\_\_09 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 10 | 2001 | 2001\_\_01\_\_10 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 11 | 2001 | 2001\_\_01\_\_11 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 12 | 2001 | 2001\_\_01\_\_12 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 13 | 2001 | 2001\_\_01\_\_13 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 01 | 14 | 2001 | 2001\_\_01\_\_14 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 02 | 01 | 2001 | 2001\_\_02\_\_01 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 02 | 02 | 2001 | 2001\_\_02\_\_02 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 02 | 03 | 2001 | 2001\_\_02\_\_03 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 02 | 04 | 2001 | 2001\_\_02\_\_04 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 02 | 05 | 2001 | 2001\_\_02\_\_05 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| 02 | 06 | 2001 | 2001\_\_02\_\_06 | 2001 | source_key_unmatched | FALSE | FALSE | source_key_inventory_not_true_unmatched_rows | These rows are the source-key inventory emitted by the fallback key-map path, not the legacy Chunk 20 unmatched_df output from merge_dfs_into_tracker(). |
| Table truncated in rendered note; full CSV has 3175 rows. |  |  |  |  |  |  |  |  |  |

Fallback source-key inventory separated from true unmatched rows

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_matching/district_matching_unmatched_rows.csv |

True unmatched rows from current matching diagnostics

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_matching/district_matching_many_to_many_cases.csv |

Many-to-many cases from current matching diagnostics
