# District Matching Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Errors are fixed in the chunk “Match districts: Manually fix errors”.
Changed for diagnostic purposes.

To extract mismatches: the legacy chunk pulled the rows whose merged
district/year fields were absent after `merge_dfs_into_tracker()`,
tagged their source, and combined them for inspection. Compare outcomes
with different tracker dfs. Identify errors for each tracker dr. Combine
all errors. Create a df with all rows from original data, so I can
search for close matches easily.

#### Correct even more NAs

The legacy row-count checks were:
`flagged_df_timeseries %>% .[!complete.cases(.),] %>% nrow()` = 155
rows; `joined_df_timeseries %>% .[!complete.cases(.),] %>% nrow()` = 305
rows; `flagged_df_tracker %>% .[!complete.cases(.),] %>% nrow()` = 92
rows; `joined_df_tracker %>% .[!complete.cases(.),] %>% nrow()` = 250
rows. \*\*\*Why the gigantic discrepancy???

``` r
analysis_deviation_note("The legacy helper objects no longer exist in the active architecture. The rendered current-code analog counts incomplete rows in target-backed tracker/panel, source-inventory, unmatched-row, and many-to-many outputs, renders the all-rows close-match search table as a bounded preview, and explicitly separates fallback source-key inventory from true legacy unmatched rows. This explanation is a prose deviation from the legacy comment because the legacy Rmd did not have a separate source-key-inventory object.")
```

**Deviation note.** The legacy helper objects no longer exist in the
active architecture. The rendered current-code analog counts incomplete
rows in target-backed tracker/panel, source-inventory, unmatched-row,
and many-to-many outputs, renders the all-rows close-match search table
as a bounded preview, and explicitly separates fallback source-key
inventory from true legacy unmatched rows. This explanation is a prose
deviation from the legacy comment because the legacy Rmd did not have a
separate source-key-inventory object.

``` r
dm_summary <- analysis_target_csv("diag_ext_district_matching", "district_matching_summary.csv")
dm_reference <- analysis_target_csv("diag_ext_district_matching", "district_matching_legacy_reference.csv")
dm_tracker_panel <- analysis_target_csv("diag_ext_district_matching", "district_matching_tracker_panel_comparison.csv")
dm_key_comparison <- analysis_target_csv("diag_ext_district_matching", "district_matching_key_comparison.csv")
dm_key_roles <- analysis_target_csv("diag_ext_district_matching", "district_matching_key_role_counts.csv")
dm_source_inventory <- analysis_target_csv("diag_ext_district_matching", "district_matching_source_key_inventory.csv")
dm_unmatched <- analysis_target_csv("diag_ext_district_matching", "district_matching_unmatched_rows.csv")
dm_many <- analysis_target_csv("diag_ext_district_matching", "district_matching_many_to_many_cases.csv")
dm_all_rows <- analysis_target_csv("diag_ext_district_matching", "district_matching_all_rows_search.csv")
```

The current analog keeps true unmatched rows separate from fallback
source-key inventory rows. It reports 0 true unmatched rows and 3,175
fallback source-key inventory rows.

#### Deviation from legacy prose: why the source-key inventory is large

The 3,175 source-key rows and 1,523 join-only canonical keys are not a
second copy of the legacy `unmatched_df` failure. They come from the
active diagnostic contract around `district_join_map`. The exact root
cause is:

1.  `_targets.R` builds `district_join_map` with
    `fuzzy_join_districts(district_tracker, district_keys_2001, district_keys_2007, district_keys_2017, district_keys_2020, cfg)`.
2.  `fuzzy_join_districts()` only returns tracker rows as the join map
    when its `district_tracker` input already has the full legacy
    tracker shape: `state_01`/`district_01`, `state_07`/`district_07`,
    `state_17`/`district_17`, and `state_20`/`district_20`.
3.  When that condition is not met, `fuzzy_join_districts()` takes its
    fallback path: it binds the 2001, 2007, 2017, and 2020 source key
    tables, sets `match_status = "source_key_unmatched"`, and stores
    that same key inventory in `attr(out, "unmatched_rows")`.
4.  `diagnose_district_matching()` then recognizes this fallback object
    with `is_source_key_inventory()` and deliberately excludes it from
    `n_unmatched_rows`. That is why true unmatched rows are 0 while
    source-key inventory rows are 3,175.
5.  `compare_join_keys_to_panel()` compares canonical panel keys from
    the final 482-row analysis panel against canonical keys from the
    fallback source-key inventory. The 1,523 join-only keys are
    therefore source keys that appear in the broad raw key universe but
    not in the final matched analysis panel. They are not direct
    final-panel join failures.

This is still an important warning. It means the diagnostic object
called `district_join_map` is not presently the same kind of object as
the legacy `merge_dfs_into_tracker()` joined tracker. The final
`district_panel` is being built from the current tracker/panel-building
path, while this diagnostic join map is mostly a broad source-key
inventory. The diagnostic is useful for search and auditing, but it
should not be interpreted as saying that 3,175 districts failed to join
into the final panel.

``` r
analysis_table(dm_summary, "District-matching diagnostic summary")
```

| n_panel_rows | n_join_rows | n_unmatched_rows | n_source_key_inventory_rows | n_many_to_many_cases | n_panel_unmatched_by_key | n_join_unmatched_by_key |
|---:|---:|---:|---:|---:|---:|---:|
| 482 | 3175 | 0 | 3175 | 0 | 0 | 1523 |

District-matching diagnostic summary

``` r
analysis_table(dm_reference, "Legacy Chunk 20 diagnostic reference")
```

| diagnostic | legacy_chunk | current_value | interpretation |
|:---|:---|---:|:---|
| unmatched_rows | Chunk 20 Match districts: Diagnose errors | 0 | Counts true legacy unmatched_df rows when they are present; source-key inventory rows are excluded. |
| source_key_inventory | Chunk 20 Match districts: Diagnose errors | 3175 | Rows marked source_key_unmatched by the fallback key-map path are preserved separately because they are not the legacy unmatched_df diagnostic. |
| many_to_many_cases | Chunk 20 Match districts: Diagnose errors | 0 | Uses explicit many_to_many attributes/flags rather than treating every source-key row as a many-to-many case. |
| all_rows_search | Chunk 20 Match districts: Diagnose errors | NA | Search table preserves the legacy View()-style close-match inspection in a CSV artifact. |

Legacy Chunk 20 diagnostic reference

``` r
analysis_table(dm_tracker_panel, "Tracker/panel/intermediate row-count comparison")
```

| object                             | n_rows | n_complete_rows |
|:-----------------------------------|-------:|----------------:|
| district_panel                     |    482 |             482 |
| district_join_map                  |   3175 |            3175 |
| unmatched_rows                     |      0 |              NA |
| source_key_inventory               |   3175 |              NA |
| many_to_many_cases                 |      0 |              NA |
| key_role:shared_panel_join_key     |    482 |              NA |
| key_role:source_key_inventory_only |   1523 |              NA |

Tracker/panel/intermediate row-count comparison

The current analog of
`flagged_df_timeseries %>% .[!complete.cases(.),] %>% nrow()` is to
count incomplete rows in the active tracker/panel comparison table and
source-key diagnostic tables rather than rerunning the deleted legacy
helper.

``` r
sapply(
  list(
    tracker_panel_comparison = dm_tracker_panel,
    source_key_inventory = dm_source_inventory,
    true_unmatched_rows = dm_unmatched,
    many_to_many_cases = dm_many
  ),
  function(x) sum(!stats::complete.cases(as.data.frame(x)))
)
```

    tracker_panel_comparison     source_key_inventory      true_unmatched_rows 
                           5                        0                        0 
          many_to_many_cases 
                           0 

``` r
if (all(c("panel_key_status", "join_key_status") %in% names(dm_key_comparison))) {
  key_summary <- as.data.frame(table(panel_key_status = dm_key_comparison$panel_key_status, join_key_status = dm_key_comparison$join_key_status), stringsAsFactors = FALSE)
  analysis_table(key_summary, "Panel-vs-join key-status summary")
} else {
  analysis_table(dm_key_comparison, "Panel-vs-join key-status summary")
}
```

| panel_key_status | join_key_status | Freq |
|:-----------------|:----------------|-----:|
| in_panel         | in_join_map     |  482 |
| not_in_panel     | in_join_map     | 1523 |

Panel-vs-join key-status summary

``` r
analysis_table(dm_key_roles, "Panel/join key roles and interpretation")
```

| key_role | n_keys | interpretation |
|:---|---:|:---|
| shared_panel_join_key | 482 | Shared by active final panel and join map. |
| source_key_inventory_only | 1523 | Observed only in the fallback source-key inventory; not a true failed final-panel join. |

Panel/join key roles and interpretation

``` r
analysis_table(dm_source_inventory, "Fallback source-key inventory separated from true unmatched rows", max_rows = 20)
```

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

``` r
analysis_table(dm_unmatched, "True unmatched rows from current matching diagnostics", max_rows = 20)
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_matching/district_matching_unmatched_rows.csv |

True unmatched rows from current matching diagnostics

``` r
analysis_table(dm_many, "Many-to-many cases from current matching diagnostics", max_rows = 20)
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_matching/district_matching_many_to_many_cases.csv |

Many-to-many cases from current matching diagnostics

``` r
analysis_table(dm_all_rows, "All-rows close-match search table", max_rows = 30)
```

| state | district | source |
|:---|:---|:---|
| Andhra Pradesh | Anantapur | panel |
| Andhra Pradesh | Chittoor | panel |
| Andhra Pradesh | East Godavari | panel |
| Andhra Pradesh | Guntur | panel |
| Andhra Pradesh | Krishna | panel |
| Andhra Pradesh | Kurnool | panel |
| Andhra Pradesh | Prakasam | panel |
| Andhra Pradesh | Srikakulam | panel |
| Andhra Pradesh | Visakhapatnam | panel |
| Andhra Pradesh | Vizianagaram | panel |
| Andhra Pradesh | West Godavari | panel |
| Arunachal Pradesh | Changlang | panel |
| Arunachal Pradesh | East Kameng | panel |
| Arunachal Pradesh | East Siang | panel |
| Arunachal Pradesh | Lohit | panel |
| Arunachal Pradesh | Tirap | panel |
| Arunachal Pradesh | Dibang Valley | panel |
| Arunachal Pradesh | Lower Subansiri | panel |
| Arunachal Pradesh | Papum Pare | panel |
| Arunachal Pradesh | Tawang | panel |
| Arunachal Pradesh | Upper Siang | panel |
| Arunachal Pradesh | Upper Subansiri | panel |
| Arunachal Pradesh | West Kameng | panel |
| Arunachal Pradesh | West Siang | panel |
| Assam | Barpeta | panel |
| Assam | Darrang | panel |
| Assam | Bongaigaon | panel |
| Assam | Cachar | panel |
| Assam | Dhemaji | panel |
| Assam | Dhubri | panel |
| Table truncated in rendered note; full CSV has 6832 rows. |  |  |

All-rows close-match search table
