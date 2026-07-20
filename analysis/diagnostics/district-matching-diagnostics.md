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
analysis_deviation_note("The legacy 454-district analysis sample and its reported first-stage estimate 2.945 (SE 0.949) and second-stage estimate 0.201 (SE 0.710) are historical comparison values, not validation targets. They were generated after an almost certainly flawed legacy district-matching procedure. The legacy partial F-statistic of 37.77 is known to be invalid because the legacy code computed it incorrectly. Future audits must evaluate the active pseudo-panel from row-level source assignments and current first-stage diagnostics rather than trying to reproduce these numbers.")
```

**Deviation note.** The legacy helper objects no longer exist in the
active architecture. The rendered current-code analog counts incomplete
rows in target-backed tracker/panel, source-inventory, unmatched-row,
and many-to-many outputs, renders the all-rows close-match search table
as a bounded preview, and explicitly separates fallback source-key
inventory from true legacy unmatched rows. This explanation is a prose
deviation from the legacy comment because the legacy Rmd did not have a
separate source-key-inventory object.

**Deviation note.** The legacy 454-district analysis sample and its
reported first-stage estimate 2.945 (SE 0.949) and second-stage estimate
0.201 (SE 0.710) are historical comparison values, not validation
targets. They were generated after an almost certainly flawed legacy
district-matching procedure. The legacy partial F-statistic of 37.77 is
known to be invalid because the legacy code computed it incorrectly.
Future audits must evaluate the active pseudo-panel from row-level source
assignments and current first-stage diagnostics rather than trying to
reproduce these numbers.

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

The current analog distinguishes the reviewed crosswalk from a future
row-level source-match ledger. It reports 734 reviewed crosswalk rows, 0
explicitly recorded unmatched rows, and 0 rows in the retired fallback
source-key-inventory compatibility output.

#### Deviation from legacy prose: what the active join map establishes

The active diagnostic contract around `district_join_map` now has a
narrower and more accurate meaning:

1.  `_targets.R` reads
    `data/metadata/district_harmonization_crosswalk.csv` and passes it
    to `prepare_district_join_map()`.
2.  The reviewed crosswalk is the sole active harmonization-map
    authority; the constructor validates its required year-specific name
    columns and adds stable internal row identifiers.
3.  Source-to-crosswalk attachment remains in
    `R/districts/source_attachment.R`. The attributes on
    `district_join_map` describe the reviewed map itself, not a
    row-level ledger of the 2001, 2007, and 2017 source assignments.
4.  The source-key-inventory diagnostic is retained only as an empty
    compatibility artifact so historical review outputs have a stable
    filename. It is no longer the active join-map fallback.
5.  `compare_join_keys_to_panel()` compares canonical 2020 crosswalk
    keys with canonical final-panel keys. This is a crosswalk/panel
    coverage check; it cannot prove that every earlier-wave source row
    was assigned to the correct crosswalk row.

This is an important scope warning. Empty join-map `unmatched_rows` and
`many_to_many_cases` attributes do not establish that production source
attachment is correct. The forthcoming district-matching redesign should
expose that production row-level source-match ledger directly.

``` r
analysis_table(dm_summary, "District-matching diagnostic summary")
```

| n_panel_rows | n_join_rows | n_unmatched_rows | n_source_key_inventory_rows | n_many_to_many_cases | n_panel_unmatched_by_key | n_join_unmatched_by_key |
|---:|---:|---:|---:|---:|---:|---:|
| 482 | 734 | 0 | 0 | 0 | 0 | 252 |

District-matching diagnostic summary

``` r
analysis_table(dm_reference, "Legacy Chunk 20 diagnostic reference")
```

| diagnostic | legacy_chunk | current_value | interpretation |
|:---|:---|---:|:---|
| unmatched_rows | Chunk 20 Match districts: Diagnose errors | 0 | Counts true legacy unmatched_df rows when they are present; source-key inventory rows are excluded. |
| source_key_inventory | Chunk 20 Match districts: Diagnose errors | 0 | Historical fallback source-key rows remain a separate compatibility output and are empty for the reviewed-crosswalk join map. |
| many_to_many_cases | Chunk 20 Match districts: Diagnose errors | 0 | Uses explicit many_to_many attributes/flags rather than treating every source-key row as a many-to-many case. |
| all_rows_search | Chunk 20 Match districts: Diagnose errors | NA | Search table preserves the legacy View()-style close-match inspection in a CSV artifact. |

Legacy Chunk 20 diagnostic reference

``` r
analysis_table(dm_tracker_panel, "Tracker/panel/intermediate row-count comparison")
```

| object                         | n_rows | n_complete_rows |
|:-------------------------------|-------:|----------------:|
| district_panel                 |    482 |             482 |
| district_join_map              |    734 |             734 |
| unmatched_rows                 |      0 |              NA |
| source_key_inventory           |      0 |              NA |
| many_to_many_cases             |      0 |              NA |
| key_role:requires_review       |    252 |              NA |
| key_role:shared_panel_join_key |    482 |              NA |

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
| not_in_panel     | in_join_map     |  252 |

Panel-vs-join key-status summary

``` r
analysis_table(dm_key_roles, "Panel/join key roles and interpretation")
```

| key_role              | n_keys | interpretation                             |
|:----------------------|-------:|:-------------------------------------------|
| requires_review       |    252 | Requires manual review.                    |
| shared_panel_join_key |    482 | Shared by active final panel and join map. |

Panel/join key roles and interpretation

``` r
analysis_table(dm_source_inventory, "Fallback source-key inventory separated from true unmatched rows", max_rows = 20)
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_matching/district_matching_source_key_inventory.csv |

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
| Table truncated in rendered note; full CSV has 1216 rows. |  |  |

All-rows close-match search table
