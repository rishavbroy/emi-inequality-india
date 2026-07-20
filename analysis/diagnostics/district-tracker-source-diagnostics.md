# District Tracker Source Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

The legacy tracker-source diagnostics compared district names across
years, inspected state and union-territory changes, noted unrecorded
state changes requiring manual attention, and checked same-name
districts that appeared across states. The analysis note keeps the
legacy recorded-change, unrecorded-change, in-period
district-name-change, and same-name-district benchmarks beside the
current row-level outputs so differences in counting level are visible.

``` r
analysis_deviation_note("The current source diagnostics report row-level outputs, event-level summaries, and per-year same-name-district counts beside the legacy expected-event benchmarks, because active tracker sources can count more rows than the legacy comments' event-level summaries.")
```

**Deviation note.** The current source diagnostics report row-level
outputs, event-level summaries, and per-year same-name-district counts
beside the legacy expected-event benchmarks, because active tracker
sources can count more rows than the legacy comments’ event-level
summaries.

``` r
tracker_counts <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_source_counts.csv")
tracker_state_changes <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_state_changes.csv")
tracker_state_events <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_state_change_events.csv")
tracker_expected_state <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_legacy_expected_state_changes.csv")
tracker_unrecorded <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_unrecorded_state_changes.csv")
tracker_inperiod <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_inperiod_district_changes.csv")
tracker_expected_inperiod <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_legacy_expected_inperiod_district_changes.csv")
tracker_same <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_same_name_districts.csv")
tracker_same_by_year <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_same_name_districts_by_year.csv")
tracker_expected_same <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_legacy_expected_same_name_districts.csv")
```

``` r
data.frame(
  current_code_analog = c(
    "nrow(tracker_state_events)",
    "nrow(tracker_expected_state)",
    "nrow(tracker_inperiod)",
    "nrow(tracker_expected_inperiod)",
    "nrow(tracker_same_by_year[tracker_same_by_year$within_legacy_range, ])"
  ),
  rows = c(
    nrow(tracker_state_events),
    nrow(tracker_expected_state),
    nrow(tracker_inperiod),
    nrow(tracker_expected_inperiod),
    if ("within_legacy_range" %in% names(tracker_same_by_year)) sum(tracker_same_by_year$within_legacy_range %in% TRUE) else NA_integer_
  )
)
```

                                                         current_code_analog rows
    1                                             nrow(tracker_state_events)    1
    2                                           nrow(tracker_expected_state)    2
    3                                                 nrow(tracker_inperiod)    1
    4                                        nrow(tracker_expected_inperiod)    1
    5 nrow(tracker_same_by_year[tracker_same_by_year$within_legacy_range, ])   NA

``` r
analysis_table(tracker_counts, "Tracker source row counts")
```

| source_file_id                 | n_rows | n_columns |
|:-------------------------------|-------:|----------:|
| district_changes_alluvial      |    808 |        16 |
| district_changes_carveouts     |    384 |         5 |
| district_changes_tracker       |    735 |        60 |
| district_changes_new_districts |    487 |         6 |
| district_changes_name_changes  |    134 |         6 |
| district_changes_splits        |    929 |         6 |

Tracker source row counts

``` r
analysis_table(tracker_state_changes, "Current row-level state/UT changes")
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_tracker_sources/tracker_state_changes.csv |

Current row-level state/UT changes

``` r
analysis_table(tracker_state_events, "Current state/UT change events")
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_tracker_sources/tracker_state_change_events.csv |

Current state/UT change events

``` r
analysis_table(tracker_expected_state, "Legacy expected recorded state/UT changes")
```

| legacy_event | first_reflected | legacy_chunk | current_detection_status |
|:---|:---|:---|:---|
| Ladakh split from Jammu and Kashmir | 2019 data | Chunk 6 district tracker source QA | must be detected from raw/pre-correction tracker columns or carried as this reference row |
| Dadra and Nagar Haveli and Daman and Diu merger | 2019 data | Chunk 6 district tracker source QA | must be detected from raw/pre-correction tracker columns or carried as this reference row |

Legacy expected recorded state/UT changes

``` r
analysis_table(tracker_unrecorded, "Legacy unrecorded state/UT changes requiring manual attention")
```

| change | legacy_note |
|:---|:---|
| Pondicherry/Puducherry district and UT rename | Legacy comment: 2007-08 NSS still uses Pondicherry despite 2006 Puducherry rename. |
| Uttaranchal/Uttarakhand state rename | Legacy comment: 2007-08 NSS uses Uttaranchal rather than Uttarakhand. |
| Orissa/Odisha state rename | Legacy comment: apply pre-2011 Orissa naming when matching earlier samples. |
| Telangana split from Andhra Pradesh | Legacy comment: apply Andhra Pradesh name before Telangana split when matching pre-2014 data. |

Legacy unrecorded state/UT changes requiring manual attention

``` r
analysis_table(tracker_inperiod, "Current district-name changes inside sampling periods")
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_tracker_sources/tracker_inperiod_district_changes.csv |

Current district-name changes inside sampling periods

``` r
analysis_table(tracker_expected_inperiod, "Legacy in-period district-name-change benchmark")
```

| diagnostic | legacy_expected_rows | legacy_chunk | legacy_note | current_detection_status |
|:---|---:|:---|:---|:---|
| in_period_district_name_changes | 16 | Chunk 6 district tracker source QA | Legacy comments counted rows where district_05 != district_06, district_07 != district_08, district_17 != district_18, or district_19 != district_20 before downstream corrections. | rendered analysis should compare this benchmark with current tracker_inperiod_district_changes.csv |

Legacy in-period district-name-change benchmark

``` r
analysis_table(tracker_same, "Current same-name districts appearing in multiple states", max_rows = 30)
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_tracker_sources/tracker_same_name_districts.csv |

Current same-name districts appearing in multiple states

``` r
analysis_table(tracker_same_by_year, "Current same-name districts per year compared to legacy 6-10 range")
```

| note |
|:---|
| No rows in analysis output: outputs/diagnostics/extended/district_tracker_sources/tracker_same_name_districts_by_year.csv |

Current same-name districts per year compared to legacy 6-10 range

``` r
analysis_table(tracker_expected_same, "Legacy same-name-district benchmark")
```

| diagnostic | legacy_expected_min_districts | legacy_expected_max_districts | legacy_chunk | legacy_note | current_detection_status |
|:---|---:|---:|:---|:---|:---|
| same_name_districts_across_states | 6 | 10 | Chunk 6 district tracker source QA | Legacy comments counted between min(n_same_name_districts$n) = 6 and max(n_same_name_districts$n) = 10 districts with shared names in each year of interest. | rendered analysis should compare this benchmark with tracker_same_name_districts.csv; a zero current count means the active tracker no longer exposes the raw same-name ambiguity, not that the legacy QA was irrelevant. |

Legacy same-name-district benchmark
