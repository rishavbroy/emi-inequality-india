# Fuzzy Matching Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

The legacy fuzzy-matching chunk tested string-distance methods,
inspected troublesome pairs, and compared thresholds before fixing the
production thresholds. Some sample pairs included
`Baleshwar`/`Balasore`, `Jammu & Kashmir`/`Jammu and Kashmir`,
`East Godavari`/`Godavari East`, `Sikim`/`Sikkim`, and long truncated
district names such as
`Sahibzada Ajit Singh Nag*`/`Sahibzada Ajit Singh Nagar`.

The final helper vectors were
`methods <- c("soundex", "qgram", "jw", "dl", "osa")` and
`thresholds <- c(0, 0, 0.15, 2, 1)`. `soundex = 0` means phonetic
variants in anglicization allowed. `qgram = 0` means rearrangements of
words allowed. `jw <= 0.15` means respellings and vowel swaps with 0.85
similarity allowed. `dl <= 2` means no more than two insertions,
deletions, substitutions, and transpositions allowed. `osa <= 1` means
one typo allowed.

``` r
analysis_deviation_note("The current diagnostic renders active target outputs rather than legacy View() calls, but preserves the sample-pair and threshold prose in the note body.")
```

**Deviation note.** The current diagnostic renders active target outputs
rather than legacy View() calls, but preserves the sample-pair and
threshold prose in the note body.

``` r
fuzzy_summary <- analysis_target_csv("diag_ext_fuzzy_matching", "fuzzy_matching_summary.csv")
fuzzy_methods <- analysis_target_csv("diag_ext_fuzzy_matching", "fuzzy_matching_legacy_methods.csv")
fuzzy_tuning <- analysis_target_csv("diag_ext_fuzzy_matching", "fuzzy_matching_legacy_tuning_reference.csv")
fuzzy_coverage <- analysis_target_csv("diag_ext_fuzzy_matching", "fuzzy_matching_candidate_pair_coverage.csv")
fuzzy_trouble <- analysis_target_csv("diag_ext_fuzzy_matching", "fuzzy_matching_troublesome_pairs.csv")
fuzzy_status <- analysis_target_csv("diag_ext_fuzzy_matching", "fuzzy_matching_join_status_counts.csv")
```

``` r
analysis_table(fuzzy_summary, "Fuzzy matching diagnostic summary")
```

| n_tracker_rows | n_join_rows | n_unmatched_rows | n_candidate_pairs | n_active_candidate_pairs |
|---:|---:|---:|---:|---:|
| 21534 | 734 | 0 | 487 | 478 |

Fuzzy matching diagnostic summary

``` r
analysis_table(fuzzy_methods, "Legacy final string-distance methods and thresholds")
```

| method  | threshold |
|:--------|----------:|
| soundex |      0.00 |
| qgram   |      0.00 |
| jw      |      0.15 |
| dl      |      2.00 |
| osa     |      1.00 |

Legacy final string-distance methods and thresholds

``` r
analysis_table(fuzzy_tuning, "Legacy tuning comments retained as benchmarks")
```

| diagnostic | legacy_result | legacy_chunk |
|:---|:---|:---|
| full_join_method_row_counts | osa/lv/dl 859 rows; hamming 872; lcs 825; qgram 829; cosine/jaccard/jw 435262 | Chunk 16 Match districts: Test joining methods |
| lcs_osa_3_3 | 189/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| jw_dl_osa_lcs | 180/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| final_method_choice | soundex=0, qgram=0, jw\<=0.15, dl\<=2, osa\<=1; 166/734 rows had any NA | Chunk 16 Match districts: Test joining methods |

Legacy tuning comments retained as benchmarks

``` r
analysis_table(fuzzy_coverage, "Candidate-pair coverage", max_rows = 30)
```

| pair_source | n_pairs | coverage_note |
|:---|---:|:---|
| join_map_district_01_to_district_05 | 21 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_05_to_district_06 | 6 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_06_to_district_07 | 8 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_07_to_district_08 | 8 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_08_to_district_11 | 29 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_11_to_district_17 | 79 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_17_to_district_18 | 3 | active source/tracker candidate pair emitted by the current pipeline |
| join_map_district_18_to_district_19 | 10 | active source/tracker candidate pair emitted by the current pipeline |
| legacy_troublesome_comment | 9 | legacy hand-picked examples from Chunk 16 |
| tracker_2001_to_2005 | 21 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2001_to_2007 | 35 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2005_to_2006 | 6 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2006_to_2007 | 8 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2007_to_2008 | 8 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2007_to_2017 | 102 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2008_to_2011 | 29 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2011_to_2017 | 79 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2017_to_2018 | 3 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2017_to_2020 | 13 | active source/tracker candidate pair emitted by the current pipeline |
| tracker_2018_to_2019 | 10 | active source/tracker candidate pair emitted by the current pipeline |

Candidate-pair coverage

``` r
analysis_table(fuzzy_trouble, "Troublesome legacy name-pair checks")
```

| str1 | str2 | pair_source | method | distance | threshold | match |
|:---|:---|:---|:---|---:|---:|:---|
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | dl | 22.000 | 2.00 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | jw | 0.509 | 0.15 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | osa | 22.000 | 1.00 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | qgram | 13.000 | 0.00 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | soundex | 1.000 | 0.00 | FALSE |
| Baleshwar | Balasore | legacy_troublesome_comment | dl | 5.000 | 2.00 | FALSE |
| Baleshwar | Balasore | legacy_troublesome_comment | jw | 0.273 | 0.15 | FALSE |
| Baleshwar | Balasore | legacy_troublesome_comment | osa | 5.000 | 1.00 | FALSE |
| Baleshwar | Balasore | legacy_troublesome_comment | qgram | 3.000 | 0.00 | FALSE |
| Baleshwar | Balasore | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| East Godavari | Godavari East | legacy_troublesome_comment | dl | 10.000 | 2.00 | FALSE |
| East Godavari | Godavari East | legacy_troublesome_comment | jw | 0.304 | 0.15 | FALSE |
| East Godavari | Godavari East | legacy_troublesome_comment | osa | 10.000 | 1.00 | FALSE |
| East Godavari | Godavari East | legacy_troublesome_comment | qgram | 0.000 | 0.00 | TRUE |
| East Godavari | Godavari East | legacy_troublesome_comment | soundex | 1.000 | 0.00 | FALSE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | dl | 0.000 | 2.00 | TRUE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | jw | 0.000 | 0.15 | TRUE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | osa | 0.000 | 1.00 | TRUE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | qgram | 0.000 | 0.00 | TRUE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | dl | 0.000 | 2.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | jw | 0.000 | 0.15 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | osa | 0.000 | 1.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | qgram | 0.000 | 0.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | dl | 2.000 | 2.00 | TRUE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | jw | 0.026 | 0.15 | TRUE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | osa | 2.000 | 1.00 | FALSE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | qgram | 2.000 | 0.00 | FALSE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | dl | 2.000 | 2.00 | TRUE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | jw | 0.026 | 0.15 | TRUE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | osa | 2.000 | 1.00 | FALSE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | qgram | 2.000 | 0.00 | FALSE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | dl | 1.000 | 2.00 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | jw | 0.056 | 0.15 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | osa | 1.000 | 1.00 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | qgram | 1.000 | 0.00 | FALSE |
| Sikim | Sikkim | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | dl | 3.000 | 2.00 | FALSE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | jw | 0.037 | 0.15 | TRUE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | osa | 3.000 | 1.00 | FALSE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | qgram | 3.000 | 0.00 | FALSE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |

Troublesome legacy name-pair checks

``` r
analysis_table(fuzzy_status, "Current join status counts")
```

| match_status           | Freq |
|:-----------------------|-----:|
| reviewed_crosswalk_row |  734 |

Current join status counts
