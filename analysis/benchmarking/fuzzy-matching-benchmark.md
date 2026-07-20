# Fuzzy Matching Benchmark


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Number of rows from fuzzy full joining `district_tracker` (734 rows) and
`mother_tongues_01` (593 rows) with different metrics: `osa`: 859; `lv`:
859; `dl`: 859; `hamming`: 872; `lcs`: 825; `qgram`: 829; `cosine`,
`jaccard`, and `jw`: 435,262. See
<https://cran.r-project.org/web/packages/stringdist/stringdist.pdf#page=23>
for more.

Testing different methods of fuzzy joining. With (`lcs`, `osa`), (3, 3):
189/734 rows have an NA. With (`jw`, `dl`, `osa`, `lcs`), (0.10, 2, 3,
5): 180/734. With final helper vectors
`methods <- c("soundex", "qgram", "jw", "dl", "osa")` and
`thresholds <- c(0, 0, 0.15, 2, 1)`: 166/734.

`soundex = 0` means phonetic variants in anglicization allowed.
`qgram = 0` means rearrangements of words allowed. `jw <= 0.15` means
respellings and vowel swaps with 0.85 similarity allowed. `dl <= 2`
means no more than two insertions, deletions, substitutions, and
transpositions allowed. `osa <= 1` means one typo allowed.

``` r
analysis_deviation_note("The legacy hand-counts are preserved as prose/reference rows; the current benchmark additionally evaluates target-generated candidate pairs from the active tracker and join-map objects.")
```

**Deviation note.** The legacy hand-counts are preserved as
prose/reference rows; the current benchmark additionally evaluates
target-generated candidate pairs from the active tracker and join-map
objects.

``` r
fuzzy_cov <- analysis_target_csv("bench_fuzzy_matching", "fuzzy_matching_candidate_pair_coverage.csv")
fuzzy_threshold <- analysis_target_csv("bench_fuzzy_matching", "fuzzy_matching_threshold_sensitivity.csv")
fuzzy_reference <- analysis_target_csv("bench_fuzzy_matching", "fuzzy_matching_legacy_tuning_reference.csv")
```

The current benchmark evaluates 333 candidate pairs across 10
pair-source groups.

``` r
data.frame(
  current_code_analog = "sum(fuzzy_cov$n_pairs, na.rm = TRUE)",
  candidate_pairs = sum(fuzzy_cov$n_pairs, na.rm = TRUE),
  pair_source_groups = nrow(fuzzy_cov)
)
```

                       current_code_analog candidate_pairs pair_source_groups
    1 sum(fuzzy_cov$n_pairs, na.rm = TRUE)             333                 10

``` r
analysis_table(fuzzy_cov, "Benchmark candidate-pair coverage", max_rows = 30)
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
| tracker_1991_to_2001 | 160 | active source/tracker candidate pair emitted by the current pipeline |

Benchmark candidate-pair coverage

``` r
analysis_table(fuzzy_reference, "Legacy fuzzy-tuning reference results")
```

| diagnostic | legacy_result | legacy_chunk |
|:---|:---|:---|
| full_join_method_row_counts | osa/lv/dl 859 rows; hamming 872; lcs 825; qgram 829; cosine/jaccard/jw 435262 | Chunk 16 Match districts: Test joining methods |
| lcs_osa_3_3 | 189/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| jw_dl_osa_lcs | 180/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| final_method_choice | soundex=0, qgram=0, jw\<=0.15, dl\<=2, osa\<=1; 166/734 rows had any NA | Chunk 16 Match districts: Test joining methods |

Legacy fuzzy-tuning reference results

``` r
analysis_table(fuzzy_threshold, "Threshold sensitivity by method and pair source", max_rows = 40)
```

| method | threshold | pair_source | n_pairs | n_matches | pct_matches |
|:---|:---|:---|:---|:---|:---|
| soundex | 0 | join_map_district_01_to_district_05 | 21 | 1 | 0.0476190476190476 |
| soundex | 0 | join_map_district_05_to_district_06 | 6 | 0 | 0 |
| soundex | 0 | join_map_district_06_to_district_07 | 8 | 0 | 0 |
| soundex | 0 | join_map_district_07_to_district_08 | 8 | 0 | 0 |
| soundex | 0 | join_map_district_08_to_district_11 | 29 | 2 | 0.0689655172413793 |
| soundex | 0 | join_map_district_11_to_district_17 | 79 | 2 | 0.0253164556962025 |
| soundex | 0 | join_map_district_17_to_district_18 | 3 | 0 | 0 |
| soundex | 0 | join_map_district_18_to_district_19 | 10 | 1 | 0.1 |
| soundex | 0 | legacy_troublesome_comment | 9 | 7 | 0.777777777777778 |
| soundex | 0 | tracker_1991_to_2001 | 160 | 14 | 0.0875 |
| qgram | 0 | join_map_district_01_to_district_05 | 21 | 0 | 0 |
| qgram | 0 | join_map_district_05_to_district_06 | 6 | 0 | 0 |
| qgram | 0 | join_map_district_06_to_district_07 | 8 | 0 | 0 |
| qgram | 0 | join_map_district_07_to_district_08 | 8 | 0 | 0 |
| qgram | 0 | join_map_district_08_to_district_11 | 29 | 0 | 0 |
| qgram | 0 | join_map_district_11_to_district_17 | 79 | 0 | 0 |
| qgram | 0 | join_map_district_17_to_district_18 | 3 | 0 | 0 |
| qgram | 0 | join_map_district_18_to_district_19 | 10 | 0 | 0 |
| qgram | 0 | legacy_troublesome_comment | 9 | 3 | 0.333333333333333 |
| qgram | 0 | tracker_1991_to_2001 | 160 | 0 | 0 |
| qgram | 1 | join_map_district_01_to_district_05 | 21 | 0 | 0 |
| qgram | 1 | join_map_district_05_to_district_06 | 6 | 0 | 0 |
| qgram | 1 | join_map_district_06_to_district_07 | 8 | 0 | 0 |
| qgram | 1 | join_map_district_07_to_district_08 | 8 | 0 | 0 |
| qgram | 1 | join_map_district_08_to_district_11 | 29 | 1 | 0.0344827586206897 |
| qgram | 1 | join_map_district_11_to_district_17 | 79 | 0 | 0 |
| qgram | 1 | join_map_district_17_to_district_18 | 3 | 0 | 0 |
| qgram | 1 | join_map_district_18_to_district_19 | 10 | 0 | 0 |
| qgram | 1 | legacy_troublesome_comment | 9 | 4 | 0.444444444444444 |
| qgram | 1 | tracker_1991_to_2001 | 160 | 6 | 0.0375 |
| qgram | 2 | join_map_district_01_to_district_05 | 21 | 0 | 0 |
| qgram | 2 | join_map_district_05_to_district_06 | 6 | 0 | 0 |
| qgram | 2 | join_map_district_06_to_district_07 | 8 | 0 | 0 |
| qgram | 2 | join_map_district_07_to_district_08 | 8 | 0 | 0 |
| qgram | 2 | join_map_district_08_to_district_11 | 29 | 1 | 0.0344827586206897 |
| qgram | 2 | join_map_district_11_to_district_17 | 79 | 0 | 0 |
| qgram | 2 | join_map_district_17_to_district_18 | 3 | 0 | 0 |
| qgram | 2 | join_map_district_18_to_district_19 | 10 | 1 | 0.1 |
| qgram | 2 | legacy_troublesome_comment | 9 | 6 | 0.666666666666667 |
| qgram | 2 | tracker_1991_to_2001 | 160 | 10 | 0.0625 |
| Table truncated in rendered note; full CSV has 150 rows. |  |  |  |  |  |

Threshold sensitivity by method and pair source
