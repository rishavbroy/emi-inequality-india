# Fuzzy Matching Diagnostics


## Legacy comments

### Legacy Chunk 16: fuzzy-join diagnostics

Number of rows from fuzzy full joining district_tracker (734 rows) and
mother_tongues_01 (593 rows) with different metrics: “osa”: 859 “lv”:
859 “dl”: 859 “hamming”: 872 “lcs”: 825 “qgram”: 829 “cosine”: 435,262
“jaccard”: 435,262 “jw”: 435,262

``` r
See https://cran.r-project.org/web/packages/stringdist/stringdist.pdf#page=23 for more
```

Testing different methods of fuzzy joining

``` r
With ("lcs","osa"), (3, 3):
colSums(is.na(joined_df))
75 NAs in 01 columns, 124 in 07-08, 105 in 17-18
sum(apply(joined_df, 1, anyNA))
189/734 rows have an NA
```

``` r
With ("jw", "dl", "osa", "lcs"), (0.10, 2, 3, 5)
colSums(is.na(joined_df))
70 in 01, 119 in 07-08, 98 in 17-18
sum(apply(joined_df, 1, anyNA))
180/734
```

``` r
methods <- c("soundex", "jw", "dl", "osa", "lcs")
thresholds <- c(0, 0.15, 2, 1, 5)
colSums(is.na(joined_df))
47 in 01, 105 in 07-08, 83 in 17-18
sum(apply(joined_df, 1, anyNA))
158/734
With lcs, the higher the better. Would need normalized version like 1 - lcs(a,b)/min(nchar(a),nchar(b))
```

``` r
See ?roxygen2::`tags-rd` for info on how to do better comments before functions
```

Ensure chr type

Compute distances for each method

Keep a consistent ordering

Some sample pairs

### Helper vectors

``` r
soundex=0 --> Phonetic variants in anglicization allowed
qgram = 0 --> Rearrangements of words allowed
jw<=0.15 --> Respellings + vowel swaps with 0.85 similarity allowed
dl<=2 --> <=2 insertions + deletions + substitutions + transpositions allowed
osa<=1 --> 1 typo allowed
```

``` r
colSums(is.na(joined_df))
53 in 01, 112 in 07-08, 89 in 17-18
sum(apply(joined_df, 1, anyNA))
166/734
```

Run it

``` r
evaluate_distances(pairs, methods, thresholds) %>% View()
```

``` r
stringdist("East Godavari", "Godavari East", method = "qgram")
0
stringdist("East Godavari", "Godavari East", method = "jaccard")
0
```

**Deviation note.** The prose above is rendered from the legacy
comments. The current tables below keep the final methods/thresholds
visible while replacing hand-inspected join-status counts with target
outputs.

## Current targets-backed results

| n_tracker_rows | n_join_rows | n_unmatched_rows | n_candidate_pairs | n_active_candidate_pairs |
|---:|---:|---:|---:|---:|
| 3476 | 3175 | 3175 | 216402 | 216393 |

Fuzzy matching diagnostic summary

| method  | threshold |
|:--------|----------:|
| soundex |      0.00 |
| qgram   |      0.00 |
| jw      |      0.15 |
| dl      |      2.00 |
| osa     |      1.00 |

Legacy final string-distance methods and thresholds

| diagnostic | legacy_result | legacy_chunk |
|:---|:---|:---|
| full_join_method_row_counts | osa/lv/dl 859 rows; hamming 872; lcs 825; qgram 829; cosine/jaccard/jw 435262 | Chunk 16 Match districts: Test joining methods |
| lcs_osa_3_3 | 189/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| jw_dl_osa_lcs | 180/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| final_method_choice | soundex=0, qgram=0, jw\<=0.15, dl\<=2, osa\<=1; 166/734 rows had any NA | Chunk 16 Match districts: Test joining methods |

Legacy tuning comments retained as benchmarks

| pair_source | n_pairs | coverage_note |
|:---|---:|:---|
| active_source_key_inventory_2001 | 19071 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2005 | 19610 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2006 | 19714 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2007 | 20069 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2008 | 20276 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2011 | 21761 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2017 | 23741 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2018 | 23793 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2019 | 24022 | active source/tracker candidate pair emitted by the current pipeline |
| active_source_key_inventory_2020 | 24022 | active source/tracker candidate pair emitted by the current pipeline |
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

| str1 | str2 | pair_source | method | distance | threshold | match |
|:---|:---|:---|:---|---:|---:|:---|
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | dl | 22.000 | 2.00 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | jw | 0.482 | 0.15 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | osa | 22.000 | 1.00 | FALSE |
| 24-Parganas ( North ) | North Twenty Four Parganas | legacy_troublesome_comment | qgram | 15.000 | 0.00 | FALSE |
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
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | dl | 3.000 | 2.00 | FALSE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | jw | 0.117 | 0.15 | TRUE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | osa | 3.000 | 1.00 | FALSE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | qgram | 4.000 | 0.00 | FALSE |
| Jammu & Kashmir | Jammu and Kashmir | legacy_troublesome_comment | soundex | 1.000 | 0.00 | FALSE |
| Mumbai | Mumbai | legacy_troublesome_comment | dl | 0.000 | 2.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | jw | 0.000 | 0.15 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | osa | 0.000 | 1.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | qgram | 0.000 | 0.00 | TRUE |
| Mumbai | Mumbai | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | dl | 2.000 | 2.00 | TRUE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | jw | 0.039 | 0.15 | TRUE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | osa | 2.000 | 1.00 | FALSE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | qgram | 3.000 | 0.00 | FALSE |
| North Twenty Four Pargan\* | North Twenty Four Parganas | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | dl | 2.000 | 2.00 | TRUE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | jw | 0.039 | 0.15 | TRUE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | osa | 2.000 | 1.00 | FALSE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | qgram | 3.000 | 0.00 | FALSE |
| Sahibzada Ajit Singh Nag\* | Sahibzada Ajit Singh Nagar | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | dl | 1.000 | 2.00 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | jw | 0.056 | 0.15 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | osa | 1.000 | 1.00 | TRUE |
| Sikim | Sikkim | legacy_troublesome_comment | qgram | 1.000 | 0.00 | FALSE |
| Sikim | Sikkim | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | dl | 3.000 | 2.00 | FALSE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | jw | 0.050 | 0.15 | TRUE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | osa | 3.000 | 1.00 | FALSE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | qgram | 4.000 | 0.00 | FALSE |
| Sri Potti Sriramulu Nell\* | Sri Potti Sriramulu Nellore | legacy_troublesome_comment | soundex | 0.000 | 0.00 | TRUE |

Troublesome legacy name-pair checks

| match_status         | Freq |
|:---------------------|-----:|
| source_key_unmatched | 3175 |

Current join status counts
