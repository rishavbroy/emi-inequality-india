# Fuzzy Matching Benchmark


## Legacy comments

### Legacy Chunk 16: string-distance tuning notes

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

**Deviation note.** The legacy prose is rendered from the old comments.
The tables below replace the old hand-counted diagnostics with current
target outputs and keep the original final thresholds visible.

## Current targets-backed results

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

Benchmark candidate-pair coverage

| diagnostic | legacy_result | legacy_chunk |
|:---|:---|:---|
| full_join_method_row_counts | osa/lv/dl 859 rows; hamming 872; lcs 825; qgram 829; cosine/jaccard/jw 435262 | Chunk 16 Match districts: Test joining methods |
| lcs_osa_3_3 | 189/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| jw_dl_osa_lcs | 180/734 rows had any NA | Chunk 16 Match districts: Test joining methods |
| final_method_choice | soundex=0, qgram=0, jw\<=0.15, dl\<=2, osa\<=1; 166/734 rows had any NA | Chunk 16 Match districts: Test joining methods |

Legacy fuzzy-tuning reference results

| method | threshold | pair_source | n_pairs | n_matches | pct_matches |
|:---|---:|:---|---:|---:|---:|
| soundex | 0.00 | active_source_key_inventory_2001 | 19071 | 549 | 0.029 |
| soundex | 0.00 | active_source_key_inventory_2005 | 19610 | 567 | 0.029 |
| soundex | 0.00 | active_source_key_inventory_2006 | 19714 | 575 | 0.029 |
| soundex | 0.00 | active_source_key_inventory_2007 | 20069 | 595 | 0.030 |
| soundex | 0.00 | active_source_key_inventory_2008 | 20276 | 602 | 0.030 |
| soundex | 0.00 | active_source_key_inventory_2011 | 21761 | 647 | 0.030 |
| soundex | 0.00 | active_source_key_inventory_2017 | 23741 | 739 | 0.031 |
| soundex | 0.00 | active_source_key_inventory_2018 | 23793 | 740 | 0.031 |
| soundex | 0.00 | active_source_key_inventory_2019 | 24022 | 749 | 0.031 |
| soundex | 0.00 | active_source_key_inventory_2020 | 24022 | 749 | 0.031 |
| soundex | 0.00 | legacy_troublesome_comment | 9 | 6 | 0.667 |
| soundex | 0.00 | tracker_2001_to_2005 | 21 | 1 | 0.048 |
| soundex | 0.00 | tracker_2001_to_2007 | 35 | 1 | 0.029 |
| soundex | 0.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| soundex | 0.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| soundex | 0.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| soundex | 0.00 | tracker_2007_to_2017 | 102 | 4 | 0.039 |
| soundex | 0.00 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| soundex | 0.00 | tracker_2011_to_2017 | 79 | 2 | 0.025 |
| soundex | 0.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| soundex | 0.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| soundex | 0.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| qgram | 0.00 | active_source_key_inventory_2001 | 19071 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2005 | 19610 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2006 | 19714 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2007 | 20069 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2008 | 20276 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2011 | 21761 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2017 | 23741 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2018 | 23793 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2019 | 24022 | 0 | 0.000 |
| qgram | 0.00 | active_source_key_inventory_2020 | 24022 | 0 | 0.000 |
| qgram | 0.00 | legacy_troublesome_comment | 9 | 2 | 0.222 |
| qgram | 0.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| qgram | 0.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| qgram | 0.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| qgram | 0.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| qgram | 0.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| qgram | 0.00 | tracker_2007_to_2017 | 102 | 0 | 0.000 |
| qgram | 0.00 | tracker_2008_to_2011 | 29 | 0 | 0.000 |
| qgram | 0.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| qgram | 0.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| qgram | 0.00 | tracker_2017_to_2020 | 13 | 0 | 0.000 |
| qgram | 0.00 | tracker_2018_to_2019 | 10 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2001 | 19071 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2005 | 19610 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2006 | 19714 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2007 | 20069 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2008 | 20276 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2011 | 21761 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2017 | 23741 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2018 | 23793 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2019 | 24022 | 0 | 0.000 |
| qgram | 1.00 | active_source_key_inventory_2020 | 24022 | 0 | 0.000 |
| qgram | 1.00 | legacy_troublesome_comment | 9 | 3 | 0.333 |
| qgram | 1.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| qgram | 1.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| qgram | 1.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| qgram | 1.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| qgram | 1.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| qgram | 1.00 | tracker_2007_to_2017 | 102 | 1 | 0.010 |
| qgram | 1.00 | tracker_2008_to_2011 | 29 | 1 | 0.034 |
| qgram | 1.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| qgram | 1.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| qgram | 1.00 | tracker_2017_to_2020 | 13 | 0 | 0.000 |
| qgram | 1.00 | tracker_2018_to_2019 | 10 | 0 | 0.000 |
| qgram | 2.00 | active_source_key_inventory_2001 | 19071 | 435 | 0.023 |
| qgram | 2.00 | active_source_key_inventory_2005 | 19610 | 449 | 0.023 |
| qgram | 2.00 | active_source_key_inventory_2006 | 19714 | 456 | 0.023 |
| qgram | 2.00 | active_source_key_inventory_2007 | 20069 | 470 | 0.023 |
| qgram | 2.00 | active_source_key_inventory_2008 | 20276 | 475 | 0.023 |
| qgram | 2.00 | active_source_key_inventory_2011 | 21761 | 505 | 0.023 |
| qgram | 2.00 | active_source_key_inventory_2017 | 23741 | 567 | 0.024 |
| qgram | 2.00 | active_source_key_inventory_2018 | 23793 | 568 | 0.024 |
| qgram | 2.00 | active_source_key_inventory_2019 | 24022 | 576 | 0.024 |
| qgram | 2.00 | active_source_key_inventory_2020 | 24022 | 576 | 0.024 |
| qgram | 2.00 | legacy_troublesome_comment | 9 | 3 | 0.333 |
| qgram | 2.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| qgram | 2.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| qgram | 2.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| qgram | 2.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| qgram | 2.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| qgram | 2.00 | tracker_2007_to_2017 | 102 | 1 | 0.010 |
| qgram | 2.00 | tracker_2008_to_2011 | 29 | 1 | 0.034 |
| qgram | 2.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| qgram | 2.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| qgram | 2.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| qgram | 2.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| jw | 0.10 | active_source_key_inventory_2001 | 19071 | 254 | 0.013 |
| jw | 0.10 | active_source_key_inventory_2005 | 19610 | 266 | 0.014 |
| jw | 0.10 | active_source_key_inventory_2006 | 19714 | 269 | 0.014 |
| jw | 0.10 | active_source_key_inventory_2007 | 20069 | 278 | 0.014 |
| jw | 0.10 | active_source_key_inventory_2008 | 20276 | 281 | 0.014 |
| jw | 0.10 | active_source_key_inventory_2011 | 21761 | 307 | 0.014 |
| jw | 0.10 | active_source_key_inventory_2017 | 23741 | 359 | 0.015 |
| jw | 0.10 | active_source_key_inventory_2018 | 23793 | 359 | 0.015 |
| jw | 0.10 | active_source_key_inventory_2019 | 24022 | 364 | 0.015 |
| jw | 0.10 | active_source_key_inventory_2020 | 24022 | 364 | 0.015 |
| jw | 0.10 | legacy_troublesome_comment | 9 | 5 | 0.556 |
| jw | 0.10 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| jw | 0.10 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| jw | 0.10 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| jw | 0.10 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| jw | 0.10 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| jw | 0.10 | tracker_2007_to_2017 | 102 | 1 | 0.010 |
| jw | 0.10 | tracker_2008_to_2011 | 29 | 1 | 0.034 |
| jw | 0.10 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| jw | 0.10 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| jw | 0.10 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| jw | 0.10 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| jw | 0.15 | active_source_key_inventory_2001 | 19071 | 462 | 0.024 |
| jw | 0.15 | active_source_key_inventory_2005 | 19610 | 481 | 0.025 |
| jw | 0.15 | active_source_key_inventory_2006 | 19714 | 488 | 0.025 |
| jw | 0.15 | active_source_key_inventory_2007 | 20069 | 505 | 0.025 |
| jw | 0.15 | active_source_key_inventory_2008 | 20276 | 511 | 0.025 |
| jw | 0.15 | active_source_key_inventory_2011 | 21761 | 543 | 0.025 |
| jw | 0.15 | active_source_key_inventory_2017 | 23741 | 620 | 0.026 |
| jw | 0.15 | active_source_key_inventory_2018 | 23793 | 621 | 0.026 |
| jw | 0.15 | active_source_key_inventory_2019 | 24022 | 628 | 0.026 |
| jw | 0.15 | active_source_key_inventory_2020 | 24022 | 628 | 0.026 |
| jw | 0.15 | legacy_troublesome_comment | 9 | 6 | 0.667 |
| jw | 0.15 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| jw | 0.15 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| jw | 0.15 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| jw | 0.15 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| jw | 0.15 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| jw | 0.15 | tracker_2007_to_2017 | 102 | 5 | 0.049 |
| jw | 0.15 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| jw | 0.15 | tracker_2011_to_2017 | 79 | 3 | 0.038 |
| jw | 0.15 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| jw | 0.15 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| jw | 0.15 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| jw | 0.20 | active_source_key_inventory_2001 | 19071 | 539 | 0.028 |
| jw | 0.20 | active_source_key_inventory_2005 | 19610 | 558 | 0.028 |
| jw | 0.20 | active_source_key_inventory_2006 | 19714 | 567 | 0.029 |
| jw | 0.20 | active_source_key_inventory_2007 | 20069 | 588 | 0.029 |
| jw | 0.20 | active_source_key_inventory_2008 | 20276 | 594 | 0.029 |
| jw | 0.20 | active_source_key_inventory_2011 | 21761 | 631 | 0.029 |
| jw | 0.20 | active_source_key_inventory_2017 | 23741 | 721 | 0.030 |
| jw | 0.20 | active_source_key_inventory_2018 | 23793 | 720 | 0.030 |
| jw | 0.20 | active_source_key_inventory_2019 | 24022 | 729 | 0.030 |
| jw | 0.20 | active_source_key_inventory_2020 | 24022 | 729 | 0.030 |
| jw | 0.20 | legacy_troublesome_comment | 9 | 6 | 0.667 |
| jw | 0.20 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| jw | 0.20 | tracker_2001_to_2007 | 35 | 1 | 0.029 |
| jw | 0.20 | tracker_2005_to_2006 | 6 | 1 | 0.167 |
| jw | 0.20 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| jw | 0.20 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| jw | 0.20 | tracker_2007_to_2017 | 102 | 10 | 0.098 |
| jw | 0.20 | tracker_2008_to_2011 | 29 | 5 | 0.172 |
| jw | 0.20 | tracker_2011_to_2017 | 79 | 5 | 0.063 |
| jw | 0.20 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| jw | 0.20 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| jw | 0.20 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| dl | 1.00 | active_source_key_inventory_2001 | 19071 | 428 | 0.022 |
| dl | 1.00 | active_source_key_inventory_2005 | 19610 | 442 | 0.023 |
| dl | 1.00 | active_source_key_inventory_2006 | 19714 | 449 | 0.023 |
| dl | 1.00 | active_source_key_inventory_2007 | 20069 | 463 | 0.023 |
| dl | 1.00 | active_source_key_inventory_2008 | 20276 | 468 | 0.023 |
| dl | 1.00 | active_source_key_inventory_2011 | 21761 | 498 | 0.023 |
| dl | 1.00 | active_source_key_inventory_2017 | 23741 | 559 | 0.024 |
| dl | 1.00 | active_source_key_inventory_2018 | 23793 | 560 | 0.024 |
| dl | 1.00 | active_source_key_inventory_2019 | 24022 | 568 | 0.024 |
| dl | 1.00 | active_source_key_inventory_2020 | 24022 | 568 | 0.024 |
| dl | 1.00 | legacy_troublesome_comment | 9 | 2 | 0.222 |
| dl | 1.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| dl | 1.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| dl | 1.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| dl | 1.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| dl | 1.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| dl | 1.00 | tracker_2007_to_2017 | 102 | 1 | 0.010 |
| dl | 1.00 | tracker_2008_to_2011 | 29 | 1 | 0.034 |
| dl | 1.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| dl | 1.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| dl | 1.00 | tracker_2017_to_2020 | 13 | 0 | 0.000 |
| dl | 1.00 | tracker_2018_to_2019 | 10 | 0 | 0.000 |
| dl | 2.00 | active_source_key_inventory_2001 | 19071 | 514 | 0.027 |
| dl | 2.00 | active_source_key_inventory_2005 | 19610 | 529 | 0.027 |
| dl | 2.00 | active_source_key_inventory_2006 | 19714 | 538 | 0.027 |
| dl | 2.00 | active_source_key_inventory_2007 | 20069 | 556 | 0.028 |
| dl | 2.00 | active_source_key_inventory_2008 | 20276 | 561 | 0.028 |
| dl | 2.00 | active_source_key_inventory_2011 | 21761 | 593 | 0.027 |
| dl | 2.00 | active_source_key_inventory_2017 | 23741 | 676 | 0.028 |
| dl | 2.00 | active_source_key_inventory_2018 | 23793 | 677 | 0.028 |
| dl | 2.00 | active_source_key_inventory_2019 | 24022 | 685 | 0.029 |
| dl | 2.00 | active_source_key_inventory_2020 | 24022 | 685 | 0.029 |
| dl | 2.00 | legacy_troublesome_comment | 9 | 4 | 0.444 |
| dl | 2.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| dl | 2.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| dl | 2.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| dl | 2.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| dl | 2.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| dl | 2.00 | tracker_2007_to_2017 | 102 | 2 | 0.020 |
| dl | 2.00 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| dl | 2.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| dl | 2.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| dl | 2.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| dl | 2.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| dl | 3.00 | active_source_key_inventory_2001 | 19071 | 703 | 0.037 |
| dl | 3.00 | active_source_key_inventory_2005 | 19610 | 724 | 0.037 |
| dl | 3.00 | active_source_key_inventory_2006 | 19714 | 735 | 0.037 |
| dl | 3.00 | active_source_key_inventory_2007 | 20069 | 759 | 0.038 |
| dl | 3.00 | active_source_key_inventory_2008 | 20276 | 766 | 0.038 |
| dl | 3.00 | active_source_key_inventory_2011 | 21761 | 812 | 0.037 |
| dl | 3.00 | active_source_key_inventory_2017 | 23741 | 910 | 0.038 |
| dl | 3.00 | active_source_key_inventory_2018 | 23793 | 910 | 0.038 |
| dl | 3.00 | active_source_key_inventory_2019 | 24022 | 920 | 0.038 |
| dl | 3.00 | active_source_key_inventory_2020 | 24022 | 920 | 0.038 |
| dl | 3.00 | legacy_troublesome_comment | 9 | 6 | 0.667 |
| dl | 3.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| dl | 3.00 | tracker_2001_to_2007 | 35 | 1 | 0.029 |
| dl | 3.00 | tracker_2005_to_2006 | 6 | 1 | 0.167 |
| dl | 3.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| dl | 3.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| dl | 3.00 | tracker_2007_to_2017 | 102 | 2 | 0.020 |
| dl | 3.00 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| dl | 3.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| dl | 3.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| dl | 3.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| dl | 3.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| osa | 1.00 | active_source_key_inventory_2001 | 19071 | 428 | 0.022 |
| osa | 1.00 | active_source_key_inventory_2005 | 19610 | 442 | 0.023 |
| osa | 1.00 | active_source_key_inventory_2006 | 19714 | 449 | 0.023 |
| osa | 1.00 | active_source_key_inventory_2007 | 20069 | 463 | 0.023 |
| osa | 1.00 | active_source_key_inventory_2008 | 20276 | 468 | 0.023 |
| osa | 1.00 | active_source_key_inventory_2011 | 21761 | 498 | 0.023 |
| osa | 1.00 | active_source_key_inventory_2017 | 23741 | 559 | 0.024 |
| osa | 1.00 | active_source_key_inventory_2018 | 23793 | 560 | 0.024 |
| osa | 1.00 | active_source_key_inventory_2019 | 24022 | 568 | 0.024 |
| osa | 1.00 | active_source_key_inventory_2020 | 24022 | 568 | 0.024 |
| osa | 1.00 | legacy_troublesome_comment | 9 | 2 | 0.222 |
| osa | 1.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| osa | 1.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| osa | 1.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| osa | 1.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| osa | 1.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| osa | 1.00 | tracker_2007_to_2017 | 102 | 1 | 0.010 |
| osa | 1.00 | tracker_2008_to_2011 | 29 | 1 | 0.034 |
| osa | 1.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| osa | 1.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| osa | 1.00 | tracker_2017_to_2020 | 13 | 0 | 0.000 |
| osa | 1.00 | tracker_2018_to_2019 | 10 | 0 | 0.000 |
| osa | 2.00 | active_source_key_inventory_2001 | 19071 | 514 | 0.027 |
| osa | 2.00 | active_source_key_inventory_2005 | 19610 | 529 | 0.027 |
| osa | 2.00 | active_source_key_inventory_2006 | 19714 | 538 | 0.027 |
| osa | 2.00 | active_source_key_inventory_2007 | 20069 | 556 | 0.028 |
| osa | 2.00 | active_source_key_inventory_2008 | 20276 | 561 | 0.028 |
| osa | 2.00 | active_source_key_inventory_2011 | 21761 | 593 | 0.027 |
| osa | 2.00 | active_source_key_inventory_2017 | 23741 | 676 | 0.028 |
| osa | 2.00 | active_source_key_inventory_2018 | 23793 | 677 | 0.028 |
| osa | 2.00 | active_source_key_inventory_2019 | 24022 | 685 | 0.029 |
| osa | 2.00 | active_source_key_inventory_2020 | 24022 | 685 | 0.029 |
| osa | 2.00 | legacy_troublesome_comment | 9 | 4 | 0.444 |
| osa | 2.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| osa | 2.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| osa | 2.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| osa | 2.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| osa | 2.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| osa | 2.00 | tracker_2007_to_2017 | 102 | 2 | 0.020 |
| osa | 2.00 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| osa | 2.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| osa | 2.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| osa | 2.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| osa | 2.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| osa | 3.00 | active_source_key_inventory_2001 | 19071 | 702 | 0.037 |
| osa | 3.00 | active_source_key_inventory_2005 | 19610 | 723 | 0.037 |
| osa | 3.00 | active_source_key_inventory_2006 | 19714 | 734 | 0.037 |
| osa | 3.00 | active_source_key_inventory_2007 | 20069 | 757 | 0.038 |
| osa | 3.00 | active_source_key_inventory_2008 | 20276 | 764 | 0.038 |
| osa | 3.00 | active_source_key_inventory_2011 | 21761 | 810 | 0.037 |
| osa | 3.00 | active_source_key_inventory_2017 | 23741 | 908 | 0.038 |
| osa | 3.00 | active_source_key_inventory_2018 | 23793 | 908 | 0.038 |
| osa | 3.00 | active_source_key_inventory_2019 | 24022 | 918 | 0.038 |
| osa | 3.00 | active_source_key_inventory_2020 | 24022 | 918 | 0.038 |
| osa | 3.00 | legacy_troublesome_comment | 9 | 6 | 0.667 |
| osa | 3.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| osa | 3.00 | tracker_2001_to_2007 | 35 | 1 | 0.029 |
| osa | 3.00 | tracker_2005_to_2006 | 6 | 1 | 0.167 |
| osa | 3.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| osa | 3.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| osa | 3.00 | tracker_2007_to_2017 | 102 | 2 | 0.020 |
| osa | 3.00 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| osa | 3.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| osa | 3.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| osa | 3.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| osa | 3.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| lcs | 3.00 | active_source_key_inventory_2001 | 19071 | 456 | 0.024 |
| lcs | 3.00 | active_source_key_inventory_2005 | 19610 | 469 | 0.024 |
| lcs | 3.00 | active_source_key_inventory_2006 | 19714 | 477 | 0.024 |
| lcs | 3.00 | active_source_key_inventory_2007 | 20069 | 492 | 0.025 |
| lcs | 3.00 | active_source_key_inventory_2008 | 20276 | 497 | 0.025 |
| lcs | 3.00 | active_source_key_inventory_2011 | 21761 | 529 | 0.024 |
| lcs | 3.00 | active_source_key_inventory_2017 | 23741 | 594 | 0.025 |
| lcs | 3.00 | active_source_key_inventory_2018 | 23793 | 595 | 0.025 |
| lcs | 3.00 | active_source_key_inventory_2019 | 24022 | 603 | 0.025 |
| lcs | 3.00 | active_source_key_inventory_2020 | 24022 | 603 | 0.025 |
| lcs | 3.00 | legacy_troublesome_comment | 9 | 4 | 0.444 |
| lcs | 3.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| lcs | 3.00 | tracker_2001_to_2007 | 35 | 0 | 0.000 |
| lcs | 3.00 | tracker_2005_to_2006 | 6 | 0 | 0.000 |
| lcs | 3.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| lcs | 3.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| lcs | 3.00 | tracker_2007_to_2017 | 102 | 2 | 0.020 |
| lcs | 3.00 | tracker_2008_to_2011 | 29 | 2 | 0.069 |
| lcs | 3.00 | tracker_2011_to_2017 | 79 | 0 | 0.000 |
| lcs | 3.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| lcs | 3.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| lcs | 3.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |
| lcs | 5.00 | active_source_key_inventory_2001 | 19071 | 735 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2005 | 19610 | 753 | 0.038 |
| lcs | 5.00 | active_source_key_inventory_2006 | 19714 | 762 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2007 | 20069 | 784 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2008 | 20276 | 791 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2011 | 21761 | 829 | 0.038 |
| lcs | 5.00 | active_source_key_inventory_2017 | 23741 | 935 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2018 | 23793 | 935 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2019 | 24022 | 945 | 0.039 |
| lcs | 5.00 | active_source_key_inventory_2020 | 24022 | 945 | 0.039 |
| lcs | 5.00 | legacy_troublesome_comment | 9 | 6 | 0.667 |
| lcs | 5.00 | tracker_2001_to_2005 | 21 | 0 | 0.000 |
| lcs | 5.00 | tracker_2001_to_2007 | 35 | 1 | 0.029 |
| lcs | 5.00 | tracker_2005_to_2006 | 6 | 1 | 0.167 |
| lcs | 5.00 | tracker_2006_to_2007 | 8 | 0 | 0.000 |
| lcs | 5.00 | tracker_2007_to_2008 | 8 | 0 | 0.000 |
| lcs | 5.00 | tracker_2007_to_2017 | 102 | 8 | 0.078 |
| lcs | 5.00 | tracker_2008_to_2011 | 29 | 5 | 0.172 |
| lcs | 5.00 | tracker_2011_to_2017 | 79 | 3 | 0.038 |
| lcs | 5.00 | tracker_2017_to_2018 | 3 | 0 | 0.000 |
| lcs | 5.00 | tracker_2017_to_2020 | 13 | 1 | 0.077 |
| lcs | 5.00 | tracker_2018_to_2019 | 10 | 1 | 0.100 |

Threshold sensitivity by method and pair source
