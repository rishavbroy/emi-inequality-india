# District Tracker Source Diagnostics


## Legacy comments

### Legacy Chunk 6: district tracker construction and QA notes

The districts listed in 2007-08 vs. 2017-18 vary for two reasons:
legitimate political changes (districts may have been created,
destroyed, or renamed) and typos. I address both here.

Import data on district changes

``` r
Though its extensive time range is informative, there seem to be some typos and factual errors here (e.g., the 2019 merger of Dadra and Nagar Haveli with Daman and Diu is not reflected here, and "Dadra" is twice misspelled as "Dadara")
But not as many mistakes as my original district_tracker, turns out! :D
```

Select only XYYYY-State and XYYYY-District for those years Rename to
state_yy / district_yy

``` r
district_creations <- read_excel_short(file.path(data_folder, "New Districts Created between 1951-2024.xlsx")) %>% select(!c.............................................................) # Some files gained an empty column upon importing
```

``` r
district_renamings <- read_excel_short(file.path(data_folder, "Name Changes_Districts_Indian States_1951-2021.xlsx")) %>% select(!c.............................................................)
```

``` r
district_splits <- read_excel_short(file.path(data_folder, "District Splits and Carve outs-decadewise  1951-2024.xls")) %>% select(!c.............................................................)
```

------------------------------------------------------------------------

Justify the district_tracker df method of matching

We want to make it so that for each 2017-18 district, either it can be
directly matched to a single 2007-08 district, or a single 2007-08
district can be matched to it. To do so, all district changes must have
been either clean partitions or name changes of old districts–new
districts being carved out of multiple old districts and/or new borders
being drawn between old districts make it far harder to equate or claim
equality between units of analysis across time.

Up to 2001, @kumarCreatingLongPanels2016 are able to measure the
*proportion* of each district’s population that was allocated into a new
district as a result of district changes, specifically those which are
not clean partitions: so carve-outs (of new districts from old
districts), mergers, renamings, and border shifts. Carve-outs from
multiple districts at once and border shifts are the most troublesome
for us, as they prevent us from directly matching new districts to a
single old district. There does not seem to be any data on how such
district carve-outs or border shifts have allocated populations between
districts since 2001.

So we justify directly matching districts by showing that most district
changes from 1991 to 2001 were equivalent to renamings, partitions, or
mergers.

``` r
title = "Population Reallocations From District Shifts and Carve-Outs, 1991-2001",
subtitle = "Number of 2001 Districts Which Absorbed a Percentage of a 1991 District's Population"
```

From 1991 to 2001, it’s evident that district changes which did not
involve clean partitions were rarely associated with changes in district
populations. In other words, most of these changes were approximately
equal to name changes. [^1]

To get the exact proportion of “other” district changes which are not
equivalent to renamings: Check pctortion of district changes which
allocated less than 100/c or 100 - 100/c of the old district’s
population to the new district

``` r
(Set c = 40) 86% of allocations from a parent district to a child district upon an "other" type of district change involved a transfer of more than 97.5% or less than 2.5% of the population. Meaning 86% of the time, the new district almost precisely matched the old district--the change was effectively a name change. 
```

``` r
mean(carveshift_count$carveshift)
= 0.9335106 if c=40, so 93.4% of district changes that were not clean partitions were equivalent to a name change (allocating more than 97.5% or less than 2.5% of the old district's population).
```

We use this as justification to assume away other types of district
changes (namely district carve-outs and border shifts), allowing us to
match each new district to a single old district or vice versa.

This is equivalent to what @jaacksIndiaDistrictChanges2020 did in their
tracking of district changes from 2001 to 2020, with the children of
district carve-outs only matched with the one parent who contributed the
most to their land area; likewise for districts following border shifts.
We thus use their data below.

------------------------------------------------------------------------

### Construct district_tracker df

Import data, do preliminary data cleaning

Set column names as interleaved named vector

Order columns by year

Interleave columns

Rename columns

### Diagnose and correct mistakes

### …in states/UTs

``` r
View state/UT changes recorded in original data
```

Two changes, both from 2019, both first reflected in the 2019 data: the
union territory (UT) of Ladakh split from Jammu and Kashmir, and the UT
Dadra and Nagar Haveli and Daman and Diu formed from the merger of Dadra
and Nagar Haveli with Daman and Diu This means four changes were not
recorded in the database: renaming Pondicherry (both the district and
its UT) to Puducherry in 2006, renaming Uttaranchal to Uttarakhand in
2007, renaming Orissa to Odisha in 2011, and cleaving Telangana out of
Andhra Pradesh in 2014.

Add remaining state/UT changes For state_xx columns with suffix \< “06”
For state_xx columns with suffix \< “07” For state_xx columns with
suffix \< “11” For state_xx columns with suffix \< “14” NOTE: The 07-08
NSS data uses Pondicherry instead of the 2006 name change of
Puducherry!! They also (more understandably) use Uttaranchal instead of
the 2007 name change Uttarakhand. Fix: To join data from 07-08 NSS, must
search for matches beyond the sample years.

### …in districts

``` r
View number of districts which changed names during a dataset's sampling period
```

16 districts :( Fix: To join data from multi-year surveys, will need to
check for matches across all sampling years at least

``` r
View number of districts with shared names in each year
```

1.  Filter rows For each suffix, we flag rows where the district (for
    that suffix) appears with more than one distinct state. Then we keep
    rows where at least one of the suffix comparisons is TRUE.

``` r
Create a list of logical vectors (one per suffix)
```

Build the column names for this suffix

Group the data by district for this suffix and count distinct states
Districts that occur with more than one unique state Return a logical
vector: TRUE if the district value in the row is in duplicate_districts

``` r
Combine the logical vectors by taking the rowwise OR (i.e., TRUE if condition holds for any suffix)
Filter the original data
```

2.  Summarize by district for each suffix For each suffix, group the
    filtered data by the district column and count:

<!-- -->

1.  the number of rows (n_rows) sharing that district value, and
2.  the number of distinct state values (n_states) in that group. Then
    keep only groups that have more than one unique state. Combine
    summaries from all suffixes into one data frame Make it easy to get
    summary stats

``` r
Between min(n_same_name_districts$n) = 6 and max(n_same_name_districts$n) = 10 districts with shared names in each year of interest
```

**Deviation note.** The prose above is rendered from the legacy
comments. The current tables below replace manually written counts with
target outputs, and separately show legacy expected-count benchmarks
when the active cleaned tracker no longer contains the same raw changes.

## Current targets-backed results

| source_file_id                 | n_rows | n_columns |
|:-------------------------------|-------:|----------:|
| district_changes_alluvial      |    808 |        16 |
| district_changes_carveouts     |    383 |         5 |
| district_changes_tracker       |    735 |        60 |
| district_changes_new_districts |    487 |         6 |
| district_changes_name_changes  |    134 |         6 |
| district_changes_splits        |    929 |         6 |

Raw district-change source coverage

| diagnostic | legacy_comment_expected | current_detected_rows | interpretation |
|:---|:---|---:|:---|
| recorded_state_ut_changes | Legacy Chunk 6 comments identify two recorded state/UT change events in the tracker sources. | 85 | Compare row-level current detections with tracker_state_change_events.csv because row counts can exceed event counts. |
| unrecorded_state_ut_changes | Legacy Chunk 6 comments identify four unrecorded state/UT naming/split changes requiring manual attention. | 4 | These are preserved as documented legacy correction notes, not inferred from active tracker rows. |
| in_period_district_name_changes | Legacy Chunk 6 comments record 16 districts changing names within the sampling periods. | 17 | A current count different from 16 reflects active tracker/correction changes and should be reviewed before describing it as an improvement. |
| same_name_districts_across_states | Legacy Chunk 6 comments record between 6 and 10 same-name districts in each year of interest. | 42 | A current count of zero means the active cleaned tracker no longer exposes this raw ambiguity; it should be reported as resolved-by-current-cleaning, not as proof that the legacy QA was unnecessary. |

Legacy comment benchmarks versus current detections

| state_transition | n_rows |
|:---|---:|
| Andhra Pradesh -\> Telangana | 33 |
| Orissa -\> Odisha | 30 |
| Uttaranchal -\> Uttarakhand | 13 |
| Pondicherry -\> Puducherry | 4 |
| Daman and Diu -\> Dadra and Nagar Haveli and Daman and Diu | 2 |
| Jammu and Kashmir -\> Ladakh | 2 |
| Dadra and Nagar Haveli -\> Dadra and Nagar Haveli and Daman and Diu | 1 |

Current recorded state/UT change events

| legacy_event | first_reflected | legacy_chunk | current_detection_status |
|:---|:---|:---|:---|
| Ladakh split from Jammu and Kashmir | 2019 data | Chunk 6 district tracker source QA | must be detected from raw/pre-correction tracker columns or carried as this reference row |
| Dadra and Nagar Haveli and Daman and Diu merger | 2019 data | Chunk 6 district tracker source QA | must be detected from raw/pre-correction tracker columns or carried as this reference row |

Recorded state/UT changes noted in the legacy comments

| change | legacy_note |
|:---|:---|
| Pondicherry/Puducherry district and UT rename | Legacy comment: 2007-08 NSS still uses Pondicherry despite 2006 Puducherry rename. |
| Uttaranchal/Uttarakhand state rename | Legacy comment: 2007-08 NSS uses Uttaranchal rather than Uttarakhand. |
| Orissa/Odisha state rename | Legacy comment: apply pre-2011 Orissa naming when matching earlier samples. |
| Telangana split from Andhra Pradesh | Legacy comment: apply Andhra Pradesh name before Telangana split when matching pre-2014 data. |

Unrecorded state/UT changes requiring manual attention

| tracker_row | period | state_start | state_end | district_start | district_end |
|---:|:---|:---|:---|:---|:---|
| 240 | 05_to_06 | Jammu and Kashmir | Jammu and Kashmir | Doda | Ramban |
| 242 | 05_to_06 | Jammu and Kashmir | Jammu and Kashmir | Jammu | Samba |
| 485 | 05_to_06 | Pondicherry | Puducherry | Pondicherry | Puducherry |
| 488 | 05_to_06 | Punjab | Punjab | Sangrur | Barnala |
| 506 | 05_to_06 | Punjab | Punjab | Rupnagar | S.A.S. Nagar |
| 508 | 05_to_06 | Punjab | Punjab | Amritsar | Tarn Taran |
| 11 | 07_to_08 | Andhra Pradesh | Andhra Pradesh | Nellore | S.P.S. Nellore |
| 204 | 07_to_08 | Haryana | Haryana | Gurgaon | Palwal |
| 260 | 07_to_08 | Jharkhand | Jharkhand | Ranchi | Khunti |
| 266 | 07_to_08 | Jharkhand | Jharkhand | Hazaribagh | Ramgarh |
| 279 | 07_to_08 | Karnataka | Karnataka | Kolar | Chikkaballapura |
| 502 | 07_to_08 | Punjab | Punjab | Nawanshahr | Shahid Bhagat Singh Nagar |
| 534 | 07_to_08 | Rajasthan | Rajasthan | Chittorgarh | Pratapgarh |
| 668 | 07_to_08 | Uttar Pradesh | Uttar Pradesh | Etah | Kasganj |
| 349 | 17_to_18 | Madhya Pradesh | Madhya Pradesh | Tikamgarh | Niwari |
| 626 | 17_to_18 | Uttar Pradesh | Uttar Pradesh | Allahabad | Prayagraj |
| 649 | 17_to_18 | Uttar Pradesh | Uttar Pradesh | Faizabad | Ayodhya |

Current district-name changes inside sampling periods

| diagnostic | legacy_expected_rows | legacy_chunk | legacy_note | current_detection_status |
|:---|---:|:---|:---|:---|
| in_period_district_name_changes | 16 | Chunk 6 district tracker source QA | Legacy comments counted rows where district_05 != district_06, district_07 != district_08, district_17 != district_18, or district_19 != district_20 before downstream corrections. | rendered analysis should compare this benchmark with current tracker_inperiod_district_changes.csv |

Legacy in-period district-name-change benchmark

| year_suffix | year | district_name | district_key | n_districts | n_states | states |
|---:|---:|:---|:---|---:|---:|:---|
| 1 | 2001 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 1 | 2001 | Bilaspur | bilaspur | 3 | 2 | Chhattisgarh; Himachal Pradesh |
| 1 | 2001 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 5 | 2005 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 5 | 2005 | Bilaspur | bilaspur | 3 | 2 | Chhattisgarh; Himachal Pradesh |
| 5 | 2005 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 6 | 2006 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 6 | 2006 | Bilaspur | bilaspur | 3 | 2 | Chhattisgarh; Himachal Pradesh |
| 6 | 2006 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 7 | 2007 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 7 | 2007 | Bijapur | bijapur | 2 | 2 | Chhattisgarh; Karnataka |
| 7 | 2007 | Bilaspur | bilaspur | 3 | 2 | Chhattisgarh; Himachal Pradesh |
| 7 | 2007 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 8 | 2008 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 8 | 2008 | Bijapur | bijapur | 2 | 2 | Chhattisgarh; Karnataka |
| 8 | 2008 | Bilaspur | bilaspur | 3 | 2 | Chhattisgarh; Himachal Pradesh |
| 8 | 2008 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 8 | 2008 | Pratapgarh | pratapgarh | 2 | 2 | Rajasthan; Uttar Pradesh |
| 11 | 2011 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 11 | 2011 | Bilaspur | bilaspur | 3 | 2 | Chhattisgarh; Himachal Pradesh |
| 11 | 2011 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 11 | 2011 | Pratapgarh | pratapgarh | 2 | 2 | Rajasthan; Uttar Pradesh |
| 17 | 2017 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 17 | 2017 | Balrampur | balrampur | 2 | 2 | Chhattisgarh; Uttar Pradesh |
| 17 | 2017 | Bilaspur | bilaspur | 2 | 2 | Chhattisgarh; Himachal Pradesh |
| 17 | 2017 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 17 | 2017 | Pratapgarh | pratapgarh | 2 | 2 | Rajasthan; Uttar Pradesh |
| 18 | 2018 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 18 | 2018 | Balrampur | balrampur | 2 | 2 | Chhattisgarh; Uttar Pradesh |
| 18 | 2018 | Bilaspur | bilaspur | 2 | 2 | Chhattisgarh; Himachal Pradesh |
| 18 | 2018 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 18 | 2018 | Pratapgarh | pratapgarh | 2 | 2 | Rajasthan; Uttar Pradesh |
| 19 | 2019 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 19 | 2019 | Balrampur | balrampur | 2 | 2 | Chhattisgarh; Uttar Pradesh |
| 19 | 2019 | Bilaspur | bilaspur | 2 | 2 | Chhattisgarh; Himachal Pradesh |
| 19 | 2019 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 19 | 2019 | Pratapgarh | pratapgarh | 2 | 2 | Rajasthan; Uttar Pradesh |
| 20 | 2020 | Aurangabad | aurangabad | 2 | 2 | Bihar; Maharashtra |
| 20 | 2020 | Balrampur | balrampur | 2 | 2 | Chhattisgarh; Uttar Pradesh |
| 20 | 2020 | Bilaspur | bilaspur | 2 | 2 | Chhattisgarh; Himachal Pradesh |
| 20 | 2020 | Hamirpur | hamirpur | 2 | 2 | Himachal Pradesh; Uttar Pradesh |
| 20 | 2020 | Pratapgarh | pratapgarh | 2 | 2 | Rajasthan; Uttar Pradesh |

Current same-name districts appearing in multiple states

| diagnostic | legacy_expected_min_districts | legacy_expected_max_districts | legacy_chunk | legacy_note | current_detection_status |
|:---|---:|---:|:---|:---|:---|
| same_name_districts_across_states | 6 | 10 | Chunk 6 district tracker source QA | Legacy comments counted between min(n_same_name_districts$n) = 6 and max(n_same_name_districts$n) = 10 districts with shared names in each year of interest. | rendered analysis should compare this benchmark with tracker_same_name_districts.csv; a zero current count means the active tracker no longer exposes the raw same-name ambiguity, not that the legacy QA was irrelevant. |

Legacy same-name-district benchmark

[^1]: While most of these changes were actually *precisely* name
    changes, some were not. See @kumarCreatingLongPanels2016, for
    example, to see that certain changes only involved transfers of
    uninhabited land.
