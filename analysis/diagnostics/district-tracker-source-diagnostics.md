# District Tracker Source Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy diagnostic intent

The legacy tracker-source diagnostics compared district names across
years, inspected state and union-territory changes, noted unrecorded
state changes requiring manual attention, and checked same-name
districts that appeared across states.

``` r
tracker_counts <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_source_counts.csv")
tracker_state_changes <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_state_changes.csv")
tracker_state_events <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_state_change_events.csv")
tracker_expected_state <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_legacy_expected_state_changes.csv")
tracker_unrecorded <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_unrecorded_state_changes.csv")
tracker_inperiod <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_inperiod_district_changes.csv")
tracker_expected_inperiod <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_legacy_expected_inperiod_district_changes.csv")
tracker_same <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_same_name_districts.csv")
tracker_expected_same <- analysis_target_csv("diag_ext_district_tracker_sources", "tracker_legacy_expected_same_name_districts.csv")
```

``` r
data.frame(
  current_code_analog = c(
    "nrow(tracker_state_events)",
    "nrow(tracker_expected_state)",
    "nrow(tracker_inperiod)",
    "nrow(tracker_expected_inperiod)"
  ),
  rows = c(
    nrow(tracker_state_events),
    nrow(tracker_expected_state),
    nrow(tracker_inperiod),
    nrow(tracker_expected_inperiod)
  )
)
```

                  current_code_analog rows
    1      nrow(tracker_state_events)    7
    2    nrow(tracker_expected_state)    2
    3          nrow(tracker_inperiod)   17
    4 nrow(tracker_expected_inperiod)    1

``` r
analysis_table(tracker_counts, "Tracker source row counts")
```

| source_file_id                 | n_rows | n_columns |
|:-------------------------------|-------:|----------:|
| district_changes_alluvial      |    808 |        16 |
| district_changes_carveouts     |    383 |         5 |
| district_changes_tracker       |    735 |        60 |
| district_changes_new_districts |    487 |         6 |
| district_changes_name_changes  |    134 |         6 |
| district_changes_splits        |    929 |         6 |

Tracker source row counts

``` r
analysis_table(tracker_state_changes, "Current row-level state/UT changes")
```

| tracker_row | years | states | first_state | last_state |
|---:|:---|:---|:---|:---|
| 141 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Dadra and Nagar Haveli -\> Dadra and Nagar Haveli and Daman and Diu | Dadra and Nagar Haveli | Dadra and Nagar Haveli and Daman and Diu |
| 142 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Daman and Diu -\> Dadra and Nagar Haveli and Daman and Diu | Daman and Diu | Dadra and Nagar Haveli and Daman and Diu |
| 143 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Daman and Diu -\> Dadra and Nagar Haveli and Daman and Diu | Daman and Diu | Dadra and Nagar Haveli and Daman and Diu |
| 246 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Jammu and Kashmir -\> Ladakh | Jammu and Kashmir | Ladakh |
| 247 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Jammu and Kashmir -\> Ladakh | Jammu and Kashmir | Ladakh |
| 453 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 454 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 455 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 456 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 457 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 458 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 459 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 460 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 461 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 462 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 463 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 464 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 465 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 466 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 467 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 468 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 469 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 470 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 471 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 472 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 473 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 474 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 475 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 476 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 477 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 478 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 479 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 480 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 481 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 482 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Orissa -\> Odisha | Orissa | Odisha |
| 483 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Pondicherry -\> Puducherry | Pondicherry | Puducherry |
| 484 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Pondicherry -\> Puducherry | Pondicherry | Puducherry |
| 485 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Pondicherry -\> Puducherry | Pondicherry | Puducherry |
| 486 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Pondicherry -\> Puducherry | Pondicherry | Puducherry |
| 583 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 584 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 585 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 586 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 587 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 588 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 589 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 590 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 591 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 592 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 593 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 594 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 595 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 596 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 597 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 598 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 599 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 600 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 601 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 602 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 603 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 604 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 605 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 606 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 607 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 608 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 609 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 610 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 611 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 612 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 613 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 614 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 615 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Andhra Pradesh -\> Telangana | Andhra Pradesh | Telangana |
| 699 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 700 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 701 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 702 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 703 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 704 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 705 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 706 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 707 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 708 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 709 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 710 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |
| 711 | 2001;2005;2006;2007;2008;2011;2017;2018;2019;2020 | Uttaranchal -\> Uttarakhand | Uttaranchal | Uttarakhand |

Current row-level state/UT changes

``` r
analysis_table(tracker_state_events, "Current state/UT change events")
```

| state_transition | n_rows |
|:---|---:|
| Andhra Pradesh -\> Telangana | 33 |
| Orissa -\> Odisha | 30 |
| Uttaranchal -\> Uttarakhand | 13 |
| Pondicherry -\> Puducherry | 4 |
| Daman and Diu -\> Dadra and Nagar Haveli and Daman and Diu | 2 |
| Jammu and Kashmir -\> Ladakh | 2 |
| Dadra and Nagar Haveli -\> Dadra and Nagar Haveli and Daman and Diu | 1 |

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

``` r
analysis_table(tracker_expected_inperiod, "Legacy in-period district-name-change benchmark")
```

| diagnostic | legacy_expected_rows | legacy_chunk | legacy_note | current_detection_status |
|:---|---:|:---|:---|:---|
| in_period_district_name_changes | 16 | Chunk 6 district tracker source QA | Legacy comments counted rows where district_05 != district_06, district_07 != district_08, district_17 != district_18, or district_19 != district_20 before downstream corrections. | rendered analysis should compare this benchmark with current tracker_inperiod_district_changes.csv |

Legacy in-period district-name-change benchmark

``` r
analysis_table(tracker_same, "Current same-name districts appearing in multiple states")
```

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

``` r
analysis_table(tracker_expected_same, "Legacy same-name-district benchmark")
```

| diagnostic | legacy_expected_min_districts | legacy_expected_max_districts | legacy_chunk | legacy_note | current_detection_status |
|:---|---:|---:|:---|:---|:---|
| same_name_districts_across_states | 6 | 10 | Chunk 6 district tracker source QA | Legacy comments counted between min(n_same_name_districts$n) = 6 and max(n_same_name_districts$n) = 10 districts with shared names in each year of interest. | rendered analysis should compare this benchmark with tracker_same_name_districts.csv; a zero current count means the active tracker no longer exposes the raw same-name ambiguity, not that the legacy QA was irrelevant. |

Legacy same-name-district benchmark
