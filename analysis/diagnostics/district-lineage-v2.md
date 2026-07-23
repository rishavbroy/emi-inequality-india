# District Lineage v2 Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Scope

``` r
analysis_deviation_note("This notebook is a new current-code diagnostic rather than preserved legacy prose. It does not treat the inherited 454-district panel or its legacy IV estimates as validation targets. The legacy matching was almost certainly flawed, and the legacy partial F-statistic of 37.77 was computed incorrectly.")
```

**Deviation note.** This notebook is a new current-code diagnostic
rather than preserved legacy prose. It does not treat the inherited
454-district panel or its legacy IV estimates as validation targets. The
legacy matching was almost certainly flawed, and the legacy partial
F-statistic of 37.77 was computed incorrectly.

The v2 pipeline runs beside the current reviewed crosswalk. It does not
replace production geography until all source identities and
administrative events used by the preferred panel are adjudicated. Exact
names and fuzzy scores generate review candidates. Only tracked code,
alias, or dated-lineage adjudications can accept a source identity.

``` r
summary <- analysis_target_csv("diag_ext_district_lineage_v2", "summary.csv")
readiness <- analysis_target_csv("diag_ext_district_lineage_v2", "migration_readiness.csv")
blockers <- analysis_target_csv("diag_ext_district_lineage_v2", "migration_blockers.csv")
inventory <- analysis_target_csv("diag_ext_district_lineage_v2", "source_inventory.csv")
source_registry <- analysis_target_csv("diag_ext_district_lineage_v2", "source_registry.csv")
source_reference_issues <- analysis_target_csv("diag_ext_district_lineage_v2", "source_reference_issues.csv")
bridge_summary <- analysis_target_csv("diag_ext_district_lineage_v2", "shrid_bridge_summary.csv")
bridge_qa <- analysis_target_csv("diag_ext_district_lineage_v2", "shrid_bridge_qa.csv")
weight_validation <- analysis_target_csv("diag_ext_district_lineage_v2", "allocation_weight_validation.csv")
adjudicated_weights <- analysis_target_csv("diag_ext_district_lineage_v2", "adjudicated_allocation_weights.csv")
adjudicated_weight_validation <- analysis_target_csv("diag_ext_district_lineage_v2", "adjudicated_allocation_validation.csv")
transition <- analysis_target_csv("diag_ext_district_lineage_v2", "district_transition_2001_2011.csv")
eligibility <- analysis_target_csv("diag_ext_district_lineage_v2", "primary_mapping_eligibility.csv")
primary_crosswalk <- analysis_target_csv("diag_ext_district_lineage_v2", "primary_source_crosswalk.csv")
excluded_sources <- analysis_target_csv("diag_ext_district_lineage_v2", "excluded_source_rows.csv")
candidates <- analysis_target_csv("diag_ext_district_lineage_v2", "source_match_candidates.csv")
adjudication_queue <- analysis_target_csv("diag_ext_district_lineage_v2", "source_adjudication_queue.csv")
adjudication_draft <- analysis_target_csv("diag_ext_district_lineage_v2", "adjudication_draft.csv")
completion_status <- analysis_target_csv("diag_ext_district_lineage_v2", "completion_status.csv")
sensitivity_crosswalk <- analysis_target_csv("diag_ext_district_lineage_v2", "sensitivity_source_crosswalk.csv")
production_comparison <- analysis_target_csv("diag_ext_district_lineage_v2", "production_crosswalk_comparison.csv")
geometry_qa <- analysis_target_csv("diag_ext_district_lineage_v2", "geometry_2001_qa.csv")
events <- analysis_target_csv("diag_ext_district_lineage_v2", "candidate_admin_events.csv")
current_components <- analysis_target_csv("diag_ext_district_lineage_v2", "current_component_registry.csv")
urban_coverage <- analysis_target_csv("diag_ext_district_lineage_v2", "current_urban_coverage.csv")
components <- analysis_target_csv("diag_ext_district_lineage_v2", "changed_component_roster.csv")
evidence <- analysis_target_csv("diag_ext_district_lineage_v2", "evidence_requests.csv")
duplicates <- analysis_target_csv("diag_ext_district_lineage_v2", "duplicate_keys.csv")
gold <- analysis_target_csv("diag_ext_district_lineage_v2", "match_gold_scored.csv")
gold_summary <- analysis_target_csv("diag_ext_district_lineage_v2", "match_gold_summary.csv")
```

## Build and migration status

``` r
analysis_table(summary, "District-lineage v2 summary")
```

| metric                           |  value |
|:---------------------------------|-------:|
| available_inputs                 |     49 |
| missing_inputs                   |      0 |
| admin_units_2001                 |    593 |
| admin_units_2011                 |    640 |
| shrid_bridge_rows                | 602923 |
| deterministic_shrid_rows         | 587155 |
| district_transition_rows         |    696 |
| nss_source_rows                  |   1259 |
| accepted_source_matches          |      0 |
| unadjudicated_source_rows        |   1259 |
| candidate_rows                   |   4051 |
| cross_vintage_exact_review_rows  |   1109 |
| single_vintage_exact_review_rows |     68 |
| fuzzy_review_rows                |     82 |
| no_candidate_rows                |      0 |
| primary_eligible_source_rows     |      0 |
| candidate_event_rows             |   1655 |
| current_component_rows           |  19278 |
| urban_coverage_rows              |  34012 |
| changed_component_rows           |  47731 |
| targeted_evidence_requests       |    115 |

District-lineage v2 summary

``` r
analysis_table(readiness, "Production-migration gates")
```

| gate | passed | note |
|:---|:---|:---|
| core_inputs_available | TRUE | All locality keys and the PC11 district geometry are present. |
| unique_2001_unit_ids | TRUE | Census 2001 unit IDs are code-based and unique. |
| unique_2011_unit_ids | TRUE | Census 2011 unit IDs are code-based and unique. |
| shrid_weights_well_formed | TRUE | Every SHRUG transition weight is finite, nonnegative, and does not overallocate its source district. |
| shrid_allocation_coverage_complete | FALSE | Every SHRUG source district has complete mapped mass across 2001 targets. |
| adjudicated_allocation_weights_valid | TRUE | Every accepted tracked sensitivity allocation sums to one by source unit. |
| all_adjudication_sources_registered | TRUE | Every accepted source match, event, and allocation cites a registered evidence source. |
| no_conflicting_duplicate_keys | TRUE | Duplicate source or registry keys are either absent or identical. |
| all_source_rows_adjudicated | FALSE | Every NSS source row is explicitly accepted or excluded in tracked metadata. |
| accepted_source_rows_present | FALSE | At least one source row is accepted for the preferred panel. |
| all_accepted_rows_primary_eligible | TRUE | Every accepted source row maps deterministically to a 2001 district. |
| production_crosswalk_migration_ready | FALSE | Passes only when every prerequisite gate passes; production replacement remains an explicit maintainer action. |

Production-migration gates

``` r
analysis_table(blockers, "Current migration blockers")
```

| gate | note | next_action |
|:---|:---|:---|
| shrid_allocation_coverage_complete | Every SHRUG source district has complete mapped mass across 2001 targets. | Resolve unmatched SHRUG mass or document accepted sensitivity allocations. |
| all_source_rows_adjudicated | Every NSS source row is explicitly accepted or excluded in tracked metadata. | Accept or exclude every NSS source identity in tracked metadata. |
| accepted_source_rows_present | At least one source row is accepted for the preferred panel. | Accept at least one source identity for the preferred panel. |

Current migration blockers

``` r
analysis_table(completion_status, "Nine-step completion status")
```

| step | work_item | complete | observed | next_action |
|---:|:---|:---|:---|:---|
| 1 | Review deterministic identities and populate source adjudications | FALSE | 0/1259 resolved | Review adjudication_draft.csv and copy verified decisions into district_adjudications_v2.csv. |
| 2 | Resolve fuzzy source identities | FALSE | 82 fuzzy or missing candidates open | Use official rename or boundary evidence; accept, exclude, or retain needs_review. |
| 3 | Review targeted administrative-event evidence | FALSE | 115 targeted evidence requests | Record accepted or rejected edges in district_admin_events_v2.csv with registered source IDs. |
| 4 | Resolve incomplete SHRID coverage and sensitivity allocations | FALSE | 536 incomplete source allocations | Investigate unmapped mass and enter reviewed allocations in district_allocation_weights_v2.csv. |
| 5 | Construct and validate Census 2001 geometry | FALSE | not constructed from local SHRID polygons | Load local SHRID polygons, dissolve with dissolve_shrid_geometry_2001_v2(), and save a derived GeoPackage. |
| 6 | Build preferred and sensitivity source crosswalks | FALSE | 0 preferred; 0 total sensitivity rows | Regenerate the diagnostic after accepted source decisions and allocation weights are tracked. |
| 7 | Compare v2 mappings with the production panel | FALSE | 0 source mappings compared | Inspect production_crosswalk_comparison.csv after preferred mappings exist. |
| 8 | Review changed observations and estimates | FALSE | 0 changed, missing, or ambiguous mappings require review | Rebuild measures and models only after mapping comparisons are complete. |
| 9 | Migrate the production crosswalk deliberately | FALSE | migration gates remain blocked | Replace the inherited crosswalk only after every migration gate passes and changes are reviewed. |

Nine-step completion status

``` r
analysis_table(inventory, "Available and missing lineage inputs", max_rows = 60)
```

| source_id | relative_path | reader | role | load_for_diagnostic | exists | size_bytes |
|:---|:---|:---|:---|:---|:---|---:|
| lgd_states | data/raw/local_government_directory/states.json | lgd_json | current_registry | TRUE | TRUE | 12323 |
| lgd_districts | data/raw/local_government_directory/districts.json | lgd_json | current_registry | TRUE | TRUE | 343242 |
| lgd_subdistricts | data/raw/local_government_directory/subdistricts.json | lgd_json | current_registry | TRUE | TRUE | 4867226 |
| lgd_villages | data/raw/local_government_directory/villages.xlsx | lgd_xlsx | current_component_registry | FALSE | TRUE | 48621654 |
| lgd_urban_local_bodies | data/raw/local_government_directory/urbanLocalBody.xlsx | lgd_xlsx | current_urban_registry | TRUE | TRUE | 267634 |
| lgd_urban_coverage | data/raw/local_government_directory/urbanLocalBody-coverage.xlsx | lgd_xlsx | urban_component_registry | TRUE | TRUE | 1876288 |
| lgd_village_categories | data/raw/local_government_directory/villages-category-urbanLocalBody.xlsx | lgd_xlsx | urban_component_registry | FALSE | TRUE | 46605315 |
| lgd_development_blocks | data/raw/local_government_directory/developmentBlocks-coveredVillages.xlsx | lgd_xlsx | component_registry | FALSE | TRUE | 35697270 |
| lgd_mod_districts | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/districts.xls | spreadsheetml | changed_unit_roster_2011_2018 | TRUE | TRUE | 55743 |
| lgd_mod_subdistricts | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/subdistricts.xls | spreadsheetml | changed_unit_roster_2011_2018 | TRUE | TRUE | 1059238 |
| lgd_mod_villages | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/villages.xls | spreadsheetml | changed_unit_roster_2011_2018 | TRUE | TRUE | 34056792 |
| lgd_mod_urban_local_bodies | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/urbanLocalBody.xls | spreadsheetml | changed_unit_roster_2011_2018 | TRUE | TRUE | 296172 |
| lgd_changes_post_2018 | data/raw/local_government_directory/changes.csv | inventory_only | post_2018_validation | FALSE | TRUE | 150427025 |
| isded_1951_2024 | data/raw/district_changes/india_state_stories/isded/1951-2024/district_proliferation_1951_2024.xlsx | xlsx | candidate_lineage | TRUE | TRUE | 152349 |
| isded_admin_units_2025 | data/raw/district_changes/india_state_stories/isded/2025/admin_units_2025.xlsx | xlsx | published_current_component_registry | TRUE | TRUE | 1432373 |
| iss_census_series_1901_2011 | data/raw/district_changes/india_state_stories/census_data_collection/1901-2011/1901-2011-State Districts-Population Time Series.xlsx | inventory_only | historical_population_validation | FALSE | TRUE | 1389673 |
| iss_subdistricts_2026 | data/raw/district_changes/india_state_stories/census_data_collection/2026/2026_subdistricts_with_2011_census_pass2_loose.xlsx | inventory_only | published_current_component_registry | FALSE | TRUE | 3309901 |
| shrug_pc01r | data/raw/shrug/shrug-pc-keys-csv/pc01r_shrid_key.csv | shrug_locality_csv | stable_locality_weight | TRUE | TRUE | 32554823 |
| shrug_pc01u | data/raw/shrug/shrug-pc-keys-csv/pc01u_shrid_key.csv | shrug_locality_csv | stable_locality_weight | TRUE | TRUE | 289671 |
| shrug_pc11r | data/raw/shrug/shrug-pc-keys-csv/pc11r_shrid_key.csv | shrug_locality_csv | stable_locality_weight | TRUE | TRUE | 33663741 |
| shrug_pc11u | data/raw/shrug/shrug-pc-keys-csv/pc11u_shrid_key.csv | shrug_locality_csv | stable_locality_weight | TRUE | TRUE | 451746 |
| shrug_pc01dist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc01dist_key.csv | shrug_district_csv | stable_locality_district_membership | TRUE | TRUE | 17222200 |
| shrug_pc11dist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc11dist_key.csv | shrug_district_csv | stable_locality_district_membership | TRUE | TRUE | 17895273 |
| shrug_pc01subdist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc01subdist_key.csv | inventory_only | stable_locality_subdistrict_membership | FALSE | TRUE | 20220170 |
| shrug_pc11subdist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdist_key.csv | inventory_only | stable_locality_subdistrict_membership | FALSE | TRUE | 21506484 |
| shrug_pc11subdistu | data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdistu_key.csv | inventory_only | stable_locality_subdistrict_membership | FALSE | TRUE | 281686 |
| shrug_pc11_district_geometry | data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg | gpkg | census_2011_geometry | TRUE | TRUE | 31633408 |
| shrug_pc11_subdistrict_geometry | data/raw/shrug/open-polygons/shrug-pc11subdist-poly-gpkg/subdistrict.gpkg | inventory_only | census_2011_geometry | FALSE | TRUE | 91037696 |
| shrug_pc11_state_geometry | data/raw/shrug/open-polygons/shrug-pc11state-poly-gpkg/state.gpkg | inventory_only | census_2011_geometry | FALSE | TRUE | 8613888 |
| shrug_pc11_village_geometry_zip | data/raw/shrug/open-polygons/shrug-pc11-village-poly-gpkg.zip | inventory_only | census_2011_geometry | FALSE | TRUE | 399235423 |
| shrug_shrid_geometry_zip | data/raw/shrug/open-polygons/shrug-shrid-poly-gpkg.zip | inventory_only | future_2001_geometry | FALSE | TRUE | 379628892 |
| lineage_geometry_2001 | outputs/derived/district_lineage_v2/district_2001.gpkg | gpkg | derived_2001_geometry | TRUE | TRUE | 73408512 |
| shrug_pca01_zip | data/raw/shrug/census_2001/shrug-pca01-csv.zip | inventory_only | census_locality_attributes | FALSE | TRUE | 50359039 |
| shrug_pca11_zip | data/raw/shrug/census_2011/shrug-pca11-csv.zip | inventory_only | census_locality_attributes | FALSE | TRUE | 66532473 |
| shrug_td01_zip | data/raw/shrug/census_2001/shrug-td01-csv.zip | inventory_only | census_locality_attributes | FALSE | TRUE | 2473771 |
| shrug_td11_zip | data/raw/shrug/census_2011/shrug-td11-csv.zip | inventory_only | census_locality_attributes | FALSE | TRUE | 4754266 |
| shrug_vd01_zip | data/raw/shrug/census_2001/shrug-vd01-csv.zip | inventory_only | census_locality_attributes | FALSE | TRUE | 32905413 |
| shrug_vd11_zip | data/raw/shrug/census_2011/shrug-vd11-csv.zip | inventory_only | census_locality_attributes | FALSE | TRUE | 69981752 |
| ipums_geo2_1987_2009 | data/raw/ipums/geo2_in1987_2009/geo2_in1987_2009.shp | inventory_only | stable_geography_sensitivity | FALSE | TRUE | 6941364 |
| concordance_plfs_nss | data/raw/concordance/plfs_nss_distcodes.csv | csv | published_concordance | TRUE | TRUE | 13452 |
| concordance_census_plfs | data/raw/concordance/census_plfs_distcodes.csv | csv | published_concordance | TRUE | TRUE | 18775 |
| concordance_nrlm_plfs | data/raw/concordance/nrlm_plfs_distcodes.csv | csv | published_concordance | TRUE | TRUE | 18601 |
| concordance_telangana | data/raw/concordance/telangana_plfs_districts.csv | csv | published_concordance | TRUE | TRUE | 695 |
| concordance_census_region | data/raw/concordance/census_region.csv | csv | published_concordance | TRUE | TRUE | 21341 |
| lineage_gold | data/metadata/district_match_gold.csv | csv | calibration | TRUE | TRUE | 7217 |
| lineage_adjudications | data/metadata/district_adjudications_v2.csv | csv | adjudication | TRUE | TRUE | 79 |
| lineage_events | data/metadata/district_admin_events_v2.csv | csv | event_adjudication | TRUE | TRUE | 81 |
| lineage_allocation_weights | data/metadata/district_allocation_weights_v2.csv | csv | allocation_adjudication | TRUE | TRUE | 74 |
| lineage_sources | data/metadata/district_sources_v2.csv | csv | source_registry | TRUE | TRUE | 4515 |

Available and missing lineage inputs

``` r
analysis_table(source_registry, "District-lineage evidence registry", max_rows = 50)
```

| source_id | citation | path_or_url | accessed |
|:---|:---|:---|:---|
| census2001_c16 | Census of India 2001 C-16 mother-tongue tables | data/raw/census_2001_mother_tongue | 2026-07-22 |
| shrug_pc_keys | Development Data Lab SHRUG Population Census location keys | data/raw/shrug/shrug-pc-keys-csv | 2026-07-22 |
| shrug_pc11_district_geometry | Development Data Lab SHRUG PC11 district polygons | data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg | 2026-07-22 |
| shrug_open_polygons | Development Data Lab SHRUG open polygons | data/raw/shrug/open-polygons | 2026-07-22 |
| lgd_states | Ministry of Panchayati Raj Local Government Directory states | data/raw/local_government_directory/states.json | 2026-07-22 |
| lgd_districts | Ministry of Panchayati Raj Local Government Directory districts | data/raw/local_government_directory/districts.json | 2026-07-22 |
| lgd_subdistricts | Ministry of Panchayati Raj Local Government Directory subdistricts | data/raw/local_government_directory/subdistricts.json | 2026-07-22 |
| lgd_villages | Ministry of Panchayati Raj Local Government Directory villages | data/raw/local_government_directory/villages.xlsx | 2026-07-22 |
| lgd_urban_local_bodies | Ministry of Panchayati Raj Local Government Directory urban local bodies | data/raw/local_government_directory/urbanLocalBody.xlsx | 2026-07-22 |
| lgd_urban_coverage | Ministry of Panchayati Raj Local Government Directory urban coverage | data/raw/local_government_directory/urbanLocalBody-coverage.xlsx | 2026-07-22 |
| lgd_mod_districts | LGD districts modified from 2011-01-01 through 2018-06-30 | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/districts.xls | 2026-07-22 |
| lgd_mod_subdistricts | LGD subdistricts modified from 2011-01-01 through 2018-06-30 | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/subdistricts.xls | 2026-07-22 |
| lgd_mod_villages | LGD villages modified from 2011-01-01 through 2018-06-30 | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/villages.xls | 2026-07-22 |
| lgd_mod_urban_local_bodies | LGD urban local bodies modified from 2011-01-01 through 2018-06-30 | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/urbanLocalBody.xls | 2026-07-22 |
| lgd_changes_post_2018 | ramSeraph archive of Local Government Directory changes from 2018-10-13 onward | data/raw/local_government_directory/changes.csv | 2026-07-22 |
| alluvial | India State Stories Alluvial district-change workbook | data/raw/district_changes/Time series- State and Districts Changes -Alluvial 1951-2024.xlsx | 2026-07-22 |
| district_splits | India State Stories district splits and carve-outs workbook | data/raw/district_changes/District Splits and Carve outs-decadewise 1951-2024.xlsx | 2026-07-22 |
| name_changes | India State Stories district and state name-change workbook | data/raw/district_changes/Name Changes_Districts_Indian States_1951-2021.xlsx | 2026-07-22 |
| new_districts_created | India State Stories new-district workbook | data/raw/district_changes/New Districts Created between 1951-2024.xlsx | 2026-07-22 |
| india_district_tracker | Jaacks Research Group India district changes tracker | data/raw/district_changes/IndiaDistrictTracker2001to2020.ods | 2026-07-22 |
| isded_1951_2024 | India State and District Evolution Database 1951-2024 | data/raw/district_changes/india_state_stories/isded/1951-2024 | 2026-07-22 |
| isded_admin_units_2025 | India State and District Evolution Database administrative units 2025 | data/raw/district_changes/india_state_stories/isded/2025 | 2026-07-22 |
| iss_census_collection | Indian Census Data Collection 1901-2026 | data/raw/district_changes/india_state_stories/census_data_collection | 2026-07-22 |
| kumar_somanathan_2016 | Kumar and Somanathan Creating Long Panels Using Census Data 1961-2001 | data/raw/district_changes/District Carve-Outs and Renamings 1961-2001.csv | 2026-07-22 |
| concordance_plfs_nss | Deshpande Khanna and Walia PLFS-to-NSS district concordance | data/raw/concordance/plfs_nss_distcodes.csv | 2026-07-22 |
| concordance_census_plfs | Deshpande Khanna and Walia Census-to-PLFS district concordance | data/raw/concordance/census_plfs_distcodes.csv | 2026-07-22 |
| concordance_nrlm_plfs | Deshpande Khanna and Walia NRLM-to-PLFS district concordance | data/raw/concordance/nrlm_plfs_distcodes.csv | 2026-07-22 |
| concordance_telangana | Deshpande Khanna and Walia Telangana district concordance | data/raw/concordance/telangana_plfs_districts.csv | 2026-07-22 |
| concordance_census_region | Deshpande Khanna and Walia Census-region concordance | data/raw/concordance/census_region.csv | 2026-07-22 |
| ipums_geo2_1987_2009 | IPUMS harmonized India second-level geography 1987-2009 | data/raw/ipums/geo2_in1987_2009 | 2026-07-22 |

District-lineage evidence registry

``` r
analysis_table(source_reference_issues, "Unregistered or missing adjudication evidence", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Unregistered or missing adjudication evidence

A failed migration gate is expected while the adjudication files are
incomplete. The diagnostic remains intentionally separate from the
current production panel.

## Census transition and allocation integrity

``` r
analysis_table(bridge_summary, "SHRUG bridge-status summary", max_rows = 50)
```

| bridge_status                        | n_shrid | population |        area |
|:-------------------------------------|--------:|-----------:|------------:|
| crosses_district_boundary            |     117 |   32160884 |    5388.844 |
| deterministic_one_district_each_year |  587155 | 1173363244 | 2705346.045 |
| missing_census_locality_key          |   15651 |    5217548 |   36158.905 |

SHRUG bridge-status summary

``` r
analysis_table(bridge_qa, "Non-deterministic or incomplete SHRID bridge records", max_rows = 50)
```

| shrid2 | state_code_2001 | district_code_2001 | n_state_memberships_2001 | n_district_memberships_2001 | deterministic_2001 | state_code_2011 | district_code_2011 | n_state_memberships_2011 | n_district_memberships_2011 | deterministic_2011 | population | area | has_locality_key_2001 | has_locality_key_2011 | deterministic | bridge_status |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 11-01-014-00055-800033 | 1 | 6 | 1 | 1 | TRUE | 1 | NA | 1 | 2 | FALSE | 150592 | 44.3380115 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-03-051-00265-038948 | 3 | 7 | 1 | 1 | TRUE | 3 | NA | 1 | 2 | FALSE | 1953 | 3.29 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-06-086-00416-062761 | 6 | 18 | 1 | 1 | TRUE | 6 | NA | 1 | 2 | FALSE | 913 | 2.61 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-06-087-00423-063314 | 6 | 18 | 1 | 1 | TRUE | 6 | NA | 1 | 2 | FALSE | 1910 | 3.58 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-08-125-00652-098913 | 8 | 28 | 1 | 1 | TRUE | 8 | NA | 1 | 2 | FALSE | 1387 | 5.48 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-08-131-00699-108605 | 8 | 29 | 1 | 1 | TRUE | 8 | NA | 1 | 2 | FALSE | 1335 | 3.1396001 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-08-131-00699-108673 | 8 | 29 | 1 | 1 | TRUE | 8 | NA | 1 | 2 | FALSE | 546 | 2.1752 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-08-131-00699-108699 | 8 | 29 | 1 | 1 | TRUE | 8 | NA | 1 | 2 | FALSE | 289 | 2.0352 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-08-131-00700-108742 | 8 | 29 | 1 | 1 | TRUE | 8 | NA | 1 | 2 | FALSE | 1192 | 7.1213 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-10-216-01199-228045 | 10 | 14 | 1 | 1 | TRUE | 10 | NA | 1 | 2 | FALSE | 794 | 1.74131673 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-256-01711-265656 | 12 | 5 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 838 | 41.0340129 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01720-266008 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 31 | 65.5251153 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01720-266014 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 38 | 81.4948098 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01720-266019 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 66 | 24.398867 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01721-266022 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 178 | 28.353183 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01721-266036 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 156 | 7.89959986 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01721-266039 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 39 | 42.8457551 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-257-01721-266043 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 196 | 7.186612 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01722-266050 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 75 | 66.1565587 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01722-266051 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 189 | 24.0492794 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01722-266068 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 113 | 49.18703824 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01723-266090 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 127 | 341.7461915 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01723-266101 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 142 | 78.3927169 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01723-266106 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 77 | 46.0342118 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01723-266112 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 30 | 29.803786 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-258-01723-266115 | 12 | 10 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 96 | 61.4389886 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-260-01736-266667 | 12 | 11 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 198 | 282.414804 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-12-260-01739-266830 | 12 | 11 | 1 | 1 | TRUE | 12 | NA | 1 | 2 | FALSE | 156 | 9.0804932 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-293-01962-272987 | 17 | 3 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 360 | 1.9660729 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-293-01963-273236 | 17 | 3 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 277 | 2.4070857 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-294-01970-274493 | 17 | 2 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 1108 | 6.5527288 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-295-01976-275952 | 17 | 1 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 193 | 3.69410975 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-295-01976-275985 | 17 | 1 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 804 | 3.62464783 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-295-01976-276001 | 17 | 1 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 313 | 4.95392786 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-296-01982-277093 | 17 | 2 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 949 | 8.7927777 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-296-01982-277114 | 17 | 2 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 1105 | 3.2467316 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-17-297-01987-278057 | 17 | 6 | 1 | 1 | TRUE | 17 | NA | 1 | 2 | FALSE | 3481 | 5.8520185 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-19-344-02450-337951 | 19 | 15 | 1 | 1 | TRUE | 19 | NA | 1 | 2 | FALSE | 5026 | 2.78680002 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-23-460-03691-501529 | 23 | 16 | 1 | 1 | TRUE | 23 | NA | 1 | 2 | FALSE | 30354 | 19.5106285 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-23-462-03701-802425 | 23 | 17 | 1 | 1 | TRUE | 23 | NA | 1 | 2 | FALSE | 13411 | 27.2575 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-28-553-05330-595096 | 28 | 23 | 1 | 1 | TRUE | 28 | NA | 1 | 2 | FALSE | 25480 | 11.58 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-33-615-05782-803638 | 33 | 16 | 1 | 1 | TRUE | 33 | NA | 1 | 2 | FALSE | 18615 | 24.92 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-33-622-05830-640126 | 33 | 7 | 1 | 1 | TRUE | 33 | NA | 1 | 2 | FALSE | 42200 | 34.29 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-33-625-05848-803793 | 33 | 26 | 1 | 1 | TRUE | 33 | NA | 1 | 2 | FALSE | 12520 | 15 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-33-633-05904-644792 | 33 | 12 | 1 | 1 | TRUE | 33 | NA | 1 | 2 | FALSE | 47986 | 12.07 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-01-010-00043-800013 | 1 | NA | 1 | 3 | FALSE | 1 | NA | 1 | 2 | FALSE | 1206419 | 265.378728 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-03-048-00254-036979 | 3 | NA | 1 | 2 | FALSE | 3 | NA | 1 | 2 | FALSE | 1779 | 4.37 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-05-059-00294-800299 | 5 | NA | 1 | 2 | FALSE | 5 | NA | 1 | 2 | FALSE | 2868 | 9.75 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-06-070-00358-057182 | 6 | NA | 1 | 2 | FALSE | 6 | NA | 1 | 2 | FALSE | 1105 | 4.67 | TRUE | TRUE | FALSE | crosses_district_boundary |
| 11-06-070-00359-057381 | 6 | NA | 1 | 2 | FALSE | 6 | NA | 1 | 2 | FALSE | 4878 | 2.29 | TRUE | TRUE | FALSE | crosses_district_boundary |
| Table truncated in rendered note; full CSV has 15768 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Non-deterministic or incomplete SHRID bridge records

``` r
analysis_table(weight_validation, "SHRUG source-district allocation checks", max_rows = 50)
```

| source_key | n_targets | n_missing_weights | n_negative_weights | weight_sum | unmapped_share | weights_well_formed | coverage_complete |
|:---|:---|:---|:---|:---|:---|:---|:---|
| 1.001 | 1 | 0 | 0 | 0.996316441356046 | 0.00368355864395398 | TRUE | FALSE |
| 1.002 | 2 | 0 | 0 | 0.980825205361533 | 0.0191747946384668 | TRUE | FALSE |
| 1.003 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.004 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.005 | 1 | 0 | 0 | 0.985858840059979 | 0.014141159940021 | TRUE | FALSE |
| 1.006 | 1 | 0 | 0 | 0.999467633850393 | 0.000532366149607055 | TRUE | FALSE |
| 1.007 | 1 | 0 | 0 | 0.999865354822487 | 0.000134645177513049 | TRUE | FALSE |
| 1.008 | 1 | 0 | 0 | 0.999546644524666 | 0.000453355475333983 | TRUE | FALSE |
| 1.009 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.01 | 3 | 0 | 0 | 0.971758536487731 | 0.0282414635122694 | TRUE | FALSE |
| 1.011 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.012 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.013 | 1 | 0 | 0 | 0.968138534643052 | 0.031861465356948 | TRUE | FALSE |
| 1.014 | 1 | 0 | 0 | 0.992569688118609 | 0.00743031188139098 | TRUE | FALSE |
| 1.015 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.016 | 1 | 0 | 0 | 0.999924378439561 | 7.56215604390542e-05 | TRUE | FALSE |
| 1.017 | 2 | 0 | 0 | 0.99877693302739 | 0.00122306697260999 | TRUE | FALSE |
| 1.018 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 1.019 | 2 | 0 | 0 | 0.999944142634485 | 5.58573655145e-05 | TRUE | FALSE |
| 1.02 | 1 | 0 | 0 | 0.999354873564753 | 0.000645126435246945 | TRUE | FALSE |
| 1.021 | 1 | 0 | 0 | 0.995973745684522 | 0.00402625431547798 | TRUE | FALSE |
| 1.022 | 3 | 0 | 0 | 0.931454571681227 | 0.0685454283187726 | TRUE | FALSE |
| 2.023 | 1 | 0 | 0 | 0.997636202512137 | 0.00236379748786297 | TRUE | FALSE |
| 2.024 | 1 | 0 | 0 | 0.998533847656573 | 0.00146615234342695 | TRUE | FALSE |
| 2.025 | 1 | 0 | 0 | 0.98691547332404 | 0.0130845266759601 | TRUE | FALSE |
| 2.026 | 1 | 0 | 0 | 0.992728983359328 | 0.00727101664067198 | TRUE | FALSE |
| 2.027 | 1 | 0 | 0 | 0.998687707358741 | 0.00131229264125898 | TRUE | FALSE |
| 2.028 | 1 | 0 | 0 | 0.999703145340042 | 0.00029685465995799 | TRUE | FALSE |
| 2.029 | 1 | 0 | 0 | 0.999879118833861 | 0.000120881166138953 | TRUE | FALSE |
| 2.03 | 1 | 0 | 0 | 0.999298348500874 | 0.000701651499126044 | TRUE | FALSE |
| 2.031 | 1 | 0 | 0 | 0.999577819134271 | 0.000422180865728983 | TRUE | FALSE |
| 2.032 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 2.033 | 1 | 0 | 0 | 0.998377169813639 | 0.001622830186361 | TRUE | FALSE |
| 2.034 | 1 | 0 | 0 | 0.996540697328848 | 0.00345930267115202 | TRUE | FALSE |
| 3.035 | 1 | 0 | 0 | 0.988358903426542 | 0.011641096573458 | TRUE | FALSE |
| 3.036 | 1 | 0 | 0 | 0.994615833791317 | 0.00538416620868298 | TRUE | FALSE |
| 3.037 | 2 | 0 | 0 | 0.997466709822711 | 0.00253329017728909 | TRUE | FALSE |
| 3.038 | 2 | 0 | 0 | 0.999988655164263 | 1.13448357366153e-05 | TRUE | FALSE |
| 3.039 | 1 | 0 | 0 | 0.998801260799268 | 0.00119873920073199 | TRUE | FALSE |
| 3.04 | 2 | 0 | 0 | 0.997357007432062 | 0.00264299256793776 | TRUE | FALSE |
| 3.041 | 1 | 0 | 0 | 0.999958842314331 | 4.11576856690354e-05 | TRUE | FALSE |
| 3.042 | 1 | 0 | 0 | 0.99981119683132 | 0.000188803168679974 | TRUE | FALSE |
| 3.043 | 1 | 0 | 0 | 0.999789559178226 | 0.000210440821774016 | TRUE | FALSE |
| 3.044 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 3.045 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 3.046 | 1 | 0 | 0 | 0.999944545470913 | 5.54545290869513e-05 | TRUE | FALSE |
| 3.047 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| 3.048 | 1 | 0 | 0 | 0.996912959499486 | 0.003087040500514 | TRUE | FALSE |
| 3.049 | 1 | 0 | 0 | 0.995989008518238 | 0.00401099148176198 | TRUE | FALSE |
| 3.05 | 1 | 0 | 0 | 0.998803172842384 | 0.00119682715761604 | TRUE | FALSE |
| Table truncated in rendered note; full CSV has 629 rows. |  |  |  |  |  |  |  |

SHRUG source-district allocation checks

``` r
analysis_table(adjudicated_weight_validation, "Tracked sensitivity-allocation checks", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Tracked sensitivity-allocation checks

``` r
analysis_table(adjudicated_weights, "Tracked sensitivity allocations", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Tracked sensitivity allocations

``` r
analysis_table(transition, "Census 2011 to Census 2001 district transitions", max_rows = 50)
```

| state_code_2011 | district_code_2011 | state_code_2001 | district_code_2001 | n_shrid_mapped | population_2011_mapped | area_2011_mapped | n_shrid_total | population_2011_total | area_2011_total | population_share_to_2001 | area_share_to_2001 | n_target_2001_districts | shrid_coverage | mapping_class |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 1 | 1 | 1 | 1 | 361 | 867148 | 661.62426696 | 363 | 870354 | 665.11401898 | 0.996316441356046 | 0.994753152210877 | 1 | 0.994490358126722 | non_nested_or_incomplete |
| 1 | 2 | 1 | 4 | 461 | 716525 | 709.292039545999 | 470 | 734193 | 719.494502655999 | 0.975935482904359 | 0.985819956827554 | 2 | 0.991489361702128 | non_nested_or_incomplete |
| 1 | 2 | 1 | 2 | 5 | 3590 | 5.02299997 | 470 | 734193 | 719.494502655999 | 0.00488972245717407 | 0.00698129026901206 | 2 | 0.991489361702128 | non_nested_or_incomplete |
| 1 | 3 | 1 | 7 | 113 | 133487 | 5269.52404821 | 113 | 133487 | 5269.52404821 | 1 | 1 | 1 | 1 | deterministic_containment |
| 1 | 4 | 1 | 8 | 126 | 140802 | 545.8243298 | 126 | 140802 | 545.8243298 | 1 | 1 | 1 | 1 | deterministic_containment |
| 1 | 5 | 1 | 11 | 172 | 470092 | 1089.7229054 | 173 | 476835 | 1094.5929054 | 0.985858840059979 | 0.995550857331548 | 1 | 0.994219653179191 | non_nested_or_incomplete |
| 1 | 6 | 1 | 12 | 378 | 642073 | 2472.49963542 | 379 | 642415 | 2518.82896942 | 0.999467633850393 | 0.981606796427044 | 1 | 0.997361477572559 | non_nested_or_incomplete |
| 1 | 7 | 1 | 14 | 497 | 616352 | 2420.7354286 | 502 | 616435 | 2426.1654286 | 0.999865354822487 | 0.997761900348595 | 1 | 0.99003984063745 | non_nested_or_incomplete |
| 1 | 8 | 1 | 2 | 511 | 1007582 | 1313.31113092 | 513 | 1008039 | 1320.06773752 | 0.999546644524666 | 0.994881621292636 | 1 | 0.996101364522417 | non_nested_or_incomplete |
| 1 | 9 | 1 | 2 | 122 | 392232 | 366.35157906 | 122 | 392232 | 366.35157906 | 1 | 1 | 1 | 1 | deterministic_containment |
| 1 | 10 | 1 | 3 | 10 | 35788 | 24.55672547 | 15 | 49962 | 37.41043493 | 0.716304391337416 | 0.656413792460552 | 3 | 0.933333333333333 | non_nested_or_incomplete |
| 1 | 10 | 1 | 4 | 3 | 10133 | 10.373 | 15 | 49962 | 37.41043493 | 0.202814138745447 | 0.277275578843424 | 3 | 0.933333333333333 | non_nested_or_incomplete |
| 1 | 10 | 1 | 2 | 1 | 2630 | 2.044 | 15 | 49962 | 37.41043493 | 0.0526400064048677 | 0.0546371621667752 | 3 | 0.933333333333333 | non_nested_or_incomplete |
| 1 | 11 | 1 | 3 | 112 | 282191 | 246.62416954 | 116 | 297446 | 267.91216924 | 0.948713379907614 | 0.92054112450215 | 2 | 1 | non_nested_or_incomplete |
| 1 | 11 | 1 | 2 | 4 | 15255 | 21.2879997 | 116 | 297446 | 267.91216924 | 0.0512866200923865 | 0.0794588754978497 | 2 | 1 | non_nested_or_incomplete |
| 1 | 12 | 1 | 5 | 317 | 560440 | 619.82715547 | 317 | 560440 | 619.82715547 | 1 | 1 | 1 | 1 | deterministic_containment |
| 1 | 13 | 1 | 5 | 225 | 257733 | 310.42881788 | 227 | 266215 | 319.85703588 | 0.968138534643052 | 0.970523649811045 | 1 | 0.991189427312775 | non_nested_or_incomplete |
| 1 | 14 | 1 | 6 | 343 | 921595 | 743.481248995 | 346 | 928494 | 747.946249015 | 0.992569688118609 | 0.994030319657489 | 1 | 0.991329479768786 | non_nested_or_incomplete |
| 1 | 15 | 1 | 6 | 233 | 424089 | 421.56422997 | 233 | 424089 | 421.56422997 | 1 | 1 | 1 | 1 | deterministic_containment |
| 1 | 16 | 1 | 9 | 402 | 409905 | 1364.387929225 | 404 | 409936 | 1368.584235325 | 0.999924378439561 | 0.996933834256096 | 1 | 0.995049504950495 | non_nested_or_incomplete |
| 1 | 17 | 1 | 9 | 98 | 238696 | 1170.65990592 | 117 | 283713 | 1367.11621422 | 0.84132908960816 | 0.856298750423286 | 2 | 0.991452991452991 | non_nested_or_incomplete |
| 1 | 17 | 1 | 10 | 18 | 44670 | 194.6679986 | 117 | 283713 | 1367.11621422 | 0.15744784341923 | 0.142393160563213 | 2 | 0.991452991452991 | non_nested_or_incomplete |
| 1 | 18 | 1 | 9 | 156 | 230696 | 1596.64499788 | 156 | 230696 | 1596.64499788 | 1 | 1 | 1 | 1 | deterministic_containment |
| 1 | 19 | 1 | 10 | 329 | 554442 | 2236.21240167 | 331 | 554985 | 2243.00125403 | 0.999021595178248 | 0.996973317626193 | 2 | 0.996978851963746 | non_nested_or_incomplete |
| 1 | 19 | 1 | 13 | 1 | 512 | 6.37 | 331 | 554985 | 2243.00125403 | 0.000922547456237556 | 0.00283994491244935 | 2 | 0.996978851963746 | non_nested_or_incomplete |
| 1 | 20 | 1 | 10 | 253 | 314464 | 1717.67848666 | 255 | 314667 | 1721.61776826 | 0.999354873564753 | 0.997711872128282 | 1 | 0.992156862745098 | non_nested_or_incomplete |
| 1 | 21 | 1 | 13 | 773 | 1523798 | 2328.313105865 | 794 | 1529958 | 2350.940105735 | 0.995973745684522 | 0.990375339714184 | 1 | 0.973551637279597 | non_nested_or_incomplete |
| 1 | 22 | 1 | 13 | 272 | 249839 | 664.38500042 | 351 | 318898 | 861.15100038 | 0.78344486324781 | 0.771508132867322 | 3 | 0.943019943019943 | non_nested_or_incomplete |
| 1 | 22 | 1 | 14 | 58 | 43984 | 143.51 | 351 | 318898 | 861.15100038 | 0.137924979146937 | 0.166649054505741 | 3 | 0.943019943019943 | non_nested_or_incomplete |
| 1 | 22 | 1 | 10 | 1 | 3216 | 17.009 | 351 | 318898 | 861.15100038 | 0.0100847292864803 | 0.0197514721488966 | 3 | 0.943019943019943 | non_nested_or_incomplete |
| 2 | 23 | 2 | 1 | 1087 | 517853 | 2651.73311397 | 1115 | 519080 | 2756.51839933 | 0.997636202512137 | 0.961986364616514 | 1 | 0.974887892376682 | non_nested_or_incomplete |
| 2 | 24 | 2 | 2 | 3596 | 1507861 | 4383.526445516 | 3623 | 1510075 | 4412.330245556 | 0.998533847656573 | 0.993471975478488 | 1 | 0.992547612475849 | non_nested_or_incomplete |
| 2 | 25 | 2 | 3 | 270 | 31151 | 1210.87451422 | 280 | 31564 | 1362.42726012 | 0.98691547332404 | 0.88876268822847 | 1 | 0.964285714285714 | non_nested_or_incomplete |
| 2 | 26 | 2 | 4 | 176 | 434719 | 710.36569385 | 196 | 437903 | 722.28839383 | 0.992728983359328 | 0.983493158575096 | 1 | 0.897959183673469 | non_nested_or_incomplete |
| 2 | 27 | 2 | 5 | 2792 | 998465 | 3463.474206326 | 2855 | 999777 | 3519.92257658599 | 0.998687707358741 | 0.983963178441629 | 1 | 0.977933450087566 | non_nested_or_incomplete |
| 2 | 28 | 2 | 6 | 1633 | 454633 | 1092.045472907 | 1639 | 454768 | 1095.132867917 | 0.999703145340042 | 0.997180803261003 | 1 | 0.99633923123856 | non_nested_or_incomplete |
| 2 | 29 | 2 | 7 | 755 | 521110 | 1430.465451256 | 757 | 521173 | 1499.385451256 | 0.999879118833861 | 0.954034501307007 | 1 | 0.997357992073976 | non_nested_or_incomplete |
| 2 | 30 | 2 | 8 | 951 | 381688 | 1025.352681715 | 957 | 381956 | 1027.081640245 | 0.999298348500874 | 0.99831662989362 | 1 | 0.993730407523511 | non_nested_or_incomplete |
| 2 | 31 | 2 | 9 | 2358 | 580075 | 1682.752395728 | 2379 | 580320 | 1706.177590378 | 0.999577819134271 | 0.986270365533983 | 1 | 0.991172761664565 | non_nested_or_incomplete |
| 2 | 32 | 2 | 10 | 969 | 529855 | 2238.233263 | 969 | 529855 | 2238.233263 | 1 | 1 | 1 | 1 | deterministic_containment |
| 2 | 33 | 2 | 11 | 2484 | 812689 | 3519.37516742301 | 2539 | 814010 | 3629.27411571301 | 0.998377169813639 | 0.96971875234935 | 1 | 0.978337928318236 | non_nested_or_incomplete |
| 2 | 34 | 2 | 12 | 218 | 83830 | 557.84079863 | 241 | 84121 | 745.42376193 | 0.996540697328848 | 0.748353925806815 | 1 | 0.904564315352697 | non_nested_or_incomplete |
| 3 | 35 | 3 | 1 | 1525 | 2271568 | 3457.748 | 1542 | 2298323 | 3512.998 | 0.988358903426542 | 0.984272692441043 | 1 | 0.988975356679637 | non_nested_or_incomplete |
| 3 | 36 | 3 | 3 | 603 | 810779 | 1572.45369988 | 620 | 815168 | 1591.23029983 | 0.994615833791317 | 0.988199948208624 | 1 | 0.97258064516129 | non_nested_or_incomplete |
| 3 | 37 | 3 | 4 | 936 | 2187969 | 2677.07741239 | 943 | 2193590 | 2685.25121239 | 0.997437533905607 | 0.996956038987233 | 2 | 0.993637327677625 | non_nested_or_incomplete |
| 3 | 37 | 3 | 10 | 1 | 64 | 2.49 | 943 | 2193590 | 2685.25121239 | 2.91759171039255e-05 | 0.000927287543344514 | 2 | 0.993637327677625 | non_nested_or_incomplete |
| 3 | 38 | 3 | 5 | 1392 | 1585960 | 3382.14159945 | 1398 | 1586625 | 3391.99159945 | 0.999580871346411 | 0.997096101298837 | 2 | 0.997138769670958 | non_nested_or_incomplete |
| 3 | 38 | 3 | 1 | 2 | 647 | 7.24 | 1398 | 1586625 | 3391.99159945 | 0.00040778381785236 | 0.00213443924836781 | 2 | 0.997138769670958 | non_nested_or_incomplete |
| 3 | 39 | 3 | 6 | 463 | 611576 | 1251.6 | 466 | 612310 | 1255.7 | 0.998801260799268 | 0.996734888906586 | 1 | 0.993562231759657 | non_nested_or_incomplete |
| 3 | 40 | 3 | 8 | 443 | 597590 | 1185.0503 | 446 | 599699 | 1190.8703 | 0.996483235756605 | 0.995112817911405 | 2 | 0.995515695067265 | non_nested_or_incomplete |
| Table truncated in rendered note; full CSV has 696 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Census 2011 to Census 2001 district transitions

``` r
analysis_table(duplicates, "Duplicate-key diagnostics", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Duplicate-key diagnostics

SHRID units that cross district boundaries or lack either Census
membership are not silently renormalized away. Their omission lowers
coverage and prevents a transition from being classified as
deterministic.

## Review queues

``` r
analysis_table(eligibility, "Primary-panel source eligibility", max_rows = 50)
```

| state_code_2001 | district_code_2001 | source_state_code_2011 | source_district_code_2011 | terminal_unit | unit_id | source_row_id | source_key | wave | source_code | raw_state | raw_district | state_std | district_std | reference_vintage | method | status | terminal_vintage | resolution_status | lineage_path | target_state_code_2001 | target_district_code_2001 | population_share_to_2001 | bridged_target_unit_2001 | mapping_class | eligible_primary | target_unit_2001 | exclusion_reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08\_\_andaman and nicobar islands\_\_35101 | nss_2007_08 | 35101 | Andaman & Nicober | South Andaman | andaman and nicobar islands | south andaman | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08\_\_andaman and nicobar islands\_\_35102 | nss_2007_08 | 35102 | Andaman & Nicober | Nicobars | andaman and nicobar islands | nicobars | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08\_\_andaman and nicobar islands\_\_35103 | nss_2007_08 | 35103 | Andaman & Nicober | North and Middle Andaman | andaman and nicobar islands | north and middle andaman | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08\_\_andhra pradesh\_\_28111 | nss_2007_08 | 28111 | Andhra Pardesh | Srikakulam | andhra pradesh | srikakulam | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08\_\_andhra pradesh\_\_28112 | nss_2007_08 | 28112 | Andhra Pardesh | Vizianagaram | andhra pradesh | vizianagaram | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08\_\_andhra pradesh\_\_28113 | nss_2007_08 | 28113 | Andhra Pardesh | Visakhapatnam | andhra pradesh | visakhapatnam | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08\_\_andhra pradesh\_\_28114 | nss_2007_08 | 28114 | Andhra Pardesh | East Godavari | andhra pradesh | east godavari | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08\_\_andhra pradesh\_\_28115 | nss_2007_08 | 28115 | Andhra Pardesh | West Godawari | andhra pradesh | west godawari | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08\_\_andhra pradesh\_\_28216 | nss_2007_08 | 28216 | Andhra Pardesh | Krishna | andhra pradesh | krishna | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08\_\_andhra pradesh\_\_28217 | nss_2007_08 | 28217 | Andhra Pardesh | Guntur | andhra pradesh | guntur | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08\_\_andhra pradesh\_\_28218 | nss_2007_08 | 28218 | Andhra Pardesh | Prakasam | andhra pradesh | prakasam | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08\_\_andhra pradesh\_\_28219 | nss_2007_08 | 28219 | Andhra Pardesh | Nellore | andhra pradesh | nellore | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08\_\_andhra pradesh\_\_28301 | nss_2007_08 | 28301 | Andhra Pardesh | Adilabad | andhra pradesh | adilabad | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08\_\_andhra pradesh\_\_28302 | nss_2007_08 | 28302 | Andhra Pardesh | Nizamabad | andhra pradesh | nizamabad | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08\_\_andhra pradesh\_\_28304 | nss_2007_08 | 28304 | Andhra Pardesh | Medak | andhra pradesh | medak | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08\_\_andhra pradesh\_\_28305 | nss_2007_08 | 28305 | Andhra Pardesh | Hyderabad | andhra pradesh | hyderabad | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08\_\_andhra pradesh\_\_28306 | nss_2007_08 | 28306 | Andhra Pardesh | Rangareddy | andhra pradesh | rangareddy | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08\_\_andhra pradesh\_\_28307 | nss_2007_08 | 28307 | Andhra Pardesh | Mahbubnagar | andhra pradesh | mahbubnagar | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08\_\_andhra pradesh\_\_28403 | nss_2007_08 | 28403 | Andhra Pardesh | Karimnagar | andhra pradesh | karimnagar | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08\_\_andhra pradesh\_\_28408 | nss_2007_08 | 28408 | Andhra Pardesh | Nalgonda | andhra pradesh | nalgonda | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08\_\_andhra pradesh\_\_28409 | nss_2007_08 | 28409 | Andhra Pardesh | Warangal | andhra pradesh | warangal | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08\_\_andhra pradesh\_\_28410 | nss_2007_08 | 28410 | Andhra Pardesh | Khammam | andhra pradesh | khammam | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08\_\_andhra pradesh\_\_28520 | nss_2007_08 | 28520 | Andhra Pardesh | Cuddapah | andhra pradesh | cuddapah | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08\_\_andhra pradesh\_\_28521 | nss_2007_08 | 28521 | Andhra Pardesh | Kurnool | andhra pradesh | kurnool | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08\_\_andhra pradesh\_\_28522 | nss_2007_08 | 28522 | Andhra Pardesh | Anantpur | andhra pradesh | anantpur | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08\_\_andhra pradesh\_\_28523 | nss_2007_08 | 28523 | Andhra Pardesh | Chittoor | andhra pradesh | chittoor | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08\_\_arunachal pradesh\_\_12101 | nss_2007_08 | 12101 | Arunachal Pradesh | Tawang | arunachal pradesh | tawang | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08\_\_arunachal pradesh\_\_12102 | nss_2007_08 | 12102 | Arunachal Pradesh | West Kameng | arunachal pradesh | west kameng | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08\_\_arunachal pradesh\_\_12103 | nss_2007_08 | 12103 | Arunachal Pradesh | East Kameng | arunachal pradesh | east kameng | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08\_\_arunachal pradesh\_\_12104 | nss_2007_08 | 12104 | Arunachal Pradesh | Papum Pare | arunachal pradesh | papum pare | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08\_\_arunachal pradesh\_\_12105 | nss_2007_08 | 12105 | Arunachal Pradesh | Lower Subansiri | arunachal pradesh | lower subansiri | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08\_\_arunachal pradesh\_\_12106 | nss_2007_08 | 12106 | Arunachal Pradesh | Upper Subansiri | arunachal pradesh | upper subansiri | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08\_\_arunachal pradesh\_\_12107 | nss_2007_08 | 12107 | Arunachal Pradesh | West Siang | arunachal pradesh | west siang | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08\_\_arunachal pradesh\_\_12108 | nss_2007_08 | 12108 | Arunachal Pradesh | East Siang | arunachal pradesh | east siang | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08\_\_arunachal pradesh\_\_12109 | nss_2007_08 | 12109 | Arunachal Pradesh | Upper Siang | arunachal pradesh | upper siang | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08\_\_arunachal pradesh\_\_12110 | nss_2007_08 | 12110 | Arunachal Pradesh | Dibang Valley | arunachal pradesh | dibang valley | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08\_\_arunachal pradesh\_\_12111 | nss_2007_08 | 12111 | Arunachal Pradesh | Lohit | arunachal pradesh | lohit | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08\_\_arunachal pradesh\_\_12112 | nss_2007_08 | 12112 | Arunachal Pradesh | Changlang | arunachal pradesh | changlang | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08\_\_arunachal pradesh\_\_12113 | nss_2007_08 | 12113 | Arunachal Pradesh | Tirap | arunachal pradesh | tirap | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08\_\_assam\_\_18112 | nss_2007_08 | 18112 | Assam | Lakhimpur | assam | lakhimpur | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08\_\_assam\_\_18113 | nss_2007_08 | 18113 | Assam | Dhemaji | assam | dhemaji | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08\_\_assam\_\_18114 | nss_2007_08 | 18114 | Assam | Tinsukia | assam | tinsukia | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08\_\_assam\_\_18115 | nss_2007_08 | 18115 | Assam | Dibrugarh | assam | dibrugarh | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08\_\_assam\_\_18116 | nss_2007_08 | 18116 | Assam | Sibsagar | assam | sibsagar | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08\_\_assam\_\_18117 | nss_2007_08 | 18117 | Assam | Jorhat | assam | jorhat | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08\_\_assam\_\_18118 | nss_2007_08 | 18118 | Assam | Golaghat | assam | golaghat | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08\_\_assam\_\_18201 | nss_2007_08 | 18201 | Assam | Kokrajhar | assam | kokrajhar | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08\_\_assam\_\_18202 | nss_2007_08 | 18202 | Assam | Dhubri | assam | dhubri | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08\_\_assam\_\_18203 | nss_2007_08 | 18203 | Assam | Goalpara | assam | goalpara | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| NA | NA | NA | NA | NA | NA | nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08\_\_assam\_\_18204 | nss_2007_08 | 18204 | Assam | Bongaigaon | assam | bongaigaon | NA | NA | NA | NA | missing_source_unit | NA | NA | NA | NA | NA | unresolved_or_non_nested | FALSE | NA | source_identity_unadjudicated |
| Table truncated in rendered note; full CSV has 1259 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Primary-panel source eligibility

``` r
analysis_table(primary_crosswalk, "Adjudicated deterministic source-to-2001 crosswalk", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Adjudicated deterministic source-to-2001 crosswalk

``` r
analysis_table(excluded_sources, "Excluded or unresolved source rows", max_rows = 50)
```

| source_row_id | wave | source_code | raw_state | raw_district | state_std | district_std | exclusion_reason |
|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | 35101 | Andaman & Nicober | South Andaman | andaman and nicobar islands | south andaman | source_identity_unadjudicated |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | Andaman & Nicober | Nicobars | andaman and nicobar islands | nicobars | source_identity_unadjudicated |
| nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08 | 35103 | Andaman & Nicober | North and Middle Andaman | andaman and nicobar islands | north and middle andaman | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | Andhra Pardesh | Srikakulam | andhra pradesh | srikakulam | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | Andhra Pardesh | Vizianagaram | andhra pradesh | vizianagaram | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | Andhra Pardesh | Visakhapatnam | andhra pradesh | visakhapatnam | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | Andhra Pardesh | East Godavari | andhra pradesh | east godavari | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | 28115 | Andhra Pardesh | West Godawari | andhra pradesh | west godawari | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | Andhra Pardesh | Krishna | andhra pradesh | krishna | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | Andhra Pardesh | Guntur | andhra pradesh | guntur | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | Andhra Pardesh | Prakasam | andhra pradesh | prakasam | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | 28219 | Andhra Pardesh | Nellore | andhra pradesh | nellore | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | Andhra Pardesh | Adilabad | andhra pradesh | adilabad | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | Andhra Pardesh | Nizamabad | andhra pradesh | nizamabad | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | Andhra Pardesh | Medak | andhra pradesh | medak | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | Andhra Pardesh | Hyderabad | andhra pradesh | hyderabad | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | 28306 | Andhra Pardesh | Rangareddy | andhra pradesh | rangareddy | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | Andhra Pardesh | Mahbubnagar | andhra pradesh | mahbubnagar | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | Andhra Pardesh | Karimnagar | andhra pradesh | karimnagar | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | Andhra Pardesh | Nalgonda | andhra pradesh | nalgonda | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | Andhra Pardesh | Warangal | andhra pradesh | warangal | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | Andhra Pardesh | Khammam | andhra pradesh | khammam | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | 28520 | Andhra Pardesh | Cuddapah | andhra pradesh | cuddapah | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | Andhra Pardesh | Kurnool | andhra pradesh | kurnool | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | 28522 | Andhra Pardesh | Anantpur | andhra pradesh | anantpur | source_identity_unadjudicated |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | Andhra Pardesh | Chittoor | andhra pradesh | chittoor | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | Arunachal Pradesh | Tawang | arunachal pradesh | tawang | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | Arunachal Pradesh | West Kameng | arunachal pradesh | west kameng | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | Arunachal Pradesh | East Kameng | arunachal pradesh | east kameng | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | Arunachal Pradesh | Papum Pare | arunachal pradesh | papum pare | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | Arunachal Pradesh | Lower Subansiri | arunachal pradesh | lower subansiri | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | Arunachal Pradesh | Upper Subansiri | arunachal pradesh | upper subansiri | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | Arunachal Pradesh | West Siang | arunachal pradesh | west siang | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | Arunachal Pradesh | East Siang | arunachal pradesh | east siang | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | Arunachal Pradesh | Upper Siang | arunachal pradesh | upper siang | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | Arunachal Pradesh | Dibang Valley | arunachal pradesh | dibang valley | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | Arunachal Pradesh | Lohit | arunachal pradesh | lohit | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | Arunachal Pradesh | Changlang | arunachal pradesh | changlang | source_identity_unadjudicated |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | Arunachal Pradesh | Tirap | arunachal pradesh | tirap | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | Assam | Lakhimpur | assam | lakhimpur | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | Assam | Dhemaji | assam | dhemaji | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08 | 18114 | Assam | Tinsukia | assam | tinsukia | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | Assam | Dibrugarh | assam | dibrugarh | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08 | 18116 | Assam | Sibsagar | assam | sibsagar | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | Assam | Jorhat | assam | jorhat | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | Assam | Golaghat | assam | golaghat | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | Assam | Kokrajhar | assam | kokrajhar | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | Assam | Dhubri | assam | dhubri | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | Assam | Goalpara | assam | goalpara | source_identity_unadjudicated |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | Assam | Bongaigaon | assam | bongaigaon | source_identity_unadjudicated |
| Table truncated in rendered note; full CSV has 1259 rows. |  |  |  |  |  |  |  |

Excluded or unresolved source rows

``` r
analysis_table(adjudication_queue, "Prioritized source-adjudication queue", max_rows = 50)
```

| source_row_id | wave | source_code | raw_state | raw_district | state_std | district_std | adjudication_status | candidate_count | candidate_name_count | exact_vintage_count | recommended_unit | recommended_name | recommended_vintage | recommended_method | recommended_score | high_precision_candidate | review_class | review_priority |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | Andaman & Nicober | Nicobars | andaman and nicobar islands | nicobars | NA | 3 | 1 | 3 | pc2001\_\_35\_\_02 | nicobars | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08 | 35103 | Andaman & Nicober | North and Middle Andaman | andaman and nicobar islands | north and middle andaman | NA | 2 | 1 | 2 | pc2011\_\_35\_\_639 | north and middle andaman | 2011 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | Andhra Pardesh | Adilabad | andhra pradesh | adilabad | NA | 2 | 1 | 2 | pc2001\_\_28\_\_01 | adilabad | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | Andhra Pardesh | Chittoor | andhra pradesh | chittoor | NA | 3 | 1 | 3 | pc2001\_\_28\_\_23 | chittoor | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | Andhra Pardesh | East Godavari | andhra pradesh | east godavari | NA | 3 | 1 | 3 | pc2001\_\_28\_\_14 | east godavari | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | Andhra Pardesh | Guntur | andhra pradesh | guntur | NA | 3 | 1 | 3 | pc2001\_\_28\_\_17 | guntur | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | Andhra Pardesh | Hyderabad | andhra pradesh | hyderabad | NA | 2 | 1 | 2 | pc2001\_\_28\_\_05 | hyderabad | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | Andhra Pardesh | Karimnagar | andhra pradesh | karimnagar | NA | 2 | 1 | 2 | pc2001\_\_28\_\_03 | karimnagar | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | Andhra Pardesh | Khammam | andhra pradesh | khammam | NA | 2 | 1 | 2 | pc2001\_\_28\_\_10 | khammam | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | Andhra Pardesh | Krishna | andhra pradesh | krishna | NA | 3 | 1 | 3 | pc2001\_\_28\_\_16 | krishna | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | Andhra Pardesh | Kurnool | andhra pradesh | kurnool | NA | 3 | 1 | 3 | pc2001\_\_28\_\_21 | kurnool | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | Andhra Pardesh | Mahbubnagar | andhra pradesh | mahbubnagar | NA | 2 | 1 | 2 | pc2001\_\_28\_\_07 | mahbubnagar | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | Andhra Pardesh | Medak | andhra pradesh | medak | NA | 2 | 1 | 2 | pc2001\_\_28\_\_04 | medak | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | Andhra Pardesh | Nalgonda | andhra pradesh | nalgonda | NA | 2 | 1 | 2 | pc2001\_\_28\_\_08 | nalgonda | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | Andhra Pardesh | Nizamabad | andhra pradesh | nizamabad | NA | 2 | 1 | 2 | pc2001\_\_28\_\_02 | nizamabad | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | Andhra Pardesh | Prakasam | andhra pradesh | prakasam | NA | 3 | 1 | 3 | pc2001\_\_28\_\_18 | prakasam | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | Andhra Pardesh | Srikakulam | andhra pradesh | srikakulam | NA | 3 | 1 | 3 | pc2001\_\_28\_\_11 | srikakulam | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | Andhra Pardesh | Visakhapatnam | andhra pradesh | visakhapatnam | NA | 3 | 1 | 3 | pc2001\_\_28\_\_13 | visakhapatnam | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | Andhra Pardesh | Vizianagaram | andhra pradesh | vizianagaram | NA | 3 | 1 | 3 | pc2001\_\_28\_\_12 | vizianagaram | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | Andhra Pardesh | Warangal | andhra pradesh | warangal | NA | 2 | 1 | 2 | pc2001\_\_28\_\_09 | warangal | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | Arunachal Pradesh | Changlang | arunachal pradesh | changlang | NA | 3 | 1 | 3 | pc2001\_\_12\_\_12 | changlang | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | Arunachal Pradesh | Dibang Valley | arunachal pradesh | dibang valley | NA | 3 | 1 | 3 | pc2001\_\_12\_\_10 | dibang valley | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | Arunachal Pradesh | East Kameng | arunachal pradesh | east kameng | NA | 3 | 1 | 3 | pc2001\_\_12\_\_03 | east kameng | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | Arunachal Pradesh | East Siang | arunachal pradesh | east siang | NA | 3 | 1 | 3 | pc2001\_\_12\_\_08 | east siang | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | Arunachal Pradesh | Lohit | arunachal pradesh | lohit | NA | 3 | 1 | 3 | pc2001\_\_12\_\_11 | lohit | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | Arunachal Pradesh | Lower Subansiri | arunachal pradesh | lower subansiri | NA | 3 | 1 | 3 | pc2001\_\_12\_\_05 | lower subansiri | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | Arunachal Pradesh | Papum Pare | arunachal pradesh | papum pare | NA | 3 | 1 | 3 | pc2001\_\_12\_\_04 | papum pare | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | Arunachal Pradesh | Tawang | arunachal pradesh | tawang | NA | 3 | 1 | 3 | pc2001\_\_12\_\_01 | tawang | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | Arunachal Pradesh | Tirap | arunachal pradesh | tirap | NA | 3 | 1 | 3 | pc2001\_\_12\_\_13 | tirap | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | Arunachal Pradesh | Upper Siang | arunachal pradesh | upper siang | NA | 3 | 1 | 3 | pc2001\_\_12\_\_09 | upper siang | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | Arunachal Pradesh | Upper Subansiri | arunachal pradesh | upper subansiri | NA | 3 | 1 | 3 | pc2001\_\_12\_\_06 | upper subansiri | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | Arunachal Pradesh | West Kameng | arunachal pradesh | west kameng | NA | 3 | 1 | 3 | pc2001\_\_12\_\_02 | west kameng | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | Arunachal Pradesh | West Siang | arunachal pradesh | west siang | NA | 3 | 1 | 3 | pc2001\_\_12\_\_07 | west siang | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18205\_\_barpeta | nss_2007_08 | 18205 | Assam | Barpeta | assam | barpeta | NA | 3 | 1 | 3 | pc2001\_\_18\_\_05 | barpeta | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | Assam | Bongaigaon | assam | bongaigaon | NA | 3 | 1 | 3 | pc2001\_\_18\_\_04 | bongaigaon | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18321\_\_cachar | nss_2007_08 | 18321 | Assam | Cachar | assam | cachar | NA | 3 | 1 | 3 | pc2001\_\_18\_\_21 | cachar | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18408\_\_darrang | nss_2007_08 | 18408 | Assam | Darrang | assam | darrang | NA | 3 | 1 | 3 | pc2001\_\_18\_\_08 | darrang | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | Assam | Dhemaji | assam | dhemaji | NA | 3 | 1 | 3 | pc2001\_\_18\_\_13 | dhemaji | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | Assam | Dhubri | assam | dhubri | NA | 3 | 1 | 3 | pc2001\_\_18\_\_02 | dhubri | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | Assam | Dibrugarh | assam | dibrugarh | NA | 3 | 1 | 3 | pc2001\_\_18\_\_15 | dibrugarh | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | Assam | Goalpara | assam | goalpara | NA | 3 | 1 | 3 | pc2001\_\_18\_\_03 | goalpara | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | Assam | Golaghat | assam | golaghat | NA | 3 | 1 | 3 | pc2001\_\_18\_\_18 | golaghat | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18323\_\_hailakandi | nss_2007_08 | 18323 | Assam | Hailakandi | assam | hailakandi | NA | 3 | 1 | 3 | pc2001\_\_18\_\_23 | hailakandi | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | Assam | Jorhat | assam | jorhat | NA | 3 | 1 | 3 | pc2001\_\_18\_\_17 | jorhat | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18206\_\_kamrup | nss_2007_08 | 18206 | Assam | Kamrup | assam | kamrup | NA | 3 | 1 | 3 | pc2001\_\_18\_\_06 | kamrup | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18319\_\_karbi anglong | nss_2007_08 | 18319 | Assam | Karbi Anglong | assam | karbi anglong | NA | 3 | 1 | 3 | pc2001\_\_18\_\_19 | karbi anglong | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | Assam | Kokrajhar | assam | kokrajhar | NA | 3 | 1 | 3 | pc2001\_\_18\_\_01 | kokrajhar | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | Assam | Lakhimpur | assam | lakhimpur | NA | 3 | 1 | 3 | pc2001\_\_18\_\_12 | lakhimpur | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18409\_\_marigaon | nss_2007_08 | 18409 | Assam | Marigaon | assam | marigaon | NA | 2 | 1 | 2 | pc2001\_\_18\_\_09 | marigaon | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| nss_2007_08\_\_assam\_\_18410\_\_nagaon | nss_2007_08 | 18410 | Assam | Nagaon | assam | nagaon | NA | 3 | 1 | 3 | pc2001\_\_18\_\_10 | nagaon | 2001 | exact_normalized_name | 1 | FALSE | cross_vintage_exact_candidate | 1 |
| Table truncated in rendered note; full CSV has 1259 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Prioritized source-adjudication queue

``` r
analysis_table(adjudication_draft, "Review-ready adjudication draft; all rows remain needs_review", max_rows = 50)
```

| source_row_id | wave | raw_state | raw_district | unit_id | method | source_id | status | note |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | Andaman & Nicober | South Andaman | pc2011\_\_35\_\_640 | proposed_exact_normalized_name | shrug_pc11_district_geometry | needs_review | Generated review draft: single_vintage_exact_candidate; preferred reference vintage=2011. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | Andaman & Nicober | Nicobars | pc2001\_\_35\_\_02 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08 | Andaman & Nicober | North and Middle Andaman | pc2011\_\_35\_\_639 | proposed_exact_normalized_name | shrug_pc11_district_geometry | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2011. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | Andhra Pardesh | Srikakulam | pc2001\_\_28\_\_11 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | Andhra Pardesh | Vizianagaram | pc2001\_\_28\_\_12 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | Andhra Pardesh | Visakhapatnam | pc2001\_\_28\_\_13 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | Andhra Pardesh | East Godavari | pc2001\_\_28\_\_14 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | Andhra Pardesh | West Godawari | pc2001\_\_28\_\_15 | proposed_fuzzy_name_candidate | census2001_c16 | needs_review | Generated review draft: high_precision_fuzzy_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | Andhra Pardesh | Krishna | pc2001\_\_28\_\_16 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | Andhra Pardesh | Guntur | pc2001\_\_28\_\_17 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | Andhra Pardesh | Prakasam | pc2001\_\_28\_\_18 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | Andhra Pardesh | Nellore | pc2001\_\_28\_\_19 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: single_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | Andhra Pardesh | Adilabad | pc2001\_\_28\_\_01 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | Andhra Pardesh | Nizamabad | pc2001\_\_28\_\_02 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | Andhra Pardesh | Medak | pc2001\_\_28\_\_04 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | Andhra Pardesh | Hyderabad | pc2001\_\_28\_\_05 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | Andhra Pardesh | Rangareddy | pc2011\_\_28\_\_537 | proposed_exact_normalized_name | shrug_pc11_district_geometry | needs_review | Generated review draft: single_vintage_exact_candidate; preferred reference vintage=2011. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | Andhra Pardesh | Mahbubnagar | pc2001\_\_28\_\_07 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | Andhra Pardesh | Karimnagar | pc2001\_\_28\_\_03 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | Andhra Pardesh | Nalgonda | pc2001\_\_28\_\_08 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | Andhra Pardesh | Warangal | pc2001\_\_28\_\_09 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | Andhra Pardesh | Khammam | pc2001\_\_28\_\_10 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | Andhra Pardesh | Cuddapah | pc2001\_\_28\_\_20 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: single_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | Andhra Pardesh | Kurnool | pc2001\_\_28\_\_21 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | Andhra Pardesh | Anantpur | pc2001\_\_28\_\_22 | proposed_fuzzy_name_candidate | census2001_c16 | needs_review | Generated review draft: high_precision_fuzzy_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | Andhra Pardesh | Chittoor | pc2001\_\_28\_\_23 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | Arunachal Pradesh | Tawang | pc2001\_\_12\_\_01 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | Arunachal Pradesh | West Kameng | pc2001\_\_12\_\_02 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | Arunachal Pradesh | East Kameng | pc2001\_\_12\_\_03 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | Arunachal Pradesh | Papum Pare | pc2001\_\_12\_\_04 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | Arunachal Pradesh | Lower Subansiri | pc2001\_\_12\_\_05 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | Arunachal Pradesh | Upper Subansiri | pc2001\_\_12\_\_06 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | Arunachal Pradesh | West Siang | pc2001\_\_12\_\_07 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | Arunachal Pradesh | East Siang | pc2001\_\_12\_\_08 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | Arunachal Pradesh | Upper Siang | pc2001\_\_12\_\_09 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | Arunachal Pradesh | Dibang Valley | pc2001\_\_12\_\_10 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | Arunachal Pradesh | Lohit | pc2001\_\_12\_\_11 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | Arunachal Pradesh | Changlang | pc2001\_\_12\_\_12 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | Arunachal Pradesh | Tirap | pc2001\_\_12\_\_13 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | Assam | Lakhimpur | pc2001\_\_18\_\_12 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | Assam | Dhemaji | pc2001\_\_18\_\_13 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08 | Assam | Tinsukia | pc2001\_\_18\_\_14 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | Assam | Dibrugarh | pc2001\_\_18\_\_15 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08 | Assam | Sibsagar | pc2001\_\_18\_\_16 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: single_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | Assam | Jorhat | pc2001\_\_18\_\_17 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | Assam | Golaghat | pc2001\_\_18\_\_18 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | Assam | Kokrajhar | pc2001\_\_18\_\_01 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | Assam | Dhubri | pc2001\_\_18\_\_02 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | Assam | Goalpara | pc2001\_\_18\_\_03 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | Assam | Bongaigaon | pc2001\_\_18\_\_04 | proposed_exact_normalized_name | census2001_c16 | needs_review | Generated review draft: cross_vintage_exact_candidate; preferred reference vintage=2001. Confirm administrative continuity and source evidence before changing status. |
| Table truncated in rendered note; full CSV has 1259 rows. |  |  |  |  |  |  |  |  |

Review-ready adjudication draft; all rows remain needs_review

``` r
analysis_table(sensitivity_crosswalk, "Preferred and accepted sensitivity crosswalk rows", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Preferred and accepted sensitivity crosswalk rows

``` r
analysis_table(production_comparison, "Accepted v2 mappings compared with production", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Accepted v2 mappings compared with production

``` r
analysis_table(geometry_qa, "Census 2001 geometry readiness and QA", max_rows = 50)
```

| metric                    | value |
|:--------------------------|------:|
| geometry_available        |     1 |
| geometry_rows             |   582 |
| expected_admin_units      |   593 |
| missing_admin_units       |    11 |
| unexpected_geometry_units |     0 |
| invalid_geometries        |   279 |

Census 2001 geometry readiness and QA

``` r
analysis_table(candidates, "Top source-match candidates; never automatic adjudications", max_rows = 50)
```

| source_row_id | wave | source_code | state_std | source_name_raw | source_name | candidate_unit | candidate_name | candidate_source_id | reference_vintage | candidate_method | rank | jw | dl | trigram | token | score | margin | reciprocal_nearest | high_precision_candidate |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | 35101 | andaman and nicobar islands | South Andaman | south andaman | pc2011\_\_35\_\_640 | south andaman | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | TRUE | FALSE |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | andaman and nicobar islands | Nicobars | nicobars | pc2001\_\_35\_\_02 | nicobars | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | andaman and nicobar islands | Nicobars | nicobars | pc2011\_\_35\_\_638 | nicobars | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | andaman and nicobar islands | Nicobars | nicobars | lgd_district\_\_603 | nicobars | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08 | 35103 | andaman and nicobar islands | North and Middle Andaman | north and middle andaman | pc2011\_\_35\_\_639 | north and middle andaman | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08 | 35103 | andaman and nicobar islands | North and Middle Andaman | north and middle andaman | lgd_district\_\_632 | north and middle andaman | lgd_districts | current_lgd | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | andhra pradesh | Srikakulam | srikakulam | pc2001\_\_28\_\_11 | srikakulam | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | andhra pradesh | Srikakulam | srikakulam | pc2011\_\_28\_\_542 | srikakulam | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | andhra pradesh | Srikakulam | srikakulam | lgd_district\_\_519 | srikakulam | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | andhra pradesh | Vizianagaram | vizianagaram | pc2001\_\_28\_\_12 | vizianagaram | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | andhra pradesh | Vizianagaram | vizianagaram | pc2011\_\_28\_\_543 | vizianagaram | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | andhra pradesh | Vizianagaram | vizianagaram | lgd_district\_\_521 | vizianagaram | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | andhra pradesh | Visakhapatnam | visakhapatnam | pc2001\_\_28\_\_13 | visakhapatnam | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | andhra pradesh | Visakhapatnam | visakhapatnam | pc2011\_\_28\_\_544 | visakhapatnam | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | andhra pradesh | Visakhapatnam | visakhapatnam | lgd_district\_\_520 | visakhapatnam | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | andhra pradesh | East Godavari | east godavari | pc2001\_\_28\_\_14 | east godavari | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | andhra pradesh | East Godavari | east godavari | pc2011\_\_28\_\_545 | east godavari | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | andhra pradesh | East Godavari | east godavari | lgd_district\_\_505 | east godavari | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | andhra pradesh | Krishna | krishna | pc2001\_\_28\_\_16 | krishna | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | andhra pradesh | Krishna | krishna | pc2011\_\_28\_\_547 | krishna | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | andhra pradesh | Krishna | krishna | lgd_district\_\_510 | krishna | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | andhra pradesh | Guntur | guntur | pc2001\_\_28\_\_17 | guntur | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | andhra pradesh | Guntur | guntur | pc2011\_\_28\_\_548 | guntur | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | andhra pradesh | Guntur | guntur | lgd_district\_\_506 | guntur | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | andhra pradesh | Prakasam | prakasam | pc2001\_\_28\_\_18 | prakasam | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | andhra pradesh | Prakasam | prakasam | pc2011\_\_28\_\_549 | prakasam | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | andhra pradesh | Prakasam | prakasam | lgd_district\_\_517 | prakasam | lgd_districts | current_lgd | exact_normalized_name | 3 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | 28219 | andhra pradesh | Nellore | nellore | pc2001\_\_28\_\_19 | nellore | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | TRUE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | andhra pradesh | Adilabad | adilabad | pc2001\_\_28\_\_01 | adilabad | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | andhra pradesh | Adilabad | adilabad | pc2011\_\_28\_\_532 | adilabad | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | andhra pradesh | Nizamabad | nizamabad | pc2001\_\_28\_\_02 | nizamabad | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | andhra pradesh | Nizamabad | nizamabad | pc2011\_\_28\_\_533 | nizamabad | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | andhra pradesh | Medak | medak | pc2001\_\_28\_\_04 | medak | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | andhra pradesh | Medak | medak | pc2011\_\_28\_\_535 | medak | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | andhra pradesh | Hyderabad | hyderabad | pc2001\_\_28\_\_05 | hyderabad | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | andhra pradesh | Hyderabad | hyderabad | pc2011\_\_28\_\_536 | hyderabad | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | 28306 | andhra pradesh | Rangareddy | rangareddy | pc2011\_\_28\_\_537 | rangareddy | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | TRUE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | andhra pradesh | Mahbubnagar | mahbubnagar | pc2001\_\_28\_\_07 | mahbubnagar | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | andhra pradesh | Mahbubnagar | mahbubnagar | pc2011\_\_28\_\_538 | mahbubnagar | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | andhra pradesh | Karimnagar | karimnagar | pc2001\_\_28\_\_03 | karimnagar | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | andhra pradesh | Karimnagar | karimnagar | pc2011\_\_28\_\_534 | karimnagar | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | andhra pradesh | Nalgonda | nalgonda | pc2001\_\_28\_\_08 | nalgonda | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | andhra pradesh | Nalgonda | nalgonda | pc2011\_\_28\_\_539 | nalgonda | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | andhra pradesh | Warangal | warangal | pc2001\_\_28\_\_09 | warangal | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | andhra pradesh | Warangal | warangal | pc2011\_\_28\_\_540 | warangal | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | andhra pradesh | Khammam | khammam | pc2001\_\_28\_\_10 | khammam | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | andhra pradesh | Khammam | khammam | pc2011\_\_28\_\_541 | khammam | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | 28520 | andhra pradesh | Cuddapah | cuddapah | pc2001\_\_28\_\_20 | cuddapah | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | TRUE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | andhra pradesh | Kurnool | kurnool | pc2001\_\_28\_\_21 | kurnool | census2001_c16 | 2001 | exact_normalized_name | 1 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | andhra pradesh | Kurnool | kurnool | pc2011\_\_28\_\_552 | kurnool | shrug_pc11_district_geometry | 2011 | exact_normalized_name | 2 | 1 | 1 | 1 | 1 | 1 | NA | FALSE | FALSE |
| Table truncated in rendered note; full CSV has 4051 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Top source-match candidates; never automatic adjudications

``` r
analysis_table(events, "Candidate administrative events", max_rows = 50)
```

| event_id | effective_date | reported_year | source_year | target_year | date_precision | event_type | from_state | from_district | to_state | to_district | source_id | status | note |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| candidate\_\_alluvial\_\_3271 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Arunachal Pradesh | Lower Subansiri | Arunachal Pradesh | Papum Pare | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3284 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Arunachal Pradesh | East Siang | Arunachal Pradesh | Upper Siang | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3338 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Sitamarhi | Bihar | Sheohar | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3341 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Saharsa | Bihar | Supaul | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3358 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Bhagalpur | Bihar | Banka | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3360 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Munger | Bihar | Lakhisarai | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3361 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Munger | Bihar | Sheikhpura | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3365 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Bhojpur | Bihar | Buxar | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3366 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Rohtas | Bihar | Kaimur (Bhabua) | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3370 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Nawadah | Bihar | Nawada | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3371 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Bihar | Munger | Bihar | Jamui | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3375 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Surguja | Chhattisgarh | Koriya | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3376 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Surguja | Chhattisgarh | Surguja | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3377 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raigarh | Chhattisgarh | Jashpur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3378 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raigarh | Chhattisgarh | Raigarh | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3379 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bilaspur | Chhattisgarh | Korba | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3380 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bilaspur | Chhattisgarh | Janjgir-Champa | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3381 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bilaspur | Chhattisgarh | Bilaspur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3382 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raj Nandgaon | Chhattisgarh | Kawardha | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3383 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raj Nandgaon | Chhattisgarh | Rajnandgaon | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3384 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Durg | Chhattisgarh | Durg | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3385 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raipur | Chhattisgarh | Raipur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3386 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raipur | Chhattisgarh | Mahasamund | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3387 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raipur | Chhattisgarh | Dhamtari | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3388 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Kanker | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3389 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Bastar | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3390 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Bastar | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3391 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Dantewada | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3392 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Dantewada | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3393 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Surguja | Chhattisgarh | Surguja | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3394 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Surguja | Chhattisgarh | Surguja | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3395 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bilaspur | Chhattisgarh | Bilaspur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3396 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Durg | Chhattisgarh | Durg | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3397 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Durg | Chhattisgarh | Durg | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3398 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Dantewada | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3399 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bastar | Chhattisgarh | Bastar | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3400 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raipur | Chhattisgarh | Raipur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3401 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Raipur | Chhattisgarh | Raipur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3402 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Madhya Pradesh | Bilaspur | Chhattisgarh | Bilaspur | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3406 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Delhi | Delhi | Delhi | North West | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3407 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Delhi | Delhi | Delhi | South | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3408 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Delhi | Delhi | Delhi | North East | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3409 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Delhi | Delhi | Delhi | East | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3410 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Delhi | Delhi | Delhi | South West | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3419 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Gujarat | Bharuch | Gujarat | Narmada | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3423 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Gujarat | Junagadh | Gujarat | Porbandar | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3424 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Gujarat | Kheda | Gujarat | Anand | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3430 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Gujarat | Mahesana | Gujarat | Patan | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3431 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Gujarat | Panch Mahals | Gujarat | Dohad | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| candidate\_\_alluvial\_\_3435 | NA | NA | 1991 | 2001 | source_target_interval | lineage_candidate | Gujarat | Valsad | Gujarat | Navsari | alluvial | candidate_unadjudicated | Tracker relation is candidate evidence only; verify the date and territorial content before acceptance. |
| Table truncated in rendered note; full CSV has 1655 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |

Candidate administrative events

``` r
analysis_table(current_components, "Current LGD subdistrict and urban-local-body registry", max_rows = 50)
```

| level | state_lgd_code | state_name | district_lgd_code | district_name | entity_code | entity_name | census2011_code | source_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| subdistrict | 8 | Rajasthan | 783 | Jaipur (Gramin) | 7140 | Aandhi | 0 | lgd_subdistricts |
| subdistrict | 24 | Gujarat | 449 | Kachchh | 3728 | Abdasa | 3728 | lgd_subdistricts |
| subdistrict | 22 | Chhattisgarh | 387 | Raipur | 3331 | Abhanpur | 3331 | lgd_subdistricts |
| subdistrict | 24 | Gujarat | 459 | Surat | 6518 | Adajan | 0 | lgd_subdistricts |
| subdistrict | 21 | Odisha | 353 | Gajapati | 3100 | Adava | 3100 | lgd_subdistricts |
| subdistrict | 36 | Telangana | 514 | Nalgonda | 6304 | Adavidevulapalli | 0 | lgd_subdistricts |
| subdistrict | 36 | Telangana | 697 | Yadadri Bhuvanagiri | 6310 | Adda Guduru | 0 | lgd_subdistricts |
| subdistrict | 23 | Madhya Pradesh | 411 | Jabalpur | 7024 | Adhartal | 0 | lgd_subdistricts |
| subdistrict | 20 | Jharkhand | 341 | Saraikela Kharsawan | 2752 | Adityapur(Gamharia) | 2752 | lgd_subdistricts |
| subdistrict | 28 | Andhra Pradesh | 511 | Kurnool | 5271 | Adoni | 5271 | lgd_subdistricts |
| subdistrict | 18 | Assam | 285 | Dhubri | 2006 | Agamoni | 2006 | lgd_subdistricts |
| subdistrict | 21 | Odisha | 348 | Bhadrak | 2910 | Agarpada | 2910 | lgd_subdistricts |
| subdistrict | 24 | Gujarat | 444 | Dangs | 3921 | Ahwa | 3921 | lgd_subdistricts |
| subdistrict | 22 | Chhattisgarh | 379 | Janjgir-Champa | 3278 | Akaltara | 3278 | lgd_subdistricts |
| subdistrict | 9 | Uttar Pradesh | 121 | Ambedkar Nagar | 910 | Akbarpur | 910 | lgd_subdistricts |
| subdistrict | 1 | Jammu And Kashmir | 5 | Jammu | 78 | Akhnoor | 78 | lgd_subdistricts |
| subdistrict | 27 | Maharashtra | 467 | Akola | 3991 | Akola | 3991 | lgd_subdistricts |
| subdistrict | 13 | Nagaland | 251 | Zunheboto | 1768 | Akuluto | 1768 | lgd_subdistricts |
| subdistrict | 33 | Tamil Nadu | 568 | Chennai | 5704 | Alandur | 5704 | lgd_subdistricts |
| subdistrict | 33 | Tamil Nadu | 581 | Perambalur | 5951 | Alathur | 0 | lgd_subdistricts |
| subdistrict | 9 | Uttar Pradesh | 120 | Prayagraj | 890 | Allahabad | 890 | lgd_subdistricts |
| subdistrict | 8 | Rajasthan | 87 | Alwar | 499 | Alwar | 499 | lgd_subdistricts |
| subdistrict | 27 | Maharashtra | 478 | Jalgaon | 3969 | Amalner | 3969 | lgd_subdistricts |
| subdistrict | 36 | Telangana | 518 | Ranga Reddy | 4563 | Amangal | 4563 | lgd_subdistricts |
| subdistrict | 10 | Bihar | 190 | Banka | 1346 | Amarpur | 1346 | lgd_subdistricts |
| subdistrict | 21 | Odisha | 370 | Rayagada | 3172 | Ambadala | 3172 | lgd_subdistricts |
| subdistrict | 16 | Tripura | 269 | Dhalai | 6697 | Ambassa | 0 | lgd_subdistricts |
| subdistrict | 33 | Tamil Nadu | 568 | Chennai | 5700 | Ambattur | 5700 | lgd_subdistricts |
| subdistrict | 27 | Maharashtra | 490 | Pune | 4188 | Ambegaon | 4188 | lgd_subdistricts |
| subdistrict | 27 | Maharashtra | 470 | Beed | 4225 | Ambejogai | 4225 | lgd_subdistricts |
| subdistrict | 33 | Tamil Nadu | 732 | Tirupathur | 5719 | Ambur | 5719 | lgd_subdistricts |
| subdistrict | 10 | Bihar | 201 | Katihar | 1157 | Amdabad | 1157 | lgd_subdistricts |
| subdistrict | 8 | Rajasthan | 112 | Rajsamand | 641 | Amet | 641 | lgd_subdistricts |
| subdistrict | 33 | Tamil Nadu | 568 | Chennai | 7161 | Aminjikarai | 0 | lgd_subdistricts |
| subdistrict | 17 | Meghalaya | 275 | West Jaintia Hills | 1998 | Amlarem | 1998 | lgd_subdistricts |
| subdistrict | 3 | Punjab | 30 | Fatehgarh Sahib | 222 | Amloh | 222 | lgd_subdistricts |
| subdistrict | 3 | Punjab | 27 | Amritsar | 256 | Amritsar -I | 256 | lgd_subdistricts |
| subdistrict | 19 | West Bengal | 313 | Howrah | 2399 | Amta - II | 2399 | lgd_subdistricts |
| subdistrict | 28 | Andhra Pradesh | 744 | Anakapalli | 4871 | Anakapalli | 4871 | lgd_subdistricts |
| subdistrict | 24 | Gujarat | 440 | Anand | 6530 | Anand City | 0 | lgd_subdistricts |
| subdistrict | 24 | Gujarat | 440 | Anand | 3864 | Anand Rural | 3864 | lgd_subdistricts |
| subdistrict | 1 | Jammu And Kashmir | 1 | Anantnag | 55 | Anantnag | 55 | lgd_subdistricts |
| subdistrict | 1 | Jammu And Kashmir | 1 | Anantnag | 6822 | Anantnag East | 0 | lgd_subdistricts |
| subdistrict | 10 | Bihar | 206 | Madhubani | 1096 | Andhratharhi | 1096 | lgd_subdistricts |
| subdistrict | 31 | Lakshadweep | 553 | Lakshadweep District | 5627 | Andrott | 5627 | lgd_subdistricts |
| subdistrict | 27 | Maharashtra | 468 | Amravati | 4003 | Anjangaon Surji | 4003 | lgd_subdistricts |
| subdistrict | 36 | Telangana | 690 | Bhadradri Kothagudem | 6268 | Annapureddypalli | 0 | lgd_subdistricts |
| subdistrict | 29 | Karnataka | 536 | Dharwad | 7080 | Annigeri | 0 | lgd_subdistricts |
| subdistrict | 21 | Odisha | 344 | Anugul | 3011 | Anugul | 3011 | lgd_subdistricts |
| subdistrict | 8 | Rajasthan | 776 | Anupgarh | 462 | Anupgarh | 462 | lgd_subdistricts |
| Table truncated in rendered note; full CSV has 19278 rows. |  |  |  |  |  |  |  |  |

Current LGD subdistrict and urban-local-body registry

``` r
analysis_table(urban_coverage, "Current LGD urban coverage bridge", max_rows = 50)
```

| urban_local_body_code | urban_local_body_name | census2011_urban_code | state_name | district_lgd_code | district_name | subdistrict_lgd_code | subdistrict_name | village_lgd_code | village_name | source_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645532 | Brichgunj (Rv) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645531 | Brookshabad Part (Rv) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645529 | Dollygunj (Rv) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645551 | Garacharma (Ct) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645530 | Minnie Bay Part (Rv) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645527 | Pahargaon Part (Rv) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645550 | Prothrapur (Ct) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 645528 | School Line Part (Rv) | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 5923 | Port Blair | 0 | NA | lgd_urban_coverage |
| 253098 | Port Blair | 804041 | Andaman And Nicobar Islands | 602 | South Andamans | 0 | NA | 0 | NA | lgd_urban_coverage |
| 257868 | Addanki | 0 | Andhra Pradesh | 517 | Prakasam | 5112 | Addanki | 0 | NA | lgd_urban_coverage |
| 251792 | Adoni | 803003 | Andhra Pradesh | 511 | Kurnool | 5271 | Adoni | 0 | NA | lgd_urban_coverage |
| 296256 | Akiveedu | 0 | Andhra Pradesh | 523 | West Godavari | 4975 | Akividu | 0 | NA | lgd_urban_coverage |
| 253272 | Allagadda | 0 | Andhra Pradesh | 755 | Nandyal | 5298 | Allagadda | 0 | NA | lgd_urban_coverage |
| 299713 | Alluru | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5167 | Allur | 591878 | Allur | lgd_urban_coverage |
| 299713 | Alluru | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5167 | Allur | 591875 | Allurupeta | lgd_urban_coverage |
| 299713 | Alluru | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5167 | Allur | 591885 | North Mopuru | lgd_urban_coverage |
| 299713 | Alluru | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5167 | Allur | 591874 | Singapeta | lgd_urban_coverage |
| 299713 | Alluru | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5167 | Allur | 0 | NA | lgd_urban_coverage |
| 299713 | Alluru | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 0 | NA | 0 | NA | lgd_urban_coverage |
| 251699 | Amadalavalasa | 802941 | Andhra Pradesh | 519 | Srikakulam | 4798 | Amadalavalasa | 0 | NA | lgd_urban_coverage |
| 251735 | Amalapuram | 802958 | Andhra Pradesh | 747 | Dr. B.R. Ambedkar Konaseema | 4941 | Amalapuram | 0 | NA | lgd_urban_coverage |
| 248112 | Anantapur | 803009 | Andhra Pradesh | 502 | Ananthapuramu | 5330 | Anantapur Rural | 0 | NA | lgd_urban_coverage |
| 258033 | Atmakur | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5164 | Atmakur | 0 | NA | lgd_urban_coverage |
| 253270 | Atmakur (Kurnool) | 0 | Andhra Pradesh | 755 | Nandyal | 5265 | Atmakur | 0 | NA | lgd_urban_coverage |
| 248111 | Badvel | 802994 | Andhra Pradesh | 504 | Y.S.R. Kadapa | 5210 | Badvel | 0 | NA | lgd_urban_coverage |
| 248111 | Badvel | 802994 | Andhra Pradesh | 504 | Y.S.R. Kadapa | 5211 | Gopavaram | 0 | NA | lgd_urban_coverage |
| 251760 | Bapatla | 802984 | Andhra Pradesh | 750 | Bapatla | 5093 | Bapatla | 0 | NA | lgd_urban_coverage |
| 296260 | Bethamcherla | 0 | Andhra Pradesh | 755 | Nandyal | 5279 | Bethamcherla | 0 | NA | lgd_urban_coverage |
| 251741 | Bhimavaram | 802964 | Andhra Pradesh | 523 | West Godavari | 4984 | Bhimavaram | 0 | NA | lgd_urban_coverage |
| 299714 | B Kothakota | 0 | Andhra Pradesh | 753 | Annamayya | 5391 | Beerongi Kothakota | 596122 | B.Kothakota | lgd_urban_coverage |
| 299714 | B Kothakota | 0 | Andhra Pradesh | 753 | Annamayya | 5391 | Beerongi Kothakota | 0 | NA | lgd_urban_coverage |
| 299714 | B Kothakota | 0 | Andhra Pradesh | 753 | Annamayya | 0 | NA | 0 | NA | lgd_urban_coverage |
| 251702 | Bobbili | 802944 | Andhra Pradesh | 521 | Vizianagaram | 4816 | Bobbili | 0 | NA | lgd_urban_coverage |
| 296328 | Buchireddypalem | 0 | Andhra Pradesh | 515 | Sri Potti Sriramulu Nellore | 5170 | Buchireddipalem | 0 | NA | lgd_urban_coverage |
| 251756 | Chilakaluripeta | 802980 | Andhra Pradesh | 751 | Palnadu | 5070 | Chilakaluripet H/O.Purushotha Patnam | 590191 | Pasumarru (R) | lgd_urban_coverage |
| 251756 | Chilakaluripeta | 802980 | Andhra Pradesh | 751 | Palnadu | 5070 | Chilakaluripet H/O.Purushotha Patnam | 0 | NA | lgd_urban_coverage |
| 251756 | Chilakaluripeta | 802980 | Andhra Pradesh | 751 | Palnadu | 0 | NA | 0 | NA | lgd_urban_coverage |
| 253274 | Chimakurthy | 0 | Andhra Pradesh | 517 | Prakasam | 5128 | Chimakurthy | 0 | NA | lgd_urban_coverage |
| 299715 | Chinthalapudi | 0 | Andhra Pradesh | 748 | Eluru | 4944 | Chintalapudi | 587947 | Chintalapudi | lgd_urban_coverage |
| 299715 | Chinthalapudi | 0 | Andhra Pradesh | 748 | Eluru | 4944 | Chintalapudi | 0 | NA | lgd_urban_coverage |
| 299715 | Chinthalapudi | 0 | Andhra Pradesh | 748 | Eluru | 0 | NA | 0 | NA | lgd_urban_coverage |
| 251763 | Chirala | 802987 | Andhra Pradesh | 750 | Bapatla | 5123 | Chirala | 0 | NA | lgd_urban_coverage |
| 248122 | Chittoor | 803019 | Andhra Pradesh | 503 | Chittoor | 5421 | Chittoor Rural | 0 | NA | lgd_urban_coverage |
| 251782 | Cuddapah | 802998 | Andhra Pradesh | 504 | Y.S.R. Kadapa | 5230 | Kadapa | 0 | NA | lgd_urban_coverage |
| 296261 | Dachepalli | 0 | Andhra Pradesh | 751 | Palnadu | 5045 | Dachepalli | 0 | NA | lgd_urban_coverage |
| 296263 | Darsi | 0 | Andhra Pradesh | 517 | Prakasam | 5114 | Darsi | 0 | NA | lgd_urban_coverage |
| 248115 | Dharmavaram | 803010 | Andhra Pradesh | 754 | Sri Sathya Sai | 5336 | Dharmavaram | 0 | NA | lgd_urban_coverage |
| 253169 | Dhone(M) | 803005 | Andhra Pradesh | 755 | Nandyal | 5289 | Dhone | 0 | NA | lgd_urban_coverage |
| 251739 | Eluru | 802962 | Andhra Pradesh | 748 | Eluru | 4966 | Eluru Rural | 0 | NA | lgd_urban_coverage |
| Table truncated in rendered note; full CSV has 34012 rows. |  |  |  |  |  |  |  |  |  |  |

Current LGD urban coverage bridge

``` r
analysis_table(components, "LGD changed-component roster", max_rows = 50)
```

| level | entity_code | entity_name | state_lgd_code | state_name | district_lgd_code | district_name | subdistrict_lgd_code | subdistrict_name | period_start | period_end | event_type | evidence_status | source_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| district | 233 | Kurung Kumey | 12 | Arunachal Pradesh | 233 | Kurung Kumey | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 234 | Lohit | 12 | Arunachal Pradesh | 234 | Lohit | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 666 | Longding | 12 | Arunachal Pradesh | 666 | Longding | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 678 | Namsai | 12 | Arunachal Pradesh | 678 | Namsai | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 679 | Siang | 12 | Arunachal Pradesh | 679 | Siang | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 239 | Tirap | 12 | Arunachal Pradesh | 239 | Tirap | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 299 | Dima Hasao | 18 | Assam | 299 | Dima Hasao | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 292 | Karbi Anglong | 18 | Assam | 292 | Karbi Anglong | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 710 | West Karbi Anglong | 18 | Assam | 710 | West Karbi Anglong | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 78 | East | 7 | Delhi | 78 | East | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 670 | South East | 7 | Delhi | 670 | South East | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 672 | Arvalli | 24 | Gujarat | 672 | Arvalli | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 676 | Botad | 24 | Gujarat | 676 | Botad | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 668 | Chhotaudepur | 24 | Gujarat | 668 | Chhotaudepur | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 674 | Devbhumi Dwarka | 24 | Gujarat | 674 | Devbhumi Dwarka | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 675 | Gir Somnath | 24 | Gujarat | 675 | Gir Somnath | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 447 | Jamnagar | 24 | Gujarat | 447 | Jamnagar | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 448 | Junagadh | 24 | Gujarat | 448 | Junagadh | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 450 | Kheda | 24 | Gujarat | 450 | Kheda | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 673 | Morbi | 24 | Gujarat | 673 | Morbi | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 457 | Rajkot | 24 | Gujarat | 457 | Rajkot | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 458 | Sabar Kantha | 24 | Gujarat | 458 | Sabar Kantha | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 460 | Surendranagar | 24 | Gujarat | 460 | Surendranagar | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 461 | Vadodara | 24 | Gujarat | 461 | Vadodara | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 59 | Bhiwani | 6 | Haryana | 59 | Bhiwani | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 62 | Gurugram | 6 | Haryana | 62 | Gurugram | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 527 | Belagavi | 29 | Karnataka | 527 | Belagavi | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 526 | Bengaluru Rural | 29 | Karnataka | 526 | Bengaluru Rural | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 532 | Chikkamagaluru | 29 | Karnataka | 532 | Chikkamagaluru | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 538 | Kalaburagi | 29 | Karnataka | 538 | Kalaburagi | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 545 | Mysuru | 29 | Karnataka | 545 | Mysuru | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 547 | Shivamogga | 29 | Karnataka | 547 | Shivamogga | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 548 | Tumakuru | 29 | Karnataka | 548 | Tumakuru | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 530 | Vijayapura | 29 | Karnataka | 530 | Vijayapura | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 430 | Shajapur | 23 | Madhya Pradesh | 430 | Shajapur | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 478 | Jalgaon | 27 | Maharashtra | 478 | Jalgaon | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 665 | Palghar | 27 | Maharashtra | 665 | Palghar | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 497 | Thane | 27 | Maharashtra | 497 | Thane | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 254 | Churachandpur | 14 | Manipur | 254 | Churachandpur | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 255 | Imphal East | 14 | Manipur | 255 | Imphal East | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 713 | Jiribam | 14 | Manipur | 713 | Jiribam | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 711 | Kakching | 14 | Manipur | 711 | Kakching | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 717 | Kamjong | 14 | Manipur | 717 | Kamjong | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 712 | Kangpokpi | 14 | Manipur | 712 | Kangpokpi | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 714 | Noney | 14 | Manipur | 714 | Noney | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 715 | Pherzawl | 14 | Manipur | 715 | Pherzawl | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 257 | Senapati | 14 | Manipur | 257 | Senapati | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 258 | Tamenglong | 14 | Manipur | 258 | Tamenglong | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 259 | Thoubal | 14 | Manipur | 259 | Thoubal | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| district | 260 | Ukhrul | 14 | Manipur | 260 | Ukhrul | NA | NA | 2011-01-01 | 2018-06-30 | unknown_modification | changed_unit_roster_only | lgd_mod_districts |
| Table truncated in rendered note; full CSV has 47731 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |

LGD changed-component roster

``` r
analysis_table(evidence, "Targeted official-evidence requests", max_rows = 50)
```

| request_id | state | affected_units | period | unresolved_question | sources_checked | requested_document |
|:---|:---|:---|:---|:---|:---|:---|
| event\_\_candidate\_\_alluvial\_\_3861 | Tamil Nadu | Chidambaranar -\> Toothukudi | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | alluvial | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_alluvial\_\_4022 | West Bengal | West Dinajpur -\> Uttar Dinajpur | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | alluvial | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_alluvial\_\_4023 | West Bengal | West Dinajpur -\> Dakshin Dinajpur | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | alluvial | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_alluvial\_\_4669 | Tamil Nadu | Toothukudi -\> Thoothukkudi | 2001-2011 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | alluvial | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_119 | Chhattisgarh | Surguja -\> Balrampur | 2012 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_405 | Telangana | Karimnagar -\> Jagtial | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_406 | Telangana | Nalgonda and Warangal -\> Jangaon | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_408 | Telangana | Mahbubnagar -\> Jogulamba Gadwal | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_414 | Telangana | Mahbubnagar -\> Nagarkurnool | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_422 | Telangana | Mahbubnagar -\> Wanaparthy | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_new_districts_created\_\_423 | Telangana | Warangal -\> Warangal Urban | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | new_districts_created | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_name_changes\_\_109 | Tamil Nadu | Chidambaranar -\> Toothukudi | 2001-2018 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | name_changes | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_district_splits\_\_685 | Chhattisgarh | Surguja -\> Balrampur | 2012 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | district_splits | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_district_splits\_\_805 | Telangana | Nalgonda -\> Jangaon | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | district_splits | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_district_splits\_\_806 | Telangana | Warangal -\> Jangaon | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | district_splits | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_district_splits\_\_867 | Telangana | Warangal -\> Warangal Rural | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | district_splits | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_district_splits\_\_868 | Telangana | Warangal -\> Warangal Urban | 2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | district_splits | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_7454 | Chhattisgarh | Surguja -\> Balrampur | 2011-2012 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10858 | Telangana | Adilabad -\> Komaram Bheem | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10864 | Telangana | Karimnagar -\> Jagtial | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10876 | Telangana | Mahbubnagar -\> Jogulamba Gadwal | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10877 | Telangana | Mahbubnagar -\> Nagarkurnool | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10878 | Telangana | Mahbubnagar -\> Wanaparthy | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10882 | Telangana | Warangal -\> Warangal Rural | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10883 | Telangana | Warangal -\> Warangal Urban | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_india_district_tracker\_\_10884 | Telangana | Warangal -\> Jangaon | 2015-2016 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | india_district_tracker | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_4 | West Bengal | West Dinajpur -\> Uttar Dinajpur | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_5 | West Bengal | West Dinajpur -\> Dakshin Dinajpur | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_36 | Telangana | Rangareddi -\> Rangareddy | 2001-2011 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_39 | Tamil Nadu | Chidambaranar -\> Toothukudi | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_63 | Tamil Nadu | Toothukudi -\> Thoothukkudi | 2001-2011 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_212 | Chhattisgarh | Raj Nandgaon -\> Kawardha | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| event\_\_candidate\_\_isded_1951_2024\_\_213 | Chhattisgarh | Raj Nandgaon -\> Rajnandgaon | 1991-2001 | Confirm event type, effective date, territorial components, and validity for the NSS source period. | isded_1951_2024 | Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events. |
| source\_\_nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | andhra pradesh | anantpur | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | andhra pradesh | west godawari | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_assam\_\_18322\_\_karimgang | assam | karimgang | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_bihar\_\_10104\_\_sitamari | bihar | sitamari | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_chhattisgarh\_\_22206\_\_janigir champa | chhattisgarh | janigir champa | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_chhattisgarh\_\_22209\_\_raj nandgaon | chhattisgarh | raj nandgaon | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_gujarat\_\_24401\_\_kachch | gujarat | kachch | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_haryana\_\_06103\_\_yamuna nagar | haryana | yamuna nagar | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_jammu and kashmir\_\_01114\_\_kathus | jammu and kashmir | kathus | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_jharkhand\_\_20115\_\_lohardanga | jharkhand | lohardanga | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_jharkhand\_\_20210\_\_pakuar | jharkhand | pakuar | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_karnataka\_\_29124\_\_dakshin kannad | karnataka | dakshin kannad | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_karnataka\_\_29110\_\_uttar kannad | karnataka | uttar kannad | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_kerala\_\_32105\_\_malapuram | kerala | malapuram | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_kerala\_\_32214\_\_triruvananthapuram | kerala | triruvananthapuram | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_kerala\_\_32207\_\_trissur | kerala | trissur | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| source\_\_nss_2007_08\_\_mizoram\_\_15105\_\_serchip | mizoram | serchip | nss_2007_08 | Resolve high_precision_fuzzy_candidate before primary-panel inclusion. | fuzzy_name_candidate | Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone. |
| Table truncated in rendered note; full CSV has 115 rows. |  |  |  |  |  |  |

Targeted official-evidence requests

## Fuzzy-name calibration

``` r
analysis_table(gold_summary, "Hand-reviewed candidate-rule summary")
```

| metric                        |  value |
|:------------------------------|-------:|
| reviewed_matches              | 57.000 |
| reviewed_nonmatches           | 11.000 |
| match_rule_recall             |  0.614 |
| observed_nonmatch_acceptances |  0.000 |

Hand-reviewed candidate-rule summary

``` r
analysis_table(gold, "Hand-reviewed fuzzy-match cases", max_rows = 50)
```

| source_wave | state | source_name | reference_name | label | case_type | evidence | source_key | reference_key | jw | dl | trigram | token | passes_name_rule |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2017_18 | Chhattisgarh | Balodabazar | Baloda Bazar | match | spacing | manual review of NSS and tracker rosters | balodabazar | baloda bazar | 0.983333333333333 | 0.916666666666667 | 0.737864787372622 | 0 | TRUE |
| nss_2017_18 | Chhattisgarh | Gariyaband | Gariaband | match | typo | manual review of NSS and tracker rosters | gariyaband | gariaband | 0.98 | 0.9 | 0.668153104781061 | 0 | TRUE |
| nss_2017_18 | Chhattisgarh | Sukama | Sukma | match | typo | manual review of NSS and tracker rosters | sukama | sukma | 0.914444444444445 | 0.833333333333333 | 0.288675134594813 | 0 | FALSE |
| nss_2017_18 | Gujarat | Panch Mahals | Panchmahal | match | spacing | manual review of NSS and tracker rosters | panch mahals | panchmahal | 0.966666666666667 | 0.833333333333333 | 0.670820393249937 | 0 | TRUE |
| nss_2017_18 | Gujarat | Dohad | Dahod | match | transliteration | manual review of NSS and tracker rosters | dohad | dahod | 0.76 | 0.6 | 0 | 0 | FALSE |
| nss_2017_18 | Gujarat | The Dangs | Dang | match | prefix | manual review of NSS and tracker rosters | the dangs | dang | 0 | 0.444444444444444 | 0.534522483824849 | 0 | FALSE |
| nss_2017_18 | Gujarat | Mahesana | Mehsana | match | transliteration | manual review of NSS and tracker rosters | mahesana | mehsana | 0.855357142857143 | 0.75 | 0.365148371670111 | 0 | FALSE |
| nss_2017_18 | Gujarat | Sabar Kantha | Sabarkantha | match | spacing | manual review of NSS and tracker rosters | sabar kantha | sabarkantha | 0.983333333333333 | 0.916666666666667 | 0.737864787372622 | 0 | TRUE |
| nss_2017_18 | Gujarat | Ahmadabad | Ahmedabad | match | transliteration | manual review of NSS and tracker rosters | ahmadabad | ahmedabad | 0.889814814814815 | 0.888888888888889 | 0.571428571428571 | 0 | FALSE |
| nss_2017_18 | Gujarat | Banas Kantha | Banaskantha | match | spacing | manual review of NSS and tracker rosters | banas kantha | banaskantha | 0.983333333333333 | 0.916666666666667 | 0.737864787372622 | 0 | TRUE |
| nss_2017_18 | Gujarat | Kachchh | Kutch | match | transliteration | manual review of NSS and tracker rosters | kachchh | kutch | 0.708571428571429 | 0.428571428571429 | 0 | 0 | FALSE |
| nss_2017_18 | Himachal Pradesh | Lahul & Spiti | Lahaul and Spiti | match | punctuation | manual review of NSS and tracker rosters | lahul and spiti | lahaul and spiti | 0.954305555555556 | 0.9375 | 0.815374248327211 | 0.5 | TRUE |
| nss_2017_18 | Jammu and Kashmir | Badgam | Budgam | match | transliteration | manual review of NSS and tracker rosters | badgam | budgam | 0.9 | 0.833333333333333 | 0.5 | 0 | FALSE |
| nss_2017_18 | Jammu and Kashmir | Baramula | Baramulla | match | typo | manual review of NSS and tracker rosters | baramula | baramulla | 0.977777777777778 | 0.888888888888889 | 0.77151674981046 | 0 | TRUE |
| nss_2017_18 | Jammu and Kashmir | Bandipore | Bandipora | match | transliteration | manual review of NSS and tracker rosters | bandipore | bandipora | 0.955555555555555 | 0.888888888888889 | 0.857142857142857 | 0 | TRUE |
| nss_2017_18 | Jammu and Kashmir | Shupiyan | Shopiyan | match | transliteration | manual review of NSS and tracker rosters | shupiyan | shopiyan | 0.933333333333333 | 0.875 | 0.5 | 0 | FALSE |
| nss_2017_18 | Jharkhand | Kodarma | Koderma | match | transliteration | manual review of NSS and tracker rosters | kodarma | koderma | 0.933333333333333 | 0.857142857142857 | 0.4 | 0 | FALSE |
| nss_2017_18 | Karnataka | Chikmagalur | Chikkamagaluru | match | official_rename | manual review; requires official alias evidence before production | chikmagalur | chikkamagaluru | 0.920779220779221 | 0.785714285714286 | 0.673575314054563 | 0 | TRUE |
| nss_2017_18 | Karnataka | Bagalkot | Bagalkote | match | transliteration | manual review of NSS and tracker rosters | bagalkot | bagalkote | 0.977777777777778 | 0.888888888888889 | 0.925820099772551 | 0 | TRUE |
| nss_2017_18 | Maharashtra | Raigarh | Raigad | match | transliteration | manual review of NSS and tracker rosters | raigarh | raigad | 0.90952380952381 | 0.714285714285714 | 0.670820393249937 | 0 | TRUE |
| nss_2017_18 | Maharashtra | Bid | Beed | match | transliteration | manual review of NSS and tracker rosters | bid | beed | 0.75 | 0.5 | 0 | 0 | FALSE |
| nss_2017_18 | Odisha | Baleshwar | Balasore | match | regional_english | manual review; requires official alias evidence before production | baleshwar | balasore | 0.808796296296296 | 0.444444444444444 | 0.154303349962092 | 0 | FALSE |
| nss_2017_18 | Odisha | Debagarh | Deogarh | match | regional_english | manual review; requires state-constrained evidence before production | debagarh | deogarh | 0.850793650793651 | 0.75 | 0.365148371670111 | 0 | FALSE |
| nss_2017_18 | Punjab | Firozpur | Ferozepur | match | transliteration | manual review of NSS and tracker rosters | firozpur | ferozepur | 0.895833333333333 | 0.777777777777778 | 0.308606699924184 | 0 | FALSE |
| nss_2017_18 | Punjab | Bhatinda | Bathinda | match | transliteration | manual review of NSS and tracker rosters | bhatinda | bathinda | 0.94375 | 0.75 | 0.333333333333333 | 0 | FALSE |
| nss_2017_18 | West Bengal | Koch Bihar | Cooch Behar | match | regional_english | manual review; requires official alias evidence before production | koch bihar | cooch behar | 0.800757575757576 | 0.727272727272727 | 0.471404520791032 | 0 | FALSE |
| nss_2017_18 | West Bengal | Haora | Howrah | match | regional_english | manual review of NSS and tracker rosters | haora | howrah | 0.84 | 0.5 | 0 | 0 | FALSE |
| nss_2017_18 | Uttar Pradesh | Farrukhabad | Firozabad | nonmatch | hard_negative | known implausible current attachment | farrukhabad | firozabad | 0.663636363636364 | 0.545454545454545 | 0.251976315339485 | 0 | FALSE |
| nss_2017_18 | Arunachal Pradesh | Upper Siang | Upper Subansiri | nonmatch | hard_negative | known implausible current attachment | upper siang | upper subansiri | 0.885151515151515 | 0.6 | 0.462250163521024 | 0.333333333333333 | FALSE |
| nss_2017_18 | Manipur | Imphal East | Imphal West | nonmatch | directional_hard_negative | known implausible current attachment | imphal east | imphal west | 0.963636363636364 | 0.818181818181818 | 0.555555555555556 | 0.333333333333333 | FALSE |
| nss_2017_18 | Gujarat | Dang | Porbandar | nonmatch | hard_negative | known implausible current attachment | dang | porbandar | 0.574074074074074 | 0.222222222222222 | 0 | 0 | FALSE |
| nss_2017_18 | Uttar Pradesh | Kasganj | Kushinagar | nonmatch | hard_negative | known implausible current attachment | kasganj | kushinagar | 0.704285714285714 | 0.3 | 0 | 0 | FALSE |
| nss_2017_18 | Uttar Pradesh | Etah | Kasganj | nonmatch | lineage_not_alias | distinct districts despite administrative relationship | etah | kasganj | 0.464285714285714 | 0.142857142857143 | 0 | 0 | FALSE |
| nss_2007_08 | Jammu and Kashmir | Kathus | Kathua | match | typo | manual review of NSS and tracker rosters | kathus | kathua | 0.933333333333333 | 0.833333333333333 | 0.75 | 0 | TRUE |
| nss_2007_08 | Haryana | Jhaijar | Jhajjar | match | typo | manual review of NSS and tracker rosters | jhaijar | jhajjar | 0.933333333333333 | 0.857142857142857 | 0.4 | 0 | FALSE |
| nss_2007_08 | Tamil Nadu | Arivalur | Ariyalur | match | typo | manual review of NSS and tracker rosters | arivalur | ariyalur | 0.941666666666667 | 0.875 | 0.5 | 0 | FALSE |
| nss_2007_08 | Assam | Karimgang | Karimganj | match | typo | manual review of NSS and tracker rosters | karimgang | karimganj | 0.955555555555555 | 0.888888888888889 | 0.857142857142857 | 0 | TRUE |
| nss_2007_08 | Tamil Nadu | Siyaganga | Sivaganga | match | typo | manual review of NSS and tracker rosters | siyaganga | sivaganga | 0.940740740740741 | 0.888888888888889 | 0.571428571428571 | 0 | TRUE |
| nss_2007_08 | Nagaland | Mukokchung | Mokokchung | match | typo | manual review of NSS and tracker rosters | mukokchung | mokokchung | 0.906666666666667 | 0.9 | 0.75 | 0 | TRUE |
| nss_2007_08 | Uttarakhand | Bageswar | Bageshwar | match | typo | manual review of NSS and tracker rosters | bageswar | bageshwar | 0.977777777777778 | 0.888888888888889 | 0.617213399848368 | 0 | TRUE |
| nss_2007_08 | Uttar Pradesh | Varanashi | Varanasi | match | typo | manual review of NSS and tracker rosters | varanashi | varanasi | 0.977777777777778 | 0.888888888888889 | 0.77151674981046 | 0 | TRUE |
| nss_2007_08 | Kerala | Triruvananthapuram | Thiruvananthapuram | match | typo | manual review of NSS and tracker rosters | triruvananthapuram | thiruvananthapuram | 0.949019607843137 | 0.944444444444444 | 0.875 | 0 | TRUE |
| nss_2007_08 | Jharkhand | Lohardanga | Lohardaga | match | typo | manual review of NSS and tracker rosters | lohardanga | lohardaga | 0.98 | 0.9 | 0.668153104781061 | 0 | TRUE |
| nss_2007_08 | Kerala | Malapuram | Malappuram | match | typo | manual review of NSS and tracker rosters | malapuram | malappuram | 0.98 | 0.9 | 0.801783725737273 | 0 | TRUE |
| nss_2007_08 | Odisha | Bolangir | Balangir | match | transliteration | manual review of NSS and tracker rosters | bolangir | balangir | 0.882142857142857 | 0.875 | 0.666666666666667 | 0 | FALSE |
| nss_2007_08 | Andhra Pradesh | West Godawari | West Godavari | match | typo | manual review of NSS and tracker rosters | west godawari | west godavari | 0.969230769230769 | 0.923076923076923 | 0.727272727272727 | 0.333333333333333 | TRUE |
| nss_2007_08 | Chhattisgarh | Janigir-Champa | Janjgir Champa | match | typo_and_punctuation | manual review of NSS and tracker rosters | janigir champa | janjgir champa | 0.948717948717949 | 0.928571428571429 | 0.75 | 0.333333333333333 | TRUE |
| nss_2007_08 | Tamil Nadu | Tiruvanmalai | Tiruvannamalai | match | typo | manual review of NSS and tracker rosters | tiruvanmalai | tiruvannamalai | 0.938095238095238 | 0.857142857142857 | 0.730296743340221 | 0 | TRUE |
| nss_2007_08 | Uttar Pradesh | Sidhartha Nagar | Siddharthnagar | match | spacing_and_transliteration | manual review of NSS and tracker rosters | sidhartha nagar | siddharthnagar | 0.916324786324786 | 0.8 | 0.640512615220349 | 0 | TRUE |
| nss_2007_08 | Karnataka | Dakshin Kannad | Dakshina Kannada | match | suffix | manual review of NSS and tracker rosters | dakshin kannad | dakshina kannada | 0.939285714285714 | 0.875 | 0.77151674981046 | 0 | TRUE |
| Table truncated in rendered note; full CSV has 68 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |

Hand-reviewed fuzzy-match cases

The calibration table is intentionally small and conservative. It
documents known spelling variants and hard negatives, but it is not
large enough to establish a 99.5 percent automatic-match precision
claim. Candidate thresholds therefore remain review aids rather than
production decision rules.
