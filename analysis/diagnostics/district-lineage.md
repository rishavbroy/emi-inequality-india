# District Lineage Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Scope

``` r
analysis_deviation_note("This notebook reports the current production district-lineage system. The inherited harmonization panel is retained only for historical comparison and is not a validation target.")
```

**Deviation note.** This notebook reports the current production
district-lineage system. The inherited harmonization panel is retained
only for historical comparison and is not a validation target.

The public analysis uses the reviewed 573-district primary panel. The
408-district conservative panel and 587-district full reviewed panel are
rebuilt with the same measure, pooled-Gini, IV, and first-stage code.

``` r
summary <- analysis_target_csv("diag_ext_district_lineage", "summary.csv")
readiness <- analysis_target_csv("diag_ext_district_lineage", "readiness.csv")
blockers <- analysis_target_csv("diag_ext_district_lineage", "blockers.csv")
completion <- analysis_target_csv("diag_ext_district_lineage", "completion_status.csv")
inventory <- analysis_target_csv("diag_ext_district_lineage", "source_inventory.csv")
registry <- analysis_target_csv("diag_ext_district_lineage", "source_registry.csv")
reference_issues <- analysis_target_csv("diag_ext_district_lineage", "source_reference_issues.csv")
transition <- analysis_target_csv("diag_ext_district_lineage", "district_transition_2001_2011.csv")
allocation_validation <- analysis_target_csv("diag_ext_district_lineage", "allocation_weight_validation.csv")
reviewed_allocations <- analysis_target_csv("diag_ext_district_lineage", "adjudicated_allocation_weights.csv")
reviewed_allocation_validation <- analysis_target_csv("diag_ext_district_lineage", "adjudicated_allocation_validation.csv")
eligibility <- analysis_target_csv("diag_ext_district_lineage", "conservative_mapping_eligibility.csv")
conservative_crosswalk <- analysis_target_csv("diag_ext_district_lineage", "conservative_source_crosswalk.csv")
primary_crosswalk <- analysis_target_csv("diag_ext_district_lineage", "primary_source_crosswalk.csv")
full_reviewed_crosswalk <- analysis_target_csv("diag_ext_district_lineage", "full_reviewed_source_crosswalk.csv")
primary_reviews <- analysis_target_csv("diag_ext_district_lineage", "primary_reviews.csv")
multi_parent_queue <- analysis_target_csv("diag_ext_district_lineage", "multi_parent_review_queue.csv")
loss_audit <- analysis_target_csv("diag_ext_district_lineage", "district_loss_audit.csv")
reclassification <- analysis_target_csv("diag_ext_district_lineage", "identity_reclassification.csv")
geometry_qa <- analysis_target_csv("diag_ext_district_lineage", "geometry_2001_qa.csv")
geometry_coverage <- analysis_target_csv("diag_ext_district_lineage", "geometry_2001_unit_coverage.csv")
panel_variants <- analysis_target_csv("diag_ext_district_lineage", "panel_variant_summary.csv")
variant_models <- analysis_target_csv("diag_ext_lineage_panel_variants", "panel_variant_model_summary.csv")
variant_coefficients <- analysis_target_csv("diag_ext_lineage_panel_variants", "panel_variant_coefficients.csv")
variant_first_stage <- analysis_target_csv("diag_ext_lineage_panel_variants", "panel_variant_first_stage.csv")
variant_ginis <- analysis_target_csv("diag_ext_lineage_panel_variants", "panel_variant_gini_reconstruction.csv")
legacy_comparison <- analysis_target_csv("diag_ext_legacy_crosswalk_comparison", "legacy_crosswalk_comparison.csv")
downstream_gates <- analysis_target_csv("diag_ext_lineage_downstream", "downstream_review_gates.csv")
```

## Production readiness

``` r
analysis_table(summary, "District-lineage summary")
```

| metric                           |  value |
|:---------------------------------|-------:|
| available_inputs                 |     52 |
| missing_inputs                   |      0 |
| admin_units_2001                 |    593 |
| admin_units_2011                 |    640 |
| shrid_bridge_rows                | 602923 |
| deterministic_shrid_rows         | 587155 |
| district_transition_rows         |    668 |
| nss_source_rows                  |   1259 |
| accepted_source_matches          |   1254 |
| unadjudicated_source_rows        |      0 |
| candidate_rows                   |      0 |
| cross_vintage_exact_review_rows  |      0 |
| single_vintage_exact_review_rows |      0 |
| fuzzy_review_rows                |      0 |
| no_candidate_rows                |      0 |
| primary_eligible_source_rows     |   1024 |
| candidate_event_rows             |      0 |
| current_component_rows           |  19278 |
| urban_coverage_rows              |  34012 |
| changed_component_rows           |  47731 |
| targeted_evidence_requests       |      0 |
| accepted_allocation_sources      |    537 |
| rejected_allocation_sources      |      3 |
| remaining_incomplete_allocations |      0 |

District-lineage summary

``` r
analysis_table(readiness, "Current lineage invariants")
```

| gate | passed | note |
|:---|:---|:---|
| core_inputs_available | TRUE | All locality keys and Census 2011 district geometry are present. |
| unique_2001_unit_ids | TRUE | Census 2001 unit IDs are unique. |
| unique_2011_unit_ids | TRUE | Census 2011 unit IDs are unique. |
| shrid_weights_well_formed | TRUE | Every SHRUG transition weight is finite, nonnegative, and does not overallocate its source district. |
| shrid_allocation_coverage_complete | TRUE | 0 source-unit allocations remain unresolved. |
| adjudicated_allocation_weights_valid | TRUE | Every accepted tracked allocation sums to one by source unit. |
| all_adjudication_sources_registered | TRUE | Every accepted lineage decision cites a registered evidence source. |
| no_conflicting_duplicate_keys | TRUE | Duplicate source or registry keys are absent or identical. |
| all_source_rows_adjudicated | TRUE | Every NSS source identity is explicitly accepted or excluded. |
| accepted_source_rows_present | TRUE | At least one source identity is accepted. |
| all_accepted_rows_conservative_classified | TRUE | Every accepted identity has an explicit conservative-panel disposition. |
| all_accepted_rows_full_reviewed_mapped | TRUE | 1254/1254 accepted identities are represented in the full reviewed crosswalk. |
| lineage_ready | TRUE | All production lineage invariants pass. |

Current lineage invariants

``` r
analysis_table(blockers, "Current lineage blockers")
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Current lineage blockers

``` r
analysis_table(completion, "Six-step completion status")
```

| step | work_item | complete | observed | next_action |
|---:|:---|:---|:---|:---|
| 1 | Adjudicate every NSS district identity | TRUE | 1259/1259 resolved | Resolve any remaining rows in the adjudication ledger. |
| 2 | Resolve open fuzzy identities and evidence requests | TRUE | 0 fuzzy identities and 0 evidence requests open | Use registered official evidence to close the remaining review queue. |
| 3 | Validate reviewed allocation weights | TRUE | 0 unresolved source-unit allocations | Correct or explicitly reject any incomplete allocation. |
| 4 | Construct and validate Census 2001 geometry | TRUE | 593/593 expected districts; 0 missing; 0 unexpected; 0 invalid | Inspect geometry_2001_unit_coverage.csv for missing, unexpected, or invalid units. |
| 5 | Build the conservative, primary, and full reviewed crosswalks | TRUE | 1024 conservative; 1232 primary; 1279 full reviewed rows | Keep panel-role definitions monotone and evidence based. |
| 6 | Verify complete accepted-identity coverage | TRUE | 1254/1254 accepted identities mapped | Record an explicit exclusion for any accepted identity that cannot be mapped. |

Six-step completion status

``` r
analysis_table(inventory, "Input inventory", max_rows = 60)
```

| source_id | relative_path | reader | role | required | load_for_diagnostic | exists | size_bytes |
|:---|:---|:---|:---|:---|:---|:---|---:|
| lgd_states | data/raw/local_government_directory/states.json | lgd_json | current_registry | FALSE | TRUE | TRUE | 12323 |
| lgd_districts | data/raw/local_government_directory/districts.json | lgd_json | current_registry | FALSE | TRUE | TRUE | 343242 |
| lgd_subdistricts | data/raw/local_government_directory/subdistricts.json | lgd_json | current_registry | FALSE | TRUE | TRUE | 4867226 |
| lgd_villages | data/raw/local_government_directory/villages.xlsx | lgd_xlsx | current_component_registry | FALSE | FALSE | TRUE | 48621654 |
| lgd_urban_local_bodies | data/raw/local_government_directory/urbanLocalBody.xlsx | lgd_xlsx | current_urban_registry | FALSE | TRUE | TRUE | 267634 |
| lgd_urban_coverage | data/raw/local_government_directory/urbanLocalBody-coverage.xlsx | lgd_xlsx | urban_component_registry | FALSE | TRUE | TRUE | 1876288 |
| lgd_village_categories | data/raw/local_government_directory/villages-category-urbanLocalBody.xlsx | lgd_xlsx | urban_component_registry | FALSE | FALSE | TRUE | 46605315 |
| lgd_development_blocks | data/raw/local_government_directory/developmentBlocks-coveredVillages.xlsx | lgd_xlsx | component_registry | FALSE | FALSE | TRUE | 35697270 |
| lgd_mod_districts_2001_2011 | data/raw/local_government_directory/modifications_01-01-2001_01-01-2011/districts.xls | spreadsheetml | official_census_code_bridge_2001_2011 | FALSE | TRUE | TRUE | 215754 |
| lgd_mod_districts | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/districts.xls | spreadsheetml | changed_unit_roster_2011_2018 | FALSE | TRUE | TRUE | 55743 |
| lgd_mod_subdistricts | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/subdistricts.xls | spreadsheetml | changed_unit_roster_2011_2018 | FALSE | TRUE | TRUE | 1059238 |
| lgd_mod_villages | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/villages.xls | spreadsheetml | changed_unit_roster_2011_2018 | FALSE | TRUE | TRUE | 34056792 |
| lgd_mod_urban_local_bodies | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/urbanLocalBody.xls | spreadsheetml | changed_unit_roster_2011_2018 | FALSE | TRUE | TRUE | 296172 |
| lgd_changes_post_2018 | data/raw/local_government_directory/changes.csv | inventory_only | post_2018_validation | FALSE | FALSE | TRUE | 150427025 |
| isded_1951_2024 | data/raw/district_changes/india_state_stories/isded/1951-2024/district_proliferation_1951_2024.xlsx | xlsx | candidate_lineage | FALSE | TRUE | TRUE | 152349 |
| isded_admin_units_2025 | data/raw/district_changes/india_state_stories/isded/2025/admin_units_2025.xlsx | xlsx | published_current_component_registry | FALSE | TRUE | TRUE | 1432373 |
| iss_census_series_1901_2011 | data/raw/district_changes/india_state_stories/census_data_collection/1901-2011/1901-2011-State Districts-Population Time Series.xlsx | inventory_only | historical_population_validation | FALSE | FALSE | TRUE | 1389673 |
| iss_subdistricts_2026 | data/raw/district_changes/india_state_stories/census_data_collection/2026/2026_subdistricts_with_2011_census_pass2_loose.xlsx | inventory_only | published_current_component_registry | FALSE | FALSE | TRUE | 3309901 |
| datameet_census_2001_districts | data/raw/datameet/Districts/Census_2001/2001_Dist.shp | inventory_only | production_census_2001_geometry | TRUE | FALSE | TRUE | 10088504 |
| shrug_pc01r | data/raw/shrug/shrug-pc-keys-csv/pc01r_shrid_key.csv | shrug_locality_csv | stable_locality_weight | FALSE | TRUE | TRUE | 32554823 |
| shrug_pc01u | data/raw/shrug/shrug-pc-keys-csv/pc01u_shrid_key.csv | shrug_locality_csv | stable_locality_weight | FALSE | TRUE | TRUE | 289671 |
| shrug_pc11r | data/raw/shrug/shrug-pc-keys-csv/pc11r_shrid_key.csv | shrug_locality_csv | stable_locality_weight | FALSE | TRUE | TRUE | 33663741 |
| shrug_pc11u | data/raw/shrug/shrug-pc-keys-csv/pc11u_shrid_key.csv | shrug_locality_csv | stable_locality_weight | FALSE | TRUE | TRUE | 451746 |
| shrug_pc01dist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc01dist_key.csv | shrug_district_csv | stable_locality_district_membership | FALSE | TRUE | TRUE | 17222200 |
| shrug_pc11dist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc11dist_key.csv | shrug_district_csv | stable_locality_district_membership | FALSE | TRUE | TRUE | 17895273 |
| shrug_pc01subdist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc01subdist_key.csv | inventory_only | stable_locality_subdistrict_membership | FALSE | FALSE | TRUE | 20220170 |
| shrug_pc11subdist | data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdist_key.csv | inventory_only | stable_locality_subdistrict_membership | FALSE | FALSE | TRUE | 21506484 |
| shrug_pc11subdistu | data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdistu_key.csv | inventory_only | stable_locality_subdistrict_membership | FALSE | FALSE | TRUE | 281686 |
| shrug_pc11_district_geometry | data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg | gpkg | census_2011_geometry | FALSE | TRUE | TRUE | 31633408 |
| shrug_pc11_subdistrict_geometry | data/raw/shrug/open-polygons/shrug-pc11subdist-poly-gpkg/subdistrict.gpkg | inventory_only | census_2011_geometry | FALSE | FALSE | TRUE | 91037696 |
| shrug_pc11_state_geometry | data/raw/shrug/open-polygons/shrug-pc11state-poly-gpkg/state.gpkg | inventory_only | census_2011_geometry | FALSE | FALSE | TRUE | 8613888 |
| shrug_pc11_village_geometry_zip | data/raw/shrug/open-polygons/shrug-pc11-village-poly-gpkg.zip | inventory_only | census_2011_geometry | FALSE | FALSE | TRUE | 399235423 |
| shrug_shrid_geometry_zip | data/raw/shrug/open-polygons/shrug-shrid-poly-gpkg.zip | inventory_only | legacy_2001_geometry_reconstruction | FALSE | FALSE | TRUE | 379628892 |
| shrug_pca01_zip | data/raw/shrug/census_2001/shrug-pca01-csv.zip | inventory_only | census_locality_attributes | FALSE | FALSE | TRUE | 50359039 |
| shrug_pca11_zip | data/raw/shrug/census_2011/shrug-pca11-csv.zip | inventory_only | census_locality_attributes | FALSE | FALSE | TRUE | 66532473 |
| shrug_td01_zip | data/raw/shrug/census_2001/shrug-td01-csv.zip | inventory_only | census_locality_attributes | FALSE | FALSE | TRUE | 2473771 |
| shrug_td11_zip | data/raw/shrug/census_2011/shrug-td11-csv.zip | inventory_only | census_locality_attributes | FALSE | FALSE | TRUE | 4754266 |
| shrug_vd01_zip | data/raw/shrug/census_2001/shrug-vd01-csv.zip | inventory_only | census_locality_attributes | FALSE | FALSE | TRUE | 32905413 |
| shrug_vd11_zip | data/raw/shrug/census_2011/shrug-vd11-csv.zip | inventory_only | census_locality_attributes | FALSE | FALSE | TRUE | 69981752 |
| ipums_geo2_1987_2009 | data/raw/ipums/geo2_in1987_2009/geo2_in1987_2009.shp | inventory_only | stable_geography_sensitivity | FALSE | FALSE | TRUE | 6941364 |
| concordance_plfs_nss | data/raw/concordance/plfs_nss_distcodes.csv | csv | published_concordance | FALSE | TRUE | TRUE | 13452 |
| concordance_census_plfs | data/raw/concordance/census_plfs_distcodes.csv | csv | published_concordance | FALSE | TRUE | TRUE | 18775 |
| concordance_nrlm_plfs | data/raw/concordance/nrlm_plfs_distcodes.csv | csv | published_concordance | FALSE | TRUE | TRUE | 18601 |
| concordance_telangana | data/raw/concordance/telangana_plfs_districts.csv | csv | published_concordance | FALSE | TRUE | TRUE | 695 |
| concordance_census_region | data/raw/concordance/census_region.csv | csv | published_concordance | FALSE | TRUE | TRUE | 21341 |
| lineage_gold | data/metadata/district_match_gold.csv | csv | calibration | FALSE | TRUE | TRUE | 7217 |
| lineage_adjudications | data/metadata/district_adjudications.csv | csv | adjudication | FALSE | TRUE | TRUE | 653364 |
| lineage_events | data/metadata/district_admin_events.csv | csv | event_adjudication | FALSE | TRUE | TRUE | 14139 |
| lineage_allocation_weights | data/metadata/district_allocation_weights.csv | allocation_csv | allocation_adjudication | FALSE | TRUE | TRUE | 226672 |
| lineage_geometry_carrybacks | data/metadata/district_geometry_carrybacks.csv | csv | geometry_adjudication | FALSE | TRUE | TRUE | 2983 |
| lineage_primary_reviews | data/metadata/district_primary_reviews.csv | csv | primary_review | FALSE | TRUE | TRUE | 78552 |
| lineage_sources | data/metadata/district_lineage_sources.csv | csv | source_registry | FALSE | TRUE | TRUE | 11737 |

Input inventory

``` r
analysis_table(registry, "Evidence registry", max_rows = 60)
```

| source_id | citation | path_or_url | accessed |
|:---|:---|:---|:---|
| census2001_c16 | Census of India 2001 C-16 mother-tongue tables | data/raw/census_2001/languages/C16 | 2026-07-22 |
| datameet_census_2001_districts | DataMeet maps; Census of India Administrative Atlas | data/raw/datameet/Districts/Census_2001/2001_Dist.shp | 2026-07-26 |
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
| census2011_delhi_admin_atlas | Office of the Registrar General & Census Commissioner, India, Census of India 2011 Administrative Atlas: NCT Delhi | https://censusindia.gov.in/nada/index.php/catalog/40/download/38/AA_2011_NCT_Delhi.pdf | 2026-07-23 |
| census2011_maharashtra_admin_atlas | Directorate of Census Operations, Maharashtra, Census of India 2011 Administrative Atlas: Maharashtra, Volume II | https://censusindia.gov.in/nada/index.php/catalog/35/download/33/AA_2011_Maharashtra_vol2.pdf | 2026-07-23 |
| nss64_education_district_codes | National Sample Survey Office, NSS 64th Round Schedule 25.2 official survey metadata and district-code materials | https://microdata.gov.in/NADA/index.php/catalog/118 | 2026-07-23 |
| nss75_shrug_exact_deterministic | National Sample Survey Office, NSS 75th Round Schedule 25.2 district names and codes; Development Data Lab SHRUG Population Census location keys and Census-2011 district geometry | https://microdata.gov.in/NADA/index.php/catalog/151; data/raw/shrug/shrug-pc-keys-csv; data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg | 2026-07-23 |
| census2011_andaman_reorganization | Office of the Registrar General & Census Commissioner, India, Census of India 2001 Andaman and Nicobar Islands district records and Census of India 2011 District Census Handbook, Andaman and Nicobar Islands | https://censusindia.gov.in/nada/index.php/catalog/43368/download/47072/35%20A-2%20Andaman%20%20Nicobar%20Islands.pdf; https://censusindia.gov.in/nada/index.php/catalog/1434/download/4511/DH_2011_3500_PART_A_DCHB_ANDAMAN_NICOBAR_ISLANDS.pdf | 2026-07-23 |
| census2011_official_district_aliases | Office of the Registrar General & Census Commissioner, India, Census of India 2011 District Census Handbooks for Leh (Ladakh), Ganganagar, YSR, Aizawl, and Bathinda; official Sri Ganganagar and Bathinda district portals | https://censusindia.gov.in/nada/index.php/catalog/498; https://censusindia.gov.in/nada/index.php/catalog/1026; https://censusindia.gov.in/nada/index.php/catalog/148; https://censusindia.gov.in/nada/index.php/catalog/874; https://censusindia.gov.in/nada/index.php/catalog/986; https://sriganganagar.rajasthan.gov.in/; https://bathinda.nic.in/ | 2026-07-23 |
| lgd_mod_districts_census_codes | Ministry of Panchayati Raj, Local Government Directory, All Districts of India modification report, 01 January 2011 through 30 June 2018 | data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/districts.xls | 2026-07-23 |
| nss75_official_district_list_census2011_exact | National Sample Survey Office, NSS 75th Round Schedule 25.2, official List of Districts NSS 2017-18; Census-2011 district registry | data/raw/nss_2017_education_75/List of Districts NSS 2017-18.csv; data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg | 2026-07-23 |
| nss75_official_exact_current_identity | National Sample Survey Office, NSS 75th Round official district list; Census-2011 district registry; Local Government Directory current district registry | data/raw/nss_2017_education_75/List of Districts NSS 2017-18.csv; data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg; data/raw/local_government_directory/districts.json | 2026-07-23 |
| nss75_reviewed_district_aliases | NSS 75th Round official district list, Census-2011 district records, Local Government Directory, and reviewed state district portals | data/raw/nss_2017_education_75/List of Districts NSS 2017-18.csv; data/raw/local_government_directory/districts.json | 2026-07-23 |
| telangana_2016_parent_district_review | Government of Telangana district portal and district history pages documenting the October 2016 district reorganization and predecessor districts | https://www.telangana.gov.in/about/districts/; https://adilabad.telangana.gov.in/about-district/; https://medchal-malkajgiri.telangana.gov.in/history/; https://wanaparthy.telangana.gov.in/about-district/; https://mahabubabad.telangana.gov.in/about-district/ | 2026-07-23 |
| chhattisgarh_2012_parent_district_review | Government of Chhattisgarh disaster-management, forest, education, investment, and district-planning materials documenting the 2012 district formations and predecessor districts | https://sdma.cg.gov.in/Gariyaband%20English.pdf; https://sdma.cg.gov.in/Bemetara%20English.pdf; https://invest.cg.gov.in/storage/pdfs/deap/DEAP%20MUNGELI.pdf; https://forest.cg.gov.in/cms/media/b36d6119-bd0d-40c9-9ff5-c550e4749e09_Surguja%20draft%20New\_.pdf; https://sainikwelfare.cg.gov.in/directorate/html/en/directorate_readmore_about_us_en.html | 2026-07-23 |
| punjab_2011_parent_district_review | Government of Punjab district and municipal records documenting the 2011 creation of Pathankot from Gurdaspur and Fazilka from Firozpur | https://mcpathankot.punjab.gov.in/; https://pathankot.nic.in/history/; https://fazilka.nic.in/about-district/ | 2026-07-23 |
| lineage_legacy_review | Archived reviewed comparison of the current conservative crosswalk with the inherited pre-lineage district panel | outputs/diagnostics/extended/district_lineage/legacy_crosswalk_comparison.csv; data/metadata/district_legacy_mapping_reviews.csv | 2026-07-23 |
| census_registry_2001_2011_continuity | Office of the Registrar General & Census Commissioner, India, Census-2001 and Census-2011 district registries; reviewed canonical state/district continuity | data/raw/census_2001/languages/C16; data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg | 2026-07-24 |
| official_single_parent_district_histories_2026 | Official Union, state, district, court, and audit histories documenting single-parent district creation or official renaming for Ganderbal, Kurung Kumey, Lower Dibang Valley, Dima Hasao, Tapi, Shopian, Ramgarh, Longleng, and Peren | https://ganderbal.nic.in/history/; https://ghcitanagar.gov.in/ghcita/Jmt10/WP%28C%29NO269%28AP%292009.pdf; https://dimahasao.assam.gov.in/about-district/district-glance; https://tapi.dcourts.gov.in/about-department/history/; https://shopian.dcourts.gov.in/about-department/history/; https://ramgarh.dcourts.gov.in/about-department/history/; https://tourism.nagaland.gov.in/districts/ | 2026-07-24 |
| official_multi_parent_exclusion_2026 | Directorate of Census Operations Assam and Government of Jammu & Kashmir district histories identifying Baksa, Udalguri, and Samba as post-2001 reorganizations for which a defensible single-parent unit weight is unavailable | https://assam.census.gov.in/census_division.php; https://samba.gov.in/location/ | 2026-07-24 |
| lgd_mod_districts_2001_2011 | Ministry of Panchayati Raj, Local Government Directory, All Districts of India modification report, 01 January 2001 through 01 January 2011 | data/raw/local_government_directory/modifications_01-01-2001_01-01-2011/districts.xls | 2026-07-24 |
| official_simdega_gumla_history_2026 | District Administration Gumla, About District; Government of India MSME and Jharkhand Space Applications Centre district profiles documenting that Simdega was carved wholly from Gumla on 30 April 2001 | https://gumla.nic.in/about-district/; https://dcmsme.gov.in/dips/2016-17/Simdega.pdf; https://jsac.jharkhand.gov.in/district_profile/Simdega.pdf | 2026-07-24 |

Evidence registry

``` r
analysis_table(reference_issues, "Unregistered evidence references", max_rows = 50)
```

| note                               |
|:-----------------------------------|
| No rows in this diagnostic output. |

Unregistered evidence references

Production readiness depends only on current lineage invariants.
Historical crosswalk comparisons are generated separately and cannot
block the public panel.

## Transition and allocation integrity

``` r
analysis_table(transition, "Census 2011 to Census 2001 transitions", max_rows = 60)
```

| state_code_2011 | district_code_2011 | state_code_2001 | district_code_2001 | population_share_to_2001 | area_share_to_2001 | shrid_coverage | mapping_class | source_id | n_shrid_mapped | population_2011_mapped | area_2011_mapped | n_shrid_total | population_2011_total | area_2011_total | n_target_2001_districts |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 35 | 638 | 35 | 2 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 35 | 640 | 35 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 12 | 253 | 12 | 12 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 12 | 257 | 12 | 10 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 12 | 258 | 12 | 15 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 12 | 245 | 12 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 12 | 252 | 12 | 9 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 18 | 308 | 18 | 13 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 18 | 310 | 18 | 15 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 18 | 302 | 18 | 3 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 18 | 300 | 18 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 18 | 309 | 18 | 14 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 225 | 10 | 23 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 222 | 10 | 20 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 232 | 10 | 30 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 236 | 10 | 35 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 217 | 10 | 15 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 238 | 10 | 37 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 239 | 10 | 33 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 233 | 10 | 31 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 223 | 10 | 21 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 210 | 10 | 8 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 227 | 10 | 25 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 213 | 10 | 11 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 207 | 10 | 5 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 226 | 10 | 24 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 216 | 10 | 14 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 229 | 10 | 27 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 237 | 10 | 36 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 203 | 10 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 230 | 10 | 28 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 204 | 10 | 2 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 211 | 10 | 9 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 234 | 10 | 32 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 214 | 10 | 12 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 221 | 10 | 19 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 219 | 10 | 17 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 228 | 10 | 26 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 205 | 10 | 3 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 206 | 10 | 4 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 218 | 10 | 16 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 10 | 208 | 10 | 6 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 4 | 55 | 4 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 414 | 22 | 15 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 417 | 22 | 16 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 412 | 22 | 13 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 409 | 22 | 10 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 402 | 22 | 3 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 404 | 22 | 5 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 411 | 22 | 12 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 22 | 410 | 22 | 11 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 30 | 585 | 30 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 482 | 24 | 15 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 488 | 24 | 21 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 473 | 24 | 6 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 468 | 24 | 1 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 471 | 24 | 4 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 487 | 24 | 20 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 490 | 24 | 24 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| 24 | 470 | 24 | 3 | 1 | 1 | 1 | official_lgd_census_code_bridge | lgd_mod_districts_2001_2011 | NA | NA | NA | NA | NA | NA | NA |
| Table truncated in rendered note; full CSV has 668 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Census 2011 to Census 2001 transitions

``` r
analysis_table(allocation_validation, "Generated allocation checks", max_rows = 60)
```

| source_key | n_targets | n_missing_weights | n_negative_weights | weight_sum | unmapped_share | weights_well_formed | coverage_complete |
|:---|:---|:---|:---|:---|:---|:---|:---|
| pc2011\_\_01\_\_001 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_002 | 2 | 0 | 0 | 0.980825205361533 | 0.0191747946384668 | TRUE | FALSE |
| pc2011\_\_01\_\_003 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_004 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_005 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_006 | 1 | 0 | 0 | 0.999467633850393 | 0.000532366149607055 | TRUE | FALSE |
| pc2011\_\_01\_\_007 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_008 | 1 | 0 | 0 | 0.999546644524666 | 0.000453355475333983 | TRUE | FALSE |
| pc2011\_\_01\_\_009 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_010 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_011 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_012 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_013 | 1 | 0 | 0 | 0.968138534643052 | 0.031861465356948 | TRUE | FALSE |
| pc2011\_\_01\_\_014 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_015 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_016 | 1 | 0 | 0 | 0.999924378439561 | 7.56215604390542e-05 | TRUE | FALSE |
| pc2011\_\_01\_\_017 | 2 | 0 | 0 | 0.99877693302739 | 0.00122306697260999 | TRUE | FALSE |
| pc2011\_\_01\_\_018 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_019 | 2 | 0 | 0 | 0.999944142634485 | 5.58573655145e-05 | TRUE | FALSE |
| pc2011\_\_01\_\_020 | 1 | 0 | 0 | 0.999354873564753 | 0.000645126435246945 | TRUE | FALSE |
| pc2011\_\_01\_\_021 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_022 | 3 | 0 | 0 | 0.931454571681227 | 0.0685454283187726 | TRUE | FALSE |
| pc2011\_\_02\_\_023 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_024 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_025 | 1 | 0 | 0 | 0.98691547332404 | 0.0130845266759601 | TRUE | FALSE |
| pc2011\_\_02\_\_026 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_027 | 1 | 0 | 0 | 0.998687707358741 | 0.00131229264125898 | TRUE | FALSE |
| pc2011\_\_02\_\_028 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_029 | 1 | 0 | 0 | 0.999879118833861 | 0.000120881166138953 | TRUE | FALSE |
| pc2011\_\_02\_\_030 | 1 | 0 | 0 | 0.999298348500874 | 0.000701651499126044 | TRUE | FALSE |
| pc2011\_\_02\_\_031 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_032 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_033 | 1 | 0 | 0 | 0.998377169813639 | 0.001622830186361 | TRUE | FALSE |
| pc2011\_\_02\_\_034 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_035 | 1 | 0 | 0 | 0.988358903426542 | 0.011641096573458 | TRUE | FALSE |
| pc2011\_\_03\_\_036 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_037 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_038 | 2 | 0 | 0 | 0.999988655164263 | 1.13448357366153e-05 | TRUE | FALSE |
| pc2011\_\_03\_\_039 | 1 | 0 | 0 | 0.998801260799268 | 0.00119873920073199 | TRUE | FALSE |
| pc2011\_\_03\_\_040 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_041 | 1 | 0 | 0 | 0.999958842314331 | 4.11576856690354e-05 | TRUE | FALSE |
| pc2011\_\_03\_\_042 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_043 | 1 | 0 | 0 | 0.999789559178226 | 0.000210440821774016 | TRUE | FALSE |
| pc2011\_\_03\_\_044 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_045 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_046 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_047 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_048 | 1 | 0 | 0 | 0.996912959499486 | 0.003087040500514 | TRUE | FALSE |
| pc2011\_\_03\_\_049 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_050 | 1 | 0 | 0 | 0.998803172842384 | 0.00119682715761604 | TRUE | FALSE |
| pc2011\_\_03\_\_051 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_052 | 2 | 0 | 0 | 0.997497160665586 | 0.00250283933441398 | TRUE | FALSE |
| pc2011\_\_03\_\_053 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_054 | 1 | 0 | 0 | 0.990393382667788 | 0.00960661733221202 | TRUE | FALSE |
| pc2011\_\_04\_\_055 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_056 | 1 | 0 | 0 | 0.999539513944851 | 0.000460486055149034 | TRUE | FALSE |
| pc2011\_\_05\_\_057 | 1 | 0 | 0 | 0.998125662338326 | 0.001874337661674 | TRUE | FALSE |
| pc2011\_\_05\_\_058 | 1 | 0 | 0 | 0.9994428049611 | 0.000557195038899949 | TRUE | FALSE |
| pc2011\_\_05\_\_059 | 1 | 0 | 0 | 0.998647813884714 | 0.00135218611528598 | TRUE | FALSE |
| pc2011\_\_05\_\_060 | 1 | 0 | 0 | 0.997190418543355 | 0.002809581456645 | TRUE | FALSE |
| Table truncated in rendered note; full CSV has 631 rows. |  |  |  |  |  |  |  |

Generated allocation checks

``` r
analysis_table(reviewed_allocation_validation, "Reviewed allocation checks", max_rows = 60)
```

| source_key | n_targets | n_missing_weights | n_negative_weights | weight_sum | unmapped_share | weights_well_formed | coverage_complete |
|:---|:---|:---|:---|:---|:---|:---|:---|
| pc2011\_\_01\_\_001 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_002 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_005 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_006 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_007 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_008 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_010 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_011 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_013 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_014 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_016 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_017 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_019 | 2 | 0 | 0 | 1 | 4.44089209850063e-16 | TRUE | TRUE |
| pc2011\_\_01\_\_020 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_01\_\_021 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_023 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_024 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_025 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_026 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_027 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_028 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_029 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_030 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_031 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_033 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_02\_\_034 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_035 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_036 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_037 | 2 | 0 | 0 | 1 | 2.22044604925031e-16 | TRUE | TRUE |
| pc2011\_\_03\_\_038 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_039 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_040 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_041 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_042 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_043 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_046 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_048 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_049 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_050 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_051 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_052 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_03\_\_054 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_056 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_057 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_058 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_059 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_060 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_061 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_062 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_063 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_064 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_065 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_066 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_067 | 2 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_05\_\_068 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_06\_\_069 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_06\_\_070 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_06\_\_071 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_06\_\_072 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| pc2011\_\_06\_\_074 | 1 | 0 | 0 | 1 | 0 | TRUE | TRUE |
| Table truncated in rendered note; full CSV has 537 rows. |  |  |  |  |  |  |  |

Reviewed allocation checks

``` r
analysis_table(reviewed_allocations, "Reviewed allocations", max_rows = 60)
```

| source_unit | target_2001 | weight | basis | reference_year | source_id | status | note |
|:---|:---|:---|:---|:---|:---|:---|:---|
| pc2011\_\_01\_\_001 | pc2001\_\_01\_\_01 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_006 | pc2001\_\_01\_\_12 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_007 | pc2001\_\_01\_\_14 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_008 | pc2001\_\_01\_\_02 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_014 | pc2001\_\_01\_\_06 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_016 | pc2001\_\_01\_\_09 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_017 | pc2001\_\_01\_\_09 | 0.842359351510061 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_017 | pc2001\_\_01\_\_10 | 0.157640648489939 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_019 | pc2001\_\_01\_\_10 | 0.999077401009813 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_019 | pc2001\_\_01\_\_13 | 0.000922598990186518 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_020 | pc2001\_\_01\_\_10 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_01\_\_021 | pc2001\_\_01\_\_13 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_023 | pc2001\_\_02\_\_01 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_024 | pc2001\_\_02\_\_02 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_026 | pc2001\_\_02\_\_04 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_027 | pc2001\_\_02\_\_05 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_028 | pc2001\_\_02\_\_06 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_029 | pc2001\_\_02\_\_07 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_030 | pc2001\_\_02\_\_08 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_031 | pc2001\_\_02\_\_09 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_033 | pc2001\_\_02\_\_11 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_02\_\_034 | pc2001\_\_02\_\_12 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_036 | pc2001\_\_03\_\_03 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_037 | pc2001\_\_03\_\_04 | 0.999970749984118 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_037 | pc2001\_\_03\_\_10 | 2.92500158818445e-05 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_038 | pc2001\_\_03\_\_01 | 0.000407788444145214 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_038 | pc2001\_\_03\_\_05 | 0.999592211555855 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_039 | pc2001\_\_03\_\_06 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_040 | pc2001\_\_03\_\_08 | 0.999123912832671 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_040 | pc2001\_\_03\_\_17 | 0.000876087167329217 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_041 | pc2001\_\_03\_\_09 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_042 | pc2001\_\_03\_\_10 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_043 | pc2001\_\_03\_\_11 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_046 | pc2001\_\_03\_\_14 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_048 | pc2001\_\_03\_\_17 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_049 | pc2001\_\_03\_\_02 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_050 | pc2001\_\_03\_\_02 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_051 | pc2001\_\_03\_\_07 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_052 | pc2001\_\_03\_\_07 | 0.622244431378916 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_052 | pc2001\_\_03\_\_17 | 0.377755568621084 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_03\_\_054 | pc2001\_\_03\_\_16 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_056 | pc2001\_\_05\_\_01 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_057 | pc2001\_\_05\_\_02 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_058 | pc2001\_\_05\_\_03 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_059 | pc2001\_\_05\_\_04 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_060 | pc2001\_\_05\_\_05 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_061 | pc2001\_\_05\_\_06 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_063 | pc2001\_\_05\_\_08 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_064 | pc2001\_\_05\_\_08 | 0.00379574946767907 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_064 | pc2001\_\_05\_\_09 | 0.996204250532321 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_065 | pc2001\_\_05\_\_10 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_067 | pc2001\_\_05\_\_11 | 0.000794951752028535 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_067 | pc2001\_\_05\_\_12 | 0.999205048247972 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_05\_\_068 | pc2001\_\_05\_\_13 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_06\_\_069 | pc2001\_\_06\_\_01 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_06\_\_070 | pc2001\_\_06\_\_02 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_06\_\_071 | pc2001\_\_06\_\_03 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_06\_\_072 | pc2001\_\_06\_\_04 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_06\_\_075 | pc2001\_\_06\_\_07 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| pc2011\_\_06\_\_076 | pc2001\_\_06\_\_08 | 1 | population_renormalized_min_99pct_mapped | 2011 | shrug_pc_keys | accepted | Sensitivity-only allocation. At least 99% of the Census-2011 source district population is represented in the SHRUG 2001 bridge; mapped population shares are renormalized to sum to one. This decision does not make the source eligible for the deterministic preferred panel. |
| Table truncated in rendered note; full CSV has 586 rows. |  |  |  |  |  |  |  |

Reviewed allocations

## Three panel specifications

``` r
analysis_table(panel_variants, "Panel definitions", max_rows = 10)
```

| panel_variant | source_rows_2007_08 | source_rows_2017_18 | target_districts_2007_08 | target_districts_2017_18 | two_wave_target_districts | accepted_primary_reviews | description |
|:---|---:|---:|---:|---:|---:|---:|:---|
| conservative | 587 | 437 | 587 | 413 | 408 | 0 | Deterministic official, registry, alias, and accepted single-parent mappings only. |
| primary | 587 | 645 | 587 | 578 | 573 | 208 | Conservative mappings plus reviewed 2017-18 single-parent allocations with at least 99 percent SHRUG coverage and corroborating LGD or India State Stories evidence. |
| full_reviewed | 588 | 691 | 587 | 592 | 587 | 0 | Primary mappings plus reviewed multi-parent fractional allocations; robustness specification only. |

Panel definitions

``` r
analysis_table(variant_models, "Analysis-sample comparison", max_rows = 10)
```

| panel_variant | panel_rows | unique_districts | complete_iv_rows | multi_source_rows | coefficient_inference_available |
|:---|---:|---:|---:|---:|:---|
| conservative | 408 | 408 | 408 | 0 | TRUE |
| primary | 573 | 573 | 573 | 0 | TRUE |
| full_reviewed | 587 | 587 | 587 | 1 | TRUE |

Analysis-sample comparison

``` r
analysis_table(variant_first_stage, "First-stage comparison", max_rows = 30)
```

| model | term | estimate | std.error | statistic | p.value | partial_f | partial_p | model_f | model_p | nobs | r.squared | adj.r.squared | sigma | status | reason | panel_variant |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| consumption | (Intercept) | 29.659029390672 | 12.1042017281198 | 2.45030858348717 | 0.0147460533468712 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | ling_distance_nonzero_mean | 0.482323133825674 | 0.626991741167012 | 0.769265529603201 | 0.442237324143954 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | log_population_2001 | 1.04731614859944 | 0.888226964846393 | 1.17910870762695 | 0.239129273909132 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | urban_share_2001 | 0.0657117461306587 | 0.0509543984316832 | 1.28961872091889 | 0.198006385138988 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | adult_secondary_plus_share_2001 | 0.59053880093247 | 0.209432740712114 | 2.81970621653767 | 0.00507125242970294 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | sc_share_2001 | -0.0253514657144677 | 0.0916637610307281 | -0.276570210837948 | 0.782267981415219 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | st_share_2001 | 0.0467297801728241 | 0.0311106878615936 | 1.50204908296202 | 0.133956209700278 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | muslim_share_2001 | 0.0657518303092147 | 0.0723703902455669 | 0.90854602394855 | 0.364194264867344 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | agricultural_worker_share_2001 | -0.0606245873036738 | 0.070732247768896 | -0.857099685305533 | 0.391956842371268 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | dependency_ratio_2001 | -0.0738536112677294 | 0.0797428529924592 | -0.926147090256645 | 0.354986560910069 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | electricity_access_share_2001 | -0.0432307333638119 | 0.0414129620168574 | -1.04389377765866 | 0.297231128535733 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | log_population_density_2001 | 0.82435996602195 | 1.08659080062476 | 0.758666432246584 | 0.448545789554273 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200102 | -38.7445649918718 | 4.74836411740003 | -8.15956064740175 | 5.61225029191059e-15 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200103 | -31.4929195258843 | 6.0126314000732 | -5.23779314419654 | 2.76345500444632e-07 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200104 | -31.7470933710516 | 6.83848358734832 | -4.64241713320567 | 4.82356596675858e-06 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200105 | -42.4871633186526 | 4.46417104734006 | -9.51736904076922 | 2.5840986753883e-19 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200106 | -36.9577833196632 | 5.63476012889484 | -6.55889203342394 | 1.87441304287388e-10 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200107 | -47.725632766569 | 6.76183542496488 | -7.05808848738979 | 8.66015980333183e-12 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200108 | -40.9825748757631 | 5.1035365992396 | -8.03023042528376 | 1.37973245339079e-14 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200109 | -46.7052438166755 | 5.02821491533041 | -9.28863316368537 | 1.48347896145007e-18 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200110 | -47.4702454813315 | 4.9990500718088 | -9.49585317199182 | 3.04901910010508e-19 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200111 | 33.0982493250598 | 4.78870506067395 | 6.9117326930136 | 2.16992273693186e-11 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200112 | 20.5625579581168 | 4.59112854984915 | 4.47875892274731 | 1.00789293735288e-05 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200113 | 26.572179180152 | 4.98868852314473 | 5.32648592047226 | 1.76289588287739e-07 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200114 | 0.141724892220746 | 4.30102230240935 | 0.0329514432281261 | 0.973731469909853 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200115 | -4.5713861205225 | 4.83286896734015 | -0.945894902472068 | 0.344833129025975 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200116 | -46.5814861237545 | 5.77203115431991 | -8.07020698231886 | 1.04588647951723e-14 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200117 | -1.44299899653982 | 5.11346818835901 | -0.282195751178204 | 0.777954683411862 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200118 | -50.4614255493461 | 4.45114637233858 | -11.3367257169829 | 1.06413695959568e-25 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| consumption | state_code_200119 | -52.90484950241 | 5.67956602012583 | -9.3149457748953 | 1.21486020967789e-18 | 0.591769455035693 | 0.442237324143954 | 69.8688370049063 | 1.66984419370626e-151 | 408 | 0.896751307417672 | 0.883916525190587 | 6.97907934818052 | estimated | NA | conservative |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

First-stage comparison

``` r
analysis_table(variant_coefficients, "2SLS coefficient comparison", max_rows = 60)
```

| model | term | estimate | std.error | statistic | p.value | status | reason | panel_variant |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| consumption | (Intercept) | -3.32587026218233 | 4.49400345366363 | -0.740068470457226 | 0.459738139474203 | estimated | NA | conservative |
| consumption | emi_exposure_all_children_0708 | 0.0859321106873742 | 0.128119111526124 | 0.670720469910943 | 0.502826313828205 | estimated | NA | conservative |
| consumption | log_population_2001 | -0.0568515655756287 | 0.142955809579068 | -0.397686290211132 | 0.691095848329733 | estimated | NA | conservative |
| consumption | urban_share_2001 | -0.000125702672071652 | 0.00899655729687147 | -0.0139723082867893 | 0.988859770844536 | estimated | NA | conservative |
| consumption | adult_secondary_plus_share_2001 | -0.0473862040937772 | 0.0801230567638603 | -0.591417826624294 | 0.554609622105867 | estimated | NA | conservative |
| consumption | sc_share_2001 | -0.00214743522100642 | 0.00959734673989405 | -0.223753009993898 | 0.82307571575288 | estimated | NA | conservative |
| consumption | st_share_2001 | -0.00584007959609253 | 0.00636023368101538 | -0.91821777138858 | 0.359116241328208 | estimated | NA | conservative |
| consumption | muslim_share_2001 | -0.00581356685509055 | 0.00969091456878405 | -0.599898679719761 | 0.548949079040198 | estimated | NA | conservative |
| consumption | agricultural_worker_share_2001 | 0.0073698931987469 | 0.00945661532805395 | 0.779337314999311 | 0.436290225013336 | estimated | NA | conservative |
| consumption | dependency_ratio_2001 | 0.0130737416939406 | 0.0119002300344611 | 1.09861251892452 | 0.272667255142005 | estimated | NA | conservative |
| consumption | electricity_access_share_2001 | 0.00222318655995795 | 0.00723353940766214 | 0.307344224544216 | 0.758758112368905 | estimated | NA | conservative |
| consumption | log_population_density_2001 | -0.12566048900264 | 0.14026894193829 | -0.895853973561177 | 0.370926075320684 | estimated | NA | conservative |
| consumption | factor(state_code_2001)02 | 3.77532330942743 | 5.20078026364825 | 0.725914789327998 | 0.468360062883283 | estimated | NA | conservative |
| consumption | factor(state_code_2001)03 | 3.13970539080737 | 4.33677078515761 | 0.72397310034297 | 0.469549844180188 | estimated | NA | conservative |
| consumption | factor(state_code_2001)04 | 2.94627093684499 | 4.32368105001587 | 0.68142652123569 | 0.49603733771438 | estimated | NA | conservative |
| consumption | factor(state_code_2001)05 | 4.04057638736031 | 5.67305087795071 | 0.712240463603932 | 0.476774667263983 | estimated | NA | conservative |
| consumption | factor(state_code_2001)06 | 3.47770704853886 | 5.02321508141438 | 0.692326924524133 | 0.489175894718131 | estimated | NA | conservative |
| consumption | factor(state_code_2001)07 | 4.36106937874161 | 6.25959551320036 | 0.69670146729847 | 0.48643676732296 | estimated | NA | conservative |
| consumption | factor(state_code_2001)08 | 3.60633139706613 | 5.45984723753054 | 0.660518736179376 | 0.509341087464416 | estimated | NA | conservative |
| consumption | factor(state_code_2001)09 | 4.0622941122495 | 6.18125500680514 | 0.657195684011934 | 0.511472715178566 | estimated | NA | conservative |
| consumption | factor(state_code_2001)10 | 4.02699455275823 | 6.23477267128991 | 0.645892763869623 | 0.518758021759368 | estimated | NA | conservative |
| consumption | factor(state_code_2001)11 | -2.61438562615508 | 4.16311157006186 | -0.627988364509825 | 0.53040752440962 | estimated | NA | conservative |
| consumption | factor(state_code_2001)12 | -2.10044765705939 | 2.51620324354943 | -0.834768678740133 | 0.404398518324489 | estimated | NA | conservative |
| consumption | factor(state_code_2001)13 | -2.51666735882645 | 3.3306177392327 | -0.755615791383562 | 0.450370960724374 | estimated | NA | conservative |
| consumption | factor(state_code_2001)14 | -0.35668378716474 | 0.424623737556412 | -0.839999641134886 | 0.401463016526426 | estimated | NA | conservative |
| consumption | factor(state_code_2001)15 | 0.431046307306181 | 0.820873365147094 | 0.525106947804233 | 0.599830297291391 | estimated | NA | conservative |
| consumption | factor(state_code_2001)16 | 4.41153854207023 | 6.10045339695253 | 0.723149289899338 | 0.470055145333345 | estimated | NA | conservative |
| consumption | factor(state_code_2001)17 | 0.0545475050852044 | 0.557030119691459 | 0.0979255935305949 | 0.922045574387774 | estimated | NA | conservative |
| consumption | factor(state_code_2001)18 | 4.17375438331999 | 6.62668866340752 | 0.629840120053836 | 0.529196523191523 | estimated | NA | conservative |
| consumption | factor(state_code_2001)19 | 4.8084990329053 | 6.86794603151711 | 0.700136403349564 | 0.484291825391862 | estimated | NA | conservative |
| consumption | factor(state_code_2001)20 | 3.71922618297307 | 6.0512140344992 | 0.614624794589814 | 0.539188652074475 | estimated | NA | conservative |
| consumption | factor(state_code_2001)21 | 4.05442257416403 | 5.9966430243839 | 0.676115379501114 | 0.499399114857485 | estimated | NA | conservative |
| consumption | factor(state_code_2001)22 | 3.78615071510909 | 5.95238943630435 | 0.636072413544869 | 0.525131162915592 | estimated | NA | conservative |
| consumption | factor(state_code_2001)23 | 3.75652417571663 | 5.61319125188728 | 0.669231459814167 | 0.50377442364672 | estimated | NA | conservative |
| consumption | factor(state_code_2001)24 | 4.59462861066307 | 6.6716870961912 | 0.688675674446139 | 0.491468502787482 | estimated | NA | conservative |
| consumption | factor(state_code_2001)25 | 3.3762780515416 | 5.01467316277033 | 0.673279781543408 | 0.501198917314801 | estimated | NA | conservative |
| consumption | factor(state_code_2001)26 | 4.56640609075992 | 6.58986710810937 | 0.692943577745382 | 0.48878927186516 | estimated | NA | conservative |
| consumption | factor(state_code_2001)27 | 3.974885218571 | 6.12074201961867 | 0.649412310767288 | 0.516483737279633 | estimated | NA | conservative |
| consumption | factor(state_code_2001)28 | 2.81803473635581 | 4.01650079093365 | 0.701614386014037 | 0.483370487657274 | estimated | NA | conservative |
| consumption | factor(state_code_2001)29 | 3.47506523305331 | 5.18699564902073 | 0.66995722923141 | 0.503312180098564 | estimated | NA | conservative |
| consumption | factor(state_code_2001)30 | 2.99104466568649 | 4.40151755451347 | 0.679548503133736 | 0.497224673702715 | estimated | NA | conservative |
| consumption | factor(state_code_2001)31 | 3.04150610672245 | 4.11559430488859 | 0.739019903664869 | 0.46037381375094 | estimated | NA | conservative |
| consumption | factor(state_code_2001)32 | 2.50051476154282 | 3.43810435148219 | 0.727294609445125 | 0.467515589809869 | estimated | NA | conservative |
| consumption | factor(state_code_2001)33 | 3.38054917572852 | 4.79945953496569 | 0.704360387060267 | 0.481661238253182 | estimated | NA | conservative |
| consumption | factor(state_code_2001)34 | 1.62985532348421 | 2.42781880561579 | 0.671324943901988 | 0.502441692493322 | estimated | NA | conservative |
| consumption | factor(state_code_2001)35 | 2.56637207830149 | 3.97498273179373 | 0.64563100055114 | 0.518927377067057 | estimated | NA | conservative |
| consumption_ancova | (Intercept) | 4.1243996178822 | 0.976314753489367 | 4.22445692144006 | 3.03634241576316e-05 | estimated | NA | conservative |
| consumption_ancova | emi_exposure_all_children_0708 | 0.00187079439205926 | 0.0171738691982262 | 0.108932609796078 | 0.913316465302892 | estimated | NA | conservative |
| consumption_ancova | log_real_consumption_0708 | 0.334394879522749 | 0.196084970940251 | 1.70535700884818 | 0.0889880882994339 | estimated | NA | conservative |
| consumption_ancova | log_population_2001 | 0.0153348210043437 | 0.02525495283252 | 0.607200540267791 | 0.544099552239853 | estimated | NA | conservative |
| consumption_ancova | urban_share_2001 | 0.00512218470447807 | 0.00174306050713305 | 2.93861554634322 | 0.0035087731019848 | estimated | NA | conservative |
| consumption_ancova | adult_secondary_plus_share_2001 | 0.00774371538627158 | 0.0102749950558965 | 0.753646628941946 | 0.451552689347753 | estimated | NA | conservative |
| consumption_ancova | sc_share_2001 | -0.00301694103823386 | 0.00233128357790485 | -1.29411156447351 | 0.196454152409464 | estimated | NA | conservative |
| consumption_ancova | st_share_2001 | -0.00162230225265934 | 0.00146134192783294 | -1.11014555988625 | 0.267675220672649 | estimated | NA | conservative |
| consumption_ancova | muslim_share_2001 | -0.00041832683630132 | 0.00195034977975056 | -0.214488109078989 | 0.830287544324437 | estimated | NA | conservative |
| consumption_ancova | agricultural_worker_share_2001 | 9.54347551402453e-05 | 0.00100493154194943 | 0.0949664242353411 | 0.924394186384311 | estimated | NA | conservative |
| consumption_ancova | dependency_ratio_2001 | 0.00430004272961503 | 0.00182775681269785 | 2.35263394984586 | 0.0191771206766049 | estimated | NA | conservative |
| consumption_ancova | electricity_access_share_2001 | 0.000604813343397899 | 0.00131012799317552 | 0.461644470271897 | 0.644614349775576 | estimated | NA | conservative |
| consumption_ancova | log_population_density_2001 | -0.032551576704456 | 0.0243184953779471 | -1.33855224998727 | 0.181558852296368 | estimated | NA | conservative |
| consumption_ancova | factor(state_code_2001)02 | 0.37886089852425 | 0.661790106510053 | 0.572478939768639 | 0.567353899619592 | estimated | NA | conservative |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |  |  |  |

2SLS coefficient comparison

``` r
analysis_table(variant_ginis, "Pooled-Gini reconstruction comparison", max_rows = 30)
```

| target_unit_2001 | wave | source_count | household_count | pooled_gini | status | panel_variant |
|:---|:---|:---|:---|:---|:---|:---|
| pc2001\_\_01\_\_03 | nss_2017_18 | 2 | 504 | 0.237026915221307 | reconstructed | conservative |
| pc2001\_\_01\_\_05 | nss_2017_18 | 2 | 168 | 0.176028438954906 | reconstructed | conservative |
| pc2001\_\_01\_\_06 | nss_2017_18 | 2 | 344 | 0.18198398694606 | reconstructed | conservative |
| pc2001\_\_03\_\_01 | nss_2017_18 | 2 | 288 | 0.267211918373247 | reconstructed | conservative |
| pc2001\_\_12\_\_10 | nss_2017_18 | 2 | 110 | 0.31891892193448 | reconstructed | conservative |
| pc2001\_\_13\_\_02 | nss_2017_18 | 2 | 176 | 0.215216689902698 | reconstructed | conservative |
| pc2001\_\_13\_\_07 | nss_2017_18 | 2 | 272 | 0.223081724421003 | reconstructed | conservative |
| pc2001\_\_20\_\_02 | nss_2017_18 | 2 | 248 | 0.19675114120572 | reconstructed | conservative |
| pc2001\_\_20\_\_04 | nss_2017_18 | 2 | 256 | 0.278996132651325 | reconstructed | conservative |
| pc2001\_\_20\_\_14 | nss_2017_18 | 2 | 312 | 0.310130004385182 | reconstructed | conservative |
| pc2001\_\_20\_\_16 | nss_2017_18 | 2 | 157 | 0.165273450415545 | reconstructed | conservative |
| pc2001\_\_20\_\_17 | nss_2017_18 | 2 | 224 | 0.295847765155981 | reconstructed | conservative |
| pc2001\_\_22\_\_07 | nss_2017_18 | 2 | 320 | 0.352847915438785 | reconstructed | conservative |
| pc2001\_\_22\_\_10 | nss_2017_18 | 3 | 351 | 0.364902629109211 | reconstructed | conservative |
| pc2001\_\_22\_\_11 | nss_2017_18 | 3 | 416 | 0.386967790805563 | reconstructed | conservative |
| pc2001\_\_22\_\_15 | nss_2017_18 | 2 | 160 | 0.301929644793495 | reconstructed | conservative |
| pc2001\_\_23\_\_16 | nss_2017_18 | 2 | 160 | 0.235098573324951 | reconstructed | conservative |
| pc2001\_\_23\_\_24 | nss_2017_18 | 2 | 160 | 0.275611458106063 | reconstructed | conservative |
| pc2001\_\_24\_\_22 | nss_2017_18 | 2 | 416 | 0.221109587553377 | reconstructed | conservative |
| pc2001\_\_28\_\_04 | nss_2017_18 | 3 | 352 | 0.236526966527562 | reconstructed | conservative |
| pc2001\_\_33\_\_05 | nss_2017_18 | 2 | 320 | 0.200926339062392 | reconstructed | conservative |
| pc2001\_\_01\_\_02 | nss_2017_18 | 2 | 336 | 0.223770111043748 | reconstructed | primary |
| pc2001\_\_01\_\_03 | nss_2017_18 | 2 | 504 | 0.237026915221307 | reconstructed | primary |
| pc2001\_\_01\_\_05 | nss_2017_18 | 2 | 168 | 0.176028438954906 | reconstructed | primary |
| pc2001\_\_01\_\_06 | nss_2017_18 | 2 | 344 | 0.18198398694606 | reconstructed | primary |
| pc2001\_\_01\_\_09 | nss_2017_18 | 2 | 224 | 0.217823881485283 | reconstructed | primary |
| pc2001\_\_03\_\_01 | nss_2017_18 | 2 | 288 | 0.267211918373247 | reconstructed | primary |
| pc2001\_\_03\_\_02 | nss_2017_18 | 2 | 477 | 0.240260105744902 | reconstructed | primary |
| pc2001\_\_03\_\_11 | nss_2017_18 | 2 | 224 | 0.208157246223697 | reconstructed | primary |
| pc2001\_\_03\_\_16 | nss_2017_18 | 2 | 288 | 0.225166522732911 | reconstructed | primary |
| Table truncated in rendered note; full CSV has 126 rows. |  |  |  |  |  |  |

Pooled-Gini reconstruction comparison

``` r
analysis_table(conservative_crosswalk, "Conservative crosswalk", max_rows = 50)
```

| source_row_id | wave | source_code | raw_state | raw_district | state_std | district_std | target_unit_2001 | target_state_code_2001 | target_district_code_2001 | mapping_class |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | 35101 | Andaman & Nicober | South Andaman | andaman and nicobar islands | south andaman | pc2001\_\_35\_\_01 | 35 | 1 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | Andaman & Nicober | Nicobars | andaman and nicobar islands | nicobars | pc2001\_\_35\_\_02 | 35 | 2 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | Andhra Pardesh | Srikakulam | andhra pradesh | srikakulam | pc2001\_\_28\_\_11 | 28 | 11 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | Andhra Pardesh | Vizianagaram | andhra pradesh | vizianagaram | pc2001\_\_28\_\_12 | 28 | 12 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | Andhra Pardesh | Visakhapatnam | andhra pradesh | visakhapatnam | pc2001\_\_28\_\_13 | 28 | 13 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | Andhra Pardesh | East Godavari | andhra pradesh | east godavari | pc2001\_\_28\_\_14 | 28 | 14 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | 28115 | Andhra Pardesh | West Godawari | andhra pradesh | west godawari | pc2001\_\_28\_\_15 | 28 | 15 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | Andhra Pardesh | Krishna | andhra pradesh | krishna | pc2001\_\_28\_\_16 | 28 | 16 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | Andhra Pardesh | Guntur | andhra pradesh | guntur | pc2001\_\_28\_\_17 | 28 | 17 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | Andhra Pardesh | Prakasam | andhra pradesh | prakasam | pc2001\_\_28\_\_18 | 28 | 18 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | 28219 | Andhra Pardesh | Nellore | andhra pradesh | nellore | pc2001\_\_28\_\_19 | 28 | 19 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | Andhra Pardesh | Adilabad | andhra pradesh | adilabad | pc2001\_\_28\_\_01 | 28 | 1 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | Andhra Pardesh | Nizamabad | andhra pradesh | nizamabad | pc2001\_\_28\_\_02 | 28 | 2 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | Andhra Pardesh | Medak | andhra pradesh | medak | pc2001\_\_28\_\_04 | 28 | 4 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | Andhra Pardesh | Hyderabad | andhra pradesh | hyderabad | pc2001\_\_28\_\_05 | 28 | 5 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | 28306 | Andhra Pardesh | Rangareddy | andhra pradesh | rangareddy | pc2001\_\_28\_\_06 | 28 | 6 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | Andhra Pardesh | Mahbubnagar | andhra pradesh | mahbubnagar | pc2001\_\_28\_\_07 | 28 | 7 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | Andhra Pardesh | Karimnagar | andhra pradesh | karimnagar | pc2001\_\_28\_\_03 | 28 | 3 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | Andhra Pardesh | Nalgonda | andhra pradesh | nalgonda | pc2001\_\_28\_\_08 | 28 | 8 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | Andhra Pardesh | Warangal | andhra pradesh | warangal | pc2001\_\_28\_\_09 | 28 | 9 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | Andhra Pardesh | Khammam | andhra pradesh | khammam | pc2001\_\_28\_\_10 | 28 | 10 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | 28520 | Andhra Pardesh | Cuddapah | andhra pradesh | cuddapah | pc2001\_\_28\_\_20 | 28 | 20 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | Andhra Pardesh | Kurnool | andhra pradesh | kurnool | pc2001\_\_28\_\_21 | 28 | 21 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | 28522 | Andhra Pardesh | Anantpur | andhra pradesh | anantpur | pc2001\_\_28\_\_22 | 28 | 22 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | Andhra Pardesh | Chittoor | andhra pradesh | chittoor | pc2001\_\_28\_\_23 | 28 | 23 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | Arunachal Pradesh | Tawang | arunachal pradesh | tawang | pc2001\_\_12\_\_01 | 12 | 1 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | Arunachal Pradesh | West Kameng | arunachal pradesh | west kameng | pc2001\_\_12\_\_02 | 12 | 2 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | Arunachal Pradesh | East Kameng | arunachal pradesh | east kameng | pc2001\_\_12\_\_03 | 12 | 3 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | Arunachal Pradesh | Papum Pare | arunachal pradesh | papum pare | pc2001\_\_12\_\_04 | 12 | 4 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | Arunachal Pradesh | Lower Subansiri | arunachal pradesh | lower subansiri | pc2001\_\_12\_\_05 | 12 | 5 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | Arunachal Pradesh | Upper Subansiri | arunachal pradesh | upper subansiri | pc2001\_\_12\_\_06 | 12 | 6 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | Arunachal Pradesh | West Siang | arunachal pradesh | west siang | pc2001\_\_12\_\_07 | 12 | 7 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | Arunachal Pradesh | East Siang | arunachal pradesh | east siang | pc2001\_\_12\_\_08 | 12 | 8 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | Arunachal Pradesh | Upper Siang | arunachal pradesh | upper siang | pc2001\_\_12\_\_09 | 12 | 9 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | Arunachal Pradesh | Dibang Valley | arunachal pradesh | dibang valley | pc2001\_\_12\_\_10 | 12 | 10 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | Arunachal Pradesh | Lohit | arunachal pradesh | lohit | pc2001\_\_12\_\_11 | 12 | 11 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | Arunachal Pradesh | Changlang | arunachal pradesh | changlang | pc2001\_\_12\_\_12 | 12 | 12 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | Arunachal Pradesh | Tirap | arunachal pradesh | tirap | pc2001\_\_12\_\_13 | 12 | 13 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | Assam | Lakhimpur | assam | lakhimpur | pc2001\_\_18\_\_12 | 18 | 12 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | Assam | Dhemaji | assam | dhemaji | pc2001\_\_18\_\_13 | 18 | 13 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08 | 18114 | Assam | Tinsukia | assam | tinsukia | pc2001\_\_18\_\_14 | 18 | 14 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | Assam | Dibrugarh | assam | dibrugarh | pc2001\_\_18\_\_15 | 18 | 15 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08 | 18116 | Assam | Sibsagar | assam | sibsagar | pc2001\_\_18\_\_16 | 18 | 16 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | Assam | Jorhat | assam | jorhat | pc2001\_\_18\_\_17 | 18 | 17 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | Assam | Golaghat | assam | golaghat | pc2001\_\_18\_\_18 | 18 | 18 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | Assam | Kokrajhar | assam | kokrajhar | pc2001\_\_18\_\_01 | 18 | 1 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | Assam | Dhubri | assam | dhubri | pc2001\_\_18\_\_02 | 18 | 2 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | Assam | Goalpara | assam | goalpara | pc2001\_\_18\_\_03 | 18 | 3 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | Assam | Bongaigaon | assam | bongaigaon | pc2001\_\_18\_\_04 | 18 | 4 | identity_or_documented_rename_to_2001 |
| nss_2007_08\_\_assam\_\_18205\_\_barpeta | nss_2007_08 | 18205 | Assam | Barpeta | assam | barpeta | pc2001\_\_18\_\_05 | 18 | 5 | identity_or_documented_rename_to_2001 |
| Table truncated in rendered note; full CSV has 1024 rows. |  |  |  |  |  |  |  |  |  |  |

Conservative crosswalk

``` r
analysis_table(primary_crosswalk, "Primary crosswalk", max_rows = 50)
```

| source_row_id | wave | source_code | target_unit_2001 | weight | basis | source_id | panel_variant |
|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | 35101 | pc2001\_\_35\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | pc2001\_\_35\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | pc2001\_\_28\_\_11 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | pc2001\_\_28\_\_12 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | pc2001\_\_28\_\_13 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | pc2001\_\_28\_\_14 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | 28115 | pc2001\_\_28\_\_15 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | pc2001\_\_28\_\_16 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | pc2001\_\_28\_\_17 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | pc2001\_\_28\_\_18 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | 28219 | pc2001\_\_28\_\_19 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | pc2001\_\_28\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | pc2001\_\_28\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | pc2001\_\_28\_\_04 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | pc2001\_\_28\_\_05 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | 28306 | pc2001\_\_28\_\_06 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | pc2001\_\_28\_\_07 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | pc2001\_\_28\_\_03 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | pc2001\_\_28\_\_08 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | pc2001\_\_28\_\_09 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | pc2001\_\_28\_\_10 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | 28520 | pc2001\_\_28\_\_20 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | pc2001\_\_28\_\_21 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | 28522 | pc2001\_\_28\_\_22 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | pc2001\_\_28\_\_23 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | pc2001\_\_12\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | pc2001\_\_12\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | pc2001\_\_12\_\_03 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | pc2001\_\_12\_\_04 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | pc2001\_\_12\_\_05 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | pc2001\_\_12\_\_06 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | pc2001\_\_12\_\_07 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | pc2001\_\_12\_\_08 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | pc2001\_\_12\_\_09 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | pc2001\_\_12\_\_10 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | pc2001\_\_12\_\_11 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | pc2001\_\_12\_\_12 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | pc2001\_\_12\_\_13 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | pc2001\_\_18\_\_12 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | pc2001\_\_18\_\_13 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08 | 18114 | pc2001\_\_18\_\_14 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | pc2001\_\_18\_\_15 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08 | 18116 | pc2001\_\_18\_\_16 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | pc2001\_\_18\_\_17 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | pc2001\_\_18\_\_18 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | pc2001\_\_18\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | pc2001\_\_18\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | pc2001\_\_18\_\_03 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | pc2001\_\_18\_\_04 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| nss_2007_08\_\_assam\_\_18205\_\_barpeta | nss_2007_08 | 18205 | pc2001\_\_18\_\_05 | 1 | identity_or_documented_rename_to_2001 | NA | primary |
| Table truncated in rendered note; full CSV has 1232 rows. |  |  |  |  |  |  |  |

Primary crosswalk

``` r
analysis_table(full_reviewed_crosswalk, "Full reviewed crosswalk", max_rows = 50)
```

| source_row_id | wave | source_code | target_unit_2001 | weight | basis | source_id | panel_variant |
|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | 35101 | pc2001\_\_35\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | pc2001\_\_35\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | pc2001\_\_28\_\_11 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | pc2001\_\_28\_\_12 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | pc2001\_\_28\_\_13 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | pc2001\_\_28\_\_14 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | 28115 | pc2001\_\_28\_\_15 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | pc2001\_\_28\_\_16 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | pc2001\_\_28\_\_17 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | pc2001\_\_28\_\_18 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | 28219 | pc2001\_\_28\_\_19 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | pc2001\_\_28\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | pc2001\_\_28\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | pc2001\_\_28\_\_04 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | pc2001\_\_28\_\_05 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | 28306 | pc2001\_\_28\_\_06 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | pc2001\_\_28\_\_07 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | pc2001\_\_28\_\_03 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | pc2001\_\_28\_\_08 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | pc2001\_\_28\_\_09 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | pc2001\_\_28\_\_10 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | 28520 | pc2001\_\_28\_\_20 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | pc2001\_\_28\_\_21 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | 28522 | pc2001\_\_28\_\_22 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | pc2001\_\_28\_\_23 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | pc2001\_\_12\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | pc2001\_\_12\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | pc2001\_\_12\_\_03 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | pc2001\_\_12\_\_04 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | pc2001\_\_12\_\_05 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | pc2001\_\_12\_\_06 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | pc2001\_\_12\_\_07 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | pc2001\_\_12\_\_08 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | pc2001\_\_12\_\_09 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | pc2001\_\_12\_\_10 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | pc2001\_\_12\_\_11 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | pc2001\_\_12\_\_12 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | pc2001\_\_12\_\_13 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | pc2001\_\_18\_\_12 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | pc2001\_\_18\_\_13 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08 | 18114 | pc2001\_\_18\_\_14 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | pc2001\_\_18\_\_15 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08 | 18116 | pc2001\_\_18\_\_16 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | pc2001\_\_18\_\_17 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | pc2001\_\_18\_\_18 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | pc2001\_\_18\_\_01 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | pc2001\_\_18\_\_02 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | pc2001\_\_18\_\_03 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | pc2001\_\_18\_\_04 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| nss_2007_08\_\_assam\_\_18205\_\_barpeta | nss_2007_08 | 18205 | pc2001\_\_18\_\_05 | 1 | identity_or_documented_rename_to_2001 | NA | deterministic |
| Table truncated in rendered note; full CSV has 1279 rows. |  |  |  |  |  |  |  |

Full reviewed crosswalk

``` r
analysis_table(primary_reviews, "Reviewed primary-panel additions", max_rows = 50)
```

| source_row_id | wave | source_code | raw_state | raw_district | terminal_unit | target_unit_2001 | review_status | reviewed_panel | evidence_basis | evidence_source_ids | notes |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2017_18\_\_andaman and nicobar islands\_\_35102\_\_north and middle andaman | nss_2017_18 | 35102 | Andaman & Nicobar Islands | North & Middle Andaman | pc2011\_\_35\_\_639 | pc2001\_\_35\_\_01 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28313\_\_chittoor | nss_2017_18 | 28313 | Andhra Pradesh | Chittoor | pc2011\_\_28\_\_554 | pc2001\_\_28\_\_23 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28104\_\_east godavari | nss_2017_18 | 28104 | Andhra Pradesh | East Godavari | pc2011\_\_28\_\_545 | pc2001\_\_28\_\_14 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28206\_\_krishna | nss_2017_18 | 28206 | Andhra Pradesh | Krishna | pc2011\_\_28\_\_547 | pc2001\_\_28\_\_16 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28311\_\_kurnool | nss_2017_18 | 28311 | Andhra Pradesh | Kurnool | pc2011\_\_28\_\_552 | pc2001\_\_28\_\_21 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28208\_\_prakasam | nss_2017_18 | 28208 | Andhra Pradesh | Prakasam | pc2011\_\_28\_\_549 | pc2001\_\_28\_\_18 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28209\_\_sri potti sriramulu nellore | nss_2017_18 | 28209 | Andhra Pradesh | Sri Potti Sriramulu Nellore | pc2011\_\_28\_\_550 | pc2001\_\_28\_\_19 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28101\_\_srikakulam | nss_2017_18 | 28101 | Andhra Pradesh | Srikakulam | pc2011\_\_28\_\_542 | pc2001\_\_28\_\_11 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28103\_\_visakhapatnam | nss_2017_18 | 28103 | Andhra Pradesh | Visakhapatnam | pc2011\_\_28\_\_544 | pc2001\_\_28\_\_13 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28102\_\_vizianagaram | nss_2017_18 | 28102 | Andhra Pradesh | Vizianagaram | pc2011\_\_28\_\_543 | pc2001\_\_28\_\_12 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28105\_\_west godavari | nss_2017_18 | 28105 | Andhra Pradesh | West Godavari | pc2011\_\_28\_\_546 | pc2001\_\_28\_\_15 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_andhra pradesh\_\_28310\_\_y s r cuddapah | nss_2017_18 | 28310 | Andhra Pradesh | Y.S.R. (Cuddapah) | pc2011\_\_28\_\_551 | pc2001\_\_28\_\_20 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12116\_\_anjaw | nss_2017_18 | 12116 | Arunachal Pradesh | Anjaw | pc2011\_\_12\_\_260 | pc2001\_\_12\_\_11 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2017_18 | 12103 | Arunachal Pradesh | East Kameng | pc2011\_\_12\_\_247 | pc2001\_\_12\_\_03 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12107\_\_east siang | nss_2017_18 | 12107 | Arunachal Pradesh | East Siang | pc2011\_\_12\_\_251 | pc2001\_\_12\_\_08 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12111\_\_lower subansiri | nss_2017_18 | 12111 | Arunachal Pradesh | Lower Subansiri | pc2011\_\_12\_\_255 | pc2001\_\_12\_\_05 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2017_18 | 12104 | Arunachal Pradesh | Papum Pare | pc2011\_\_12\_\_248 | pc2001\_\_12\_\_04 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12105\_\_upper subansiri | nss_2017_18 | 12105 | Arunachal Pradesh | Upper Subansiri | pc2011\_\_12\_\_249 | pc2001\_\_12\_\_06 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2017_18 | 12102 | Arunachal Pradesh | West Kameng | pc2011\_\_12\_\_246 | pc2001\_\_12\_\_02 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_arunachal pradesh\_\_12106\_\_west siang | nss_2017_18 | 12106 | Arunachal Pradesh | West Siang | pc2011\_\_12\_\_250 | pc2001\_\_12\_\_07 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18204\_\_barpeta | nss_2017_18 | 18204 | Assam | Barpeta | pc2011\_\_18\_\_303 | pc2001\_\_18\_\_05 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18220\_\_bongaigaon | nss_2017_18 | 18220 | Assam | Bongaigaon | pc2011\_\_18\_\_319 | pc2001\_\_18\_\_04 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18317\_\_cachar | nss_2017_18 | 18317 | Assam | Cachar | pc2011\_\_18\_\_316 | pc2001\_\_18\_\_21 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18426\_\_darrang | nss_2017_18 | 18426 | Assam | Darrang | pc2011\_\_18\_\_325 | pc2001\_\_18\_\_08 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18319\_\_hailakandi | nss_2017_18 | 18319 | Assam | Hailakandi | pc2011\_\_18\_\_318 | pc2001\_\_18\_\_23 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18222\_\_kamrup | nss_2017_18 | 18222 | Assam | Kamrup | pc2011\_\_18\_\_321 | pc2001\_\_18\_\_06 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18223\_\_kamrup metropolitan | nss_2017_18 | 18223 | Assam | Kamrup Metropolitan | pc2011\_\_18\_\_322 | pc2001\_\_18\_\_06 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18318\_\_karimganj | nss_2017_18 | 18318 | Assam | Karimganj | pc2011\_\_18\_\_317 | pc2001\_\_18\_\_22 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18405\_\_morigaon | nss_2017_18 | 18405 | Assam | Morigaon | pc2011\_\_18\_\_304 | pc2001\_\_18\_\_09 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18406\_\_nagaon | nss_2017_18 | 18406 | Assam | Nagaon | pc2011\_\_18\_\_305 | pc2001\_\_18\_\_10 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_assam\_\_18112\_\_sivasagar | nss_2017_18 | 18112 | Assam | Sivasagar | pc2011\_\_18\_\_311 | pc2001\_\_18\_\_16 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10107\_\_araria | nss_2017_18 | 10107 | Bihar | Araria | pc2011\_\_10\_\_209 | pc2001\_\_10\_\_07 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10238\_\_arwal | nss_2017_18 | 10238 | Bihar | Arwal | pc2011\_\_10\_\_240 | pc2001\_\_10\_\_33 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10233\_\_aurangabad | nss_2017_18 | 10233 | Bihar | Aurangabad | pc2011\_\_10\_\_235 | pc2001\_\_10\_\_34 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10222\_\_bhagalpur | nss_2017_18 | 10222 | Bihar | Bhagalpur | pc2011\_\_10\_\_224 | pc2001\_\_10\_\_22 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10229\_\_bhojpur | nss_2017_18 | 10229 | Bihar | Bhojpur | pc2011\_\_10\_\_231 | pc2001\_\_10\_\_29 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10113\_\_darbhanga | nss_2017_18 | 10113 | Bihar | Darbhanga | pc2011\_\_10\_\_215 | pc2001\_\_10\_\_13 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_bihar\_\_10118\_\_vaishali | nss_2017_18 | 10118 | Bihar | Vaishali | pc2011\_\_10\_\_220 | pc2001\_\_10\_\_18 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22127\_\_balrampur | nss_2017_18 | 22127 | Chhattisgarh | Balrampur | pc2011\_\_22\_\_401 | pc2001\_\_22\_\_02 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22317\_\_dakshin bastar dantewada | nss_2017_18 | 22317 | Chhattisgarh | Dakshin Bastar Dantewada | pc2011\_\_22\_\_416 | pc2001\_\_22\_\_16 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22206\_\_janjgir champa | nss_2017_18 | 22206 | Chhattisgarh | Janjgir-Champa | pc2011\_\_22\_\_405 | pc2001\_\_22\_\_06 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22208\_\_kabeerdham | nss_2017_18 | 22208 | Chhattisgarh | Kabeerdham | pc2011\_\_22\_\_407 | pc2001\_\_22\_\_08 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22101\_\_koriya | nss_2017_18 | 22101 | Chhattisgarh | Koriya | pc2011\_\_22\_\_400 | pc2001\_\_22\_\_01 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22204\_\_raigarh | nss_2017_18 | 22204 | Chhattisgarh | Raigarh | pc2011\_\_22\_\_403 | pc2001\_\_22\_\_04 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22209\_\_rajnandgaon | nss_2017_18 | 22209 | Chhattisgarh | Rajnandgaon | pc2011\_\_22\_\_408 | pc2001\_\_22\_\_09 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22322\_\_sukama | nss_2017_18 | 22322 | Chhattisgarh | Sukama | pc2011\_\_22\_\_416 | pc2001\_\_22\_\_16 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22126\_\_surajpur | nss_2017_18 | 22126 | Chhattisgarh | Surajpur | pc2011\_\_22\_\_401 | pc2001\_\_22\_\_02 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22102\_\_surguja | nss_2017_18 | 22102 | Chhattisgarh | Surguja | pc2011\_\_22\_\_401 | pc2001\_\_22\_\_02 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_chhattisgarh\_\_22314\_\_uttar bastar kanker | nss_2017_18 | 22314 | Chhattisgarh | Uttar Bastar Kanker | pc2011\_\_22\_\_413 | pc2001\_\_22\_\_14 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| nss_2017_18\_\_goa\_\_30102\_\_south goa | nss_2017_18 | 30102 | Goa | South Goa | pc2011\_\_30\_\_586 | pc2001\_\_30\_\_02 | accepted_primary | primary | iss_2001_2011_continuity_and_shrug_min_99pct_single_parent | alluvial\|shrug_pc_keys | India State Stories links the 2011 district to the same Census-2001 district; SHRUG allocation is single-target with at least 99 percent mapped population. |
| Table truncated in rendered note; full CSV has 208 rows. |  |  |  |  |  |  |  |  |  |  |  |

Reviewed primary-panel additions

## Remaining bounded review work

``` r
analysis_table(loss_audit, "Census-2001 district accounting", max_rows = 60)
```

| target_unit_2001 | state_code | district_code | conservative_nss_2007_08_source_count | conservative_nss_2017_18_source_count | full_reviewed_nss_2007_08_source_count | full_reviewed_nss_2017_18_source_count | conservative_two_wave | full_reviewed_two_wave | loss_stage |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| pc2001\_\_01\_\_01 | 1 | 1 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_02 | 1 | 2 | 1 | 1 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_03 | 1 | 3 | 1 | 2 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_04 | 1 | 4 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_05 | 1 | 5 | 1 | 2 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_06 | 1 | 6 | 1 | 2 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_07 | 1 | 7 | 0 | 1 | 0 | 1 | FALSE | FALSE | no_2007_08_source_mapping |
| pc2001\_\_01\_\_08 | 1 | 8 | 0 | 1 | 0 | 1 | FALSE | FALSE | no_2007_08_source_mapping |
| pc2001\_\_01\_\_09 | 1 | 9 | 1 | 1 | 1 | 3 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_10 | 1 | 10 | 1 | 0 | 1 | 3 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_01\_\_11 | 1 | 11 | 0 | 1 | 0 | 1 | FALSE | FALSE | no_2007_08_source_mapping |
| pc2001\_\_01\_\_12 | 1 | 12 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_01\_\_13 | 1 | 13 | 1 | 1 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_01\_\_14 | 1 | 14 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_01 | 2 | 1 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_02 | 2 | 2 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_03 | 2 | 3 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_04 | 2 | 4 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_05 | 2 | 5 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_02\_\_06 | 2 | 6 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_07 | 2 | 7 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_02\_\_08 | 2 | 8 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_02\_\_09 | 2 | 9 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_10 | 2 | 10 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_02\_\_11 | 2 | 11 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_02\_\_12 | 2 | 12 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_01 | 3 | 1 | 1 | 2 | 1 | 3 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_02 | 3 | 2 | 1 | 1 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_03 | 3 | 3 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_04 | 3 | 4 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_05 | 3 | 5 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_03\_\_06 | 3 | 6 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_03\_\_07 | 3 | 7 | 1 | 1 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_08 | 3 | 8 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_09 | 3 | 9 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_03\_\_10 | 3 | 10 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_11 | 3 | 11 | 1 | 0 | 1 | 2 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_03\_\_12 | 3 | 12 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_13 | 3 | 13 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_14 | 3 | 14 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_15 | 3 | 15 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_16 | 3 | 16 | 1 | 1 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_03\_\_17 | 3 | 17 | 1 | 0 | 1 | 2 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_04\_\_01 | 4 | 1 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_05\_\_01 | 5 | 1 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_02 | 5 | 2 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_03 | 5 | 3 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_04 | 5 | 4 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_05 | 5 | 5 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_06 | 5 | 6 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_07 | 5 | 7 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_05\_\_08 | 5 | 8 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_05\_\_09 | 5 | 9 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_05\_\_10 | 5 | 10 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_11 | 5 | 11 | 1 | 1 | 1 | 2 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_05\_\_12 | 5 | 12 | 1 | 0 | 1 | 1 | FALSE | TRUE | available_only_under_full_reviewed_rule |
| pc2001\_\_05\_\_13 | 5 | 13 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_06\_\_01 | 6 | 1 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_06\_\_02 | 6 | 2 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| pc2001\_\_06\_\_03 | 6 | 3 | 1 | 1 | 1 | 1 | TRUE | TRUE | retained_conservative_two_wave |
| Table truncated in rendered note; full CSV has 593 rows. |  |  |  |  |  |  |  |  |  |

Census-2001 district accounting

``` r
analysis_table(reclassification, "NSS-75 identity classes", max_rows = 60)
```

| source_row_id | wave | source_code | raw_state | raw_district | status | eligible_conservative | target_unit_2001 | exclusion_reason | recovery_class | recommended_panel | allocation_target_count | allocation_weight_sum | allocation_basis | allocation_source_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_andaman and nicobar islands\_\_35102\_\_nicobars | nss_2007_08 | 35102 | Andaman & Nicober | Nicobars | accepted | TRUE | pc2001\_\_35\_\_02 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andaman and nicobar islands\_\_35103\_\_north and middle andaman | nss_2007_08 | 35103 | Andaman & Nicober | North and Middle Andaman | accepted | FALSE | NA | primary_near_complete_requires_review | primary_near_complete | primary | 1 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys |
| nss_2007_08\_\_andaman and nicobar islands\_\_35101\_\_south andaman | nss_2007_08 | 35101 | Andaman & Nicober | South Andaman | accepted | TRUE | pc2001\_\_35\_\_01 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | Andhra Pardesh | Adilabad | accepted | TRUE | pc2001\_\_28\_\_01 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | 28522 | Andhra Pardesh | Anantpur | accepted | TRUE | pc2001\_\_28\_\_22 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | Andhra Pardesh | Chittoor | accepted | TRUE | pc2001\_\_28\_\_23 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28520\_\_cuddapah | nss_2007_08 | 28520 | Andhra Pardesh | Cuddapah | accepted | TRUE | pc2001\_\_28\_\_20 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | Andhra Pardesh | East Godavari | accepted | TRUE | pc2001\_\_28\_\_14 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | Andhra Pardesh | Guntur | accepted | TRUE | pc2001\_\_28\_\_17 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | Andhra Pardesh | Hyderabad | accepted | TRUE | pc2001\_\_28\_\_05 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | Andhra Pardesh | Karimnagar | accepted | TRUE | pc2001\_\_28\_\_03 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | Andhra Pardesh | Khammam | accepted | TRUE | pc2001\_\_28\_\_10 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | Andhra Pardesh | Krishna | accepted | TRUE | pc2001\_\_28\_\_16 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | Andhra Pardesh | Kurnool | accepted | TRUE | pc2001\_\_28\_\_21 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28307\_\_mahbubnagar | nss_2007_08 | 28307 | Andhra Pardesh | Mahbubnagar | accepted | TRUE | pc2001\_\_28\_\_07 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | Andhra Pardesh | Medak | accepted | TRUE | pc2001\_\_28\_\_04 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | Andhra Pardesh | Nalgonda | accepted | TRUE | pc2001\_\_28\_\_08 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28219\_\_nellore | nss_2007_08 | 28219 | Andhra Pardesh | Nellore | accepted | TRUE | pc2001\_\_28\_\_19 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | Andhra Pardesh | Nizamabad | accepted | TRUE | pc2001\_\_28\_\_02 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | Andhra Pardesh | Prakasam | accepted | TRUE | pc2001\_\_28\_\_18 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28306\_\_rangareddy | nss_2007_08 | 28306 | Andhra Pardesh | Rangareddy | accepted | TRUE | pc2001\_\_28\_\_06 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | Andhra Pardesh | Srikakulam | accepted | TRUE | pc2001\_\_28\_\_11 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | Andhra Pardesh | Visakhapatnam | accepted | TRUE | pc2001\_\_28\_\_13 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | Andhra Pardesh | Vizianagaram | accepted | TRUE | pc2001\_\_28\_\_12 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | Andhra Pardesh | Warangal | accepted | TRUE | pc2001\_\_28\_\_09 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | 28115 | Andhra Pardesh | West Godawari | accepted | TRUE | pc2001\_\_28\_\_15 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | Arunachal Pradesh | Changlang | accepted | TRUE | pc2001\_\_12\_\_12 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | Arunachal Pradesh | Dibang Valley | accepted | TRUE | pc2001\_\_12\_\_10 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | Arunachal Pradesh | East Kameng | accepted | TRUE | pc2001\_\_12\_\_03 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | Arunachal Pradesh | East Siang | accepted | TRUE | pc2001\_\_12\_\_08 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | Arunachal Pradesh | Lohit | accepted | TRUE | pc2001\_\_12\_\_11 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | Arunachal Pradesh | Lower Subansiri | accepted | TRUE | pc2001\_\_12\_\_05 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | Arunachal Pradesh | Papum Pare | accepted | TRUE | pc2001\_\_12\_\_04 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | Arunachal Pradesh | Tawang | accepted | TRUE | pc2001\_\_12\_\_01 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | Arunachal Pradesh | Tirap | accepted | TRUE | pc2001\_\_12\_\_13 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | Arunachal Pradesh | Upper Siang | accepted | TRUE | pc2001\_\_12\_\_09 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | Arunachal Pradesh | Upper Subansiri | accepted | TRUE | pc2001\_\_12\_\_06 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | Arunachal Pradesh | West Kameng | accepted | TRUE | pc2001\_\_12\_\_02 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | Arunachal Pradesh | West Siang | accepted | TRUE | pc2001\_\_12\_\_07 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18205\_\_barpeta | nss_2007_08 | 18205 | Assam | Barpeta | accepted | TRUE | pc2001\_\_18\_\_05 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | Assam | Bongaigaon | accepted | TRUE | pc2001\_\_18\_\_04 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18321\_\_cachar | nss_2007_08 | 18321 | Assam | Cachar | accepted | TRUE | pc2001\_\_18\_\_21 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18408\_\_darrang | nss_2007_08 | 18408 | Assam | Darrang | accepted | TRUE | pc2001\_\_18\_\_08 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | Assam | Dhemaji | accepted | TRUE | pc2001\_\_18\_\_13 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | Assam | Dhubri | accepted | TRUE | pc2001\_\_18\_\_02 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | Assam | Dibrugarh | accepted | TRUE | pc2001\_\_18\_\_15 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | Assam | Goalpara | accepted | TRUE | pc2001\_\_18\_\_03 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | Assam | Golaghat | accepted | TRUE | pc2001\_\_18\_\_18 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18323\_\_hailakandi | nss_2007_08 | 18323 | Assam | Hailakandi | accepted | TRUE | pc2001\_\_18\_\_23 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | Assam | Jorhat | accepted | TRUE | pc2001\_\_18\_\_17 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18206\_\_kamrup | nss_2007_08 | 18206 | Assam | Kamrup | accepted | TRUE | pc2001\_\_18\_\_06 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18319\_\_karbi anglong | nss_2007_08 | 18319 | Assam | Karbi Anglong | accepted | TRUE | pc2001\_\_18\_\_19 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18322\_\_karimgang | nss_2007_08 | 18322 | Assam | Karimgang | accepted | TRUE | pc2001\_\_18\_\_22 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | Assam | Kokrajhar | accepted | TRUE | pc2001\_\_18\_\_01 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | Assam | Lakhimpur | accepted | TRUE | pc2001\_\_18\_\_12 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18409\_\_marigaon | nss_2007_08 | 18409 | Assam | Marigaon | accepted | TRUE | pc2001\_\_18\_\_09 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18410\_\_nagaon | nss_2007_08 | 18410 | Assam | Nagaon | accepted | TRUE | pc2001\_\_18\_\_10 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18207\_\_nalbari | nss_2007_08 | 18207 | Assam | Nalbari | accepted | TRUE | pc2001\_\_18\_\_07 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18320\_\_north cachar hills | nss_2007_08 | 18320 | Assam | North Cachar Hills | accepted | TRUE | pc2001\_\_18\_\_20 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| nss_2007_08\_\_assam\_\_18116\_\_sibsagar | nss_2007_08 | 18116 | Assam | Sibsagar | accepted | TRUE | pc2001\_\_18\_\_16 | NA | conservative_mapping | conservative | NA | NA | NA | NA |
| Table truncated in rendered note; full CSV has 1259 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

NSS-75 identity classes

``` r
analysis_table(multi_parent_queue, "Multi-parent fractional-allocation review queue", max_rows = 60)
```

| source_row_id | wave | source_code | raw_state | raw_district | target_unit_2001 | weight | allocation_rank | allocation_target_count | allocation_weight_sum | basis | source_id | review_status | required_evidence |
|:---|:---|---:|:---|:---|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| nss_2017_18\_\_assam\_\_18221\_\_chirang | nss_2017_18 | 18221 | Assam | Chirang | pc2001\_\_18\_\_04 | 0.684 | 1 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_assam\_\_18221\_\_chirang | nss_2017_18 | 18221 | Assam | Chirang | pc2001\_\_18\_\_01 | 0.297 | 2 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_assam\_\_18221\_\_chirang | nss_2017_18 | 18221 | Assam | Chirang | pc2001\_\_18\_\_05 | 0.020 | 3 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_assam\_\_18202\_\_dhubri | nss_2017_18 | 18202 | Assam | Dhubri | pc2001\_\_18\_\_02 | 0.972 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_assam\_\_18202\_\_dhubri | nss_2017_18 | 18202 | Assam | Dhubri | pc2001\_\_18\_\_01 | 0.028 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_chhattisgarh\_\_22316\_\_narayanpur | nss_2017_18 | 22316 | Chhattisgarh | Narayanpur | pc2001\_\_22\_\_15 | 0.934 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_chhattisgarh\_\_22316\_\_narayanpur | nss_2017_18 | 22316 | Chhattisgarh | Narayanpur | pc2001\_\_22\_\_16 | 0.066 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_jammu and kashmir\_\_01217\_\_ramban | nss_2017_18 | 1217 | Jammu & Kashmir | Ramban | pc2001\_\_01\_\_09 | 0.842 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_jammu and kashmir\_\_01217\_\_ramban | nss_2017_18 | 1217 | Jammu & Kashmir | Ramban | pc2001\_\_01\_\_10 | 0.158 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_jammu and kashmir\_\_01219\_\_udhampur | nss_2017_18 | 1219 | Jammu & Kashmir | Udhampur | pc2001\_\_01\_\_10 | 0.999 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_jammu and kashmir\_\_01219\_\_udhampur | nss_2017_18 | 1219 | Jammu & Kashmir | Udhampur | pc2001\_\_01\_\_13 | 0.001 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_madhya pradesh\_\_23605\_\_datia | nss_2017_18 | 23605 | Madhya Pradesh | Datia | pc2001\_\_23\_\_05 | 0.945 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_madhya pradesh\_\_23605\_\_datia | nss_2017_18 | 23605 | Madhya Pradesh | Datia | pc2001\_\_23\_\_06 | 0.055 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_maharashtra\_\_27419\_\_aurangabad | nss_2017_18 | 27419 | Maharashtra | Aurangabad | pc2001\_\_27\_\_19 | 0.998 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_maharashtra\_\_27419\_\_aurangabad | nss_2017_18 | 27419 | Maharashtra | Aurangabad | pc2001\_\_27\_\_18 | 0.002 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_odisha\_\_21111\_\_jagatsinghapur | nss_2017_18 | 21111 | Odisha | Jagatsinghapur | pc2001\_\_21\_\_11 | 1.000 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_odisha\_\_21111\_\_jagatsinghapur | nss_2017_18 | 21111 | Odisha | Jagatsinghapur | pc2001\_\_21\_\_12 | 0.000 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_punjab\_\_03104\_\_hoshiarpur | nss_2017_18 | 3104 | Punjab | Hoshiarpur | pc2001\_\_03\_\_05 | 1.000 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_punjab\_\_03104\_\_hoshiarpur | nss_2017_18 | 3104 | Punjab | Hoshiarpur | pc2001\_\_03\_\_01 | 0.000 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_punjab\_\_03118\_\_sahibzada ajit singh nagar | nss_2017_18 | 3118 | Punjab | Sahibzada Ajit Singh Nagar | pc2001\_\_03\_\_07 | 0.622 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_punjab\_\_03118\_\_sahibzada ajit singh nagar | nss_2017_18 | 3118 | Punjab | Sahibzada Ajit Singh Nagar | pc2001\_\_03\_\_17 | 0.378 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08206\_\_alwar | nss_2017_18 | 8206 | Rajasthan | Alwar | pc2001\_\_08\_\_06 | 0.999 | 1 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08206\_\_alwar | nss_2017_18 | 8206 | Rajasthan | Alwar | pc2001\_\_08\_\_07 | 0.000 | 2 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08206\_\_alwar | nss_2017_18 | 8206 | Rajasthan | Alwar | pc2001\_\_08\_\_11 | 0.000 | 3 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08224\_\_bhilwara | nss_2017_18 | 8224 | Rajasthan | Bhilwara | pc2001\_\_08\_\_24 | 0.996 | 1 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08224\_\_bhilwara | nss_2017_18 | 8224 | Rajasthan | Bhilwara | pc2001\_\_08\_\_25 | 0.002 | 2 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08224\_\_bhilwara | nss_2017_18 | 8224 | Rajasthan | Bhilwara | pc2001\_\_08\_\_21 | 0.002 | 3 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08103\_\_bikaner | nss_2017_18 | 8103 | Rajasthan | Bikaner | pc2001\_\_08\_\_03 | 0.876 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08103\_\_bikaner | nss_2017_18 | 8103 | Rajasthan | Bikaner | pc2001\_\_08\_\_04 | 0.124 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08429\_\_kota | nss_2017_18 | 8429 | Rajasthan | Kota | pc2001\_\_08\_\_30 | 1.000 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08429\_\_kota | nss_2017_18 | 8429 | Rajasthan | Kota | pc2001\_\_08\_\_31 | 0.000 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08433\_\_pratapgarh | nss_2017_18 | 8433 | Rajasthan | Pratapgarh | pc2001\_\_08\_\_29 | 0.658 | 1 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08433\_\_pratapgarh | nss_2017_18 | 8433 | Rajasthan | Pratapgarh | pc2001\_\_08\_\_26 | 0.219 | 2 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08433\_\_pratapgarh | nss_2017_18 | 8433 | Rajasthan | Pratapgarh | pc2001\_\_08\_\_28 | 0.123 | 3 | 3 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08332\_\_udaipur | nss_2017_18 | 8332 | Rajasthan | Udaipur | pc2001\_\_08\_\_26 | 1.000 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_rajasthan\_\_08332\_\_udaipur | nss_2017_18 | 8332 | Rajasthan | Udaipur | pc2001\_\_08\_\_25 | 0.000 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_tamil nadu\_\_33326\_\_thoothukkudi | nss_2017_18 | 33326 | Tamil Nadu | Thoothukkudi | pc2001\_\_33\_\_28 | 0.988 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_tamil nadu\_\_33326\_\_thoothukkudi | nss_2017_18 | 33326 | Tamil Nadu | Thoothukkudi | pc2001\_\_33\_\_29 | 0.012 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_tamil nadu\_\_33432\_\_tiruppur | nss_2017_18 | 33432 | Tamil Nadu | Tiruppur | pc2001\_\_33\_\_12 | 0.763 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_tamil nadu\_\_33432\_\_tiruppur | nss_2017_18 | 33432 | Tamil Nadu | Tiruppur | pc2001\_\_33\_\_10 | 0.237 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_uttar pradesh\_\_09344\_\_allahabad | nss_2017_18 | 9344 | Uttar Pradesh | Allahabad | pc2001\_\_09\_\_45 | 1.000 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_uttar pradesh\_\_09344\_\_allahabad | nss_2017_18 | 9344 | Uttar Pradesh | Allahabad | pc2001\_\_09\_\_44 | 0.000 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_uttar pradesh\_\_09209\_\_ghaziabad | nss_2017_18 | 9209 | Uttar Pradesh | Ghaziabad | pc2001\_\_09\_\_09 | 0.994 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_uttar pradesh\_\_09209\_\_ghaziabad | nss_2017_18 | 9209 | Uttar Pradesh | Ghaziabad | pc2001\_\_09\_\_07 | 0.006 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_uttarakhand\_\_05112\_\_udham singh nagar | nss_2017_18 | 5112 | Uttarakhand | Udham Singh Nagar | pc2001\_\_05\_\_12 | 0.999 | 1 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |
| nss_2017_18\_\_uttarakhand\_\_05112\_\_udham singh nagar | nss_2017_18 | 5112 | Uttarakhand | Udham Singh Nagar | pc2001\_\_05\_\_11 | 0.001 | 2 | 2 | 1 | population_renormalized_min_99pct_mapped | shrug_pc_keys | needs_fractional_validation | Confirm predecessor districts and territorial shares with an official district history, Gazette or atlas schedule, LGD changed-locality record, or a validated locality-level crosswalk before considering primary use. |

Multi-parent fractional-allocation review queue

``` r
analysis_table(geometry_qa, "Census-2001 geometry QA")
```

| metric                    | value |
|:--------------------------|------:|
| geometry_available        |     1 |
| geometry_rows             |   593 |
| expected_admin_units      |   593 |
| missing_admin_units       |     0 |
| unexpected_geometry_units |     0 |
| invalid_geometries        |     0 |

Census-2001 geometry QA

``` r
analysis_table(geometry_coverage, "Geometry unit coverage", max_rows = 60)
```

| unit_id | state_code | district_code | state_std | district_std | expected | observed | coverage_status |
|:---|:---|:---|:---|:---|:---|:---|:---|
| pc2001\_\_01\_\_01 | 1 | 1 | jammu and kashmir | kupwara | TRUE | TRUE | present |
| pc2001\_\_01\_\_02 | 1 | 2 | jammu and kashmir | baramula | TRUE | TRUE | present |
| pc2001\_\_01\_\_03 | 1 | 3 | jammu and kashmir | srinagar | TRUE | TRUE | present |
| pc2001\_\_01\_\_04 | 1 | 4 | jammu and kashmir | badgam | TRUE | TRUE | present |
| pc2001\_\_01\_\_05 | 1 | 5 | jammu and kashmir | pulwama | TRUE | TRUE | present |
| pc2001\_\_01\_\_06 | 1 | 6 | jammu and kashmir | anantnag | TRUE | TRUE | present |
| pc2001\_\_01\_\_07 | 1 | 7 | jammu and kashmir | leh ladakh | TRUE | TRUE | present |
| pc2001\_\_01\_\_08 | 1 | 8 | jammu and kashmir | kargil | TRUE | TRUE | present |
| pc2001\_\_01\_\_09 | 1 | 9 | jammu and kashmir | doda | TRUE | TRUE | present |
| pc2001\_\_01\_\_10 | 1 | 10 | jammu and kashmir | udhampur | TRUE | TRUE | present |
| pc2001\_\_01\_\_11 | 1 | 11 | jammu and kashmir | punch | TRUE | TRUE | present |
| pc2001\_\_01\_\_12 | 1 | 12 | jammu and kashmir | rajauri | TRUE | TRUE | present |
| pc2001\_\_01\_\_13 | 1 | 13 | jammu and kashmir | jammu | TRUE | TRUE | present |
| pc2001\_\_01\_\_14 | 1 | 14 | jammu and kashmir | kathua | TRUE | TRUE | present |
| pc2001\_\_02\_\_01 | 2 | 1 | himachal pradesh | chamba | TRUE | TRUE | present |
| pc2001\_\_02\_\_02 | 2 | 2 | himachal pradesh | kangra | TRUE | TRUE | present |
| pc2001\_\_02\_\_03 | 2 | 3 | himachal pradesh | lahul and spiti | TRUE | TRUE | present |
| pc2001\_\_02\_\_04 | 2 | 4 | himachal pradesh | kullu | TRUE | TRUE | present |
| pc2001\_\_02\_\_05 | 2 | 5 | himachal pradesh | mandi | TRUE | TRUE | present |
| pc2001\_\_02\_\_06 | 2 | 6 | himachal pradesh | hamirpur | TRUE | TRUE | present |
| pc2001\_\_02\_\_07 | 2 | 7 | himachal pradesh | una | TRUE | TRUE | present |
| pc2001\_\_02\_\_08 | 2 | 8 | himachal pradesh | bilaspur | TRUE | TRUE | present |
| pc2001\_\_02\_\_09 | 2 | 9 | himachal pradesh | solan | TRUE | TRUE | present |
| pc2001\_\_02\_\_10 | 2 | 10 | himachal pradesh | sirmaur | TRUE | TRUE | present |
| pc2001\_\_02\_\_11 | 2 | 11 | himachal pradesh | shimla | TRUE | TRUE | present |
| pc2001\_\_02\_\_12 | 2 | 12 | himachal pradesh | kinnaur | TRUE | TRUE | present |
| pc2001\_\_03\_\_01 | 3 | 1 | punjab | gurdaspur | TRUE | TRUE | present |
| pc2001\_\_03\_\_02 | 3 | 2 | punjab | amritsar | TRUE | TRUE | present |
| pc2001\_\_03\_\_03 | 3 | 3 | punjab | kapurthala | TRUE | TRUE | present |
| pc2001\_\_03\_\_04 | 3 | 4 | punjab | jalandhar | TRUE | TRUE | present |
| pc2001\_\_03\_\_05 | 3 | 5 | punjab | hoshiarpur | TRUE | TRUE | present |
| pc2001\_\_03\_\_06 | 3 | 6 | punjab | nawanshahr | TRUE | TRUE | present |
| pc2001\_\_03\_\_07 | 3 | 7 | punjab | rupnagar | TRUE | TRUE | present |
| pc2001\_\_03\_\_08 | 3 | 8 | punjab | fatehgarh sahib | TRUE | TRUE | present |
| pc2001\_\_03\_\_09 | 3 | 9 | punjab | ludhiana | TRUE | TRUE | present |
| pc2001\_\_03\_\_10 | 3 | 10 | punjab | moga | TRUE | TRUE | present |
| pc2001\_\_03\_\_11 | 3 | 11 | punjab | firozpur | TRUE | TRUE | present |
| pc2001\_\_03\_\_12 | 3 | 12 | punjab | muktsar | TRUE | TRUE | present |
| pc2001\_\_03\_\_13 | 3 | 13 | punjab | faridkot | TRUE | TRUE | present |
| pc2001\_\_03\_\_14 | 3 | 14 | punjab | bathinda | TRUE | TRUE | present |
| pc2001\_\_03\_\_15 | 3 | 15 | punjab | mansa | TRUE | TRUE | present |
| pc2001\_\_03\_\_16 | 3 | 16 | punjab | sangrur | TRUE | TRUE | present |
| pc2001\_\_03\_\_17 | 3 | 17 | punjab | patiala | TRUE | TRUE | present |
| pc2001\_\_04\_\_01 | 4 | 1 | chandigarh | chandigarh | TRUE | TRUE | present |
| pc2001\_\_05\_\_01 | 5 | 1 | uttarakhand | uttarkashi | TRUE | TRUE | present |
| pc2001\_\_05\_\_02 | 5 | 2 | uttarakhand | chamoli | TRUE | TRUE | present |
| pc2001\_\_05\_\_03 | 5 | 3 | uttarakhand | rudraprayag | TRUE | TRUE | present |
| pc2001\_\_05\_\_04 | 5 | 4 | uttarakhand | tehri garhwal | TRUE | TRUE | present |
| pc2001\_\_05\_\_05 | 5 | 5 | uttarakhand | dehradun | TRUE | TRUE | present |
| pc2001\_\_05\_\_06 | 5 | 6 | uttarakhand | garhwal | TRUE | TRUE | present |
| pc2001\_\_05\_\_07 | 5 | 7 | uttarakhand | pithoragarh | TRUE | TRUE | present |
| pc2001\_\_05\_\_08 | 5 | 8 | uttarakhand | bageshwar | TRUE | TRUE | present |
| pc2001\_\_05\_\_09 | 5 | 9 | uttarakhand | almora | TRUE | TRUE | present |
| pc2001\_\_05\_\_10 | 5 | 10 | uttarakhand | champawat | TRUE | TRUE | present |
| pc2001\_\_05\_\_11 | 5 | 11 | uttarakhand | nainital | TRUE | TRUE | present |
| pc2001\_\_05\_\_12 | 5 | 12 | uttarakhand | udham singh nagar | TRUE | TRUE | present |
| pc2001\_\_05\_\_13 | 5 | 13 | uttarakhand | hardwar | TRUE | TRUE | present |
| pc2001\_\_06\_\_01 | 6 | 1 | haryana | panchkula | TRUE | TRUE | present |
| pc2001\_\_06\_\_02 | 6 | 2 | haryana | ambala | TRUE | TRUE | present |
| pc2001\_\_06\_\_03 | 6 | 3 | haryana | yamunanagar | TRUE | TRUE | present |
| Table truncated in rendered note; full CSV has 593 rows. |  |  |  |  |  |  |  |

Geometry unit coverage

The 21 multi-parent identities remain confined to the full reviewed
sensitivity panel until official territorial evidence validates their
shares. Six Census-2001 districts lack 2007–08 support.

## Historical comparison

``` r
analysis_table(legacy_comparison, "Archived legacy crosswalk comparison", max_rows = 60)
```

| source_row_id | wave | source_code | conservative_target_unit_2001 | legacy_target_unit_2001 | comparison_status | review_decision | review_status |
|:---|:---|:---|:---|:---|:---|:---|:---|
| nss_2007_08\_\_arunachal pradesh\_\_12106\_\_upper subansiri | nss_2007_08 | 12106 | pc2001\_\_12\_\_06 | pc2001\_\_12\_\_09 | changed_target | accept | accepted |
| nss_2007_08\_\_arunachal pradesh\_\_12109\_\_upper siang | nss_2007_08 | 12109 | pc2001\_\_12\_\_09 | pc2001\_\_12\_\_06 | changed_target | accept | accepted |
| nss_2007_08\_\_manipur\_\_14106\_\_imphal west | nss_2007_08 | 14106 | pc2001\_\_14\_\_06 | pc2001\_\_14\_\_07 | changed_target | accept | accepted |
| nss_2007_08\_\_manipur\_\_14107\_\_imphal east | nss_2007_08 | 14107 | pc2001\_\_14\_\_07 | pc2001\_\_14\_\_06 | changed_target | accept | accepted |
| nss_2007_08\_\_uttar pradesh\_\_09359\_\_kushinagar | nss_2007_08 | 9359 | pc2001\_\_09\_\_59 | pc2001\_\_09\_\_17 | changed_target | accept | accepted |
| nss_2007_08\_\_uttar pradesh\_\_09516\_\_firozabad | nss_2007_08 | 9516 | pc2001\_\_09\_\_16 | pc2001\_\_09\_\_29 | changed_target | accept | accepted |
| nss_2007_08\_\_uttar pradesh\_\_09529\_\_farrukhabad | nss_2007_08 | 9529 | pc2001\_\_09\_\_29 | pc2001\_\_09\_\_16 | changed_target | accept | accepted |
| nss_2007_08\_\_andhra pradesh\_\_28111\_\_srikakulam | nss_2007_08 | 28111 | pc2001\_\_28\_\_11 | pc2001\_\_28\_\_11 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28112\_\_vizianagaram | nss_2007_08 | 28112 | pc2001\_\_28\_\_12 | pc2001\_\_28\_\_12 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28113\_\_visakhapatnam | nss_2007_08 | 28113 | pc2001\_\_28\_\_13 | pc2001\_\_28\_\_13 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28114\_\_east godavari | nss_2007_08 | 28114 | pc2001\_\_28\_\_14 | pc2001\_\_28\_\_14 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28115\_\_west godawari | nss_2007_08 | 28115 | pc2001\_\_28\_\_15 | pc2001\_\_28\_\_15 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28216\_\_krishna | nss_2007_08 | 28216 | pc2001\_\_28\_\_16 | pc2001\_\_28\_\_16 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28217\_\_guntur | nss_2007_08 | 28217 | pc2001\_\_28\_\_17 | pc2001\_\_28\_\_17 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28218\_\_prakasam | nss_2007_08 | 28218 | pc2001\_\_28\_\_18 | pc2001\_\_28\_\_18 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28301\_\_adilabad | nss_2007_08 | 28301 | pc2001\_\_28\_\_01 | pc2001\_\_28\_\_01 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28302\_\_nizamabad | nss_2007_08 | 28302 | pc2001\_\_28\_\_02 | pc2001\_\_28\_\_02 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28304\_\_medak | nss_2007_08 | 28304 | pc2001\_\_28\_\_04 | pc2001\_\_28\_\_04 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28305\_\_hyderabad | nss_2007_08 | 28305 | pc2001\_\_28\_\_05 | pc2001\_\_28\_\_05 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28403\_\_karimnagar | nss_2007_08 | 28403 | pc2001\_\_28\_\_03 | pc2001\_\_28\_\_03 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28408\_\_nalgonda | nss_2007_08 | 28408 | pc2001\_\_28\_\_08 | pc2001\_\_28\_\_08 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28409\_\_warangal | nss_2007_08 | 28409 | pc2001\_\_28\_\_09 | pc2001\_\_28\_\_09 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28410\_\_khammam | nss_2007_08 | 28410 | pc2001\_\_28\_\_10 | pc2001\_\_28\_\_10 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28521\_\_kurnool | nss_2007_08 | 28521 | pc2001\_\_28\_\_21 | pc2001\_\_28\_\_21 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28522\_\_anantpur | nss_2007_08 | 28522 | pc2001\_\_28\_\_22 | pc2001\_\_28\_\_22 | same_target | NA | not_required |
| nss_2007_08\_\_andhra pradesh\_\_28523\_\_chittoor | nss_2007_08 | 28523 | pc2001\_\_28\_\_23 | pc2001\_\_28\_\_23 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12101\_\_tawang | nss_2007_08 | 12101 | pc2001\_\_12\_\_01 | pc2001\_\_12\_\_01 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12102\_\_west kameng | nss_2007_08 | 12102 | pc2001\_\_12\_\_02 | pc2001\_\_12\_\_02 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12103\_\_east kameng | nss_2007_08 | 12103 | pc2001\_\_12\_\_03 | pc2001\_\_12\_\_03 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12104\_\_papum pare | nss_2007_08 | 12104 | pc2001\_\_12\_\_04 | pc2001\_\_12\_\_04 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12105\_\_lower subansiri | nss_2007_08 | 12105 | pc2001\_\_12\_\_05 | pc2001\_\_12\_\_05 | same_target | NA | not_required |
| nss_2007_08\_\_himachal pradesh\_\_02210\_\_sirmapur | nss_2007_08 | 2210 | pc2001\_\_02\_\_10 | pc2001\_\_02\_\_10 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12107\_\_west siang | nss_2007_08 | 12107 | pc2001\_\_12\_\_07 | pc2001\_\_12\_\_07 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12108\_\_east siang | nss_2007_08 | 12108 | pc2001\_\_12\_\_08 | pc2001\_\_12\_\_08 | same_target | NA | not_required |
| nss_2007_08\_\_jammu and kashmir\_\_01113\_\_jammu | nss_2007_08 | 1113 | pc2001\_\_01\_\_13 | pc2001\_\_01\_\_13 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12110\_\_dibang valley | nss_2007_08 | 12110 | pc2001\_\_12\_\_10 | pc2001\_\_12\_\_10 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12111\_\_lohit | nss_2007_08 | 12111 | pc2001\_\_12\_\_11 | pc2001\_\_12\_\_11 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12112\_\_changlang | nss_2007_08 | 12112 | pc2001\_\_12\_\_12 | pc2001\_\_12\_\_12 | same_target | NA | not_required |
| nss_2007_08\_\_arunachal pradesh\_\_12113\_\_tirap | nss_2007_08 | 12113 | pc2001\_\_12\_\_13 | pc2001\_\_12\_\_13 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18112\_\_lakhimpur | nss_2007_08 | 18112 | pc2001\_\_18\_\_12 | pc2001\_\_18\_\_12 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18113\_\_dhemaji | nss_2007_08 | 18113 | pc2001\_\_18\_\_13 | pc2001\_\_18\_\_13 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18114\_\_tinsukia | nss_2007_08 | 18114 | pc2001\_\_18\_\_14 | pc2001\_\_18\_\_14 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18115\_\_dibrugarh | nss_2007_08 | 18115 | pc2001\_\_18\_\_15 | pc2001\_\_18\_\_15 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18117\_\_jorhat | nss_2007_08 | 18117 | pc2001\_\_18\_\_17 | pc2001\_\_18\_\_17 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18118\_\_golaghat | nss_2007_08 | 18118 | pc2001\_\_18\_\_18 | pc2001\_\_18\_\_18 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18201\_\_kokrajhar | nss_2007_08 | 18201 | pc2001\_\_18\_\_01 | pc2001\_\_18\_\_01 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18202\_\_dhubri | nss_2007_08 | 18202 | pc2001\_\_18\_\_02 | pc2001\_\_18\_\_02 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18203\_\_goalpara | nss_2007_08 | 18203 | pc2001\_\_18\_\_03 | pc2001\_\_18\_\_03 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18204\_\_bongaigaon | nss_2007_08 | 18204 | pc2001\_\_18\_\_04 | pc2001\_\_18\_\_04 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18205\_\_barpeta | nss_2007_08 | 18205 | pc2001\_\_18\_\_05 | pc2001\_\_18\_\_05 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18206\_\_kamrup | nss_2007_08 | 18206 | pc2001\_\_18\_\_06 | pc2001\_\_18\_\_06 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18207\_\_nalbari | nss_2007_08 | 18207 | pc2001\_\_18\_\_07 | pc2001\_\_18\_\_07 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18319\_\_karbi anglong | nss_2007_08 | 18319 | pc2001\_\_18\_\_19 | pc2001\_\_18\_\_19 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18321\_\_cachar | nss_2007_08 | 18321 | pc2001\_\_18\_\_21 | pc2001\_\_18\_\_21 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18322\_\_karimgang | nss_2007_08 | 18322 | pc2001\_\_18\_\_22 | pc2001\_\_18\_\_22 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18323\_\_hailakandi | nss_2007_08 | 18323 | pc2001\_\_18\_\_23 | pc2001\_\_18\_\_23 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18408\_\_darrang | nss_2007_08 | 18408 | pc2001\_\_18\_\_08 | pc2001\_\_18\_\_08 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18410\_\_nagaon | nss_2007_08 | 18410 | pc2001\_\_18\_\_10 | pc2001\_\_18\_\_10 | same_target | NA | not_required |
| nss_2007_08\_\_assam\_\_18411\_\_sonitpur | nss_2007_08 | 18411 | pc2001\_\_18\_\_11 | pc2001\_\_18\_\_11 | same_target | NA | not_required |
| nss_2007_08\_\_bihar\_\_10103\_\_sheohar | nss_2007_08 | 10103 | pc2001\_\_10\_\_03 | pc2001\_\_10\_\_03 | same_target | NA | not_required |
| Table truncated in rendered note; full CSV has 1024 rows. |  |  |  |  |  |  |  |

Archived legacy crosswalk comparison

``` r
analysis_table(downstream_gates, "Historical downstream comparison checks", max_rows = 50)
```

| gate | passed | next_action |
|:---|:---|:---|
| inherited_legacy_duplicates_identified | TRUE | Confirm every inherited duplicate is explicitly excluded from the review candidate. |
| lineage_panel_unique_by_2001_unit | TRUE | Resolve duplicated Census-2001 units in the district lineage panel. |
| panel_membership_adjudicated | TRUE | Complete accepted-identity coverage and review the generated panel-membership adjudication. |
| primary_panel_constructed_from_reviewed_sources | TRUE | Require a nonempty primary panel constructed only from adjudicated NSS source identities and reviewed Census-2001 mappings. Differences from the legacy-panel support are diagnostic, not vetoes. |
| shared_support_comparison_available | TRUE | Use the shared, unique Census-2001 support for interpretable model comparisons. |
| multi_source_ginis_reconstructed | TRUE | Recompute every queued Gini from pooled household microdata before review. |
| legacy_reviewable | TRUE | Record the downstream-results decision only after membership and pooled-Gini gates pass. |

Historical downstream comparison checks

The legacy panel and archived review ledger are loaded only by optional
comparison targets.
