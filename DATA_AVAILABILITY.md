# Data availability

Raw data are not tracked in this repository. The active manifest is [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv); the source-level inventory is [`data/metadata/data_sources.csv`](data/metadata/data_sources.csv). Where redistribution terms are unclear, this repository records the expected local path and reconstruction route without redistributing the raw files.

This table covers the active current-pipeline sources and tracked processed outputs. It does not attempt to inventory exploratory or future-use files that may exist locally under `data/raw_future/`, `data/interim/`, or other gitignored working directories.

| source name | source ID / manifest ID | tracked | redistributed | where to obtain it | license / redistribution uncertainty | expected local path | reconstruction path / reader / target | notes |
|---|---|---|---|---|---|---|---|---|
| NSS 2007-08 Participation and Expenditure in Education, 64th Round | `nss_2007_education`; manifest IDs beginning `nss0708edu_` | no | no | NSS/National Data Archive source files for the 64th round education survey | Redistribution terms are not asserted here. | `data/raw/nss_2007_education_64/` | `haven::read_sav`, `read_sav_short`, `readxl::read_xlsx`; target `raw_nss_2007_education` | Core baseline education and participation source. |
| NSS 2007-08 Household Consumer Expenditure Survey, 64th Round | `nss_2007_consumption`; `nss0708cons_hhchar` | no | no | NSS/National Data Archive source files for the 64th round consumption survey | Redistribution terms are not asserted here. | `data/raw/nss_2007_consumption_64/` | `haven::read_sav`; target `raw_nss_2007_consumption` | Core baseline consumption source. |
| NSS 2017-18 Household Social Consumption: Education, 75th Round | `nss_2017_education`; manifest IDs beginning `nss1718_` | no | no | NSS/National Data Archive source files for the 75th round education survey | Redistribution terms are not asserted here. | `data/raw/nss_2017_education_75/` | `read_sav_short`, `read_csv_short`; target `raw_nss_2017_education` | Core follow-up education/consumption source. |
| Census of India 2001 C-16 Mother Tongue | `census_2001_mother_tongue`; manifest IDs beginning `census2001_c16_` | no | no | Census of India 2001 C-16 tables | Redistribution terms are not asserted here. | `data/raw/census_2001/languages/C16/PC01_C16_01.xls` through `PC01_C16_35.xls` | `readxl::read_excel`; target `raw_census_2001` | Source for linguistic distance instrument construction. State/UT workbook acquisition URLs are tracked in `data/metadata/census_2001_download_manifest.tsv` and restored with `make download-census-tables`. |
| India Official District Boundaries 2020 | `district_boundaries_2020`; manifest IDs beginning `district_boundaries_2020_` | no | no | District boundary source cited in [`paper/references.bib`](paper/references.bib) | Redistribution terms are not asserted here. | `data/raw/district_boundaries_2020/district/` | `sf::st_read`; target `raw_boundaries_2020` | Used for maps and spatial weights in the current draft pipeline. |
| District change and tracker sources | `district_changes`; manifest IDs beginning `district_changes_` | no | no | Local district-change source files listed in the manifest | Mixed-source redistribution terms are not asserted here. | `data/raw/district_changes/` | `readxl::read_xlsx`, `readr::read_csv`, `readODS::read_ods`; target `raw_district_changes` | Used to build and validate the district tracker; the reviewed tracked authority is `data/metadata/district_harmonization_crosswalk.csv`. |
| ILO figures | `ilo_figures`; manifest IDs beginning `ilo_` | yes | yes, as static image assets already in this repository | [`assets/ilo_figures/`](assets/ilo_figures/) | Static image assets are tracked for rendering; underlying ILO citation remains in [`paper/references.bib`](paper/references.bib). | [`assets/ilo_figures/`](assets/ilo_figures/) | `magick::image_read`; target `raw_ilo_figures` | Used in the paper figure assembly. |
| Processed district panel | processed output | yes | yes | Reconstructed from raw files above, or read directly from tracked processed output | Derived data; no raw microdata are included. | [`data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`](data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv) | `save_processed_district_panel()`; target `processed_district_panel_file` | Public processed district-level analysis panel. |

## District-lineage rebuild inputs

The current paper uses the reviewed 573-district lineage panel. A larger local-only source collection supports validation, historical comparison, and future resolution of the bounded multi-parent and missing-support cases. Methodology and source roles are documented in [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md); tracked schema guidance is in [`data/metadata/README.md`](data/metadata/README.md).

The principal additional source families are:

| family | expected local path | acquisition route | intended use | authority limitation |
|---|---|---|---|---|
| LGD current registries and coverage | `data/raw/local_government_directory/` | [data.gov.in LGD catalog](https://www.data.gov.in/catalog/local-government-directory-lgd) and LGD directory downloads | Current codes, hierarchy, Census-code bridges, ULB/village coverage | Current hierarchy is not historical geography. |
| LGD modification rosters, 2011-01-01 through 2018-06-30 | `data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/` | LGD download-directory reports | Identify entities requiring post-2011 investigation | Reports generally omit action, predecessor, exact date, and territorial share. |
| ramSeraph LGD changes, from 2018-10-13 | `data/raw/local_government_directory/changes.csv` | [ramSeraph LGD archive](https://ramseraph.github.io/opendata/lgd/) | Post-period validation and event-code testing | Begins after the NSS 2017-18 reference period; never determines primary treatment geography. |
| SHRUG Census keys and attributes | `data/raw/shrug/` | [Development Data Lab SHRUG downloads](https://www.devdatalab.org/shrug_download/) | Stable-locality 2001/2011 bridge, population/area weights, validation | Keys may be nonunique on `shrid2`; lower-level NSS household identity remains unavailable. |
| DataMeet Census-2001 district boundaries | `data/raw/datameet/Districts/Census_2001/` | [DataMeet maps](https://github.com/datameet/maps/tree/master/Districts/Census_2001) | Production map, contiguity, and Moran geometry keyed by Census state/district codes | CC BY 2.5 India; includes one noncanonical 99/99 national aggregate that the reader excludes. |
| SHRUG open polygons | `data/raw/shrug/open-polygons/` | SHRUG downloads and metadata | Census-2011 geometry and locality evidence used by lineage diagnostics | Retained for transitions and validation, not production map boundaries. |
| India State Stories/ISDED/Census Collection | `data/raw/district_changes/india_state_stories/` | [India State Stories downloads](https://www.indiastatestory.in/datadownloads) and Harvard Dataverse DOIs in `data_sources.csv` | Candidate events, aliases, historical population checks | Candidate evidence only; known omissions and errors require corroboration. |
| Jaacks district tracker | `data/raw/district_changes/IndiaDistrictTracker2001to2020.ods` | [Jaacks tracker repository](https://github.com/Jaacks-Research-Group/india-district-changes-tracker) | Annual candidate names and lineage paths | Not an authoritative event ledger. |
| Published Census/PLFS/NSS/NRLM concordance | `data/raw/concordance/` | Oxford research archive listed in `data_sources.csv` | Independent candidate mappings, especially Telangana | Includes unmatched rows and lacks legal dates and territorial shares. |
| IPUMS GEO2_IN | `data/raw/ipums/geo2_in1987_2009/` | [IPUMS International](https://international.ipums.org/international-action/variables/GEO2_IN) | Stable-geography sensitivity analysis | Harmonized combined units, not exact Census 2001 districts. |
| Administrative Atlas and official orders | `data/raw_future/Administrative Atlas/` and targeted local evidence | Census Atlas portal and state gazettes | Resolve consequential event dates and component membership | Download only when an unresolved event requires it. |

Large locality attributes and village/SHRID polygon archives are not ordinary public-build inputs. Dedicated lineage targets reduce them to compact reviewed crosswalks; the production Census-2001 GeoPackage is converted directly from the DataMeet shapefile; historical and exploratory sources are loaded only by extended diagnostics or benchmarks.

## Redistribution and provenance policy

- Do not commit raw NSS microdata or other raw files whose redistribution terms are not established.
- Preserve original filenames and downloaded formats. Derived UTF-8 or normalized tables belong under processed/interim outputs, not over the raw original.
- Record the acquisition URL, access date, local path, and methodological role in [`data/metadata/data_sources.csv`](data/metadata/data_sources.csv).
- A source's presence does not make it authoritative. Candidate sources must be corroborated according to [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md).

## DISE/UDISE district report-card archive

Historical NIEPA/NUEPA DISE raw workbooks and report-card PDFs are local research inputs under `data/raw/dise_internet_archive/`. Redistribution rights are not asserted, so the raw archive is excluded from repository/Zenodo deposits. The repository tracks only provenance/decoding metadata and derived diagnostic outputs.
