# Data metadata

This directory contains tracked descriptions, manifests, checksums, crosswalks, and adjudication ledgers. Raw survey, Census, LGD, SHRUG, and boundary files remain local under `data/raw/`.

## General metadata

- `data_sources.csv`: project-wide source catalog, acquisition route, local path, role, and redistribution caveat.
- Census migration acquisition uses the year-specific download manifests; D-02 through D-07 district semantics, cross-table accounting checks, the 2001 D-03 granularity limitation, the SHRUG Census-2011 PCA population denominator, and the distinction between 2001 validity evidence and 2011 mechanisms are documented in `docs/CENSUS_MIGRATION.md`.
- Census worker-structure acquisition uses both year manifests: 2001 B-04/B-25/B-26 feed predetermined balance diagnostics, while 2011 B-04/B-06/B-25A/B feed post-treatment mechanism diagnostics; B-04/B-06 industry accounting, B-25A/B occupational universes, and deterministic 2011-to-2001 count pooling are documented in `docs/CENSUS_WORKERS.md`.
- Census housing/living-standard diagnostics use 2001 H-09/H-12/H-13 and 2011 HL-07/HL-11/HL-12. The module validates household/electricity universes across tables, pools 2011 counts only through complete deterministic parents, and computes common 2001–2011 household-share changes; see `docs/CENSUS_HOUSING.md`.
- `file_manifest.csv`: exact files required by the existing production pipeline. Missing required files must fail before a raw reader is called.
- `census_2001_download_manifest.tsv` and `census_2011_download_manifest.tsv`: official Census of India download URLs and repository destinations for project Census acquisition sets. Both manifests use the 35 state/UT workbooks (state codes 01-35), omitting redundant all-India workbooks. The 2001 manifest includes control/age tables, B-04/B-25/B-26 worker tables, and C-17 bilingualism/trilingualism. The 2011 manifest includes C-13 single-year-age files plus the planned economic (B-01, B-04, B-06, B-25A/B), language (C-16/C-17), migration (D-02 through D-07), housing/living-standard (HL-04, HL-06 through HL-13), and household mechanism (HH-08, HH-10, HH-11) acquisition families. Files are grouped under thematic raw-data subdirectories (`workers/`, `languages/`, `migration/`, `housing/`, `households/`). Acquisition metadata is intentionally broader than `file_manifest.csv`: a downloaded table does not enter production until a table-specific reader, denominator contract, harmonization rule, and target explicitly use it. The active housing branch now includes H-05/H-08/H-10/H-11 alongside H-09/H-12/H-13, and HL-04/HL-06/HL-08/HL-09/HL-10/HL-13 alongside HL-07/HL-11/HL-12. H-05 is treated as a validated subset; H-10 provides the complete baseline latrine count while H-12 remains a source cross-check; and richer 2011 water, sanitation, and fuel categories are collapsed only to explicit 2001 counterparts. Census 2001 C-17 is the current exception in the useful sense: it is activated only in extended diagnostics, where its hierarchical bilingualism/trilingualism denominators are validated and its native-speaker totals must reconcile exactly to C-16; no C-17 regression is public yet.
- `variable_dictionary.csv`: public processed-variable definitions.
- `english_opportunity_measures.csv`: cross-source reporting semantics for the compact C-17/NSS/DISE mechanism sequence. It records units, numerators, denominators, populations, stages, and paper roles only; formulas and estimators remain in their source-specific modules.
- `checksums.csv`: checksums for every tracked metadata CSV/TSV and processed CSV. Run `Rscript scripts/update_checksums.R` after intentionally changing any of those files; the test suite verifies both inventory completeness and current MD5 values.

## Current production geography

- `district_harmonization_crosswalk.csv`: inherited reviewed crosswalk used by the present paper pipeline.
- `manual_district_corrections.csv`: tracked correction interface for the current pipeline.

The legacy harmonization crosswalk is retained only for historical comparison. Current public analysis uses the reviewed district-lineage crosswalks described below.

## District-lineage

- `district_lineage_sources.csv`: compact source IDs that can be cited by accepted matches, events, and weights.
- `district_match_gold.csv`: manually reviewed positive, negative, and ambiguous name-match examples used to evaluate candidate rules.
- `district_adjudications.csv`: one accepted, excluded, or needs-review source identity per source row.
- `district_admin_events.csv`: reviewed directed administrative-event edges.
- `district_allocation_weights.csv`: reviewed non-primary or sensitivity allocation shares; current accepted rows renormalize mapped population shares only when at least 99 percent of the source population is covered. `source_unit` is canonicalized as the zero-padded `SS.DDD` Census-2011 code.
- `district_geometry_carrybacks.csv`: reviewed cases where an official later-vintage polygon is carried back to an unchanged Census-2001 district.
- `district_primary_reviews.csv`: reviewed near-complete single-parent mappings admitted to the 573-district primary panel.
- `district_legacy_mapping_reviews.csv`: archived provenance for comparisons with the inherited pre-lineage panel; loaded only by extended legacy-comparison targets.

Ledgers may begin blank, but accepted rows must remain narrow, source-backed decisions. Generated candidates belong under `outputs/diagnostics/extended/district_lineage/`; they must not be copied into tracked adjudications without review.

- `vanneman_panel4_dist91_adjudications.csv`: reviewed one-to-one promotions for historical Vanneman stable panel units whose 1991 labels are not exact matches. Each accepted row is machine-checked against the registered Liu-Shamdasani-Taraz stable-ID crosswalk, raw Census-1991 code/name row, and an explicit published district-name alias rule; aggregate/split cases are excluded.
- `helms_lim_linguistic_distance_1991.csv`: compact district-level extract from the verified Helms-Lim AJPS replication package. `IPUM1993` is retained as provenance and must decompose exactly into the two-digit Census-1991 state/district codes. The continuous variable is Shastry-derived and is used as an external coverage/replication benchmark, not as an independent linguistic-distance construction or a silent replacement for the Atlas source.
- Vanneman pretrend diagnostics read only the archived stable `panel4` fixed-width count records specified by the author SAS reader, then restrict them through the reviewed one-to-one 1991→2001 bridge; split historical districts are never copied to multiple 2001 descendants.

See [`docs/DISTRICT_LINEAGE.md`](../../docs/DISTRICT_LINEAGE.md) for authority rules, source caveats, schemas, invariants, and the implementation plan.

## Consumption prices and Census 2001 controls

`price_series_registry.csv` records the role of each price source.
`census_2001_control_registry.csv` is the semantic authority for Census-2001
controls. It records labels/descriptions, theoretical control blocks, preferred
versus alternative parameterizations, denominator/source provenance, and whether
each variable belongs to the main, absorption, or appendix sets. Production
targets track the file explicitly, and R control/specification helpers derive
their vectors from it rather than maintaining parallel hard-coded lists. The
construction rules are described in `docs/CONSUMPTION_AND_PRICES.md` and
`docs/CENSUS_2001_CONTROLS.md`.

- `cpi_iw_centres_2001.csv`: the 78 Labour Bureau CPI-IW centres and their
  All-India weights on the 2001=100 base. The three centres later assigned to
  Telangana are assigned to undivided Andhra Pradesh for the 2007-08 price
  series. `R/prices/read_price_sources.R` normalizes known spelling variants,
  requires complete centre coverage by state and month, and renormalizes the
  official weights within each state through `weighted.mean()`.

- `R/prices/build_temporal_price_series.R` constructs the direct monthly temporal chain: CPI-RL for rural areas and state-weighted CPI-IW for urban areas before January 2013, followed by the state CPI-R/U 2012-base series. State-sector median overlap ratios place the older observations on the newer scale; insufficient links stop construction rather than invoking an undocumented fallback.

- `price_state_crosswalk.csv`: sector-specific, dated temporal fallback rules. Direct state observations always take precedence. The five small-UT donors mirror the Planning Commission's official Tendulkar substitutions; Telangana inherits undivided Andhra Pradesh before the post-2012 state CPI series. States outside the 20-state CPI-RL system or the 78-centre CPI-IW system use the corresponding published All-India series, with that fallback recorded explicitly.
- `tendulkar_poverty_lines_2011_12.csv`: one resolved rural and urban 2011-12 poverty line for each of the 36 analysis states/UTs. It records the original source state, official UT substitution, source page, and table. All spatial relatives use the common all-India rural value of Rs. 816 so the national rural-urban price-level difference is retained rather than normalized away.
- `R/prices/price_deflators.R` applies direct observations first, uses only dated rules from the crosswalk when a direct month is absent, and then multiplies temporal relatives by the Tendulkar spatial relative. Missing or overlapping rules are fatal, and provenance is retained through household attachment.

- `R/prices/nss_period_deflators.R` converts NSS 64 and NSS 75 sub-rounds into their three constituent survey months and averages the validated monthly state-sector deflator. `R/measures/build_real_consumption.R` attaches that object to household Block 3 records before person-weighted district aggregation.

## Archived DISE/UDISE district data

- `dise_archive_registry.csv` inventories annual raw workbooks and report-card PDFs.
- `dise_medium_slot_crosswalk.csv` records report-derived medium-of-instruction slot identities for 2005-06 through 2007-08.
- `dise_publication_checks.csv` records small raw-to-publication validation anchors.

See `docs/DISE_TREATMENTS.md` for construction and scope.

- `dise_report_language_enrollment.csv` stores report-card-derived English/Hindi district enrollment for 2008-09 through 2014-15 with reviewed PDF/page provenance. `scripts/build_dise_report_language_enrollment.py` uses that provenance as an extraction manifest and re-reads the registered pages with Poppler `pdftotext -layout`; it can verify the tracked numeric counts or write a candidate rebuilt CSV. Page discovery remains a reviewed metadata decision rather than an untested PDF heuristic. The normal R/targets pipeline never invokes Poppler.
- `dise_report_total_enrollment_2010_11.csv` stores published current-year elementary enrollment (`Total Pr.` + `Total U.P.`) for the reviewed 2010-11 district pages available in the report-language provenance. It repairs the archived 2010-11 enrollment workbook's district-row alignment corruption without guessing from neighboring rows. Historical report spellings are normalized through the shared state canonicalizer; if a raw 2010-11 district still lacks a reviewed report total, its raw enrollment remains QA-only and the analytical denominator is missing rather than falling back to the corrupt workbook row. `scripts/build_dise_report_total_enrollment_2010.py` verifies the tracked counts from the same reviewed page provenance; it is a maintainer command and is not part of the targets runtime graph.
- `dise_report_school_quality_2011_15.csv` stores published all-school PTR, single-teacher-school percentages, and girls'-toilet percentages for 2011-12 through 2014-15 with page-level provenance. `scripts/build_dise_report_school_quality.py` re-verifies every tracked page against the registered PDFs. The girls'-toilet denominator changes after 2011-12, so longitudinal infrastructure diagnostics begin in 2012-13; PTR and single-teacher trajectories can begin in 2011-12. Because these publication values are already ratios, later child districts are never averaged back to a Census-2001 parent: only one-to-one deterministic source/target rows enter the mechanism panel.

`dise_archive_registry.csv` records the round-specific Teacher sheet for 2005-06 through 2013-14. The 2014-15 and 2015-16 summary sheets co-locate teacher and school-quality counts, so those rows intentionally leave `teacher_sheet` blank.

DISE metadata files used by the `targets` pipeline are declared as explicit `format = "file"` dependencies before parsing. This ensures incremental builds invalidate cached parsed metadata when a registry, crosswalk, publication check, or report-language CSV changes.

### Census 2011 household mechanisms

HH-08, HH-10, and HH-11 are active extended-diagnostic sources under `data/raw/census_2011/households/`. `R/io/read_census_households.R` validates each table's published accounting and `R/measures/build_census_households.R` reconciles their household universes before using the shared complete deterministic 2011-to-2001 count bridge. The branch is descriptive in this phase; it does not add post-treatment HH variables to preferred controls or automatically create another weak-IV outcome family.


Census housing diagnostics now activate 2011 HL-13 for follow-up structural durability. The exact 2001 counterpart is H-04; it is not yet in the 2001 acquisition manifest, so no durability change is constructed from substitute material tables.
