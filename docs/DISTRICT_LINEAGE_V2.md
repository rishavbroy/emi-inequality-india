# District-lineage rebuild: data, method, and work plan

## Status and purpose

This document is the durable handoff for the parallel district-lineage-v2 rebuild. It is written for researchers and coding agents who may not have access to the local raw files.

The substantive goal is a district-level pseudo-panel whose analytical unit is a **Census 2001 district**. The 2007-08 and 2017-18 NSS observations must be mapped backward to that geography before English-medium-instruction exposure, consumption outcomes, inequality, controls, and spatial relationships are constructed.

The v2 rebuild currently runs only as an extended diagnostic. The inherited [`data/metadata/district_harmonization_crosswalk.csv`](../data/metadata/district_harmonization_crosswalk.csv) remains the production authority until the new ledgers are adjudicated and all migration gates pass. Do not silently substitute v2 candidates into the paper.

## Non-negotiable warnings

- The legacy 454-district count and the legacy first-stage estimate 2.945 (SE 0.949) and second-stage estimate 0.201 (SE 0.710) followed an almost certainly flawed district-matching procedure. They are historical comparisons, not parity targets.
- The legacy partial F-statistic of 37.77 is known to be wrong because the legacy code computed it incorrectly.
- India State Stories, Jaacks, LGD changed-unit reports, published concordances, fuzzy scores, and unchanged names are **evidence**, not self-validating geographic truth.
- The public NSS files expose district identifiers and survey FSUs, but no demonstrated Census village, town, tehsil, or subdistrict identifier. Lower-level Census data can characterize territorial transitions; it generally cannot identify which part of a non-nested NSS district contains a particular household.
- Gini coefficients and other nonlinear statistics cannot be averaged across source districts. Pool or allocate microdata and recompute them, or exclude the non-identified mapping from the preferred panel.

## Methodological target

The preferred panel should include a source district only when its relationship to a 2001 district is deterministic:

1. unchanged geography;
2. documented rename;
3. one later child wholly contained in one 2001 parent;
4. multiple later children wholly contained in one 2001 parent and pooled backward.

A source district that overlaps more than one 2001 district is non-nested. Unless household locations below district can be recovered, it should be excluded from the preferred specification and handled through sensitivity analyses:

- Census-population allocation;
- dominant-parent rules with prespecified thresholds;
- stable composite regions;
- IPUMS harmonized GEO2 geography;
- NSS-region aggregation.

Population and area weights are useful for transitions and additive quantities. They do not recover target-specific means, shares, distributions, or Ginis when the household location within the source district is hidden.

## Architecture

The v2 design separates five objects that the legacy/current method partly conflates.

1. **Administrative units:** dated versions of districts and relevant component units.
2. **Administrative events:** sourced parent-child edges for renames, splits, mergers, carve-outs, transfers, and state reorganizations.
3. **Source identities:** one adjudicated administrative unit for every unique NSS wave/state/district row.
4. **Allocation weights:** source-unit to Census-2001-district shares, used only when appropriate and always with a stated basis.
5. **Diagnostics:** candidate scores, duplicate keys, unresolved events, evidence requests, exclusions, and migration gates generated from the same objects used by production.

The compact tracked ledgers are:

- [`data/metadata/district_sources_v2.csv`](../data/metadata/district_sources_v2.csv): source citations and paths;
- [`data/metadata/district_match_gold.csv`](../data/metadata/district_match_gold.csv): manually reviewed string-match examples and hard negatives;
- [`data/metadata/district_adjudications_v2.csv`](../data/metadata/district_adjudications_v2.csv): accepted, excluded, or needs-review source identities;
- [`data/metadata/district_admin_events_v2.csv`](../data/metadata/district_admin_events_v2.csv): accepted/rejected/needs-review administrative edges;
- [`data/metadata/district_allocation_weights_v2.csv`](../data/metadata/district_allocation_weights_v2.csv): reviewed sensitivity allocations.

Do not add columns unless they support a real decision, invariant, or citation. Git history records editors and dates; reviewer metadata need not be repeated in every row.

## Source hierarchy

### Primary official anchors

**Census 2001 C-16 Mother Tongue.** The 35 state/UT workbooks under `data/raw/census_2001_mother_tongue/` define the linguistic-composition input to the IV. The IV must be constructed directly on the final 2001 geography.

**Census 2001 and 2011 locality data through SHRUG.** Population Census keys link `shrid2` to village/town, subdistrict, district, and state identifiers in 2001 and 2011. PCA files provide population and household denominators; Village and Town Directories provide area, facilities, and administrative attributes. These are the principal inputs for a locality-based 2001-2011 transition matrix.

**Census 2011 polygons through SHRUG.** District, state, and subdistrict GeoPackages provide code-based 2011 geometry. Large village and SHRID polygon archives are local-only inputs for a dedicated geometry build. Dissolving SHRID geometry by 2001 district membership is the planned starting point for `districts_2001.gpkg`.

**Local Government Directory.** Current JSON/XLSX files provide LGD codes, current hierarchy, Census-code fields, ULB coverage, and component registries. The 2011-01-01 to 2018-06-30 modification exports identify entities changed during the interval but generally do not identify the action, predecessor, effective date, or territorial share. Treat them as changed-unit rosters requiring corroboration.

### Candidate lineage and alias sources

**India State Stories.** Use the Alluvial workbook, split/carve-out workbook, name-change workbook, new-district workbook, ISDED, and Indian Census Data Collection to generate and cross-check candidate names and lineage edges. Do not assume the Alluvial workbook is complete or correct; known issues include missing or inconsistent entries and it is not a normalized event table.

**Jaacks Research Group tracker.** Use `IndiaDistrictTracker2001to2020.ods` as an annual-name and candidate-lineage reference. Legacy work found omissions and tracker errors, so it cannot adjudicate an event by itself.

**Kumar and Somanathan (2016).** The locally extracted headerless CSV records pre-2001 changes from the paper. The paper's approach to clean partitions, non-nested transfers, and population shares is methodological evidence and a benchmark, not a complete post-2001 crosswalk.

**Published PLFS/NSS/NRLM concordance.** The Oxford concordance folder provides Census-to-PLFS, PLFS-to-NSS, NRLM-to-PLFS, Census-region, and Telangana mappings. It is useful independent evidence, especially for the 31-to-10 Telangana relationship, but contains unmatched rows and does not provide legal event dates or territorial shares.

### Geometry and sensitivity sources

**2019/2020 boundary repositories.** These contain later district and, for 2019, subdistrict geometry. Use them for candidate spatial checks and maps, not as the authoritative 2017-18 or 2001 geography.

**IPUMS GEO2_IN 1987-2009.** This is a harmonized, temporally stable second-level geography with some district detail combined. Use it as an independent sensitivity geography, not as the 2001-district target.

**NSS regions.** Murthi et al. motivate regions when district samples are weak. Region definitions can also change and can cut across district boundaries, so this remains a lower-priority sensitivity analysis.

### Post-period validation

`data/raw/local_government_directory/changes.csv`, downloaded from the ramSeraph LGD archive, begins on 2018-10-13. It does not identify the 2017-18 source geography and must not affect the preferred mapping. It can validate later LGD code changes, test event-processing code, and detect whether a current hierarchy differs from the first archived post-period state.

## Source-specific caveats

- LGD JSON was retained instead of CSV to avoid local-name encoding errors in the user's CSV viewer.
- LGD files named `.xls` in the modification directory are SpreadsheetML XML, not legacy binary Excel files.
- The LGD modification reports contain many duplicate rows and describe entities modified within a date interval, not complete event records.
- The Alluvial workbook uses year-paired columns and needs a dedicated parser.
- The Kumar-Somanathan CSV is headerless.
- SHRUG locality keys are not necessarily unique on `shrid2`; duplicate locality rows must be aggregated for population and area weights.
- SHRUG district keys are unique on the full SHRID-state-district key, not necessarily on `shrid2`. Multi-district SHRID rows are explicit cross-boundary cases and must remain distinguishable from genuinely missing Census membership.
- SHRUG open polygons are analytical approximations assembled from multiple sources. Geometry QA, validity repair, area checks, and alternative-source comparison are required before final adjacency is accepted.
- The current 2020 geometry belongs to the existing production draft and should not be relabeled as 2001 geometry.

## Matching policy

Matching proceeds within compatible state and date/lineage universes:

1. exact official code;
2. exact normalized name;
3. curated alias or official rename;
4. documented lineage candidate;
5. fuzzy candidate generation;
6. manual adjudication.

Exact names are candidates rather than automatic acceptance because a stable name can conceal a boundary change. Fuzzy matching must never create an administrative lineage.

The generated candidate ledger records Jaro-Winkler, normalized Damerau-Levenshtein, trigram cosine, token similarity, rank, best-versus-second-best margin, reciprocal-nearest status, and a high-precision-candidate flag. The provisional rule is a review aid only. It has not been validated on enough independent cases to support a 99.5% precision claim.

The gold set deliberately contains:

- real NSS spelling, punctuation, transliteration, abbreviation, and historical-name variants;
- known bad close-name matches such as Farrukhabad/Firozabad, Upper Siang/Upper Subansiri, Imphal East/Imphal West, Dang/Porbandar, and Kasganj/Kushinagar;
- 2007-08 typos observed in the NSS roster;
- cases requiring official rename evidence rather than string similarity.

Do not train and evaluate thresholds on the same entire gold set. Grow it, hold out states or cases, and report precision/recall by case type.

## Variable construction rules

- **Counts and totals:** sum pooled observations or allocate additive components.
- **Means:** pool microdata and recompute weighted means.
- **Shares/rates:** pool or allocate numerators and denominators, then divide.
- **EMIE:** recompute from pooled eligible children.
- **Consumption Gini:** recompute from pooled household records and survey weights; never average district Ginis.
- **Linguistic-distance IV:** construct directly from Census 2001 language composition.
- **Geometry:** attach the validated 2001 polygon after the analytical identity is fixed.

For a non-nested source district, population allocation is a sensitivity assumption. Multiplying every household weight by the same source-to-target share cannot recover target-specific within-source distributions.

## Required invariants before migration

The existing production crosswalk must not be replaced until all of the following pass:

- one unique analytical ID per Census 2001 district;
- every included source row has one accepted identity valid for its wave;
- every accepted event and allocation cites a registered source;
- no unexplained cross-state match;
- no conflicting duplicate source key;
- no unresolved or merely fuzzy candidate enters production;
- primary mappings are deterministic containments or documented renames;
- allocation weights are finite, nonnegative, and sum to one by source group;
- every exclusion has a reason;
- treatment, outcome, IV, and validated 2001 geometry are present for every included panel row;
- production and diagnostics use the same adjudicated ledgers;
- mainland spatial islands are explained or repaired;
- sensitivity results are reported for non-nested and low-confidence cases.

Only after those gates pass should `strict_district_panel_validation` and `strict_analysis_panel_validation` be reconsidered for final-mode activation.

## Immediate work plan

### 1. Make the parallel diagnostic green

Run the full unit suite and extended diagnostic. Fix source parsers and empty-input behavior without weakening invariants. The diagnostic should fail clearly for missing required local inputs while inventory-only sources remain optional.

### 2. Audit the raw-source inventory

Confirm exact local paths, file formats, encoding, date coverage, and source roles. Keep [`data/metadata/data_sources.csv`](../data/metadata/data_sources.csv) as the general source catalog and [`data/metadata/district_sources_v2.csv`](../data/metadata/district_sources_v2.csv) as the compact citation IDs referenced by adjudication ledgers.

### 3. Build and validate the 2001/2011 registries

Quarantine special `000`/unassigned Census polygon rows. Validate state and district code widths, official counts, missing names, and duplicates.

### 4. Build the SHRID transition bridge

Use rural and urban keys, full source-district denominators, population and area shares, and explicit coverage flags. Do not renormalize away unmatched or cross-boundary SHRID units.

### 5. Generate source rosters and candidate ledgers

Create unique NSS wave/state/district rows. Detect identical and conflicting duplicates. Score candidates but leave all unadjudicated identities outside production.

### 6. Adjudicate deterministic cases first

Accept code matches, documented renames, and clean containment. Record exclusions and evidence sources. Do not begin with fuzzy cases.

### 7. Resolve only consequential ambiguous events

Search official web sources first. Add a row to `evidence_requests.csv` only if the event affects an observed NSS source or target 2001 district and remains unresolved. Request a specific Gazette order, Atlas annex, or village schedule rather than a whole national collection.

### 8. Construct compact 2001 geometry

Use local SHRID polygons to dissolve by the accepted 2001 registry. Save a compact derived GeoPackage and QA outputs; raw giant polygons remain local-only.

### 9. Rebuild measures and compare panels

Construct the deterministic panel and compare it with the inherited production panel row by row. Explain every changed inclusion, treatment, outcome, IV, and geometry. Do not optimize the crosswalk to recover legacy coefficients or the invalid legacy F-statistic.

### 10. Add sensitivity panels and migrate deliberately

Estimate deterministic, population-allocation, stable-composite, IPUMS, and NSS-region variants. Switch production only after review and strict gates pass.

## Generated review outputs

The extended target writes to:

```text
outputs/diagnostics/extended/district_lineage_v2/
```

Key files include source inventory and registry, SHRID bridge QA and transition weights, source candidates, candidate events, eligibility/exclusion tables, duplicate keys, evidence requests, gold-set scores, and migration-readiness gates. These outputs must be included in `review.zip`.

## Developer workflow

Before returning a district-lineage patch:

```bash
python3 scripts/test_impact.py --base <pre-patch-commit>
make test-affected BASE=<pre-patch-commit>  # when R is available
make test
```

Inspect every selected test file. Update production code, tests, metadata, and documentation in the same patch. The final user-facing verification remains the canonical public-build audit with extended diagnostics.
