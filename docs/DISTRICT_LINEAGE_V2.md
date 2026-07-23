# District-lineage rebuild: data, method, and work plan

## Status and purpose

This document is the durable handoff for the parallel district-lineage-v2 rebuild. It is written for researchers and coding agents who may not have access to the local raw files.

The substantive goal is a district-level pseudo-panel whose analytical unit is a **Census 2001 district**. The 2007-08 and 2017-18 NSS observations must be mapped backward to that geography before English-medium-instruction exposure, consumption outcomes, inequality, controls, and spatial relationships are constructed.

The v2 rebuild currently runs only as an extended diagnostic. The inherited [`data/metadata/district_harmonization_crosswalk.csv`](../data/metadata/district_harmonization_crosswalk.csv) remains the production authority until the new ledgers are adjudicated and all migration gates pass. Do not silently substitute v2 candidates into the paper.

Migration readiness is derived from the prerequisite gates rather than held permanently false. Passing readiness means the v2 evidence is internally complete; replacing the production crosswalk remains an explicit maintainer action. Failed prerequisites are written to `migration_blockers.csv` with one concrete next action per gate.

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

The generated `source_adjudication_queue.csv` is the compact work queue for those ledgers. It emits one row per NSS source identity, selects the wave-preferred top representation without accepting it, and classifies the row as a cross-vintage exact candidate, single-vintage exact candidate, high-precision fuzzy candidate, ordinary fuzzy review, or no candidate. The full candidate ledger remains available for evidence review.

## Source hierarchy

### Primary official anchors

**Census 2001 C-16 Mother Tongue.** The 35 state/UT workbooks under `data/raw/census_2001_mother_tongue/` define the linguistic-composition input to the IV. The IV must be constructed directly on the final 2001 geography. The 2001 registry uses the project’s Census-2001 state-code table rather than current LGD state names: historical codes 25 and 26 identify Daman & Diu and Dadra & Nagar Haveli separately, while current LGD represents their later merged union territory.

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

The generated candidate ledger records Jaro-Winkler, normalized Damerau-Levenshtein, trigram cosine, token similarity, rank, best-versus-second-best margin, reciprocal-nearest status, and a high-precision-candidate flag. The provisional rule is a review aid only. It has not been validated on enough independent cases to support a 99.5% precision claim. Directional qualifiers are identity-bearing: candidates with incompatible north/south, east/west, central, upper, or lower tokens cannot receive the high-precision flag. Fuzzy ranks and margins are computed across distinct normalized candidate names, not repeated representations of the same name in multiple reference vintages; ties use the survey wave's preferred reference vintage.

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
- generated SHRUG transition weights are finite, nonnegative, and never overallocate a source district;
- complete mapped mass is tracked separately from weight validity, and any unmapped mass must be resolved or represented in a reviewed sensitivity allocation;
- accepted sensitivity-allocation weights sum to one by source group;
- every exclusion has a reason;
- treatment, outcome, IV, and validated 2001 geometry are present for every included panel row;
- production and diagnostics use the same adjudicated ledgers;
- mainland spatial islands are explained or repaired;
- sensitivity results are reported for non-nested and low-confidence cases.

Only after those gates pass should `strict_district_panel_validation` and `strict_analysis_panel_validation` be reconsidered for final-mode activation.

## Completion workflow

The diagnostic now writes five additional completion artifacts:

- `adjudication_draft.csv`: one review-ready row per source identity, always
  `needs_review`; it is never an automatic acceptance ledger;
- `sensitivity_source_crosswalk.csv`: deterministic accepted mappings plus
  accepted reviewed allocation weights;
- `production_crosswalk_comparison.csv`: accepted v2 source mappings compared
  with the current production panel;
- `geometry_2001_qa.csv`: the geometry availability and topology contract;
- `completion_status.csv`: one row for each of the nine remaining research
  steps, with observed progress and the next required action.

A completion step passes only when its own evidence is complete. In particular, one accepted sensitivity allocation cannot clear unrelated coverage gaps, and production review passes only when every accepted v2 mapping has a corresponding `same_target` production comparison. Generated adjudication drafts omit already accepted or excluded identities, join evidence on the exact `(source_row_id, recommended_unit)` pair, and report only unresolved fuzzy rows. Production comparison also treats a legacy source code with multiple distinct targets as `ambiguous_production_mapping` rather than expanding the comparison table.

The draft is intentionally generated rather than tracked. A researcher must
verify administrative continuity and evidence, then copy only reviewed rows
into `district_adjudications_v2.csv`. This preserves the distinction between a
reproducible recommendation and a historical fact.

`dissolve_shrid_geometry_2001_v2()` implements the compact geometry operation
for local SHRID polygons. It joins only deterministic 2001 memberships and
unions polygons by code-based 2001 district ID. `geometry_qa_v2()` checks
coverage against the canonical registry and counts unexpected, missing, and
invalid geometries. The raw polygons remain local-only; the derived compact
GeoPackage may be retained after review.

The canonical public-build audit now runs the cached
`lineage-geometry-build` stage immediately before extended diagnostics whenever
`--with-extended-diagnostics` is enabled. Therefore, the preferred workflow is
the usual `scripts/run_public_build_audit.sh` command; a separate
`make lineage-geometry` call is unnecessary. The standalone target remains
available for geometry-only debugging.

The build is skipped when the optional raw archive is absent and is also skipped
when the compact GeoPackage is newer than its material SHRID keys, Census 2001
registry inputs, manifest, and implementation files. When rebuilding, invalid
input and dissolved geometries are repaired with `sf::st_make_valid()` and then
revalidated.

The generated GeoPackage is deliberately ignored by ordinary Git because it is
locally reproducible and currently exceeds GitHub's 50 MiB warning threshold.
It remains available to the review-archive workflow. The compact CSV QA and
`geometry_2001_unit_coverage.csv` identify every expected, missing, or
unexpected district without requiring reviewers to inspect the binary file.

Run `make lineage-geometry` directly only when debugging the local
`data/raw/shrug/open-polygons/shrug-shrid-poly-gpkg.zip` archive.
The command builds only the cached source targets needed by the geometry
script, extracts the single GeoPackage, dissolves deterministic SHRID
memberships, writes
`outputs/derived/district_lineage_v2/district_2001.gpkg`, and then runs
extended diagnostics once so geometry QA and completion status update. The
dissolve uses the active `sf` geometry column rather than assuming it is named
`geometry`. The compact derived
file is treated as a file dependency; routine diagnostics do not repeatedly
read or union the 380 MB source archive.

## Reviewed high-coverage sensitivity allocations

Allocation source keys use the same canonical unit identifier as the
administrative registry: `pc2011__SS__DDD`, where `SS` is the two-digit
Census-2011 state code and `DDD` is the three-digit district code. The tracked
allocation CSV is read with identifier columns explicitly declared as
character, following the CSV reader's standard `colClasses` contract.
Legacy character values such as `01.010` may be normalized at the ingestion
boundary, but numeric reconstruction is prohibited because it cannot recover
lost leading or trailing zeros reliably.


The first allocation tranche addresses incomplete SHRID coverage without
relaxing the preferred-panel rule. For a Census-2011 source district with at
least 99 percent of its population represented in the SHRUG bridge, the mapped
population shares are renormalized to sum to one and recorded as an accepted
`sensitivity-only` allocation. This produces 503 source-target rows covering
457 source districts.

The threshold does not convert a non-nested transition into a deterministic
identity. These rows appear only in the sensitivity crosswalk. Districts below
99 percent mapped population remain unresolved, and the preferred panel
continues to require direct Census-2001 identity or deterministic containment.

Allocation source keys use the canonical `SS.DDD` Census-2011 code form.
Readers normalize numeric CSV imports back to this representation before
validation; this is necessary because base R type conversion can remove leading
zeros and trailing zeros from values such as `01.010`. Generated and reviewed
coverage are compared only after this normalization.

## First reviewed decisions

The first accepted tranche is deliberately narrow:

- the nine NCT Delhi districts in the Census 2001 frame;
- Mumbai and Mumbai Suburban in the Census 2001 frame.

The Census 2011 NCT Delhi Administrative Atlas explicitly reports no change in
the number of districts or tahsils during 2001–2011. The corresponding
Maharashtra Administrative Atlas preserves Mumbai and Mumbai Suburban across
the two census frames. Their 2011 polygons are therefore carried back to the
same named Census 2001 units without interpolation. Before row binding, both
the SHRID-derived and carried-back features are reduced to a common
`unit_id + geometry` schema so differences in source attribute names or active
`sf` geometry-column names cannot alter the result. The decision is recorded in
`district_geometry_carrybacks_v2.csv`, and only the corresponding exact-name
NSS source identities are accepted in `district_adjudications_v2.csv`.

This is not a general rule that repeated names imply unchanged geography.
Other exact-name rows remain unresolved until their own boundary history or
code-based continuity is reviewed.

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

### Lineage-source execution

Extended district-lineage diagnostics track and cache each loaded raw source independently. Large LGD SpreadsheetML changed-unit rosters are streamed into their canonical columns, and SHRUG key readers retain only columns needed by the bridge. Inventory-only geometry and locality-attribute archives remain visible in the source inventory without being loaded into the general lineage bundle. Incremental audits therefore reread only sources whose specification, reader, or file changed; village changed-unit coverage remains included.

The Census-2001 lineage registry takes district labels from C-16 `district_name` and state labels from the project’s vintage-specific Census-2001 code table; numeric production aliases and current-LGD mergers are never substituted for historical administrative names.

The adjudication queue distinguishes candidate representations from semantic alternatives. Repeated exact names across Census 2001, Census 2011, and current LGD are one name candidate with several vintage representations; they are classified as `cross_vintage_exact_candidate`, not as several competing exact names. A single-vintage exact name remains `single_vintage_exact_candidate`. External evidence requests are generated only after these deterministic exact-name cases have been reviewed, for fuzzy, missing, or explicitly `needs_review` identities and the events relevant to them.
