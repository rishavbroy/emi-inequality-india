# Historical linguistic-distance validation

## Objective

The historical-instrument exercise asks whether the district linguistic-distance
variation used in the Census-2001 IV was already present before the expansion
of English-medium instruction. The preferred validation keeps the language
ontology fixed and changes only Census-year speaker shares:

`distance(d, t) = sum_l share(d, l, t) * distance(l, Hindi)`.

The 1991 and 2001 constructions must therefore reuse the same reviewed language
identity/distance machinery. A high correlation is descriptive persistence, not
proof of the exclusion restriction; historical balance and pre-trend tests are
a separate phase.

## 1991 language source

The Office of the Registrar General and Census Commissioner's *Language Atlas of
India 1991* is the primary historical language-distribution source. Annexure IV,
"District-wise data sheet of different languages," begins on printed page 192
(PDF page 205). It reports 1991 district total population and speaker counts by
scheduled and non-scheduled language. The Atlas is based on the classified 1991
Census language inventory.

Production ingestion must preserve the raw PDF and produce a separate auditable
long-form district-language extraction. The PDF's existing text layer is useful
for extraction but is not itself a clean machine-readable table, so production
values should not be accepted without row-total/coverage validation against the
1991 PCA and targeted source review of extraction anomalies.

## Geography before language allocation

SHRUG provides stable `shrid2` locality keys linking Population Census places
across 1991, 2001, and 2011, plus district-membership keys for each Census year.
The project now constructs the 1991-to-2001 bridge before ingesting Atlas counts.
Transition weights use **1991** locality population because the source quantity
being carried forward is the 1991 population distribution.

The first real bridge diagnostic showed that exact SHRID completeness is too
strict for a speaker-weighted historical comparison: only 64 of 451 source
districts are completely mapped even though median deterministic **population**
coverage is about 99.96%. The preferred geography therefore requires exactly
one observed deterministic Census-2001 target district and at least 99% deterministic 1991 population
coverage. Exact one-to-one districts remain separately flagged, and threshold
sensitivity is reported at 95%, 98%, 99%, 99.5%, 99.9%, and 100% coverage.

This is a coverage criterion, not an exact reconstruction claim. If a 1991
district splits across multiple 2001 districts, district-level Atlas language
shares do not reveal the within-parent location of each language. Those cases
must not be fractionally disaggregated in the preferred exercise merely because
SHRUG can allocate total population. They remain explicit sensitivity/exclusion
cases.

The diagnostic outputs are:

- `historical_linguistic_geography_1991_2001.csv`: one row per 1991 source
  district with SHRID/population coverage, number of 2001 targets, mapping class,
  and preferred-persistence eligibility;
- `historical_linguistic_geography_coverage_sensitivity.csv`: preferred-sample
  counts and represented 1991 population under alternative coverage cutoffs;
- `historical_linguistic_transition_1991_2001.csv`: population/area transition
  weights for deterministic SHRID membership;
- `historical_linguistic_shrid_bridge_1991_2001.csv`: bridge-status summary.


## Atlas extraction contract

Annexure IV occupies PDF pages 205--260. Direct inspection shows seven repeated
eight-page district blocks. Across each block the language headers cover Atlas
columns 4--117 exactly once, for 114 language columns in total. The PDF has a
usable positioned text layer, so OCR is not part of the production plan.

`scripts/build_language_atlas_1991.py` is the maintainer-side extractor. It uses
Poppler's `pdftotext -bbox-layout`, preserves the raw positioned text for every
candidate cell, and applies only conservative integer parsing: commas, periods,
and whitespace are treated as thousands-separator/scanning artifacts, while the
standalone OCR glyphs `O` and `()` are retained as explicitly tagged zero
normalizations. Other nonnumeric glyphs and cells with no positioned text are
not guessed. They enter a review queue.

A maintainer run writes the positioned-cell products plus a separate district/PCA validation layer:

```bash
python3 scripts/build_language_atlas_1991.py \
  --pdf data/raw/census_1961-91/Language_Atlas_of_India_1991.pdf \
  --candidate-output /tmp/language_atlas_1991_candidate.csv \
  --review-output /tmp/language_atlas_1991_review.csv \
  --layout-output /tmp/language_atlas_1991_layout.csv \
  --pca-zip data/raw/shrug/census_1991/shrug-pca91-csv.zip \
  --state-crosswalk data/metadata/language_atlas_1991_state_crosswalk.csv \
  --district-output /tmp/language_atlas_1991_district_population.csv \
  --population-review-output /tmp/language_atlas_1991_population_review.csv
```

The candidate output is deliberately keyed by Atlas page/block, detected row,
and source column rather than pretending that OCR-distorted district labels are
already Census identifiers. The review output contains every blank/unparsed
cell plus any page whose detected row count disagrees with the modal row count
for its eight-page block. The layout self-test runs in every source-syntax
audit; the raw PDF itself remains outside the normal targets dependency graph.

On the currently registered Atlas scan, all 56 pages preserve the 114-column
layout. The positioned-text candidate pass recovers 54,981 cells, including 478
Atlas population-column cells used for later denominator checks. Of all candidate
cells, 50,664 are plain conservative integer parses, 763 are explicit OCR-zero
normalizations, 659 contain other nonnumeric text-layer artifacts, and 2,895 have
no positioned text at the candidate row/column intersection. Fifteen pages also
require row-alignment review. These figures are extraction diagnostics, not
data-quality claims and not production speaker counts.

District identity now uses the official 1991 Census coding structure rather than
name similarity. The ORGI 1991 editing/coding manual defines state codes and
within-state district serials; `language_atlas_1991_state_crosswalk.csv` records
the reviewed OCR form of the 31 state/UT headings present in the Atlas scan
(Jammu and Kashmir is absent because the 1991 Census was not conducted there).
Within each reviewed state block, the extractor accepts only a source serial from
the Atlas serial column or a leading serial printed with the district label. It
does not infer a missing serial from the district name, row order, or nearest
population.

SHRUG's `pc91_pca_clean_pc91dist.csv` then provides an independent population
check keyed by `pc91_state_id` and `pc91_district_id`. A 1% relative-population
difference is used only as an **extraction QA tolerance**, not as a substantive
sample-support rule. Rows outside that tolerance remain review items; population
is never used to discover district identity. Obvious wrapped labels whose first
line carries the district serial but whose second line carries the population
are joined only under that structural condition.

On the currently registered sources this produces 443 district-row candidates.
Three wrapped labels are deterministically rejoined and 414 rows have a unique
official-code candidate plus Atlas/PCA population agreement within 1%. The
remaining queue includes OCR population failures, source-population differences,
missing/garbled district serials, duplicate serial candidates, and PCA districts
whose Atlas population row was not recovered. These are source-review statuses,
not silently imputed observations.

The next promotion step must resolve that district/PCA review queue and validate
row alignment across the remaining seven language pages before a tracked
district-language long table can enter targets.

## Vanneman source provenance

The downloaded Vanneman-Barnes snapshot is useful for later pre-treatment
balance and pre-trend diagnostics, but it is not yet promoted to a production
historical panel. The codebook documents identifier column 10 as version 2 for
cross-sectional data and version 6 for the 1961--91 panel. Direct source QA finds
`panel4.data.gz` entirely at version 5, while `dist81.data.gz` mixes versions 2
and 3 (the non-contract records are education records 151--156 in the downloaded
snapshot). `dist91.data.gz` matches the documented version-2 cross-section.

`vanneman_historical_source_qa.csv` records these contracts explicitly. A file
whose year/version identifiers disagree with the downloaded codebook is not
eligible for baseline values until its vintage is resolved. This prevents the
pipeline from silently interpreting an undocumented harmonized-panel revision.
The Vanneman panel's own historical harmonization assumptions also remain a
sensitivity source rather than geography authority for the Census-2001 panel.

## Next phases

1. Resolve the remaining Atlas district/PCA population review queue and bind the
   validated district identities across all eight pages of each Atlas block.
2. Reuse the frozen Shastry/Glottolog language identity machinery rather than
   defining a separate 1991 distance scale.
3. Construct 1991 district linguistic distance on native 1991 geography.
4. Compare 1991 and 2001 distance on the one-target, high-population-coverage
   preferred sample; report the exact one-to-one result separately and show
   population-weighted Pearson/Spearman and within-state persistence.
5. Treat split/non-nested geography as sensitivity evidence, not as preferred
   exact reconstruction.
6. Add 1961-1991 predetermined baseline/pre-trend diagnostics from Vanneman and
   SHRUG PCA/VD/TD in a separate historical-balance module.
