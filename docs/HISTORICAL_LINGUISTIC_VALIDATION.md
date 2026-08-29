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
The project now constructs the 1991-to-2001 bridge before ingesting Atlas counts. `summarize_shrug_source_district_mapping()` in `R/districts/lineage_bridge.R` owns the source-district coverage classification, so historical persistence uses the same deterministic SHRID semantics as the production lineage system.
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
candidate cell, and applies only conservative single-integer parsing. Commas or
periods may be thousands separators (including Indian grouping), but internal
whitespace between digit groups is **not** collapsed: text such as
`500 1,375,267` or `893 67` can contain values leaked from adjacent table cells
and therefore enters review rather than becoming a fabricated concatenated
count. The standalone OCR glyphs `O` and `()` remain explicitly tagged zero
normalizations. Other nonnumeric glyphs and cells with no positioned text are
not guessed.

A maintainer run writes the positioned-cell products plus a separate district/PCA validation layer:

```bash
python3 scripts/build_language_atlas_1991.py \
  --pdf data/raw/census_1961-91/Language_Atlas_of_India_1991.pdf \
  --candidate-output /tmp/language_atlas_1991_candidate.csv \
  --review-output /tmp/language_atlas_1991_review.csv \
  --layout-output /tmp/language_atlas_1991_layout.csv \
  --pca-zip data/raw/shrug/census_1991/shrug-pca91-csv.zip \
  --state-crosswalk data/metadata/language_atlas_1991_state_crosswalk.csv \
  --language-registry data/metadata/language_atlas_1991_languages.csv \
  --cell-review-registry data/metadata/language_atlas_1991_cell_reviews.csv \
  --cell-review-template-output /tmp/language_atlas_1991_cell_review_template.csv \
  --accepted-source-output /tmp/language_atlas_1991_accepted_source.csv \
  --district-output /tmp/language_atlas_1991_district_population.csv \
  --population-review-output /tmp/language_atlas_1991_population_review.csv \
  --page0-language-output /tmp/language_atlas_1991_page0_languages.csv \
  --page0-language-review-output /tmp/language_atlas_1991_page0_language_review.csv \
  --all-language-output /tmp/language_atlas_1991_all_languages.csv \
  --all-language-review-output /tmp/language_atlas_1991_all_language_review.csv \
  --alignment-review-output /tmp/language_atlas_1991_alignment_review.csv \
  --coverage-output /tmp/language_atlas_1991_coverage.csv \
  --coverage-review-output /tmp/language_atlas_1991_coverage_review.csv \
  --coverage-sensitivity-output /tmp/language_atlas_1991_coverage_sensitivity.csv \
  --excess-triage-output /tmp/language_atlas_1991_excess_triage.csv \
  --unresolved-triage-output /tmp/language_atlas_1991_unresolved_triage.csv
```

The excess-triage output is a review aid for districts whose accepted speaker
sum exceeds Atlas population. It ranks already accepted cells and marks whether
removing one cell would, by itself, restore the population bound. It never edits,
drops, or substitutes a count; any correction still requires source review. On
the current source pass it contains 837 accepted cells across the 10 impossible
districts; 57 are individually large enough that correcting/removing that one
cell could restore the population bound.

The unresolved-triage output addresses the other high-value review queue without
choosing a preferred coverage threshold. It contains only fully aligned districts
whose accepted-speaker lower bound already certifies the lowest registered
sensitivity threshold (95%) but which still have unresolved cells. For each cell
it reports the district's current certified threshold, the next registered
threshold, the additional speaker mass needed to reach it, and any independently
parseable numeric groups present in the raw PDF text. Numeric groups are exposed
only as review candidates; the extractor never selects among them. On the current
source pass this yields 161 unresolved cells across 30 districts: 69 blank cells,
50 ambiguous multi-number cells, and 42 other unparsed cells.

The optional cell-review-template output joins those two priority queues back to
the canonical all-language cells and prefills the exact fingerprint columns
required by `language_atlas_1991_cell_reviews.csv`: page, raw cell text, machine
candidate, parse status, and alignment status. It also records which triage queue
and rank produced the row. The actual `review_decision`, `reviewed_speaker_count`,
and `review_basis` fields remain blank. The template is therefore a transcription
safety aid, not an adjudication file; maintainers must inspect the PDF source and
then copy only completed decisions into the tracked review ledger. The extractor
continues to fail closed if a later parser run changes any fingerprinted field.

The candidate output is deliberately keyed by Atlas page/block, detected row,
and source column rather than pretending that OCR-distorted district labels are
already Census identifiers. The review output contains every blank/unparsed
cell plus any page whose detected row count disagrees with the modal row count
for its eight-page block. The layout self-test runs in every source-syntax
audit; the raw PDF itself remains outside the normal targets dependency graph.

On the currently registered Atlas scan, all 56 pages preserve the 114-column
layout. The positioned-text candidate pass recovers 54,981 cells. Under the
single-integer parser, 49,098 are ordinary parses, 763 are explicit OCR-zero
normalizations, 1,562 contain multiple numeric groups and are rejected as
ambiguous, 656 contain other unparsed text-layer artifacts, and 2,902 have no
positioned text at the candidate row/column intersection. Fifteen pages also
require row-alignment review. These figures are extraction diagnostics, not
data-quality claims and not production speaker counts.

A source-level row-ownership defect was found during impossible-sum review. At
some state-heading → district-01 boundaries, bold state-total values sit lower
in the PDF text layer than the heading itself, so a symmetric label-centered
window can assign a state total to district 01. The extractor now changes row
ownership only at that transition and only when the data columns expose two
source numeric baselines separated by approximately one printed row. The vertical
boundary is the midpoint of those two numeric baselines; if the source page does
not expose a convincing pair, the established extraction rule is left unchanged
and downstream QA handles the case. Ordinary district-row extraction is
unchanged. This retains Srikakulam's printed Bengali and Hindi counts while
removing, for example, a 393,825 Madhya Pradesh state-total Kurukh/Oraon value
that had leaked into Morena; the Morena cell now remains unresolved rather than
being accepted as a district count. No source-review override is used.

District identity now uses the official 1991 Census coding structure rather than
name similarity. The ORGI 1991 editing/coding manual defines state codes and
within-state district serials; `language_atlas_1991_state_crosswalk.csv` records
the reviewed OCR form of the 31 state/UT headings present in the Atlas scan
(Jammu and Kashmir is absent because the 1991 Census was not conducted there).
Within each reviewed state block, the extractor accepts a source serial from the
Atlas serial column or a leading serial printed with the district label. Because
Annexure IV repeats the same district rows across all eight language pages, a
serial that is missing on the population page may also be recovered from an
**exactly repeated raw district label** elsewhere in the same eight-page block
when all non-missing serial evidence for that exact label agrees. This is source
repetition, not name similarity: no fuzzy label match, row-order fill, or nearest-
population match is used. Conflicting repeated serial evidence remains a review
case.

SHRUG's `pc91_pca_clean_pc91dist.csv` then provides an independent population
check keyed by `pc91_state_id` and `pc91_district_id`. A 1% relative-population
difference is used only as an **extraction QA tolerance**, not as a substantive
sample-support rule. Rows outside that tolerance remain review items; population
is never used to discover district identity. Obvious wrapped labels whose first
line carries the district serial but whose second line carries the population
are joined only under that structural condition.

On the currently registered sources this produces 443 district-row candidates.
Three wrapped labels are deterministically rejoined. Cross-page exact-label
serial recovery resolves Greater Bombay and West Tripura and identifies Dhubri
as district 01 (its population remains outside the 1% QA tolerance). Tightening
the numeric parser also reclassifies one formerly accepted population string,
leaving **415** population-validated rows. The remaining queue includes OCR
population failures, source-population differences, conflicting/garbled district
serials, duplicate serial candidates, and PCA districts whose Atlas population
row was not recovered. These are source-review statuses, not silently imputed
observations.

For those 415 population-validated rows, the extractor first binds the population
page directly to Atlas language columns 4--14. It then aligns repeated district
rows on pages 2--8 with the same conservative identity philosophy used by the
district-lineage system. Exact raw district labels are alignment anchors. A
mismatched run is positionally aligned only when it is bounded by exact anchors
on both sides, has the same number of source and target rows, and all reference
rows plus both anchors belong to one 1991 Census state. Python's standard-library
`difflib.SequenceMatcher` is used only to locate exact matching blocks; no string
similarity score or fuzzy-name threshold enters the rule.

On the currently registered scan, the stricter parser and population gate align
31,488 district-language cells across all 114 Atlas columns for the 415
population-validated districts. Of those, 28,310 pass both conservative numeric
parsing and the per-language bound that a mother-tongue count cannot exceed its
district population; 3,178 remain review-required. A further 1,066 district-page
combinations cannot be aligned by the exact/bounded rule and remain in a
dedicated row-alignment review queue. These are extraction candidates, not a
production language table: unresolved district/page alignments are not filled by
sequence order, and no speaker count enters targets.


### Reviewed cell adjudication ledger

Machine extraction is now separated from source-reviewed acceptance.
`data/metadata/language_atlas_1991_cell_reviews.csv` is the sole tracked ledger
for cell-level source decisions. Its key is `(state_code_1991,
district_code_1991, atlas_column)`, and every decision fingerprints the exact PDF
page, raw extracted cell text, machine candidate, parse status, and alignment
status that were reviewed. If any of those source-extraction fields changes
later, application fails closed as a stale review rather than silently carrying
the old decision forward.

The allowed decisions are deliberately narrow:

- `accept_extracted`: source review confirms an already valid machine candidate;
- `replace_count`: source review supplies a nonnegative integer count that must
  not exceed Atlas district population;
- `leave_unresolved`: review has occurred but the cell is still not safely
  quantifiable.

The machine fields (`raw_value`, `speaker_count_candidate`, `parse_status`, and
`alignment_status`) are never overwritten. Reviewed decisions change only
`accepted_speaker_count` and its provenance. Coverage, impossible-sum, and
unresolved-cell diagnostics operate on this accepted-count layer. The review
ledger may remain header-only until source decisions are actually completed; no
empty ledger row is interpreted as acceptance. `--cell-review-template-output`
can prefill the exact source fingerprints for the current impossible-sum and
high-coverage-unresolved queues, but blank template rows are not valid ledger
decisions. This mirrors the repository's reviewed Glottolog and district-lineage
pattern: generated candidates remain non-authoritative until an explicit tracked
decision is applied.

The optional `--accepted-source-output` is the canonical promotion boundary from
maintainer extraction/adjudication to R. It contains one row per aligned
1991 district-language cell, retains unresolved cells with a blank
`accepted_speaker_count`, repeats the district coverage/status contract, and
preserves both machine and reviewed provenance. It applies **no analysis
threshold** and therefore is not itself the preferred historical-IV sample.

R reads that contract with `read_language_atlas_1991_accepted_source()` and
constructs the historical primary-distance analogue with
`build_historical_linguistic_distance_1991()`. The latter requires an explicit
`min_accepted_coverage` argument; there is deliberately no default. The long-form
cell rows are authoritative: R recomputes the number of aligned Atlas columns,
accepted/unresolved cell counts, accepted-speaker lower bound, population share,
and population-bound status and requires every repeated district summary field
to agree. A stale or hand-edited summary therefore cannot turn an incomplete or
impossible district into an eligible one.

Eligible districts must have the full 114-column inventory, must not violate the
district population bound, must meet the supplied accepted-speaker lower-bound
threshold, and must contain positive speaker mass on at least one nonzero
Shastry-mapped language. A district that passes source coverage but has no such
mass is retained with `historical_language_status = "no_nonzero_mapped_speakers"`
and an `NA` primary distance. The distance itself reuses the frozen
Atlas-to-Shastry resolver and the same speaker-weighted **nonzero mapped-language
mean** used by the 2001 primary IV. English remains a separately treated
reference language exactly as in the 2001 construction. Ineligible districts
stay in the output with explicit statuses rather than being silently dropped or
renormalized into eligibility.

The extractor also writes district-level lower-bound coverage diagnostics. It
sums **only** accepted counts: machine candidates that passed numeric and
per-language population QA, plus any source-reviewed replacements. Reviewed or
unreviewed unresolved cells are omitted without renormalization. The sum is
therefore an accepted-speaker lower bound, not an estimate of classified-language
coverage when columns remain unresolved. The district status distinguishes incomplete page
alignment, unresolved cells, and the stronger impossibility check in which even
the accepted-cell sum exceeds the Atlas district population. Review rows are
ordered first by impossible accepted-count sums, then by fully aligned districts
with unresolved cells, and finally by incomplete page alignment.

Coverage-threshold sensitivity is reported separately at 95%, 98%, 99%, 99.5%,
99.9%, and 100%. A district is *certified* at threshold `t` only when its
accepted-speaker lower-bound share is at least `t` and its accepted-count sum
does not exceed Atlas population. This is deliberately one-sided: unresolved
cells are never set to zero, and the sensitivity table does not declare a
preferred analysis threshold. It mirrors the geographic-lineage sensitivity
contract and lets a later phase choose a historical-language coverage rule from
explicit evidence rather than parser convenience. On the current source pass,
the accepted-speaker lower bound certifies 240 districts at 95%, 212 at 98%,
201 at 99%, 175 at 99.5%, and 124 at 99.9%; none is certified at 100%.
These counts remain extraction diagnostics, not a chosen analysis sample.

On the current source pass, 342 districts have incomplete alignment, 63 have all
114 columns aligned but unresolved cells, and 10 still have accepted-cell sums
above Atlas population. **No district yet has a complete 114-cell accepted
inventory**, so 1991 linguistic distance remains blocked from production.

`language_atlas_1991_languages.csv` now freezes the reviewed Atlas column
inventory for columns 4--117. It records the 18 Scheduled and 96 Non-Scheduled
languages in Atlas order, their published 1991 family classification, and a
coarse Shastry-resolution class. The family counts provide a source-structure
check: 19 Indo-Aryan, 1 Germanic, 17 Dravidian, 14 Austro-Asiatic, 62
Tibeto-Burmese, and 1 Semito-Hamitic language. The maintainer extractor requires
this registry before writing district-language outputs and attaches the reviewed
labels to every emitted candidate cell.

The registry does **not** define a new historical distance scale.
`resolve_language_atlas_1991_shastry_mapping()` passes the reviewed 1991 labels
through the same frozen Shastry concordance/adjudication resolver used for
Census 2001. Exact published concordance matches remain primary; already-reviewed
code-less language adjudications may be reused by exact label; and the existing
Shastry convention assigns degree 5 to reviewed non-Indo-European languages.
English remains a special excluded mother tongue. Under the preferred frozen
mapping, 108 of the 114 Atlas labels currently resolve, while Bishnupuriya,
Halabi, Lahnda, Nepali, and Sanskrit remain explicitly unresolved and English
remains special. No new degree is invented to make the historical table complete.

The Atlas reports 114 classified languages rather than every raw mother-tongue
return, so eventual district language coverage must be measured against the Atlas/PCA
population denominator rather than silently renormalizing the classified-language
counts to 100 percent. The next promotion step must resolve or characterize the
remaining district/PCA, cell, and district-page alignment review queues and then
write a tracked reviewed district-language long table for normal R/targets
ingestion.

The post-review persistence machinery is now implemented separately from source
promotion. `build_historical_linguistic_persistence_validation()` requires a
threshold-explicit 1991 distance table, the existing 1991-to-2001 geography
object, and a Census-2001 district table carrying the same primary
`ling_distance_nonzero_mean`. It fails if the geography summary and transition
disagree on the number of target districts. The preferred comparison uses only
one-target source districts passing the existing 99% 1991-population geography
rule; exact one-to-one mappings are reported as a separate nested sample.

For each sample the diagnostic reports ordinary and 1991-population-weighted
Pearson/Spearman persistence, the population-weighted slope, the corresponding
state-fixed-effect slope, mean absolute level/rank change, and quintile
stability. Population-weighted correlations use base R's `stats::cov.wt`; the
weighted Spearman statistic applies the same estimator to ordinary ranks. Split
or otherwise nonpreferred geography stays in the district panel with an
explicit status and cannot enter either persistence summary. This diagnostic is
not yet wired into targets because the reviewed Atlas source and preferred
accepted-speaker threshold remain unresolved.

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

1. Review the impossible-sum and high-coverage unresolved triage queues first,
   then characterize the remaining Atlas district/PCA and cross-page alignment
   cases before promoting a tracked reviewed district-language table.
2. Reuse the now-registered 114 Atlas labels and frozen Shastry/Glottolog identity
   machinery; keep unresolved labels and speaker coverage explicit.
3. Promote adjudicated cells through the accepted-source contract, choose the
   preferred accepted-speaker coverage threshold from reviewed evidence, and
   construct 1991 district linguistic distance on native 1991 geography.
4. Run the implemented 1991--2001 persistence diagnostic once the reviewed
   source/threshold are promoted; report preferred one-target and exact
   one-to-one samples separately with weighted Pearson/Spearman, weighted
   regression, state-FE persistence, and rank/quintile stability.
5. Treat split/non-nested geography as sensitivity evidence, not as preferred
   exact reconstruction.
6. Add 1961-1991 predetermined baseline/pre-trend diagnostics from Vanneman and
   SHRUG PCA/VD/TD in a separate historical-balance module.
