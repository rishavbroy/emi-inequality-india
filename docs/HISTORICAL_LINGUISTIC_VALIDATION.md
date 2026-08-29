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

The literature-derived Kumar--Somanathan 1991--2001 carve-out table is now used
only as an **independent geography benchmark**, not as a mapping authority. Its
published CSV lacks state codes, so the benchmark identifies a 1991 source
district only when its reported 1991 population matches exactly one SHRUG source
district nationwide. It then canonicalizes the reported 2001 destination name
against the Census-2001 registry and compares the paper's source-population
transfer share with the independently constructed SHRUG population share. No
fuzzy 1991 name match, nearest-population match, or benchmark-based override is
allowed. Ambiguous source populations and unmatched destination names remain
explicitly unbenchmarked.

The source CSV itself contains line-wrap artifacts from the table extraction,
including split source labels (`Chengalpattu-/MGR`, `Pasumpon M. The-/var`) and
split destination labels (`Gautam Buddha/Nagar`, `Jyotiba Phule Na-/gar`).
`read_district_carveouts()` repairs only these structural continuation rows:
a nonblank source continuation with no population must follow a hyphen-terminated
source label; rows carrying transfer percentages remain separate destination
edges, while continuation-only rows extend the preceding source/destination
label. The working paper distinguishes semantic hyphens from typographic word
wraps in these cases: an uppercase continuation preserves the source hyphen
(`Chengalpattu-MGR`), whereas a lowercase continuation removes the wrap hyphen
(`Pasumpon M. Thevar`). Wrapped destination fragments use the same word-wrap
rule. Unexpected continuation shapes fail closed. After repair, the reader also
checks the table's substantive accounting identity from Kumar--Somanathan: the
published child shares must partition each 1991 parent district to 100%, allowing
0.05 percentage points for two-decimal rounding, and both published share columns
must remain within 0--100. Dropped or duplicated extraction rows therefore fail at
the source boundary rather than contaminating lineage diagnostics.

Two additional diagnostic outputs record this comparison:

- `historical_linguistic_geography_carveout_benchmark.csv`: one row per
  Kumar--Somanathan source-to-target edge with conservative match status and
  absolute transfer-share difference;
- `historical_linguistic_geography_carveout_benchmark_summary.csv`: matched
  source/edge counts, explicit source/edge benchmark-coverage shares, separate
  counts for source populations that are absent versus non-unique in SHRUG, and
  share-difference summaries. Agreement statistics therefore cannot be read
  without the benchmark coverage reported beside them.

This benchmark uses the first transfer margin reported by Kumar--Somanathan:
the share of each 1991 parent district that went to each 2001 child. Their paper
also reports the reverse margin (the share of each parent in a child district),
but the SHRUG transition does not carry an independently complete 1991-population
denominator on 2001 boundaries. The reverse margin is therefore not fabricated by
renormalizing deterministic SHRUG mass; it remains source documentation rather
than a benchmark statistic.


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
sum exceeds Atlas population. It ranks **unreviewed** accepted cells and marks
whether removing one cell would, by itself, restore the population bound. A cell
with an explicit ledger decision is never queued again. If source-confirmed
accepted counts alone already exceed Atlas population, the district is marked
`source_confirmed_population_bound_violation` in the coverage diagnostics and is
removed from cell triage: no correction to an unreviewed cell can restore the
bound while preserving the reviewed evidence. The diagnostic never edits, drops,
or substitutes a count.

The first direct source-review pass inspected all 57 formerly individually
sufficient cells against rendered Annexure-IV pages 205--226 and recorded
`accept_extracted` where the printed district-language value matched the machine
extraction. After applying those decisions, five districts are already decisive
source-level population-bound contradictions: Barpeta (04-05), Gopalganj (05-11),
Deoghar (05-31), Hazaribagh (05-34), and Palamu (05-35). Their reviewed accepted
counts alone exceed the printed Atlas population, so they no longer generate
cell-level excess triage. The remaining five impossible districts produce 333
unreviewed accepted cells; none is individually sufficient to restore the bound.
This narrows the next impossible-sum review from single-cell misreads to
combinations, denominator/source-table consistency, or an explicit decision to
exclude the contradictory source rows.

The unresolved-triage output addresses the other high-value review queue without
choosing a preferred coverage threshold. It contains only **unreviewed** cells in
fully aligned districts whose accepted-speaker lower bound already certifies the
lowest registered sensitivity threshold (95%) but which still have unresolved
cells. A `leave_unresolved` decision therefore documents a source-reviewed
ambiguity without sending the same cell back to the priority template. For each cell
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
`accepted_speaker_count` and its provenance. Coverage and population-bound
diagnostics operate on this accepted-count layer, while the **review triage**
operates only on cells without an existing ledger decision. The tracked ledger
now contains the first completed direct-source reviews; no absent ledger row is
interpreted as acceptance. `--cell-review-template-output` prefills the exact
source fingerprints for the remaining impossible-sum and high-coverage-unresolved
queues, but blank template rows are not valid ledger decisions. This mirrors the
repository's reviewed Glottolog and district-lineage pattern: generated candidates
remain non-authoritative until an explicit tracked decision is applied, and a
completed review is not repeatedly re-queued.

The optional `--accepted-source-output` is the canonical promotion boundary from
maintainer extraction/adjudication to R. It contains one row per aligned
1991 district-language cell, retains unresolved cells with a blank
`accepted_speaker_count`, repeats the district coverage/status contract, and
preserves both machine and reviewed provenance. It applies **no analysis
threshold** and therefore is not itself the preferred historical-IV sample.
The reviewed source pass is now tracked as
`data/metadata/language_atlas_1991_accepted_source.csv`; rerunning the documented
maintainer command against the frozen language/state/cell-review registries
reproduces that artifact byte-for-byte before it is promoted into targets.

R reads that contract with `read_language_atlas_1991_accepted_source()` and
constructs the historical primary-distance analogue with
`build_historical_linguistic_distance_1991()`. The latter requires an explicit
`min_accepted_coverage` argument; there is deliberately no default. The long-form
cell rows are authoritative: R recomputes the number of aligned Atlas columns,
accepted/unresolved cell counts, accepted-speaker lower bound, population share,
and population-bound status and requires every repeated district summary field
to agree. A stale or hand-edited summary therefore cannot turn an incomplete or
impossible district into an eligible one.

The accepted-speaker threshold is necessary but not sufficient for instrument
quality. Following Shastry's regression treatment of the non-monotonic response
among zero-distance Hindi/Urdu speakers, the project's primary scalar is a mean
over **positive-distance mapped-language speakers**, not over total district
population. A small unresolved population share can therefore matter
disproportionately in a Hindi-dominant district.
`historical_linguistic_distance_1991_candidates()` now separates source
construction from the analysis-quality gate and computes conservative
partial-identification bounds before any preferred threshold is applied.

For each district, accepted speakers with a known positive Shastry degree form
the observed numerator and denominator. Accepted Hindi/zero-degree speakers and
English are known not to enter that nonzero mean. Every other person in the
Atlas population denominator--including unaligned cells, unresolved extracted
cells, accepted languages whose frozen Shastry degree remains unresolved, and
classified-language residual population--is treated as potentially unresolved
for the distance. The lower and upper bounds allow that unresolved mass either
to leave the accepted-speaker mean unchanged or to enter at the minimum/maximum
positive degree supported by the frozen Shastry scale. This is deliberately
worst-case and never assigns an unresolved speaker to a particular language.

`build_historical_linguistic_distance_1991()` therefore requires **two explicit
source-quality thresholds**: `min_accepted_coverage` and
`max_distance_bound_width`. A district is eligible only if it does not violate
the Atlas population bound, has positive accepted nonzero mapped-speaker mass,
meets the population-coverage threshold, and has a worst-case distance interval
no wider than the supplied bound threshold. Full 114-column alignment remains a
reported QA field (`complete_atlas_alignment_1991`) but is no longer an automatic
analysis exclusion when the unresolved speaker mass already yields sufficiently
tight instrument bounds. This makes the accepted-speaker threshold operational
rather than defeating it with a stricter all-columns rule.

The promoted point value remains the observed accepted-speaker mean used by
the existing 2001 analogue, while the accepted point, lower/upper bounds, bound
width, resolved speaker mass, and unresolved-mass upper bound remain in the
historical-distance output. The same district calculation now also reproduces
Shastry's second reduced-form measure: the percent of the district population
speaking languages at least three degrees from Hindi. Its accepted count divided
by Atlas population is a conservative lower bound; assigning every unresolved
person to a degree-three-or-higher language gives the upper bound. The interval
can exceed the raw unaccepted-speaker share because an accepted Atlas count may
still lack a frozen Shastry degree; those speakers also remain unresolved for the
threshold measure. The historical point measure is therefore labelled as the
accepted distant-speaker share and reported together with its bound width rather
than renormalizing classified-language counts. Persistence summaries expose the
mean and maximum 1991 bound width on the analysis sample. No second language
resolver or parser is introduced.

A source-quality sensitivity helper reports district and represented-population
counts over the registered coverage grid crossed with 0.10, 0.25, 0.50, and 1.00
distance-width cutoffs for the primary scalar.

The preferred source-only rule is now frozen at **99% accepted speaker coverage
and a maximum 0.50 Shastry-degree bound width**. `historical_linguistic_preferred_source_quality()`
owns this decision. The 99% coverage requirement caps unaccepted/unresolved
speaker mass at one percent of the Atlas district population; the 0.50 width
requires that remaining uncertainty to leave the historical IV within half of
one degree on Shastry's 0--5 scale. The two gates are deliberately distinct from
the separate 1991-to-2001 geography-quality rule.
This rule was selected before real persistence or historical first-stage results
were available and must not be changed in response to those coefficients. The
full source-quality grid remains a sensitivity diagnostic.
`historical_linguistic_source_quality_geography_grid()` then joins those source
rules to the already-reviewed preferred/exact geography flags and reports usable
district counts, represented population, and preferred-state coverage for every
threshold pair. This join is diagnostic only: geography never changes an Atlas
count or a distance bound. A district that fails either source gate stays in the
output with an explicit status (`below_coverage_threshold`,
`distance_bound_too_wide`, etc.) rather than being silently dropped or
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
above Atlas population. No district yet has a complete 114-cell accepted
inventory. That fact remains important extraction QA, but it is no longer by
itself a production blocker: the new distance-bound diagnostic asks whether the
unresolved mass is small enough to leave the historical IV tightly bounded.
Population-bound contradictions remain ineligible regardless of coverage or
bound width. They also do not receive derived historical-language estimands in
the candidate table. The raw accepted counts, printed Atlas population, coverage
ratio, and review provenance remain visible for QA, but any population-normalized
share or partial-identification bound is `NA`: once accepted speaker counts exceed
the source population, neither capping a share at 100 percent nor continuing to
compute distance bounds has a defensible interpretation. Both the primary
positive-distance mean and the percent-distant robustness measure use this same
source-coherence contract.

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
counts to 100 percent. The next promotion step must preserve the remaining
population contradictions and unresolved/alignment cells in the tracked accepted
source rather than pretending they are complete. The frozen coverage + bound rule
then determines analytical eligibility from that reviewed long table; source
review can continue later where it materially tightens bounds or resolves a
population contradiction.

The post-review persistence machinery is now implemented separately from source
promotion. `build_historical_linguistic_persistence_validation()` consumes the
production Census-2001 linguistic-distance object at an explicit schema boundary:
`state_code`,
`district_code`, and `ling_distance_nonzero_mean` are normalized locally to the
historical diagnostic's year-suffixed keys. The core Census-2001 IV object is
not given duplicate `_2001` geography aliases solely for this diagnostic.

`build_historical_linguistic_persistence_validation()` requires a
threshold-explicit 1991 distance table, the existing 1991-to-2001 geography
object, and a Census-2001 district table carrying the same primary
`ling_distance_nonzero_mean`. It fails if the geography summary and transition
disagree on the number of target districts. The preferred comparison uses only
one-target source districts passing the existing 99% 1991-population geography
rule; exact one-to-one mappings are reported as a separate nested sample.

For each sample the diagnostic reports the same level/rank persistence metrics
for the project's primary `nonzero_mean` and for Shastry's percent-distant
robustness measure. The latter is evaluated at both the accepted/lower endpoint
(`accepted_distant_share_ge3`) and the district-specific feasible upper endpoint
(`distant_share_ge3_upper_endpoint`). The upper-endpoint row is a source-uncertainty
sensitivity, **not** a sharp bound on the correlation or regression coefficient:
correlation extrema over interval-valued regressors need not occur when every
district is placed at the same endpoint. It asks whether the persistence
conclusion changes materially under a uniformly upper-endpoint reconstruction
from the already-frozen source bounds, without introducing a new post-results
quality threshold. Ordinary and 1991-population-weighted Pearson/Spearman
correlations, population-weighted slopes, and state-fixed-effect slopes are
therefore computed by one shared metrics routine rather than parallel code.
Quintile-transition output remains tied to the primary scalar.
Population-weighted correlations use base R's `stats::cov.wt`; the
weighted Spearman statistic applies the same estimator to ordinary ranks.
Weighted persistence slopes use `stats::lm`, while R-squared is recovered
directly from the fitted values, residuals, and model weights using the same
model/residual sum-of-squares definition as base R's `summary.lm`. The diagnostic
does not call `summary.lm` merely to obtain R-squared: current R versions warn
on essentially perfect fits because standard-error summaries become unreliable,
whereas the descriptive fitted-value R-squared remains well-defined. Split
or otherwise nonpreferred geography stays in the district panel with an
explicit status and cannot enter either persistence summary. This diagnostic is
wired into the extended target graph through the tracked accepted-source artifact.
The preferred **coverage + distance-bound** source-quality rule is frozen from
source-only diagnostics and must not be revised after inspecting
persistence or first-stage coefficients.

The historical-IV relevance robustness is implemented at the same boundary.
`build_historical_linguistic_first_stage_robustness()` maps only persistence-
eligible 1991 districts to their unique Census-2001 target, joins the existing
DISE treatment and Census-2001 controls, and compares the 1991 and 2001 primary
nonzero-distance instruments on one common support. It reuses the canonical
first-stage absorption registry and clustered inference rather than defining a
parallel historical regression stack. The compact robustness set is the
instrument-only, region/state main-control, and region/state expanded-control
specifications. Preferred-geography and exact-one-to-one samples are reported
separately, and each specification uses the identical district observations for
the two instrument vintages. This comparison is diagnostic-only with respect to
the main IV specification: the reviewed Atlas source and explicit coverage/bound
source-quality rule are now promoted, but historical first-stage results do not
alter the main IV registry or
paper tables automatically.

The 1991 baseline balance results also motivate a separate
**predetermined-control sensitivity** for that historical first stage. Several
eventual-EMI associations are visible in PCA91 and the rural/urban development
blocks, so historical validation should not rely only on the Census-2001
adjustment ladder. When a SHRUG 1991 baseline table is supplied,
`build_historical_linguistic_first_stage_robustness()` additionally reports two
state-1991-FE specifications: the complete-coverage PCA91 controls and the full
selected PCA91+VD91+TD91 baseline set. These are robustness diagnostics, not
changes to the primary IV specification. Each specification constructs common
support for both the 1991 and 2001 linguistic-distance vintages independently;
missing VD91/TD91 values therefore do not shrink the PCA91-only sensitivity,
and missing Census-2001 controls do not shrink either predetermined-control
sensitivity.

Small exact-one-to-one samples can make state-FE/control specifications saturated
or can absorb all residual instrument variation. Those rows are retained for
auditability but are `status = "not_estimable"`; the diagnostic does not call a
zero-variance correlation, fabricate a partial R-squared/F statistic, or compare
that row across instrument vintages. This follows base R's linear-model contract:
`lm` exposes model rank and residual degrees of freedom, and aliased/rank-deficient
coefficients are not valid inferential estimates. Preferred-geography estimates
remain the high-support historical robustness benchmark; the exact sample is a
strict nested geography sensitivity, not a requirement that every rich nuisance
specification be estimable.

## Vanneman source provenance

The downloaded Vanneman-Barnes snapshot now has two complementary documentation
layers. The archived HTML codebook defines substantive record meanings and a generic
column-10 "version number" convention. The archived author-distributed SAS programs
are file-specific readers for `panel4.data.gz`, `dist81.data.gz`, and `dist91.data.gz`.
For parser/layout provenance, the file-specific readers are the stronger contract: they
name the source file, declare the identifier positions, and declare the fixed-width
fields for the records actually consumed.

This changes how column 10 is interpreted in QA. The generic HTML codebook says
version 2 denotes cross-sectional data and version 6 denotes the 1961--91 panel, while
the observed `panel4.data.gz` carries 5 throughout. More decisively,
`dist81.sas` itself labels the field "check always=2" but explicitly reads education
records 151--156 even though those exact raw records carry 3. The digit therefore
cannot be used as a stand-alone schema-validity gate. QA retains observed versus
generic-codebook values, but parser eligibility now requires the corresponding
file-specific SAS reader to target the source, declare the identifier layout, and cover
all records whose column-10 value differs from the generic convention.

The three SAS readers and the compressed `panel4.data.gz`, `dist81.data.gz`, and
`dist91.data.gz` files have now been recovered from the **same** author files-page
Wayback snapshot (`vanneman.umd.edu/districts/files/index.html`, 2013-07-22). The
archived compressed files are byte-for-byte identical to the previously downloaded
local copies. Production QA therefore treats `data_archived/` and
`sas_commands_archived/` as the canonical provenance pair and verifies their recorded
archive sizes and MD5 checksums before declaring the parser contract eligible. The
duplicate top-level copies are not part of the production contract.

The remaining substantive integration task is now the geography contract. Vanneman's
own documentation says that the longitudinal database maintains comparable boundaries
by aggregating simple district splits back to older units and, for territory transfers,
estimating earlier values on recreated 1991 geography. `panel4` therefore has fewer
stable units than the raw 1991 Census and its panel state/district IDs must not be
silently interpreted as Census-1991 codes.

The archived `stateid.html` page closes the first part of this geography problem: it
publishes panel (`P`), 1981, and 1991 state IDs side by side and explicitly notes that
panel/1981 state IDs differ from the 1991 file. The tracked
`vanneman_panel_state_crosswalk.csv` transcribes that table, including two cases that
must not be forced into a one-to-one state map: Jammu & Kashmir has no 1991 Census and
Goa, Daman & Diu splits across two 1991 state IDs. This same source documents the `-1`
1991 missing-value sentinel for Jammu & Kashmir. Accordingly,
`vanneman_panel4_geography_inventory()` preserves those nine panel units with
`population_1991 = NA` and `population_1991_status = "no_1991_census"` rather than
failing or imputing a population.

`vanneman_panel4_dist91_crosswalk()` then uses the author's own 1991 cross-section as
the immediate bridge target. It auto-accepts only a unique exact normalized 1991 label
within the documented 1991 state map. Panel units listed in the author's
`combining.html`, labels containing explicit `+` aggregates, small-state district-00
units, state-split cases, Jammu & Kashmir, and spelling/name mismatches remain
non-preferred review cases. No fuzzy match enters the preferred pretrend geography.
This bridge is deliberately Vanneman-panel -> Vanneman-dist91 first; the next step is to
validate the deterministic dist91 IDs against the existing Census-1991 geography and
compose that with the already-reviewed 1991->2001 lineage. The Vanneman harmonization
remains sensitivity evidence rather than authority for Census-2001 geography.

## Next phases

1. Treat the now-green real historical chain as evidence, not as a specification
   search. The preferred sample shows strong 1991--2001 persistence, broadly
   reassuring `LD_1991` baseline balance, and weak relevance after rich
   adjustment. Preserve the frozen 99% / 0.50 source rule and main-IV registry
   rather than tuning either to improve the historical first stage.
2. Continue Atlas source review only where it can materially tighten a district's
   distance bound or resolve a population contradiction; do not spend effort on
   tiny cells that cannot change preferred eligibility.
3. Treat both historical persistence constructions as completed diagnostics.
   Shastry's regression weighted average is represented by the project's
   positive-distance mean, while her nonlinear percent-distant measure is
   reported at both the accepted/lower and feasible upper source endpoints.
   Interpret the endpoint comparison as source-uncertainty sensitivity rather
   than a formal bound on persistence, and do not relabel either robustness
   construction as a new primary instrument based on favorable results.
4. Treat split/non-nested geography as sensitivity evidence, not as preferred
   exact reconstruction.
5. Complete the 1961--91 Vanneman pre-trend value construction on the reviewed
   `vanneman_pretrend_geography.csv` sample. The panel4-to-dist91 alias ledger and
   direct Census-code composition now resolve the geography gate without importing
   Liu et al.'s separate six-census harmonized IDs. Extract only source variables
   whose archived parser contracts are verified, then test pre-existing literacy,
   urbanization, and occupational trends against eventual EMI and `LD_1991`.
   Aggregate/split geography remains sensitivity evidence rather than preferred
   pretrend support.

## SHRUG Census-1991 predetermined baseline balance

The historical-balance branch now reads the district products distributed in the
SHRUG Census-1991 PCA, Village Directory, and Town Directory archives directly;
it does not reaggregate SHRID-level village/town files. PCA91 is the complete
452-district demographic denominator. VD91 and TD91 retain their own source
coverage, so individual balance regressions use covariate-specific support
rather than forcing rural and urban directory variables onto a single complete
case sample.

The PCA block constructs only transparent count ratios: log population, female
and age-0--6 shares, Scheduled Caste and Scheduled Tribe shares, literacy among
the population age 7+, main- and marginal-worker shares, and cultivator and
agricultural-labourer shares among main workers. The rural-directory block uses
primary/high-school and hospital/primary-health-centre counts per 100,000 total
district residents; the denominator is deliberately labelled as total district
population because the district VD91 rural-population aggregate is incomplete in
the distributed table. The town-directory block uses its internally documented
age-7+ population denominator for urban literacy and primary-school, hospital,
and bank rates. Ambiguous aggregated quantities such as district TD91 rail
distance are not promoted merely because they are available.

`build_historical_baseline_balance_1991()` maps native 1991 baseline districts
through the already-reviewed preferred one-target 1991--2001 geography and joins
the eventual DISE English-medium exposure from the Census-2001 target district.
It reports population-weighted bivariate and 1991-state-FE balance regressions,
with state-clustered inference, for both the preferred geography and the nested
exact-one-to-one sensitivity. Domain-level reverse regressions provide joint
balance tests for demography, human capital, economic structure, rural
development, and urban development. Each individual covariate retains its own
available source support. A joint row is labelled `estimated` only when the
state-FE model has residual degrees of freedom, all tested covariates are
estimable, and the clustered Wald statistic and p-value are finite. Saturated,
aliased, or covariance-singular small-sample rows remain in the diagnostic as
`not_estimable` with an explicit reason rather than appearing as successful
estimates with missing joint statistics.

The current extended target executes this exercise for both eventual EMI exposure
and the promoted threshold-explicit `LD_1991` table on the same baseline-variable
registry.
The two predictors retain their own valid observation support: missing DISE
treatment does not remove a district from `LD_1991` balance, and an ineligible
historical-language observation does not remove it from eventual-EMI balance.
This keeps the two pre-treatment claims distinct while sharing only the reviewed
historical geography and each covariate's own source support.

## Real historical-instrument execution

The extended-diagnostic target graph now consumes the tracked accepted-source
artifact rather than requiring a maintainer PDF extraction during ordinary
builds. One `historical_linguistic_distance_validation` object owns the source
candidates, the frozen preferred distance, the source-quality sensitivity grid,
and the source-quality-by-geography grid. The preferred distance from that same
object is then reused unchanged by persistence, the historical first-stage
ladder, the 1991 predetermined-control first-stage sensitivities, and the 1991
baseline-balance diagnostics. This prevents threshold drift or independent
reconstruction across downstream analyses.

The written diagnostics are deliberately long-form and diagnostic-only. They do
not alter the main IV registry or paper tables automatically. The real run writes
the historical distance/source-quality files, persistence panel/summary/quintile
transition, the standard and predetermined first-stage registries/estimates, and
comparison tables under `outputs/diagnostics/extended/instrument_relevance/`.
The existing historical baseline outputs now contain both eventual-EMI and
`LD_1991` predictors whenever the historical instrument is eligible.

## External Vanneman geography benchmark

Liu, Shamdasani, and Taraz (2023) publish a six-decade Indian Census
replication package with `Vanneman_district_crosswalk.dta`, `panel4_lst.data`,
`PCA_census1991_dist_match.dta`, and a companion 2011 crosswalk. These files
are useful independent evidence about historical district identities, but they
are not promoted as this project's production lineage authority.

The benchmark keeps the geography layers explicit. The published Vanneman
crosswalk must cover exactly the stable IDs in the canonical author-distributed
`panel4` inventory before any name comparison is reported. Its Vanneman name
and independently curated harmonized name are then compared against the
canonical panel's 1961 and 1991 labels. The bundled `panel4_lst.data` is treated
only as a replication-package copy: missing IDs remain visible as benchmark QA
and never remove a canonical Vanneman unit.

The published `PCA_census1991_dist_match.dta` belongs to a different harmonized
geography. Its Census-1991 rows are grouped into its own `state_id` / `district_id`
units; those IDs must not be joined numerically to the Vanneman stable panel
IDs. The diagnostic therefore inventories the number of PCA rows and harmonized
groups but does not use those identifiers to promote a Vanneman-to-Census match.
This prevents an externally useful benchmark from silently becoming a second,
incompatible geography authority.

The written benchmark files are
`vanneman_liu_geography_benchmark.csv` and
`vanneman_liu_geography_benchmark_summary.csv`. They are intended to prioritize
manual review of Vanneman label cases and to independently check the stable-ID
universe. Promotion of any external alias into the preferred pretrend lineage
requires an explicit reviewed rule in the project's own geography ledger.


The replication package's construction scripts make the separation even more
explicit. `lst-dm-01a-clean_Vanneman_data.do` merges
`Vanneman_district_crosswalk.dta` 1:1 on Vanneman `state_id` / `dist_id`.
`lst-dm-01b-make_pca_1961_1991.do` carries those stable identifiers into the
1961--1991 builder. Only `lst-dm-01d-make_pca_1961_2011.do`, after appending
later Census rounds, creates a new six-census geography from normalized
state/district names. The diagnostic records these source-code invariants in
`vanneman_liu_construction_contract.csv`; it does not infer that the later
harmonized IDs are Vanneman IDs.

The initial exact-name alias-candidate layer was removed after real-data execution: it resolved none of the 50 `label_review_required` units because the author `dist91` labels themselves contain historical spellings and transcription variants. The project now keeps a narrow tracked `vanneman_panel4_dist91_adjudications.csv` ledger instead. Every accepted row must already be a label-review case, retain the author-documented 1991 state, point to a non-aggregate `dist91` code, agree with Liu et al.'s raw Census-1991 `st_code`/`dist_code` name, and be connected to Liu's stable Vanneman name by an explicit `replace dtname_temp=...` rule in the published construction code. No fuzzy threshold can promote a row.

The first reviewed ledger contains 22 such one-to-one aliases and raises preferred panel4-to-1991 support from 245 to 267 of 339 stable Vanneman units. `vanneman_panel4_dist91_adjudication_evidence.csv` records the externally verified raw 1991 label, stable Vanneman label, and published source-code line used for each promotion. The next geography step is deliberately code-based rather than another name matcher: accepted `dist91` state/district codes are joined directly to the project's existing reviewed Census-1991 geography and 1991-to-2001 transition. Only one-target preferred transitions enter the Vanneman pretrend sample; split sources remain review/sensitivity cases. This produces `vanneman_pretrend_geography.csv` and preserves the project lineage as the production authority.


## Vanneman 1961–1991 pre-treatment trends

The pretrend diagnostic reads a deliberately narrow family directly from the
archived stable `panel4` file using the author-supplied `panel4.sas` fixed-width
layout contract. It does not use `dist81`, does not import Liu et al.'s separate
six-Census harmonized IDs, and does not introduce another district-name matcher.

The registered records are total population (100), main workers (111), main farm
workers (112), literates ages 5+ (140), primary-school-or-higher population (151),
and matriculates-or-higher population (153). They produce log population, urban
share, main-worker share, non-farm composition among main workers, the literate
share of total population, and two educational-attainment shares. The literate
share is deliberately not labelled a harmonized age-specific literacy rate. The
author SAS reader marks the 1961 main-worker and farm-worker counts as estimated,
so labor pretrends are supporting diagnostics rather than pristine Census-series
outcomes. Negative archived values are treated as unavailable because counts
cannot be negative: the codebook explicitly documents `-1` as missing, while the
distributed panel also contains a few `-2` education values whose meaning is not
documented in the attached source material.

Changes are computed on stable panel units for 1961–71, 1971–81, 1981–91,
1961–81, and 1961–91, then restricted through the conservative reviewed
Vanneman→1991→2001 bridge. Regressions use 1961 population weights, so later
population growth cannot determine observation weights. Future EMIE and reviewed
1991 linguistic distance are evaluated as predictors of already-realized
pre-treatment changes, with state-FE specifications aligned to the preferred
modern design. Domain-level joint tests summarize demography, labor structure,
and education.

These diagnostics are evidence about pre-existing differential development, not
proof of instrument exogeneity. Null pretrends are reassuring; non-null pretrends
are substantive warnings.
