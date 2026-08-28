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
usable text layer, so OCR is not part of the planned production workflow.
`scripts/inspect_language_atlas_1991.py` is a maintainer-side layout checker; its
self-test runs in every source-syntax audit, while the raw PDF itself remains
outside the normal targets dependency graph. The next extraction stage should
write a tracked long-form source and validate district language totals against
PCA91 before any linguistic-distance target consumes them.

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

1. Extract Annexure IV to a reviewed district-language long table and validate
   district totals against SHRUG PCA91.
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
