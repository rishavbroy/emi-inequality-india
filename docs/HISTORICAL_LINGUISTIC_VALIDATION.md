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

For the preferred persistence sample, a 1991 district must have complete
SHRID coverage and exactly one Census-2001 target district. If a 1991 district
splits across multiple 2001 districts, district-level Atlas language shares do
not reveal the within-parent location of each language. Those cases must not be
fractionally disaggregated in the preferred exercise merely because SHRUG can
allocate total population. They remain explicit sensitivity/exclusion cases.

The diagnostic outputs are:

- `historical_linguistic_geography_1991_2001.csv`: one row per 1991 source
  district with SHRID/population coverage, number of 2001 targets, mapping class,
  and preferred-persistence eligibility;
- `historical_linguistic_transition_1991_2001.csv`: population/area transition
  weights for deterministic SHRID membership;
- `historical_linguistic_shrid_bridge_1991_2001.csv`: bridge-status summary.

## Next phases

1. Extract Annexure IV to a reviewed district-language long table and validate
   district totals against SHRUG PCA91.
2. Reuse the frozen Shastry/Glottolog language identity machinery rather than
   defining a separate 1991 distance scale.
3. Construct 1991 district linguistic distance on native 1991 geography.
4. Compare 1991 and 2001 distance on the deterministic one-to-one sample;
   report population-weighted Pearson/Spearman persistence and within-state
   persistence.
5. Treat split/non-nested geography as sensitivity evidence, not as preferred
   exact reconstruction.
6. Add 1961-1991 predetermined baseline/pre-trend diagnostics from Vanneman and
   SHRUG PCA/VD/TD in a separate historical-balance module.
