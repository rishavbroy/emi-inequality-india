# Census household human-capital and worker-intensity diagnostics

This extended Census-2011 module activates HH-08, HH-10, and HH-11 as district-level post-treatment household mechanisms. It decodes published counts on the native 640-district Census-2011 geography, validates their independent household universes, pools counts only through the common complete deterministic 2011-to-2001 transition, and constructs a compact set of household-level human-capital and labor-intensity measures afterward.

The tracked Census-2001 acquisition manifest now also includes HH-09, HH-13, HH-15, and HH-15 Appendix, the predeclared candidate baselines for literacy depth, matriculate/graduate access, and worker intensity. These files are **acquisition-ready, not analysis-active**: no 2001-to-2011 change is constructed until actual workbooks confirm the row/category layout and an exact numerator/denominator mapping to the 2011 measures. In particular, HH-15's top-coded worker cells must not be used to fabricate exact marginal-worker counts.

Until that validation is complete, the active HH-08/HH-10/HH-11 objects remain 2011-only mechanism descriptors. They are **not** preferred controls, 2001-to-2011 changes, or identified mediation effects. Given the weak scalar-IV first stages already documented for the Census mechanism sample, the module does not automatically add a new IV outcome family.

## HH-08: literacy depth

HH-08 classifies ordinary households by the number of literate members age 7 years and above: none, 1, 2, 3, and 4+. The reader requires those categories to exhaust total households exactly. It also checks that the published household-size cells exhaust every literacy-count row.

The harmonized measures retain deliberately coarse margins that do not assign a synthetic value to the open-ended `4+` category:

- no-literate households / households;
- households with at least two literates / households;
- households with at least four literates / households.

The module therefore measures literacy depth without pretending that an average number of literates can be recovered from an open-ended category.

## HH-10: matriculate and graduate access

HH-10 reports household counts with no matriculate, at least one matriculate, sex-specific matriculate access, and graduate access. Male and female access categories overlap when a household contains both, so they are never added together.

HH-10 supplies the substantive denominator directly: the age-15+ counts in the mutually exclusive `no matriculate` and `at least one matriculate` rows sum to households with at least one member age 15+. Matriculate and graduate access shares use that denominator. Separately, the all-household counts in those same two rows exactly reproduce the ordinary-household universe independently published by HH-08/HH-11, while HH-10's `at least one literate` count exactly matches HH-08 total households minus HH-08 no-literate households. The broader universe is therefore a source-integrity check, not the denominator substituted into the age-15+ access measures.

The retained measures are household shares with:

- at least one matriculate / households with a member age 15+;
- at least one female matriculate / households with a member age 15+;
- at least one graduate / households with a member age 15+;
- at least one female graduate / households with a member age 15+.

## HH-11: worker intensity

HH-11 partitions households by number of workers: none, 1, 2, 3, and 4+. Those categories must exhaust households. Independently, published total workers must equal main workers plus marginal workers working 3-6 months plus marginal workers working less than 3 months.

The retained measures are:

- workerless households / households;
- households with at least two workers / households;
- households with at least four workers / households;
- workers per household;
- marginal workers / workers;
- less-than-3-month marginal workers / marginal workers.

`workers_per_household` is a count intensity rather than a share, so it is intentionally not constrained to `[0,1]`.

## Cross-table source validation

HH-08 and HH-11 must agree exactly on total households for every native Census-2011 district. HH-10's matriculation partition must agree with that same household total, and HH-10's literate-household count must agree with HH-08's independently implied literate-household count. These checks occur before geography harmonization so pooling cannot conceal a malformed source table.

All three attached table families contain all 640 Census-2011 districts. After source validation, count columns are passed to `harmonize_census_2011_counts_to_2001()` and shares/intensities are constructed from pooled numerators and pooled denominators. Partial parent reconstructions remain withheld under the same rule used by migration, workers, housing, and the Census-2011 population denominator.

## Persisted diagnostics

The branch deliberately persists only two forensic artifacts under `outputs/diagnostics/extended/census_households/`:

- `household_2011_harmonized_2001.csv`: the harmonized count and measure table;
- `source_validation_2011.csv`: exact cross-table source reconciliations.

No separate coverage file or household-specific estimator output is written because reconstruction provenance already travels with the harmonized rows and no new inferential family is introduced in this phase.
