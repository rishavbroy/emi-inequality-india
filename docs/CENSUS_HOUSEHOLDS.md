# Census household human-capital and worker-intensity diagnostics

This extended module constructs concept-matched Census-2001 to Census-2011 household-capacity changes from HH-09/HH-13/HH-15 and HH-08/HH-10/HH-11. Table-specific readers validate the native published accounting before any geography operation. Census-2011 counts are then pooled only through the common complete deterministic 2011-to-2001 transition; Census-2001 rows already live on the target geography. Shares are constructed only after the relevant counts have been validated and, for 2011, pooled.

The raw 2001 workbooks confirm exact matches for literacy-count categories, matriculate/graduate household access, and household worker-count categories across all 593 Census-2001 districts. HH-15 Appendix independently reproduces the HH-15 worker-count partition and is used as a source-integrity check. HH-13 uses the six published age-15+ buckets as its denominator contract. The `None` bucket is valid for every education row because the row label describes household educational composition while columns 9--14 classify the number of household members age 15+; longitudinal age-15+ counts therefore sum only the `1`, `2`, `3-6`, `7-10`, and `11+` buckets. In the Odisha workbook the redundant `Total Households` cells are blank, but the six buckets remain in their standard columns and are complete; readers therefore validate a published row total when present without requiring one to reconstruct the age-15+ denominator. The longitudinal registry deliberately excludes `workers_per_household`, marginal-worker shares, and short-marginal shares: 2001 HH-15 top-codes worker counts at `4+` and does not publish the 2011 HH-11 marginal-worker decomposition, so those changes cannot be recovered exactly.

These household-capacity changes remain descriptive post-treatment evidence. They are **not** preferred controls or identified mediation effects, and the module does not automatically create another weak-IV outcome family.

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

The branch persists a compact measurement bundle under `outputs/diagnostics/extended/census_households/`:

- `household_2001.csv`: validated 2001 baseline counts and shares;
- `household_2011_harmonized_2001.csv`: harmonized 2011 counts and shares;
- `household_change_2011_2001.csv`: exact common-concept share changes;
- `change_coverage.csv`: finite baseline/follow-up/change coverage by concept;
- `source_validation_2001.csv` and `source_validation_2011.csv`: exact within-vintage source reconciliations.

No household-specific estimator output is written in this phase. The purpose is measurement and narrative validation, not another weak-IV search grid.
