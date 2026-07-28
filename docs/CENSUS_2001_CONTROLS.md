# Census 2001 controls

The revised main regression uses a compact set of variables measured before the
2007-08 treatment measure. Ratios are calculated after lower-level counts have
been summed to Census 2001 districts.

## Main paper

- log population;
- urban population share;
- adult secondary-or-higher attainment;
- Scheduled Caste and Scheduled Tribe shares;
- Muslim population share;
- agricultural-worker share;
- demographic dependency ratio;
- household electricity access;
- log population density;
- state fixed effects.

These variables come from SHRUG's Census 2001 PCA and the official C-01, C-08,
C-14, and H-09 tables. The main set is intentionally limited. Including every
available Census variable would consume degrees of freedom and make the first
stage harder to interpret.

## Appendix and balance analysis

Literacy, total worker share, Hindu share, banking access, television ownership,
telephone ownership, and primary-school supply are reserved for expanded-control
and balance specifications. Additional industry, housing, and asset variables
may be added after their source tables have passed district-code and denominator
checks.

## Active source pipeline

The active reader now uses the district-level SHRUG Census 2001 PCA archive together with official Census C-01, C-08, C-14, and H-09 state workbooks. State and district codes are padded and joined jointly; district numbers are never treated as nationally unique. C-08 attainment categories are summed before division by the age-7-plus population, C-14 age bands are summed before constructing the dependency ratio, and H-09 household counts are used before constructing electricity access. District area is computed from the accepted Census-2001 geometry.

The public legacy model is retained for continuity, while `revised_iv_models` and `revised_first_stage_tests` are active pipeline targets. Their coverage, instrument-balance, model-status, and first-stage outputs are written under `outputs/diagnostics/extended/census_2001_controls/`. Promotion of the revised model to the headline table should follow review of these generated diagnostics rather than occur silently.


## Coverage contract

The five count sources (SHRUG PCA, C-01, C-08, C-14, and H-09) must each
contain the same 593 Census-2001 state-district keys. The pipeline stops if a
count source is incomplete, duplicated, or contains an unexpected key. District
area is handled separately because the accepted 2001 geometry currently covers
582 of the 593 administrative districts; its missingness is retained explicitly
rather than dropping otherwise valid Census rows. Extended diagnostics write
`source_coverage.csv` alongside control-level missingness and revised-model
results.
