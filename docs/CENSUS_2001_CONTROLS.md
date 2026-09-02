# Census 2001 controls

The revised main regression uses a compact set of variables measured before the
2007-08 treatment measure. Ratios are calculated after lower-level counts have
been summed to Census 2001 districts. `data/metadata/census_2001_control_registry.csv`
is the single semantic authority for control membership, labels, theoretical
blocks, and alternative parameterizations; R helpers derive the main, absorption,
and appendix vectors from that registry rather than restating them in code.

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
C-14, and H-09 tables. The historical identifier `main` now means **compact 2001
adjustment**, not a claim that this is the uniquely correct causal conditioning
set. Including every available Census variable would consume degrees of freedom
and make the first stage harder to interpret, but a compact vector is not
automatically innocuous either: linguistic structure predates 2001, so some
2001 socioeconomic characteristics may themselves lie on long-run pathways from
language to later English-medium exposure or welfare.

For causal robustness, the design registry therefore distinguishes three finite
adjustment philosophies under both region and state fixed effects: (i) geography
fixed effects only, which avoid conditioning on measured socioeconomic descendants
but place more burden on the exclusion restriction; (ii) the compact 2001 vector,
which adjusts for observed scale, composition, human capital, economic structure,
demography, and development; and (iii) the compact vector without human capital,
which specifically avoids conditioning on a plausible language/education pathway.
These are registered as a separate control-strategy robustness axis. Alternative
literacy/secondary-plus and compact/decomposed economic-structure choices remain a
second, measurement-parameterization axis rather than being conflated with the
causal role of controls.

## Alternative parameterizations and balance analysis

The historical name `expanded` is retained in specification identifiers for
backward-compatible output contracts, but it should not be read as a generic
"more controls is better" hierarchy. The alternative absorption parameterization
adds literacy alongside secondary attainment and replaces the compact aggregate
agricultural-worker share with total-worker, cultivator, and agricultural-labourer
shares. The registry records those relationships explicitly through
`parameterization` and `alternative_to`. This preserves the occupational
composition information available in the active PCA source without including the
aggregate and its components in the same regression.

The theoretical blocks used by the absorption ladder—scale/geography, social
composition, human capital, demography, economic structure, and basic
development—are also derived from the registry. Hindu share, asset ownership, and
primary-school supply remain appendix/context variables rather than automatic
additions to the preferred control vector.

A separate worker-structure validity block now uses official Census B-04/B-25/B-26
tables to test whether linguistic distance already predicted detailed industrial
or occupational specialization in 2001. These variables are balance outcomes,
not automatic additions to the preferred control vector. The 2001 publication
categories are kept in their own denominator contract and are not silently
treated as directly comparable to Census 2011 B-series categories; see
`docs/CENSUS_WORKERS.md`.

## Active source pipeline

The active reader now uses the district-level SHRUG Census 2001 PCA archive together with official Census C-01, C-08, C-14, and H-09 state workbooks. State and district codes are padded and joined jointly; district numbers are never treated as nationally unique. C-08 attainment categories are summed before division by the age-7-plus population, C-14 age bands are summed before constructing the dependency ratio, and H-09 household counts are used before constructing electricity access. District area is computed from the accepted Census-2001 geometry.

The public revised model remains unchanged while expanded controls are evaluated in
extended diagnostics. The absorption ladder, VIF/GVIF results, state residual
ranges, leave-one-state-out estimates, and district influence measures are written
under `outputs/diagnostics/extended/instrument_relevance/`. Expanded variables are
diagnostic alternatives, not silently added to the headline specification.


## Coverage contract

The five count sources (SHRUG PCA, C-01, C-08, C-14, and H-09) must each
contain the same 593 Census-2001 state-district keys. The pipeline stops if a
count source is incomplete, duplicated, or contains an unexpected key. District
area is handled separately because the accepted 2001 geometry currently covers
582 of the 593 administrative districts; its missingness is retained explicitly
rather than dropping otherwise valid Census rows. Extended diagnostics write
`source_coverage.csv` alongside control-level missingness and revised-model
results.
