# Current empirical roadmap

This file records the active research plan. Historical brainstorming and refactor-era questions belong in `archive/`; they should not be treated as current implementation requirements. The code, tracked metadata, generated diagnostics, and this roadmap are the source of truth when older paper prose or conversation excerpts disagree.

## Completed foundations

### Analytical geography and lineage

- Census-2001 districts are the analytical geography.
- District lineage is reviewed and fail-closed; fuzzy matching generates candidates but never establishes production lineage by itself.
- Census-2011 counts are pooled only through complete deterministic child-to-2001-parent reconstructions before rates or shares are computed.
- Production and diagnostic geography have explicit, tested source roles.

### Instrument and identification diagnostics

- Census-2001 mother-tongue composition feeds the primary linguistic-distance construction.
- Alternative linguistic constructions and historical benchmarks are tracked separately from the preferred measure.
- First-stage relevance, balance, monotonicity, state/region adjustment, weak-IV diagnostics, Anderson-Rubin inference, and overidentification diagnostics are registry-driven.
- Historical Census/SHRUG controls provide predetermined balance evidence rather than an ever-growing preferred control vector.

### Consumption and welfare

- Historical detailed-consumption rounds include 2000-01, 2001-02, 2004-05, 2007-08, 2009-10, and 2011-12 where supported.
- Modern HCES 2022-23 and 2023-24 are active long-run endpoints.
- Official MPCE aggregates are validation gates.
- Temporal/spatial price adjustment is active.
- Survey design, lineage, support diagnostics, mean/log/median welfare, and registered distributional outcomes are centralized rather than reimplemented by round.
- Welfare comparability and dynamic IV outputs are generated separately from raw district estimates.

### Census mechanisms

- 2001 controls, migration, worker structure, household mechanisms, and C-17 diagnostics are implemented in dedicated modules.
- 2011 migration D02-D07, worker B-series, HH08/HH10/HH11, and housing/living-standard tables are harmonized through the shared Census count/geography architecture.
- Housing includes the validated H04A-HL13 structural-durability pair. Durability changes remain descriptive and do not expand the frozen housing weak-IV outcome family.

## Remaining high-value work

### 1. Firm dynamics / Economic Census

The required source families are now local under `data/raw/ec/` and `data/raw/shrug/`. Start from the existing `shrug_economic_census` registration and the documented SHRUG `ec05_pc01dist` district product; use the raw Fifth/Sixth Economic Census archives for source validation rather than duplicating a standard district aggregation without a methodological reason. Development Data Lab publishes the 2005 product directly on Census-2001 district identifiers with total non-farm employment, firms, hired employment, public/private/informal employment, manufacturing, services, and detailed industry groups.

The EC05 source/measurement phase is now active: the documented SHRUG `ec05_pc01dist` product is read directly, validated against the canonical Census-2001 district registry, and reduced to a small core of firm/nonfarm counts and shares. Source gaps are retained explicitly rather than imputed, and no EC outcome has yet been added to the IV mechanism registry. See `docs/ECONOMIC_CENSUS.md`.

The EC13 source/measurement phase is now active: the local SHRUG `ec13_pc11dist` product is validated on all 640 Census-2011 districts, its counts are pooled through the existing complete-parent Census-2011-to-2001 bridge, and common 2005-2013 log/count-composition changes are generated only after pooling. EC05 informal employment is excluded from the longitudinal family because EC13 does not publish a comparable district field.

The EC05/EC13 causal mechanism family is now predeclared and routed through the shared district-mechanism inference layer. The registered outcomes are log non-farm employment growth, log establishment growth, hired/private employment-share changes, services-share change, and a secondary manufacturing-share change. Female employment, mean firm size, and EC05-only informal employment remain descriptive rather than enlarging the inferential family. One Economic Census diagnostic object now owns both measurement and model outputs, avoiding parallel writer targets.

Remaining implementation order:

1. use the 2005 level only for a separately justified near-treatment structure/balance exercise; do not condition post-treatment firm mechanisms on 2013 structure;
2. inspect EC90/EC98 only for a separately predeclared historical firm-pretrend exercise; do not allocate 1991 district totals across later splits or infer EC98 district totals without the documented key bridge and coverage accounting;
3. after the EC model outputs have passed the full audit, move to NSS 2007-08 labor/migration rather than expanding the firm outcome family.

Priority candidate measures are non-farm employment, establishment density, hired-employment share, private/informal employment, manufacturing employment, services employment, and a narrowly justified English-intensive services measure if the published industry mapping supports it.

### 2. Labor-market outcomes

The NSS 2007-08 source-contract phase is now active. The official Schedule 10.2 DDI, Block 4 usual-activity records, and Block 6 migration records are registered under `data/raw/nss/`, validated on one common person universe, and normalized with the published NSS survey-design fields and combined multiplier. No district labor outcome is yet registered; see `docs/LABOR_MARKET.md`.

Next, define the reviewed district-lineage/support contract and predeclare a small near-treatment outcome family before estimating district results. Then add NSS 2009-10 through the same interface, followed by later PLFS waves so near-treatment structure remains separate from long-run labor outcomes. Wage outcomes should wait for the weekly-status/earnings source rather than being inferred from the usual-activity block.

Use the same discipline as consumption:

- canonical household/person ingestion;
- official survey-design identifiers and weights;
- explicit source geography;
- support diagnostics before district inference;
- no forced district estimate when public geography or effective sample size is inadequate;
- common outcome registry and shared estimator where possible.

### 3. Identification consolidation

Do not try to repair a weak within-state first stage by adding many correlated controls. The final identification section should synthesize the evidence already generated:

- relevance under the predeclared instrument constructions;
- region versus state fixed-effects sensitivity;
- historical balance/pretrend evidence;
- C-17/English-acquisition mechanism evidence;
- monotonicity diagnostics;
- weak-IV-robust confidence sets;
- influence/state-deletion sensitivity;
- explicit limits on causal interpretation when identifying variation is primarily between states.

Any new control must have a clear exclusion-threat rationale and predetermined timing. Post-treatment Census, firm, migration, or labor-market variables are mechanisms/outcomes, not preferred controls.

### 4. Outcome-specification consolidation

Before final paper claims, choose a small registered causal outcome family. Historical and modern consumption rounds now make it possible to compare baseline-adjusted levels, changes, and dynamic specifications without selecting a specification after seeing significance. Use the comparability diagnostics to define which HCES/NSS contrasts are substantive versus survey-redesign sensitivity.

### 5. Paper and reviewer-facing outputs

Once the firm/labor phases and final outcome registry are fixed:

- rewrite the introduction around the outcomes the data can actually identify;
- report first-stage and weak-IV limitations prominently;
- distinguish local effects from individual effects when migration or firm relocation is plausible;
- align discussion/appendix caveats with generated mechanism evidence;
- remove stale references to obsolete geography, nominal consumption, and superseded estimator choices.

## Explicit non-goals

- Do not force unresolved district lineage to 100 percent.
- Do not use fuzzy-only district matches in production.
- Do not reconstruct unpublished migrant-by-industry/occupation cells by multiplying migration totals by destination worker shares.
- Do not add post-treatment variables to the preferred control set merely because they are available.
- Do not expand a registered hypothesis family after inspecting results without labeling the expansion exploratory.
- Do not add parallel estimators when the shared Census mechanism or survey-design layers already represent the estimand.
- Do not add raw-source adapters before the underlying files have been inspected.
