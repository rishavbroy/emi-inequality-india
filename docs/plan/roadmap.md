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

Source-first implementation order:

1. inspect raw/archive schema, identifiers, missingness, and accounting identities;
2. validate direct Census-2001 district keys against the project registry;
3. define a small predeclared set of economically interpretable measures before IV results are seen;
4. distinguish predetermined 2005 structure/balance uses from later firm-growth mechanisms;
5. reuse the shared mechanism specification/inference layer rather than create an Economic-Census-specific estimator;
6. persist one compact source/coverage diagnostic plus registered model outputs, not redundant table-specific artifacts.

Priority candidate measures are non-farm employment, establishment density, hired-employment share, private/informal employment, manufacturing employment, services employment, and a narrowly justified English-intensive services measure if the published industry mapping supports it.

### 2. Labor-market outcomes

After firm dynamics, build the labor-market module from the now-local `data/raw/nss/` and `data/raw/plfs/` sources. Prioritize NSS 2007-08 employment/unemployment/migration and NSS 2009-10 employment/unemployment before the later PLFS waves so near-treatment structure is separated from long-run labor outcomes. The scientific targets are labor-force participation, employment/unemployment, regular salaried work, casual/self-employment composition, real wages, occupational skill, services employment, female labor-force participation, and migration where sample support permits.

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
