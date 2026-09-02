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

### Firm and labor mechanisms

- Economic Census 2005/2013 firm dynamics are harmonized to Census-2001 geography and routed through the shared post-treatment mechanism inference layer. The causal family is frozen; EC05-only and thin descriptive measures do not expand it.
- NSS64 labor/migration is frozen as a near-treatment reference. NSS66 is the completed `early_post` labor wave; PLFS 2017-18 is the completed `long_run_post` usual-status wave.
- NSS66 and PLFS use canonical person ingestion, official survey design, denominator-specific support rules, reviewed lineage, a shared district estimator, and the common weak-IV/Anderson--Rubin mechanism layer. PLFS primary versus conservative geography is an explicit robustness comparison rather than a competing outcome family.
- Detailed source, weighting, lineage, support, and outcome decisions live in `docs/ECONOMIC_CENSUS.md` and `docs/LABOR_MARKET.md`; this roadmap does not duplicate their completed implementation history.

## Remaining high-value work

### 1. Identification consolidation

Do not try to repair a weak within-state first stage by adding many correlated controls. The immediate next empirical gate is to inspect the expanded, de-duplicated control-intervention first stages (block-only, leave-one-block-out, and declared parameterization substitutions) as relevance evidence; do not promote a control design because it raises F. Scientific aliases are retained separately from execution cells: 55 named absorption questions map to 49 unique first-stage models, and `first_stage_absorption_semantic_summary.csv` projects the fitted diagnostics back onto every named question for direct review. The broader diagnostic universe contains 119 unique IV designs. Only after that diagnostic family is reviewed should the registered scalar-IV consumption robustness family be activated with its multiplicity rule frozen in advance.

The cross-family post-treatment mechanism evidence ledger now synthesizes the registered Census migration/housing, Economic Census, NSS66, and PLFS weak-IV results into a common model-level grid plus family summary. It records first-stage strength, multiplicity-adjusted reduced-form and Anderson--Rubin signals, confidence-set boundedness, temporal role, and whether the run is a causal mechanism or geography robustness analysis. The final identification section should use that generated ledger to synthesize the evidence already generated:

- relevance under the predeclared instrument constructions;
- region versus state fixed-effects sensitivity;
- historical balance/pretrend evidence;
- C-17/English-acquisition mechanism evidence;
- monotonicity diagnostics;
- weak-IV-robust confidence sets;
- influence/state-deletion sensitivity;
- explicit limits on causal interpretation when identifying variation is primarily between states.

Any new control must have a clear exclusion-threat rationale and predetermined timing. Post-treatment Census, firm, migration, or labor-market variables are mechanisms/outcomes, not preferred controls.

### 2. Outcome-specification consolidation

Before final paper claims, choose a small registered causal outcome family. Historical and modern consumption rounds now make it possible to compare baseline-adjusted levels, changes, and dynamic specifications without selecting a specification after seeing significance. Use the comparability diagnostics to define which HCES/NSS contrasts are substantive versus survey-redesign sensitivity.

Specification governance is now consolidated at the execution boundary. Public headline models are declared by `public_iv_specification_registry()` through the same canonical IV row contract used by newer analyses, while their longstanding model names and formula-based downstream table/report interface are preserved by the thin `iv_specification_formulas()` adapter. `analysis_design_registry` inventories those public designs alongside the newer families.

The candidate-design ledger is now organized around the methodological reference plan rather than around an arbitrary preference for a small model count. It makes theoretically distinct relevance families visible (geography/control absorption, symmetric block interventions, registered control parameterizations, scalar and multi-share linguistic constructions, DISE treatment definitions, and historical instrument vintages), records the distinct C-17 and district-schooling mechanism grids, and enumerates causal robustness candidates for consumption horizons, scalar-IV bases, alternative welfare outcomes, the enrolled-child intensive margin, control parameterizations, and historical/geographic-access adjustments. Each family carries an execution policy, multiplicity family, prerequisites, and explicit rationale.

Comprehensiveness still does **not** imply universality. The ledger rejects the endpoint-by-full-diagnostic Cartesian product and the automatic crossing of every individually defensible robustness axis because representability is not a scientific rationale for an interaction. Post-treatment mechanisms remain inadmissible baseline controls. Expand an executable family only after its estimand, sample rule, and multiplicity family are registered—never because a coefficient in an adjacent design is attractive.

### 3. Paper and reviewer-facing outputs

With the firm and labor source/mechanism phases now fixed, once the final causal outcome/robustness registry is frozen:

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
- Do not add parallel estimators when the shared post-treatment mechanism or survey-design layers already represent the estimand.
- Do not add raw-source adapters before the underlying files have been inspected.
