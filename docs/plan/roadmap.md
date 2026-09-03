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

Do not try to repair a weak within-state first stage by adding many correlated controls. The expanded, de-duplicated control-intervention first stages (block-only, leave-one-block-out, and declared parameterization substitutions) are now relevance evidence, not a model-selection device; no control design should be promoted because it raises F. Scientific aliases are retained separately from execution cells: 55 named absorption questions map to 49 unique first-stage models, and `first_stage_absorption_semantic_summary.csv` projects the fitted diagnostics back onto every named question for direct review. The broader diagnostic universe contains 119 unique IV designs. That review has already closed the gate for the registered scalar-IV consumption family; the remaining control question is now governed separately as causal strategy, proxy parameterization, and historical-vintage robustness.

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

The symmetric first-stage audit has now been realized. Alternative parameterizations can strengthen the six-region first stage, but state-FE relevance remains weak and none of the six serious scalar region/state × Shastry/Glottolog/Dyen designs approaches the registered effective-F threshold. This closes the relevance-review gate without selecting a stronger-looking specification. The predeclared 48-cell scalar-IV consumption robustness family is therefore activated on common support within each endpoint/estimand, with Holm correction within each six-design welfare family and across the full 48 cells. The 119-design diagnostic universe remains diagnostic-only and is not crossed mechanically with outcomes.

The realized 48-cell results retain the weak-identification warning rather than overturning it. Effective-F values remain far below the registered threshold; only the 2022-23 change/state-FE/preferred-distance AR beta-zero test survives Holm correction across all 48 cells, and several AR confidence sets are disconnected or grid-truncated. The next predeclared axis is therefore response definition, not instrument selection. The 20 survey-compatible mean-log/median/bottom-40 endpoint×estimand templates are crossed with the same six scalar designs as a Tier-C 120-cell family, on common support within response template and with Holm correction within template and across the full secondary family.


The realized 120-cell response-definition family closes that axis without overturning the weak-identification warning: no cell survives Holm correction across the complete family, although four 2022-23 cells survive only their six-design within-template adjustment. Effective-F values remain far below the registered threshold and most AR sets are grid-truncated. The next predeclared axis is therefore the finite causal control-strategy family, holding the preferred Shastry instrument and primary real-mean-MPCE response definitions fixed. Eight endpoint/estimand designs are crossed with six region/state geography-only, compact-2001, and compact-without-human-capital strategies on common support, with Holm correction within endpoint and across all 48 cells.

The realized 48-cell control-strategy family shows that the 2022-23 change signal is sensitive to conditioning philosophy but does not identify a uniquely defensible adjustment set. Geography-only and compact-2001 state-FE cells survive full-family Holm-adjusted AR inference; the no-human-capital state-FE cell survives only within endpoint. All remain weak by the effective-F criterion and the rejecting AR sets are disconnected/grid-truncated. The next isolated axis is therefore compact-2001 **parameterization**, not control selection: re-estimate the compact-secondary benchmark and the three registered literacy/economic-structure substitutions under region/state FE on common support. This is an 8-adjustment × 8 endpoint/estimand = 64-cell family with the preferred Shastry instrument fixed.

The realized 64-cell parameterization family closes that axis without producing family-wide weak-IV-robust evidence: no cell survives Holm adjustment across all 64 designs, although three 2022-23 state-FE variants survive the within-endpoint correction. No effective-F statistic reaches the registered threshold; many AR sets remain grid-truncated or disconnected. The next isolated axis is therefore **remote historical adjustment**. For each of the eight registered real-mean-MPCE endpoint/estimand designs, re-estimate the compact-2001 benchmark and a population-interpolated PCA91 adjustment under region and state FE on the same four-design common sample. Use the already validated G2 Census-1991→2001 population interpolation at the frozen 99% source-coverage threshold. Treat this as a remote-baseline robustness exercise, not a pure same-variable vintage substitution: PCA91 and compact-2001 controls overlap in population, composition, literacy, and worker structure but do not contain identical concepts.

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


### Control-strategy governance

The historical `main` Census-2001 vector is retained as a compact benchmark, not as a uniquely theory-preferred causal adjustment set. The finite control-strategy and proxy-parameterization families are now complete. The next isolated axis is the remote 1991 baseline comparison: compact-2001 benchmark versus population-interpolated PCA91 controls under region/state FE, using common support and the preferred Shastry instrument. Do not automatically cross this family with alternative response, treatment, or instrument definitions.

### Historical-control source/concept validation

The 48-cell concept-matched historical family has now been realized on common support. The 2022-23 state-FE change design rejects beta zero after family-wide Holm adjustment under compact-2001, PCA91, and Vanneman adjustment; the 2023-24 state-FE change survives under PCA91 and Vanneman but not compact-2001. Effective F remains extremely weak (maximum about 2.82), and the relevant AR sets remain grid-truncated/disconnected, so this does not resolve identification. The official 1991 primary-source gate is now implemented with ORGI B-01(S), C-02/C-02U, C-06, and district-level C-09. Aggregate-only small-UT state workbooks are explicitly non-district evidence and are skipped rather than synthesized into district observations. On the downloaded source files, the registered count contracts reconcile exactly with Vanneman across all 397 Vanneman districts for population, urban population, secondary-plus education, main workers, dependency components, Muslim population, and the C-09 religion-category population sum; C-09 published total population is kept diagnostic because Dhule has a source-table total anomaly. This materially reduces the source-validity concern without changing the already-observed causal family. Remaining historical concept gaps are agricultural-worker composition, SC/ST independent primary validation, and district electrification; state-only H-series workbooks must not be substituted for district controls. After the next green audit records this validation, the predeclared intensive-margin EMI treatment family becomes the next substantive robustness axis.

The official Census-1991 validation gate is now realized: every exact-required B-01(S), C-02/C-02U, C-06, and C-09 comparison matches all 397 Vanneman reference districts, with only the predeclared non-fatal Dhule C-09 published-total anomaly remaining. The next predeclared causal axis is therefore treatment definition. A 48-cell intensive-margin EMI robustness family is activated using `emi_share_enrolled_0708` across the same eight primary-welfare endpoint/estimand designs and six region/state × scalar-distance designs as the frozen scalar-IV family. This family is Tier C, uses endpoint-specific common support, and retains its own Holm multiplicity boundary rather than being merged retrospectively with the already-observed preferred all-child-exposure family.

The intensive-margin treatment family is now realized. No cell reaches the registered effective-F threshold (maximum about 5.26). One 2022-23 state-FE change/preferred-distance cell survives family-wide Holm-adjusted reduced-form and AR beta-zero inference, but its AR set is disconnected and grid-truncated; the treatment axis therefore does not resolve weak identification. With the major predeclared consumption robustness axes now realized, the next architecture step is synthesis rather than another automatic axis: the audit writes a common consumption robustness evidence grid and family summary spanning scalar-IV, treatment-definition, welfare-definition, control-strategy, control-parameterization, PCA91 historical, and concept-matched historical families. Use this summary for paper/reviewer interpretation before opening data-dependent geographic-access controls or any new causal family.

The final admissible first-stage relevance candidate is now implemented as the two-cell Shastry Hindi-belt comparison. It freezes Shastry's published state definition on Census-2001 codes and adds the indicator to the main-control first stage with either no geographic FE or six-region FE, on one common support and against paired baselines. State FE are excluded because the indicator is state-level and would be absorbed. Once the realized comparison is reviewed, no admissible executable candidate remains in the design ledger apart from items explicitly requiring unavailable/deferred data or marked do-not-estimate; the next substantive phase is paper-facing identification synthesis rather than another automatic specification axis.
