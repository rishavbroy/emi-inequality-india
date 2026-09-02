# Instrumental-variable diagnostic architecture

The extended IV diagnostics use one canonical specification layer rather than reconstructing fixed effects, controls, language adjustments, and excluded instruments inside each diagnostic.

`R/iv/specification_registry.R` defines:

- admissible fixed-effect terms;
- Census-2001 control blocks, the historical absorption ladder, symmetric block interventions, and finite alternative-control parameterizations;
- the main and expanded adjustment sets;
- alternative linguistic-distance constructions;
- structural specification metadata with treatment, outcome, excluded instruments, included language controls, clustering variable, sample rule, panel variant, and diagnostic tier;
- a de-duplicated diagnostic specification registry combining the alternative-instrument grid with the additional absorption/control-block specifications;
- a diagnostic registry and saved applicability relation.

The registry is intentionally not the Cartesian product of every imaginable project option. Comprehensiveness is defined by scientific rationale, not by a target number of specifications: theoretically distinct relevance questions are made visible even when that enlarges the diagnostic universe, while mechanically crossable dimensions are excluded unless the interaction itself has a substantive interpretation. Specifications that are algebraically identical are de-duplicated before the general diagnostic suite runs.

All adjustment registries now use one named execution contract: `label`, `fixed_effect`, and `controls`, with optional family-specific metadata. Older absorption and parameterization builders previously emitted anonymous positional triples, while canonical IV adjustments used named fields; that split allowed a valid parameterization design to fail only when it reached the consumption compiler. Keeping one schema makes diagnostic, governance, and causal adjustment families interchangeable at the compiler boundary and removes positional `[[1]]`/`[[2]]`/`[[3]]` coupling.

The absorption diagnostics therefore retain the historical cumulative ladder **and** add two symmetric families. `iv_block_intervention_adjustments()` estimates every main theoretical control block on its own and omits every block from the main set under both region and state fixed effects. `iv_main_parameterization_adjustments()` exhausts the finite substitutions already declared by the control metadata: secondary-plus versus literacy and compact versus decomposed economic structure. These families answer interpretable questions about attenuation and measurement without enumerating arbitrary individual-covariate subsets.

Scientific questions and execution cells are intentionally distinct. A symmetric intervention can be algebraically identical to an older diagnostic (for example, the first cumulative block is also that block alone, and the historical human-capital omission already answers one leave-one-block-out question). `iv_absorption_specification_candidates()` retains every named scientific question, `iv_absorption_specification_registry()` de-duplicates formula/sample signatures for execution, and `iv_absorption_specification_aliases()` records the semantic-to-execution mapping. The current 55 named absorption questions reduce to 49 unique first-stage executions; after overlap with the five canonical nonzero-mean adjustment cells, the general diagnostic registry contains 119 unique IV designs. `first_stage_absorption_aliases.csv` preserves those aliases for reviewer audit rather than recomputing identical regressions under different labels.

The candidate-design ledger correspondingly separates `candidate_cells`, `implemented_cells`, and `execution_cells`: a scientific question can be implemented by an already-existing execution cell. This keeps governance comprehensive without making estimation duplicate work.
Because DISE crosses each of its eight registered treatment constructs with the unique diagnostic registry, this implies 952 first-stage treatment-definition cells at the current 119-design universe rather than 984 cells from double-counting semantic aliases.

The `cluster` field is part of that self-describing specification contract. Registry-driven relevance, balance, reduced-form, and Anderson-Rubin inference use the declared cluster variable directly rather than substituting a hard-coded state column. The current registry declares `state_code_2001` throughout, so this is a structural invariant rather than a change to the preferred inference.

The effective-F statistic is computed with the standard `momentfit::MOPtest()` implementation of Montiel Olea and Pflueger (2013). The project uses the simplified TSLS test with the conventional 10% relative-bias tolerance (`tau = 0.10`) and 5% test size, and records the effective F, effective degrees of freedom, critical value, and p-value. The wrapper reconstructs the moment model from the already-fitted canonical `ivreg` formula and exact fitted sample; `ivreg` remains the source of record for 2SLS coefficients and conventional clustered inference.

For the registered state-clustered specifications, `momentfit` uses clustered moment covariance (`vcov = "CL"`) with HC0 and its finite-cluster adjustment. The existing excluded-instrument Wald F deliberately remains the project's HC1 `sandwich::vcovCL()` statistic. The two relevance diagnostics are therefore reported side by side rather than forced to coincide. In a just-identified model the effective F equals the appropriately robust first-stage F when covariance conventions are matched; the test suite verifies that documented identity under unclustered HC0, but the project does not assert equality between its clustered HC0 MOP statistic and HC1 Wald F. Weak-identification-robust Anderson-Rubin inference remains the inferential safeguard when relevance is weak.

MOP effective F is attached only to outcome-defined structural IV specifications because its standard definition uses the structural IV model. Outcome-free absorption ladders and alternative first-stage-only comparisons continue to report their clustered excluded-instrument F and partial R-squared without inventing an outcome solely to obtain an effective F.

## Diagnostic families

The current registry distinguishes:

- **relevance**: clustered excluded-instrument Wald tests, Montiel Olea–Pflueger effective F, individual first-stage coefficients, and partial R-squared;
- **independence evidence**: specification-matched covariate balance plus an omnibus holdout-covariate balance test;
- **weak-identification-robust inference**: Anderson-Rubin tests and inverted grids for structural IV specifications;
- **monotonicity evidence**: residualized scalar first-stage shape, isotonic fit, binned means, and state-specific slopes;
- **overidentification**: the standard Sargan overidentifying-restrictions diagnostic for specifications with more excluded instruments than endogenous regressors.

Applicability and implementation are separate fields. `will_run` is true only when a diagnostic is methodologically applicable and implemented. Scalar first-stage shape is explicitly inapplicable to multi-instrument constructions rather than being silently coerced into a one-dimensional ordering.

Balance is evidence about the independence argument, not a separate IV identifying assumption. Exogeneity and exclusion are not directly testable from observed data; historical balance, placebo outcomes, migration, geography, and related exercises provide cumulative evidence rather than proof.

## Relevance and specification contract

The alternative linguistic-distance grid and the richer first-stage absorption ladder now obtain their specification metadata from the same IV registry layer. Historical output files are retained where useful for compatibility, but fixed effects, controls, instrument sets, and control-block definitions are no longer independently declared inside the two diagnostic modules.

### Anderson--Rubin artifact retention

Anderson--Rubin grids are computational inputs to confidence-set inversion, not automatically reportable artifacts. The diagnostic objects retain the pointwise grids so validation and downstream inference can inspect them. The broad alternative-distance permutation universe no longer writes its full grid to disk; its persisted weak-IV summary already records the beta-zero test and the inverted confidence-set components for every registered design. Raw grids remain persisted only for compact, predeclared candidate/preferred analyses where the pointwise acceptance path is itself a useful review artifact.

### Candidate-design governance

`candidate_design_ledger.csv` is an explicit map from the project's methodological reference plan to the executable design space. Each row records the motivating reference section, scientific question, design axis, execution policy, multiplicity family, prerequisites, admissibility, and implementation status. It separates:

- **relevance diagnostics**, where broad variation across geography, control blocks, treatment definitions, linguistic-distance bases, and historical vintages is itself scientifically informative;
- **causal robustness families**, where response definitions, treatment margins, instrument bases, control parameterizations, and horizons are admitted only when the estimand remains interpretable;
- **mechanism/falsification designs**, including the distinct C-17 state-by-language registry and the predeclared three-geography district-schooling grid;
- **data-dependent candidates**, such as Shastry-style major-city/coast controls, which remain visible without fabricating inputs;
- **non-goals**, including post-treatment mechanisms as baseline controls and mechanical Cartesian products of individually defensible robustness axes.

This ledger is deliberately broader than the paper-facing model family. Visibility is not execution: `diagnostic_only`, `estimate_if_registered`, `requires_data`, and `do_not_estimate` are distinct policies. A design may therefore be scientifically justified and visible without being automatically dispatched.

### Alternative-distance candidate-design comparison

The Glottolog and Dyen constructions are robustness measurements of linguistic
distance, not candidate instruments to promote merely because a particular
fixed-effect specification produces a larger first stage. The diagnostic layer
therefore compares the shared candidate construction set registered by
`iv_candidate_design_constructions()` under both main-control designs from
`iv_candidate_design_adjustments()`: six-region fixed effects and Census-2001
state fixed effects. The same two-by-three contract is reused by post-treatment
mechanisms and the candidate-design governance ledger.

`alternative_distance_design_evidence.csv` places the Shastry nonzero-mean,
Glottolog, and Dyen constructions under both candidate adjustments side by
side, with clustered first-stage strength, partial R-squared, Montiel
Olea--Pflueger effective F and its critical value, and Anderson--Rubin
confidence-set diagnostics. `alternative_distance_design_comparison.csv`
summarizes relevance separately within each FE design.

No diagnostic rule chooses between region and state fixed effects. State FE
remove more state-level confounding but also absorb substantial linguistic
variation; region FE preserve more cross-state variation but leave more
state-level institutional and historical heterogeneity available to correlate
with the instrument. Relative or absolute first-stage strength is evidence
about relevance, not a sufficient criterion for choosing the identifying
design.

A robustness construction is likewise not promoted because it has a larger
conventional F statistic. Weak-identification screens and Anderson--Rubin
inference remain conditional on the candidate design. Final methodology should
integrate relevance with historical balance and pretrends, migration/sorting,
spatial evidence, mechanism evidence, and the substantive interpretation of
the remaining variation.

### Scientific questions versus fitted first-stage models

The expanded control-intervention family distinguishes **semantic specifications** from **execution specifications**. Fifty-five named absorption questions are retained for scientific governance, but exact formula/sample aliases are fitted only once through the canonical IV-signature de-duplicator. The current registry therefore maps 55 scientific questions to 49 unique executions. `first_stage_absorption_aliases.csv` records the mapping, while `first_stage_absorption_semantic_summary.csv` joins every named question back to the corresponding fitted estimate, partial R-squared, excluded-instrument F statistic, and other execution diagnostics. This keeps the specification audit comprehensive without duplicating regressions or forcing reviewers to join artifacts manually.

## Symmetric control evidence and bounded consumption robustness

The expanded first-stage control audit is diagnostic rather than a model-selection exercise. In the realized 2007-08 EMI first stage, theory-motivated substitutions can materially improve the six-region specification (for example, literacy plus decomposed economic structure produces an excluded-instrument F near 10, and omitting the human-capital block produces an F near 6.4), but the corresponding state-FE designs remain weak (roughly 1.8 and 1.6). The registered scalar linguistic alternatives are weaker still by the effective-F criterion: the six region/state × Shastry/Glottolog/Dyen candidate designs have effective F statistics of roughly 0.07--3.33 against a critical value near 23.1. These results are evidence about where relevance is absorbed; they do not justify promoting the strongest region/control specification.

The predeclared consumption robustness family therefore keeps exactly the six serious scalar-IV/geography designs for each of the eight registered endpoint/estimand specifications: region and state **compact-2001** adjustment (the historical `main` IDs) crossed with Shastry, Glottolog, and Dyen distance. Holding that adjustment vector fixed is an axis-isolation rule for the instrument/geography robustness family; it is **not** a claim that the compact-2001 vector is uniquely justified. Each endpoint/estimand is estimated on one common district sample across its six designs so instrument comparisons do not silently change support. Reduced-form and Anderson--Rubin beta-zero p-values receive Holm correction both within each six-design endpoint/estimand family and across the full 48-cell family. Conventional 2SLS estimates remain reported, but weak-IV-robust Anderson--Rubin inference governs causal interpretation.

Control choice is governed separately. The candidate ledger distinguishes a finite causal control-strategy family—geography FE only, compact-2001 adjustment, and compact-2001 adjustment without human capital, each under region/state FE—from the separate finite measurement-parameterization family that substitutes literacy for secondary-plus attainment and decomposed for compact economic structure. This prevents the older `main`/`expanded` labels from masquerading as a causal hierarchy and avoids automatic Cartesian crossing of response, instrument, treatment, and control robustness axes.

The full 119-design diagnostic IV universe remains a relevance/validity diagnostic and is not crossed with consumption outcomes. The 48-cell family is the registered causal robustness boundary for the instrument-definition/geography axis.


### Extended-target reachability

The extended audit invokes targets by the `diag_ext_` prefix. Durable extended-IV artifacts therefore terminate in `diag_ext_` file targets; their upstream specification and estimation objects remain ordinary internal targets. The compiled cross-family design ontology is persisted as `outputs/diagnostics/extended/iv/analysis_design_registry.csv`, while the scalar-consumption robustness bundle is reached through `diag_ext_consumption_scalar_iv_robustness_files`. This naming rule is part of the pipeline contract, not a cosmetic convention.


The realized 48-cell scalar-consumption family confirms pervasive weak identification: no effective-F value approaches the registered critical value. One 2022-23 change/state-FE/preferred-distance Anderson-Rubin beta-zero test survives Holm correction across the 48-cell family, while most specifications do not and many AR sets are disconnected or grid-truncated. This pattern motivates the next predeclared robustness axis—response definition—without changing the preferred instrument or control specification. The 120-cell alternative-welfare family therefore reuses the same six scalar designs and weak-IV-robust inference, with its own separately frozen Holm family.

The realized 120-cell response-definition family does not strengthen that claim at the family level. No mean-log, weighted-median, or bottom-40 cell survives Holm correction across all 120 response-definition specifications. Four 2022-23 cells survive only their six-design within-template correction, while effective-F statistics remain far below the critical value and most AR sets are grid-truncated. This closes the response-definition gate without promoting a different welfare outcome.

The next registered axis is therefore causal control strategy with the preferred Shastry scalar instrument held fixed. For each of the eight primary mean-MPCE endpoint/estimand designs, six theory-defined adjustments compare region/state geography-only, compact-2001, and compact-2001-without-human-capital strategies on one common sample. Holm correction is frozen within each six-strategy endpoint/estimand and across the full 48-cell `consumption_control_strategy` family. This family tests alternative exclusion/conditioning philosophies; it is not a search for the adjustment set with the largest first-stage F.

The realized control-strategy family leaves the weak-identification warning intact. Two 2022-23 change/state-FE cells survive family-wide Holm-adjusted AR inference (geography-only and compact-2001), while the no-human-capital state-FE cell survives only the within-endpoint family. Their effective-F values remain far below the registered critical value and their AR confidence sets are disconnected and grid-truncated. The next axis therefore holds the compact-2001 conditioning philosophy fixed and varies only its registered proxy parameterization. The parameterization family includes the benchmark plus all three literacy/economic-structure substitutions under both region and state FE, yielding eight adjustments per endpoint and 64 cells total on common support.

The realized 64-cell parameterization family also leaves the weak-identification warning intact: no cell survives family-wide Holm adjustment, even though three 2022-23 state-FE cells survive their within-endpoint correction. The next causal-control axis therefore compares the compact-2001 benchmark with a more remote PCA91 baseline. `production_historical_baseline_1991_controls()` promotes only the already validated G2 population-interpolated PCA91 controls at the frozen 99% source-coverage threshold to production-analysis geography. `iv_historical_adjustment_comparison_adjustments()` then pairs those controls with the compact-2001 benchmark under region/state FE. Each endpoint uses a common four-design sample, so any difference is not induced by support drift. Because the 1991 and 2001 concept sets are not identical, this is an historical-adjustment robustness family rather than a pure same-variable vintage test.
