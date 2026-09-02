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

The absorption diagnostics therefore retain the historical cumulative ladder **and** add two symmetric families. `iv_block_intervention_adjustments()` estimates every main theoretical control block on its own and omits every block from the main set under both region and state fixed effects. `iv_main_parameterization_adjustments()` exhausts the finite substitutions already declared by the control metadata: secondary-plus versus literacy and compact versus decomposed economic structure. These families answer interpretable questions about attenuation and measurement without enumerating arbitrary individual-covariate subsets.

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
