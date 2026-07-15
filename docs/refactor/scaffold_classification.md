# Scaffold Classification

This file classifies scaffold-level functions that remain after the semantic refactor. The project should not treat a runnable placeholder as a completed implementation. Cleanup patches should delete unused placeholders once a function is neither called by targets nor tied to a documented legacy diagnostic.

| classification | functions / files | action |
|---|---|---|
| Active and implemented | `build_2007_measures()`, `build_2017_measures()`, `build_linguistic_distance_iv()`, `estimate_selection_probit()`, `compute_average_marginal_effects()`, `estimate_2sls()`, `estimate_first_stage()`, `diagnose_spatial_autocorrelation()`, `make_tables()`, `save_tables()`, `make_figures()`, `save_figures()` | Covered by tests and active targets. |
| Active methodology diagnostics | `diagnose_weak_instruments()`, `diagnose_overidentification()`, `diagnose_multicollinearity()`, `diagnose_spatial_weights()`, `diagnose_district_matching()` | Keep as target-backed diagnostics; these should report current-method analogs rather than pretend exact legacy parity. |
| Active but still undergoing district-harmonization audit | `build_district_panel()`, source-attachment helpers in `R/measures/build_district_panel.R`, `fuzzy_join_districts()`, `join_district_panel()` | Keep and audit through panel validation, public IV-panel diagnostics, and extended district-matching diagnostics. |
| Explicitly experimental / opt-in | `estimate_spatial_iv_experimental()` and benchmark adapters in `R/benchmarking/benchmarking_targets.R` | Run only behind optional benchmarking targets; do not generate final paper claims. |
| Legacy placeholders to remove or implement | `clean_edu0708_households()`, `standardize_edu0708_weights()`, `standardize_cons0708_weights()`, `standardize_edu1718_weights()`, `collapse_or_expand_split_districts()`, `attach_spatial_ids()` | Keep only if a downstream target calls them; otherwise delete in a cleanup pass after district harmonization and cleaning contracts are finalized. |

The following unused scaffolds were removed because they had no callers and no implemented legacy diagnostic contract: generic model-robustness stubs, generic diagnostic-table factory, weak-IV jackknife placeholders, inactive first-difference/state-FE formula placeholders, linguistic-distance variant pass-throughs, and first-stage pass-through wrappers. Future functionality should be introduced only with a target, test, and documented legacy or methodological contract.
