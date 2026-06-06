# Scaffold Classification

This file classifies scaffold-level functions that remain after the semantic refactor. The project should not treat a runnable placeholder as a completed implementation.

| classification | functions / files | action |
|---|---|---|
| Active and implemented | `build_2007_measures()`, `build_2017_measures()`, `build_linguistic_distance_iv()`, `estimate_selection_probit()`, `compute_average_marginal_effects()`, `estimate_2sls()`, `estimate_first_stage()`, `make_tables()`, `save_tables()`, `make_figures()`, `save_figures()` | Covered by tests and active targets. |
| Active but blocked on validated district harmonization | `build_district_panel()`, `attach_panel_geometry()`, map-generation branches in `make_figures()` / `save_figures()` | Final mode now fails unless the `sf` geometry join covers at least 75% of district-panel rows. The current processed panel reaches about 1.5% coverage, so final maps are intentionally blocked. |
| Active but incomplete legacy port | `build_district_tracker()`, `fuzzy_join_districts()`, `join_district_panel()` | Must be replaced with the full cascading matching logic from legacy chunks 16-20 before final geography, state fixed effects, and spatial diagnostics can be trusted. |
| Explicitly inactive / future work | `build_fd_2sls_formula()`, `build_state_fe_2sls_formula()`, `estimate_spatial_iv_experimental()`, `diagnose_spatial_autocorrelation()` | Return explicit inactive statuses or documented blockers. They should not generate final paper claims. |
| Legacy placeholders to remove or implement | `clean_edu0708_households()`, `standardize_edu0708_weights()`, `standardize_cons0708_weights()`, `standardize_edu1718_weights()`, `attach_baseline_controls()`, `attach_iv_measures()`, `collapse_or_expand_split_districts()`, `attach_spatial_ids()` | Keep only if a downstream target calls them; otherwise delete in a cleanup pass after district harmonization is ported. |

Remaining generic roxygen `@return Function-specific return value.` comments are documentation debt, not evidence of completed implementation. They should be replaced file-by-file as each module's contract stabilizes.
