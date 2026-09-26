# Current roadmap

This file lists unfinished work only. Completed implementation history is retained in [`../../archive/research-log/documentation-before-2026-09-rewrite/docs/plan/roadmap.md`](../../archive/research-log/documentation-before-2026-09-rewrite/docs/plan/roadmap.md) and in the domain documentation. The current release objective is a defensible paper, coding sample, and replication repository in which the stated statistical procedures match the code that produces the reported results.

## Open methodological issues

1. **Average marginal effects.** Replace the current broad-error fallback with one standard, verified AME implementation for the survey-weighted probit. The final build should not substitute a different uncertainty calculation when the preferred estimator fails. Document the derivative/inference rule and test it against a trusted implementation.
2. **Survey-estimator failure policy.** Final-mode selection estimation should fail if the required survey design cannot be constructed or the required package is unavailable; it should not silently fall back to an ordinary unweighted probit.
3. **First-stage formula fidelity.** First-stage and spatial first-stage diagnostics should preserve the transformations, factors, and interactions in the registered IV specification rather than reconstructing formulas from `all.vars()` names.
4. **Clustered inference policy.** Revisit small-sample degrees of freedom and covariance corrections for state-clustered Wald/AR inference. Record the chosen finite-sample convention once and apply it consistently.
5. **Bounded exclusion sensitivity.** Keep the custom procedure only if its derivation, clustered uncertainty calculation, and interpretation can be stated and tested directly. Prefer a standard implementation where an equivalent maintained method exists.
6. **District fuzzy-candidate scoring.** Reassess hand-set similarity weights and thresholds against the reviewed adjudication set; final identities must continue to require deterministic/reviewed evidence rather than fuzzy scores alone.
7. **Consumption support duplication.** Determine whether currently identical common-support files represent one shared support definition or intentionally separate families. Consolidate the computation if the estimand is truly shared; otherwise encode the intentional equality so hygiene checks remain informative.

## Open engineering issues

1. Compose the processed-data and full-build analytical target definitions from the same reusable target families so the two routes cannot drift.
2. Remove remaining compatibility aliases/wrappers that have no non-test consumers, including old NSS naming and period-deflator wrappers where backward compatibility is not an external requirement.
3. Consolidate the repeated candidate-IV selection logic, DISE enrollment-ratio calculation, and survey-option context handling identified in the code review.
4. Replace remaining source-text tests with behavioral tests unless the literal text is itself an interface.
5. Remove or archive genuinely dead branches and review-only files after verifying that no active paper, sample, or replication path consumes them.
6. Reduce noisy successful-build logging from auxiliary XeLaTeX/reference passes while preserving complete logs on failure.

## Open documentation work

The documentation restructure is complete when the active files describe current behavior rather than development chronology, each concept has one authoritative explanation, and the repository-wide local-link audit passes. Future methodological changes should update the relevant domain document in the same commit as the code.

## Open presentation and release work

1. Finish the typography pass for the paper and application samples, including remaining overfull boxes and float/page balance.
2. Resolve the nominal five-page writing sample, which currently remains an advisory page-count issue until presentation is final.
3. Remove machine-local Zotero `file = {...}` fields from `paper/references.bib` before final release.
4. Review tracked paper/sample PDFs after the final methodological and typography changes and refresh them once, rather than repeatedly committing intermediate renders.

## Deferred work

The following ideas are outside the present release unless a new paper objective makes them necessary: new data acquisitions not required by the current manuscript; IHDS longitudinal EMI-to-capability/mobility extensions; the low-cost-private-school/RTE project; mother-tongue-versus-EMI learning extensions; and attempts to force unresolved district lineage to full coverage.

## Explicit non-goals

- Do not force unresolved district lineage to 100 percent.
- Do not use fuzzy-only district matches as final production identities.
- Do not manufacture unpublished cells by combining totals with unrelated destination shares.
- Do not add controls simply to repair a weak first stage.
- Do not broaden a weak-identification hypothesis family merely because additional outcomes can be constructed.
- Do not treat descriptive post-treatment mechanisms as identified mediation effects.

## Completion criteria

The release is ready when the preferred estimators fail closed, methodological documentation matches the executed estimators, the processed and full routes agree on their declared shared results, all strict inputs and tracked outputs pass preflight/audit, the full test suite and final build pass, reviewer-facing PDFs have been visually inspected, and the roadmap contains no unresolved release-blocking item.
