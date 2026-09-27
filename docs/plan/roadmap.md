# Current roadmap

This file lists unfinished work only. Completed implementation history is retained in [`../../archive/research-log/documentation-before-2026-09-rewrite/docs/plan/roadmap.md`](../../archive/research-log/documentation-before-2026-09-rewrite/docs/plan/roadmap.md) and in the domain documentation. The current release objective is a defensible paper, coding sample, and replication repository in which the stated statistical procedures match the code that produces the reported results.

## Open methodological issues

1. **Anderson--Rubin tail classification.** Interior confidence-set boundaries are root-refined, but accepted components that reach the finite topology-search range are still reported as search-truncated. Add adaptive range expansion or another defensible stopping rule before classifying a tail as genuinely unbounded, and benchmark the ordinary homoskedastic special case against an independent implementation.
2. **Price-deflator sensitivity.** Revisit the documented overlap/linking choices, the 2011--12 spatial normalization, and the sensitivity of real-consumption results to defensible alternatives. Production temporal linking should also consume the shared `price_link_factor()` rule rather than maintain a second median-ratio implementation.
3. **Moran inference convention.** Decide whether the public spatial residual check is intentionally the asymptotic `spdep::moran.test()` or whether a fixed-seed `spdep::moran.mc()` permutation result should be the reported robustness check. Remove the inactive 9,999-draw scaffold once that choice is documented.
4. **District fuzzy-candidate scoring.** Reassess hand-set similarity weights and thresholds against the reviewed adjudication set; final identities must continue to require deterministic/reviewed evidence rather than fuzzy scores alone.

## Open engineering issues

1. Replace regex discovery in the manual district-correction API with explicit correction fields/schemas so a correction cannot mutate an unrelated state/district-like column. Keep identity/name corrections distinct from lineage events such as splits, merges, carve-outs, and border shifts.
2. Remove remaining compatibility aliases/wrappers that have no non-test consumers, including old NSS naming and period-deflator wrappers where backward compatibility is not an external requirement.
3. Consolidate the repeated candidate-IV selection logic and DISE enrollment-ratio calculation identified in the code review.
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
