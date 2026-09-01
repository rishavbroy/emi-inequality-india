# Diagnostic outputs

- `build/` and `public/` contain short-lived target metadata and public-build diagnostics. They are reset by public build audits, ignored by Git, and copied into `review.zip` only as current-run evidence.
- `extended/` contains opt-in research diagnostics preserved across ordinary public builds.

## Retention rule

The pipeline is **objects first, artifacts last**. Intermediate calculations belong in cached `{targets}` objects. Persist an extended diagnostic only when the file is itself useful as a scientific summary, a public/report input, or a forensic QA ledger that a reviewer may need to inspect independently. Do not write a second copy of an object merely because another analysis family consumes it.

In particular, benchmark directories own benchmark-specific results, while the diagnostic directory owns shared candidate universes and QA references. Historical comparison savers may reuse canonical source levels/changes instead of persisting byte-identical copies under every comparison prefix. Exhaustive Anderson--Rubin inversion grids for permutation universes are retained in cached `{targets}` objects rather than serialized as CSVs. Persist raw inversion grids only for compact, predeclared candidate/preferred designs whose pointwise acceptance path is itself useful for independent review; otherwise persist the summarized confidence-set diagnostics.

This split keeps reproducible research diagnostics in version control while keeping volatile build metadata and redundant intermediate artifacts out of ordinary commits.
