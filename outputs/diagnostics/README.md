# Diagnostic outputs

- `build/` and `public/` contain short-lived target metadata and public-build diagnostics. They are reset by public build audits, ignored by Git, and copied into `review.zip` only as current-run evidence.
- `extended/` contains opt-in diagnostics preserved across ordinary public builds.

This split keeps reproducible research diagnostics in version control while keeping volatile build metadata out of ordinary commits.
