# Diagnostics outputs

Diagnostics are split by run policy:

- `build/`: short-lived target metadata and target warning files from public build audits.
- `public/`: public parity/audit diagnostics that should be regenerated during public audits.
- `extended/`: longer diagnostic outputs that are preserved unless explicitly cleaned or rerun.

Ordinary public builds should not delete `extended/`.
