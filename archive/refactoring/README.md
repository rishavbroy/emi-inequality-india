# Refactoring proof archive

The executable proof of the completed legacy-to-current refactor is preserved by
the annotated tag and archive branch created before active refactor machinery was
removed from `main`.

Recommended freeze commands:

```bash
tag="refactoring-complete-$(date +%Y%m%d)"
git tag -a "$tag" -m "All legacy results, whether used in the paper or used in my diagnoses/testing, have either been replicated or improved upon."
git branch archive/refactoring-complete "$tag"
```

After tagging, generate an archival proof log from a clean state, preferably
after clearing cached/pre-saved results:

```bash
make clean-targets
make clean-renders
make clean-analysis
make clean-extended-diagnostics
make clean-benchmarking
caffeinate -dimsu bash scripts/run_public_build_audit.sh \
  --with-samples \
  --archive-always \
  --with-analysis-notes \
  --with-extended-diagnostics \
  --with-benchmarks \
  2>&1 | tee archive/refactoring/final-audit-log.txt
unzip -l review.zip > archive/refactoring/review-archive-inventory.txt
```

The active `main` branch no longer regenerates public Quarto sources from the
archived R Markdown draft and no longer audits legacy parity during ordinary
builds. Historical refactor notes, initial migration/admin material, and one-off
repository/raw-data migration scripts are kept under this archive directory for
provenance only. To rerun the legacy-regeneration or legacy-parity machinery,
check out the frozen tag or branch instead of trying to run archived materials
from their moved locations.
