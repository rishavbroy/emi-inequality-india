# Analysis notebooks

These notebooks preserve diagnostic, benchmarking, troubleshooting, and exploratory material from the legacy R Markdown workflow. They are rendered separately from the public paper build because some outputs are slow, noisy, or intended only for audit/research review.

Render them directly with:

```sh
make analysis-notes
```

Or include them in the same log as a public audit with:

```sh
bash scripts/run_public_build_audit.sh --incremental --archive-always --with-analysis-notes
```

The notebooks render to GitHub-flavored Markdown (`.md`) rather than PDF/HTML so they are readable in GitHub and avoid LaTeX-cache failures. They read current CSV outputs from `outputs/diagnostics/` and `outputs/benchmarking/` rather than relying on manually pasted results from legacy comments.

Notebook coverage currently includes the legacy comments from Chunks 3, 6, 8, 10, 15, 16, 20, 24, 29, and 30. The prose sections use the legacy comments as the source of truth, with comment markers removed and code-like commented blocks fenced. Current-output tables follow each legacy-comment section so methodological changes are visible without rewriting legacy prose.

Do not add `--with-samples` unless the changes may affect application-sample PDFs or the sample extraction/rendering code.

- `exploratory/map-tuning.qmd`: map palette, break, and export-size tuning notes moved out of legacy comments and refreshed from current figure targets.
