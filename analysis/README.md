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

Notebook coverage currently includes the legacy comments and diagnostic/exploratory outputs from Chunks 3, 6, 8, 10, 15, 16, 20, 24, 29, and 30. The QMDs keep prose in the notebooks themselves, show current-code analog chunks with `echo: true` and `output: true`, and read current target-backed tables and figures rather than relying on manually pasted legacy results.

Do not add `--with-samples` unless the changes may affect application-sample PDFs or the sample extraction/rendering code.



## Prose parity

The analysis notes preserve legacy diagnostic prose where possible while replacing hard-coded legacy results with current target-backed results. Nontrivial prose deviations are documented in [`docs/refactor/analysis_prose_deviations.md`](../docs/refactor/analysis_prose_deviations.md).
