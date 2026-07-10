# Analysis notebooks

These notebooks preserve diagnostic, benchmarking, troubleshooting, and exploratory material from the legacy R Markdown workflow. They are rendered separately from the public paper build because some outputs are slow, noisy, or intended only for audit/research review.

Render them directly with:

```sh
make analysis-notes
```

Or include them in the same log as a full public audit with:

```sh
bash scripts/run_public_build_audit.sh --incremental --archive-always --with-analysis-notes
```

The notebooks render to GitHub-flavored Markdown (`.md`) rather than PDF/HTML so they are readable in GitHub and avoid LaTeX-cache failures. They read current CSV outputs from `outputs/diagnostics/` and `outputs/benchmarking/` rather than relying on manually pasted results from legacy comments.

Do not add `--with-samples` unless the changes may affect application-sample PDFs or the sample extraction/rendering code.
