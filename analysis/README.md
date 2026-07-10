# Analysis notebooks

These notebooks preserve diagnostic, benchmarking, troubleshooting, and exploratory material from the legacy R Markdown workflow. They should be rendered separately from the public paper build because some outputs are slow, noisy, or intended only for audit/research review.

Run:

```sh
make analysis-notes
```

The notebooks read current CSV outputs from `outputs/diagnostics/` and `outputs/benchmarking/` rather than relying on manually pasted results from legacy comments.
