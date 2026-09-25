# Application samples

Application samples are generated from the current paper and current R files. The single configuration file, `samples.yml`, specifies writing-sample section IDs and coding-sample marker IDs. Generated working files are written to `.work/`; every other file under `application-samples/`, including the reviewer-facing PDFs in `output/`, is intended to be tracked.

Writing excerpts use the ordinary Quarto section IDs already present in `paper/paper.qmd`. The short variants build outward from the Introduction: the 10-page sample adds the EMI-access section, and the 15-page sample also adds inherited linguistic conditions. The full variant uses the complete paper. Excerpts suppress the repeated bibliography while retaining rendered in-text citations; the linked full paper contains complete references. Named and anonymous versions use the same substantive selections.

Coding samples continue to use `sample-start`/`sample-end` comments because R files do not have an equivalent section-ID system. The short sample emphasizes survey econometrics, weak-instrument assessment, and economic measurement. The full sample adds Anderson--Rubin sensitivity analysis and cross-vintage district matching. Both are rendered in named and anonymous forms.

Run:

```bash
make samples
```

or run the ordinary full build, which includes application samples by default:

```bash
bash scripts/run_full_build.sh
```

The first-page sample notice is generated from the manifest and the current paper headings. Short writing samples currently replace references to omitted material with a neutral pointer to the full paper. A later application-sample step will preserve the full paper's current figure, table, equation, and section numbers in those pointers.
