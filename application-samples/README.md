# Application samples

Application samples are generated from the current paper and current R files. The single configuration file, `samples.yml`, specifies writing-sample section IDs and coding-sample marker IDs. Generated working files are written to `.work/`; every other file under `application-samples/`, including the reviewer-facing PDFs in `output/`, is intended to be tracked.

Writing excerpts use the ordinary Quarto section IDs already present in `paper/paper.qmd`. The short variants build outward from the Introduction: the 10-page sample adds the measurement and social-access portions of the EMI section, while the 15-page sample also adds the persistence and reported-language portions of the inherited-linguistic-conditions section. The full variant uses the complete paper. Excerpts suppress the repeated bibliography while retaining rendered in-text citations; the linked full paper contains complete references. Named and anonymous versions use the same substantive selections and comparable first-page notices so anonymity does not alter pagination.

Coding samples continue to use `sample-start`/`sample-end` comments because R files do not have an equivalent section-ID system. The short sample emphasizes survey econometrics, weak-instrument assessment, and economic measurement. The full sample adds Anderson--Rubin sensitivity analysis and cross-vintage district matching. Both append selected empirical outputs from the same analyses and are rendered in named and anonymous forms.

Run:

```bash
make samples
```

or run the ordinary full build, which includes application samples by default:

```bash
bash scripts/run_full_build.sh
```

The first-page sample notice is generated from the manifest and the current paper headings. The full-paper render keeps `paper.tex` and makes a separate XeLaTeX label pass that writes `paper-reference-labels.aux`; writing excerpts use LaTeX's standard `xr` package to import those labels. References to retained targets use the excerpt's local numbering; references to omitted targets use the full paper's current number and are marked as full-paper references.

Named paper and application-sample PDFs are published from the tracked rendered files at <https://rishavbroy.github.io/emi-inequality-india/>. The Pages job does not rerun the empirical build. Anonymous variants are intended for direct application uploads and are deliberately not published on the identity-revealing Pages site.

Coding samples use Quarto's idiomatic LaTeX syntax highlighting with line wrapping so long source lines remain visible in the PDF.
