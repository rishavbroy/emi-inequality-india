# Application samples

Application samples are generated from the current paper and current R files. The single configuration file, `samples.yml`, specifies writing-sample section IDs and coding-sample marker IDs. Generated working files are written to `.work/`; every other file under `application-samples/`, including the reviewer-facing PDFs in `output/`, is intended to be tracked.

Writing excerpts use the ordinary Quarto section IDs already present in `paper/paper.qmd`. The 5-page version contains the Introduction. The 10-page version adds the complete main-text sequence on measuring EMI access, institutional factors, and social factors. The 15-page version also includes the persistence and reported-language subsections from the inherited-linguistic-conditions section. The full variant uses the complete paper. Excerpts suppress the repeated bibliography while retaining rendered in-text citations; the linked full paper contains complete references. Named and anonymous versions use the same substantive selections and comparable first-page notices so anonymity does not alter pagination.

Coding samples continue to use `sample-start`/`sample-end` comments because R files do not have an equivalent section-ID system. The short sample covers survey-weighted enrollment, average marginal effects, weak-identification diagnostics and Anderson--Rubin inference, and the linked temporal and state-sector price construction used for real consumption. The long sample adds cross-vintage district matching and residual Moran diagnostics for the IV and first-stage models. Both append paper-formatted empirical outputs from the same analyses and are rendered in named and anonymous forms.

Run:

```sh
make samples
```

or run the ordinary full build, which includes application samples by default:

```sh
scripts/run_full_build.sh
```

The first-page writing-sample notice is generated from the manifest and the full-paper section labels. The full-paper render keeps `paper.tex` and makes a separate XeLaTeX label pass that writes `paper-reference-labels.aux`. Before an excerpt is rendered, references to omitted sections, tables, figures, and equations are replaced with their current full-paper labels from that file. Named copies link the displayed label to the hosted paper; anonymous copies display the same label without an identity-bearing URL. References whose targets remain in the excerpt stay as ordinary Quarto cross-references. The Pandoc filter retains the selected sections and their ancestor headings and resets LaTeX counters so retained section headings, tables, figures, and equations keep their full-paper numbers.

The 5-, 10-, and 15-page values in `samples.yml` are current page targets. During the remaining formatting pass, a mismatch is reported as a `WARNING:` line in the build output while the rendered PDF is retained and the build continues. The targets can return to strict validation once sample presentation is settled.

Named paper and application-sample PDFs are published from the tracked rendered files at <https://rishavbroy.github.io/emi-inequality-india/>. The Pages job does not rerun the empirical build. Anonymous variants are intended for direct application uploads and are deliberately not published on the identity-revealing Pages site.

Coding samples use Pandoc's `tango` syntax highlighting with `fvextra` line wrapping and a smaller code font. Tables are included from the same generated LaTeX files used by the paper, retain their full-paper table numbers, and are separated with LaTeX float barriers so a later table cannot pass an earlier output. Figures reuse the paper's generated PDF files.
