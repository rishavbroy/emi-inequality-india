# Application samples

Application samples are generated from the current paper and R code. [`samples.yml`](samples.yml) is the single configuration file for sample content, identity variants, paper/repository links, and selected code outputs.

## Outputs

The build writes named and anonymous variants under [`output/`](output/):

- writing samples targeting 5, 10, and 15 pages plus the full paper;
- short and long coding samples.

Generated PDFs are reviewer-facing publication files. Intermediate Markdown/LaTeX files are temporary and are not separately maintained documents.

## Content selection

Writing samples select ordinary Quarto section IDs from `paper/paper.qmd`. Their content therefore remains part of the paper rather than being copied into parallel sample documents.

Coding samples use paired `sample-start:` / `sample-end:` comments in active R files because R has no document-section analogue. Marker IDs are declared in `samples.yml`, and validation requires each selected marker pair to be unique and well formed.

## Build

From the repository root:

```sh
make samples
```

The standard complete build also renders samples unless sample rendering is explicitly disabled for a maintainer-only run. Build flags belong in [`../docs/BUILD.md`](../docs/BUILD.md).

## Named and anonymous variants

Both variants are generated from the same selected content. Named samples may link to the hosted paper, repository, Makefile, and build script. Anonymous samples omit identifying names and repository URLs and are checked against the forbidden strings declared in `samples.yml`.

Identity changes belong in the manifest/rendering helpers, not in duplicate prose files.

## Writing-sample numbering and references

Writing excerpts preserve the current full paper's section, table, figure, and equation numbering. References to omitted material are displayed using the current full-paper labels rather than a second hard-coded numbering scheme. This keeps excerpts synchronized when the paper structure changes.

The low-level LaTeX/reference extraction is an implementation detail of the renderer; maintainers normally change section selections only in `samples.yml`.

## Coding-sample outputs

Selected code excerpts may include paper-formatted tables or figures listed under `coding_outputs`. A selected LaTeX table declares the paper cross-reference whose displayed number it should retain. Output composition does not re-estimate results independently of the main analysis.

## Page-count policy

Writing-sample target lengths are checked after rendering. During the typography pass, a mismatch is a visible warning rather than a release failure. Strict enforcement should be enabled only after the paper/sample layout is considered final.

## Publication

GitHub Pages publishes the tracked reviewer-facing PDFs from this repository; it does not rerun restricted-data analyses in CI. Anonymous files are published only when that is explicitly intended by the Pages configuration and review policy.

## Maintenance

- Change writing/code selections in `samples.yml`.
- Add or remove code markers in the active R implementation that the sample should display.
- Keep selected paper/table labels stable through ordinary Quarto IDs rather than sample-only IDs.
- Test anonymity, marker uniqueness, numbering behavior, and output existence as behavioral invariants; avoid tests that freeze explanatory prose.
- Review generated PDFs after changes to selections, typography, or paper numbering.
