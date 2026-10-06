# Application samples

Application samples are generated from the current paper and research code. [`samples.yml`](samples.yml) is the single configuration file for sample content, identity variants, paper/replication-package links, and selected code outputs.

## Outputs

The build writes named and anonymous variants under [`output/`](output/):

- writing samples targeting 5, 10, and 15 pages, file-size-limited copies, and the full paper;
- one code sample plus a file-size-limited copy with the same excerpts.

Generated PDFs are the maintained application files. Intermediate Markdown and LaTeX files are temporary build products.

## Content selection

Writing samples select ordinary Quarto section IDs from `paper/paper.qmd`, so the excerpts are generated directly from the paper.

Code samples use paired `sample-start:` / `sample-end:` comments in active R and Python files. Marker IDs are declared in `samples.yml`, and validation requires each selected marker pair to be unique and well formed. An excerpt may declare a `section` in the manifest; the renderer emits that section heading immediately before the excerpt, so thematic organization remains configuration rather than duplicated prose in generated files.

Each code-sample description and each selected code comment should make sense to a technical reader who has not read the paper or the rest of the replication package. Spell out project-specific abbreviations at first use, explain the empirical quantity or data irregularity before implementation details, and reserve comments for methodological choices or data assumptions that the code does not make obvious.

## Build

From the repository root:

```sh
make samples
```

The standard complete build also renders samples unless sample rendering is explicitly disabled for a maintainer-only run. Build flags belong in [`../docs/BUILD.md`](../docs/BUILD.md).

## Named and anonymous variants

Both variants are generated from the same selected content. Named samples may link to the current hosted sample, paper, replication package, Makefile, and build script. Anonymous samples omit identifying names and repository URLs. Writing-sample acknowledgments replace the configured personal names with the configured anonymous label, and all anonymous PDFs are checked against the identifying strings declared in `samples.yml`.

Define identity changes once in the manifest or rendering helpers.

## Writing-sample numbering and references

Writing excerpts preserve the current full paper's section, table, figure, and equation numbering. File-size-limited variants selectively use a tracked PNG counterpart only when it is smaller than the paper's PDF figure; captions, numbering, and text are unchanged, and each declared byte limit is enforced after rendering. The ordinary page-constrained and full writing samples retain the paper's publication-quality PDF figures. A writing-sample specification may also retain selected paper figures outside the included sections; those figures keep the paper's caption and number. References to omitted material use the current full-paper labels. The writing samples also retain the paper's acknowledgments; the replication-package link in the paper title footnote remains exclusive to the full paper. This keeps excerpts synchronized when the paper structure changes.

The low-level LaTeX/reference extraction is an implementation detail of the renderer; maintainers normally change section selections only in `samples.yml`.

## Code-sample outputs

Selected code excerpts may attach tables or figures from the paper listed under `coding_outputs`. Each attached result declares its generated file or files, the paper cross-reference label used to preserve the full paper's numbering and caption, and any separate files that format the displayed result. The sample prints these as `Output:` or `Outputs:` and, when applicable, `Rendering code:` before reproducing the result. Named samples link those paths to the repository; anonymous samples show only the paths. Figures are copied from the corresponding `paper.qmd` figure blocks, and tables include the generated LaTeX used by the paper. The file-size-limited code sample uses the same excerpts and substitutes a smaller PNG only when a figure's PNG counterpart is smaller than its PDF; the ordinary code sample retains the paper's PDF figures.

## Page-count policy

Writing-sample target lengths are checked after rendering. During the typography pass, a mismatch produces a visible warning. File-size limits declared in the manifest are hard requirements and stop the build when exceeded.

## Publication

GitHub Pages publishes the tracked application PDFs from this repository. `scripts/stage_pages.sh` owns the static-site layout, and the Pages job does not rerun the restricted-data analyses. Historical named code-sample URLs remain compatibility aliases for the current code sample so links in previously submitted applications continue to resolve without retaining duplicate generated PDFs in the repository. Other missing Pages paths use the site's custom 404 page to send browser users back to the project landing page. Anonymous files are not published by the current staging policy.

## Maintenance

- Change writing/code selections in `samples.yml`.
- Add or remove code markers in the active R implementation that the sample should display.
- Keep selected paper/table labels stable through ordinary Quarto IDs.
- Test anonymity, marker uniqueness, numbering behavior, and output existence as behavioral invariants; avoid tests that freeze explanatory prose.
- Review generated PDFs after changes to selections, typography, or paper numbering.
