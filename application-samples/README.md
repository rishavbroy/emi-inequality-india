# Application samples

Application samples are generated from the current paper and research code. [`samples.yml`](samples.yml) is the single configuration file for sample content, identity variants, paper/replication-package links, and selected code outputs.

## Outputs

The build writes named and anonymous variants under [`output/`](output/):

- writing samples targeting 5, 10, and 15 pages, file-size-limited copies, and the full paper;
- short and long coding samples.

Generated PDFs are the maintained application files. Intermediate Markdown and LaTeX files are temporary build products.

## Content selection

Writing samples select ordinary Quarto section IDs from `paper/paper.qmd`, so the excerpts are generated directly from the paper.

Coding samples use paired `sample-start:` / `sample-end:` comments in active R and Python files. Marker IDs are declared in `samples.yml`, and validation requires each selected marker pair to be unique and well formed.

Each coding-sample description and each selected code comment should make sense to a technical reader who has not read the paper or the rest of the replication package. Spell out project-specific abbreviations at first use, explain the empirical quantity or data irregularity before implementation details, and reserve comments for methodological choices or data assumptions that the code does not make obvious.

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

## Coding-sample outputs

Selected code excerpts may attach tables or figures from the paper listed under `coding_outputs`. Results appear immediately after the excerpt they illustrate. Selected tables and figures declare their paper cross-reference labels and retain the full paper's numbering and captions; figures are copied from the corresponding `paper.qmd` figure blocks rather than described again in the sample manifest. If an output combines results from code outside the displayed excerpt, `code_files` lists those files and the named sample links to them in the repository. Citations in named coding samples link to the full paper, while anonymous samples retain the citation text without the identifying URL. Output composition does not re-estimate results independently of the main analysis.

## Page-count policy

Writing-sample target lengths are checked after rendering. During the typography pass, a mismatch produces a visible warning. File-size limits declared in the manifest are hard requirements and stop the build when exceeded.

## Publication

GitHub Pages publishes the tracked application PDFs from this repository. The Pages job copies committed files and does not rerun the restricted-data analyses. Anonymous files are published only when that is explicitly intended by the Pages configuration and review policy.

## Maintenance

- Change writing/code selections in `samples.yml`.
- Add or remove code markers in the active R implementation that the sample should display.
- Keep selected paper/table labels stable through ordinary Quarto IDs.
- Test anonymity, marker uniqueness, numbering behavior, and output existence as behavioral invariants; avoid tests that freeze explanatory prose.
- Review generated PDFs after changes to selections, typography, or paper numbering.
