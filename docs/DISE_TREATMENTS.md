# DISE/UDISE treatment extensions

The extended diagnostic pipeline uses archived NIEPA/NUEPA DISE district report-card raw data as an independent administrative measurement of the district English-medium schooling environment.

## Source contract

`data/metadata/dise_archive_registry.csv` inventories the archived district raw files and report-card PDFs. The public pipeline does not require these files; `--with-extended-diagnostics` does. Raw archival files remain local under `data/raw/dise_internet_archive/` and are not redistributed by the repository.

The raw 2005-06 through 2007-08 workbooks store five ordered medium-of-instruction enrollment slots but do not name the languages in the machine-readable field names. The corresponding published district report cards do name those ordered slots. `data/metadata/dise_medium_slot_crosswalk.csv` therefore records the report-derived district-year slot identities with PDF/page provenance. The pipeline never assumes that slot 1 is English nationally.

The 2009-10 `DRC 2009-10.pdf` is a provisional 635-district combined report. Its pages overlapping the final Volume I reproduce the published numeric report-card values and it is retained as a fallback/validation source for the otherwise unavailable Volume II; final Volume I remains preferred wherever both exist.

## Baseline administrative measures

The first implementation deliberately limits treatment construction to 2005-06 through 2007-08, the baseline period required to diagnose the current IV relevance problem. It constructs:

- 2007-08 English-medium enrollment / total elementary enrollment;
- 2007-08 English-medium enrollment / enrollment with a reported medium;
- pooled 2005-06 to 2007-08 analogues, requiring all three years;
- 2007-08 Hindi-medium enrollment share;
- English share among English + Hindi enrollment;
- private enrollment share;
- private-school share.

The first four English-medium measures are genuine alternative treatment definitions and enter the full structural-IV diagnostic specification universe. Hindi, English-versus-Hindi, and private-sector shares are relevance/mechanism outcomes: they enter the same first-stage specification permutations but are not mislabeled as causal EMI treatments.

The extended diagnostic output also compares the 2007-08 administrative EMI shares with the NSS district measures on the common 0-100 scale, reporting raw Pearson/Spearman agreement, mean difference, RMSE, and correlation after residualizing both measurements by 2001 state.

All shares are constructed from counts and stored on the repository's established 0-100 percentage scale, matching the NSS EMI measures. Multi-year EMI is the ratio of pooled English enrollment to pooled denominator, never an unweighted average of annual shares.

## Medium reporting and missingness

DISE publications warn that classificatory totals can differ because schools do not always answer every item. Accordingly, the pipeline preserves both total-enrollment and medium-reported denominators and records the medium-reporting share. English/Hindi measures are `NA` when a positive medium slot lacks a validated report-card identity; unknown language identity is never silently treated as zero English enrollment.

## Geography

The 2007-08 administrative measures attach to the active analysis panel using the reviewed 2007 district names already carried by the lineage panel. The pooled 2005-08 measure is restricted to canonical district names observed consistently in all three annual files. This is intentionally conservative: non-nested pre-2007 district changes are not allocated from district aggregates without evidence about within-district enrollment locations.

## Deferred extensions

Later DISE years remain inventoried for a district-year dynamic treatment panel, teacher/school-quality mechanisms, and a future HCES endpoint. An all-school-age DISE EMI exposure is also deferred until a defensible age-specific Census denominator is added. Historical school-level medium availability and language-taught-as-subject measures require school-level DISE/UDISE data rather than these district report-card aggregates.
