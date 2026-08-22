# DISE/UDISE treatment extensions

The extended diagnostic pipeline uses archived NIEPA/NUEPA DISE district report-card raw data as an independent administrative measurement of the district English-medium schooling environment.

## Source contract

`data/metadata/dise_archive_registry.csv` inventories the archived district raw files and report-card PDFs. The public pipeline does not require these files; `--with-extended-diagnostics` does. Raw archival files remain local under `data/raw/dise_internet_archive/` and are not redistributed by the repository.

The raw 2005-06 through 2007-08 workbooks store five ordered medium-of-instruction enrollment slots but do not name the languages in the machine-readable field names. The corresponding published district report cards do name those ordered slots. `data/metadata/dise_medium_slot_crosswalk.csv` therefore records the report-derived district-year slot identities with PDF/page provenance. The pipeline never assumes that slot 1 is English nationally.

The 2005-06 workbook also contains a duplicated machine-header block: the third ordered medium block is mislabeled as a second `enr_med2_*` block. The reader repairs medium names from their ordered column blocks before applying ordinary uniqueness repair, so the duplicated header cannot silently erase the third medium. This is a source-schema repair only; the report-card crosswalk still supplies the language identity.

Historical workbooks also contain a human-readable header row immediately above the machine-name row. The reader therefore selects the unique highest-scoring machine row rather than requiring only one row to contain district/state labels. This matters after normalizing aliases such as `State Code` to `statecd`: both human and machine rows can otherwise look superficially valid.

The 2010-11 enrollment workbook has the documented Kupwara header/data collision: the machine-name row contains `statecd` and the enrollment field names, while the district-code/name cells contain Kupwara data. The reader repairs only those key column names from the preceding human header row so later district rows remain machine-readable; it does not reconstruct or impute the lost Kupwara enrollment row.

The 2009-10 `DRC 2009-10.pdf` is a provisional 635-district combined report. Its pages overlapping the final Volume I reproduce the published numeric report-card values and it is retained as a fallback/validation source for the otherwise unavailable Volume II; final Volume I remains preferred wherever both exist.

## Baseline administrative measures

The first implementation deliberately limits treatment construction to 2005-06 through 2007-08, the baseline period required to diagnose the current IV relevance problem. It constructs:

- 2007-08 English-medium enrollment / total elementary enrollment;
- the pooled 2005-06 to 2007-08 analogue, requiring all three years;
- 2007-08 Hindi-medium enrollment share;
- English share among English + Hindi enrollment;
- private enrollment share;
- private-school share.

The two English-medium measures are genuine alternative treatment definitions and enter the full structural-IV diagnostic specification universe. Hindi, English-versus-Hindi, and private-sector shares are relevance/mechanism outcomes: they enter the same first-stage specification permutations but are not mislabeled as causal EMI treatments.

The extended diagnostic output also compares the 2007-08 administrative EMI share with the NSS district measures on the common 0-100 scale, reporting raw Pearson/Spearman agreement, mean difference, RMSE, and correlation after residualizing both measurements by 2001 state.

The DISE diagnostic saver follows the repository-wide diagnostic-manifest convention: it returns a data-frame manifest of written outputs rather than pretending that manifest itself is a `targets` file target. The individual CSVs are still written explicitly and are discoverable by the analysis-note helpers.

All shares are constructed from counts and stored on the repository's established 0-100 percentage scale, matching the NSS EMI measures. Multi-year EMI is the ratio of pooled English enrollment to pooled denominator, never an unweighted average of annual shares.

## Medium classification and missingness

The district report cards explicitly warn that classificatory totals, including enrolment by medium of instruction, may not match district totals because item response is incomplete and some inconsistencies remain unresolved. The archived raw data confirm that warning: summed medium-classification counts can be below or above independently reported total enrolment, with a small number of extreme discrepancies.

For that reason, the sum of the five medium slots is retained only as a quality-control quantity (`dise_medium_classified_enrollment`). `dise_medium_classification_ratio` compares it with total grade-I-VIII enrolment, but it is not a response-rate estimate and is never used as an alternative treatment denominator. The former reported-medium-denominator EMI variants were removed rather than stabilized with an arbitrary trimming rule.

Total elementary enrolment is taken from the direct grade-I-VIII totals when available, with the government-plus-private school-category total retained as an independent cross-check. `dise_management_enrollment_difference` exposes any disagreement between those two denominator representations.

Parser-level quality-control fields such as `dise_medium_classification_ratio` are not required inputs to treatment construction. When present they are carried into the 2007-08 baseline treatment table for review, but the English-medium treatment and pooled baseline measure depend only on the validated language counts and elementary-enrolment denominator.

Language resolution is deliberately language-specific. If a report card explicitly identifies an English slot, English enrollment is usable even when some different positive medium slot remains unidentified. Conversely, English is coded as zero only when every positive slot is decoded and none is English. The same rule is applied separately to Hindi. `dise_medium_identity_complete` remains a stricter diagnostic describing whether every positive medium slot is known; it is not unnecessarily imposed on the English treatment.


## Longitudinal administrative EMI

The extended pipeline now constructs a 2005-06 through 2015-16 district-year EMI panel on Census-2001 geography. Baseline years use validated raw medium slots; 2008-09 through 2014-15 combine raw administrative enrollment denominators with report-card-derived English/Hindi counts; 2015-16 uses the workbook's explicit medium codes and enrollment blocks. The official UDISE coding convention identifies Hindi as 04 and English as 19.

Dynamic relevance is estimated only for distinct scalar excluded-distance variables. Instrument constructions that differ only by time-invariant included language controls are algebraically equivalent after district fixed effects and are represented once, with equivalent construction IDs retained as metadata. Five-share vector instruments remain outside this event-study diagnostic because they do not define one scalar distance trajectory.

The event-study reference year is 2007-08. Two models are reported: district FE plus academic-year FE, and the more demanding district FE plus state-by-academic-year FE. In both cases the coefficients are changes in the EMI-distance gradient relative to 2007-08, with inference clustered by Census-2001 district. A joint clustered Wald test evaluates whether all distance-by-year changes are zero.

## Deferred extensions

Teacher/school-quality mechanisms and a future HCES endpoint remain deferred. An all-school-age DISE EMI exposure is also deferred until a defensible age-specific Census denominator is added. Historical school-level medium availability and language-taught-as-subject measures require school-level DISE/UDISE data rather than these district report-card aggregates.

## Census-2001 geographic harmonization

Baseline DISE counts are harmonized to the same Census-2001 district units used by the main analysis before EMI shares are constructed. The bridge reuses reviewed district-lineage evidence rather than maintaining a DISE-specific district history.

A DISE district is eligible when its canonical state/district identity either matches a Census-2001 district directly or appears in the reviewed NSS lineage with a deterministic weight-one mapping to exactly one Census-2001 target. Conflicting candidate targets and mappings that require fractional allocation remain unresolved. Population-allocation weights are never applied to school counts.

The bridge treats an empty candidate source as an ordinary no-evidence condition: an empty Census registry slice or an empty deterministic reviewed-lineage slice yields a typed zero-row candidate table, not an error. A DISE identity with no surviving candidate is explicitly labeled `unresolved_no_deterministic_lineage`.

When multiple DISE districts deterministically map to the same Census-2001 parent, the pipeline sums English enrollment and total enrollment first and recomputes EMI from those pooled counts. It never averages child-district percentages. The pooled 2005-06 to 2007-08 treatment is then constructed from the harmonized annual counts and still requires all three academic years.

The extended DISE output writes both `dise_lineage_bridge.csv` and `dise_district_year_2001.csv` so the geographic recovery is reviewable independently of the regression results.
