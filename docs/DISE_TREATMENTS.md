# DISE/UDISE treatment extensions

The extended diagnostic pipeline uses archived NIEPA/NUEPA DISE district report-card raw data as an independent administrative measurement of the district English-medium schooling environment.

## Source contract

`data/metadata/dise_archive_registry.csv` inventories the archived district raw files and report-card PDFs. The public pipeline does not require these files; `--with-extended-diagnostics` does. Raw archival files remain local under `data/raw/dise_internet_archive/` and are not redistributed by the repository.

The raw 2005-06 through 2007-08 workbooks store five ordered medium-of-instruction enrollment slots but do not name the languages in the machine-readable field names. The corresponding published district report cards do name those ordered slots. `data/metadata/dise_medium_slot_crosswalk.csv` therefore records the report-derived district-year slot identities with PDF/page provenance. The pipeline never assumes that slot 1 is English nationally.

The 2005-06 workbook also contains a duplicated machine-header block: the third ordered medium block is mislabeled as a second `enr_med2_*` block. The reader repairs medium names from their ordered column blocks before applying ordinary uniqueness repair, so the duplicated header cannot silently erase the third medium. This is a source-schema repair only; the report-card crosswalk still supplies the language identity.

Historical workbooks also contain a human-readable header row immediately above the machine-name row. The reader therefore selects the unique highest-scoring machine row rather than requiring only one row to contain district/state labels. This matters after normalizing aliases such as `State Code` to `statecd`: both human and machine rows can otherwise look superficially valid.

The 2010-11 enrollment workbook has three independent archival defects. First, `DRC_Raw_Data2010-11.xls` is named with a legacy `.xls` extension but has an OOXML/XLSX file signature. The materializer uses `readxl::format_from_signature()` and creates an extension-correct temporary view when the filename disagrees with the file bytes; the raw archive is never renamed or modified. Second, the machine-name row contains `statecd` and the enrollment field names while the district-code/name cells contain Kupwara data. The reader repairs only those key column names from the preceding human header row so later district rows remain machine-readable; it does not reconstruct or impute the lost Kupwara enrollment row. Third, the surviving enrollment rows are not reliably aligned to district identifiers: published report-card totals show large row-level swaps, including Mahe/Pondicherry-scale values attached to neighboring Puducherry district labels. For 2010-11 only, the production denominator therefore requires the published report-card current-year total (`Total Pr.` + `Total U.P.`) from the reviewed district page provenance. Raw grade, management, and direct totals remain attached as QA fields, including the report-to-raw ratio, but they never serve as analytical fallbacks in that year. If no reviewed report total matches a raw district, the 2010-11 denominator is left missing. Historical publication spellings such as `Chhatisgarh` and `Dadar & Nagar Haveli` are normalized through the shared state canonicalizer before attachment.

Key columns are recovered positionally from the local header block (the selected machine row plus preceding human-label rows). This supports the documented 2010-11 collision as well as later sheets that omit a state-code column, while leaving substantive variable names untouched.

The 2009-10 `DRC 2009-10.pdf` is a provisional 635-district combined report. Its pages overlapping the final Volume I reproduce the published numeric report-card values and it is retained as a fallback/validation source for the otherwise unavailable Volume II; final Volume I remains preferred wherever both exist.

The tracked 2008-09 through 2014-15 report-language CSV has an offline maintainer at `scripts/build_dise_report_language_enrollment.py`. The script deliberately treats the CSV's reviewed PDF/page fields as provenance rather than attempting to rediscover district pages heuristically. It validates every source PDF against `dise_archive_registry.csv`, extracts one registered page at a time with `pdftotext -layout`, supports the two documented medium-of-instruction table orientations, and refuses to infer a total when the page lacks an explicit total row/column. With no `--output` argument it verifies the rebuilt numeric counts against the tracked CSV; `--output` writes a candidate extraction for review. This maintainer is not part of `_targets.R`, so ordinary replication does not acquire a Poppler dependency.

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

Total elementary enrolment normally follows one hierarchy across workbook generations: the sum of grade-I-VIII enrolment fields is preferred when available, the workbook's direct `ENRTOT` field is used only when grade totals are unavailable, and the government-plus-private management total is the final fallback. The archived 2010-11 workbook is the documented exception because its district rows are misaligned across the numeric enrollment block. For that year, the published district report-card `Total Pr.` + `Total U.P.` count is an explicit higher-priority candidate. The reader retains the raw grade, direct, and management totals as source-QA fields; Census-2001 child-district aggregation sums every available count candidate first and then reapplies the hierarchy, so the reviewed 2010-11 publication total cannot be accidentally replaced by the corrupt raw grade sum after harmonization.

Parser-level quality-control fields such as `dise_medium_classification_ratio` are not required inputs to treatment construction. When present they are carried into the 2007-08 baseline treatment table for review, but the English-medium treatment and pooled baseline measure depend only on the validated language counts and elementary-enrolment denominator.

Language resolution is deliberately language-specific. If a report card explicitly identifies an English slot, English enrollment is usable even when some different positive medium slot remains unidentified. Conversely, English is coded as zero only when every positive slot is decoded and none is English. The same rule is applied separately to Hindi. `dise_medium_identity_complete` remains a stricter diagnostic describing whether every positive medium slot is known; it is not unnecessarily imposed on the English treatment.


## Longitudinal administrative EMI

For 2015-16, the shared workbook reader treats the selected machine-header row as the canonical column schema; downstream medium decoding therefore uses the normalized `m1`-`m5` and `enre*` names directly instead of rediscovering them through a second schema layer. Official UDISE codes identify Hindi as `04` and English as `19`. A slot is harmlessly absent only when both its medium identity and enrollment are absent. Any positive enrollment under an unidentified/zero-coded slot, or any identified positive medium code whose enrollment is unobserved, makes the district's language-specific counts unresolved.

The extended pipeline now constructs a 2005-06 through 2015-16 district-year EMI panel on Census-2001 geography. Baseline years use validated raw medium slots; 2008-09 through 2014-15 combine raw administrative enrollment denominators with report-card-derived English/Hindi counts; 2015-16 uses the workbook's explicit medium codes and enrollment blocks. The official UDISE coding convention identifies Hindi as 04 and English as 19.

Dynamic relevance is estimated only for distinct scalar excluded-distance variables. Instrument constructions that differ only by time-invariant included language controls are algebraically equivalent after district fixed effects and are represented once, with equivalent construction IDs retained as metadata. Five-share vector instruments remain outside this event-study diagnostic because they do not define one scalar distance trajectory.

The event-study reference year is 2007-08. Two models are reported: district FE plus academic-year FE, and the more demanding district FE plus state-by-academic-year FE. In both cases the coefficients are changes in the EMI-distance gradient relative to 2007-08, with inference clustered by Census-2001 district. The cluster vector is aligned directly to the complete-case model data rather than reconstructed from retained row names. A joint clustered Wald test evaluates whether all distance-by-year changes are zero; when `car::linearHypothesis()` cannot evaluate a model because nuisance fixed effects are aliased, the shared inference helper evaluates the same zero-restriction block directly from the clustered covariance matrix and returns `NA` only when that requested covariance block is itself rank deficient.

Later-year language counts are also checked against the independently read elementary-enrollment denominator before EMI is constructed. The same validity contract is reapplied after child-district counts are summed to Census-2001 geography, so harmonization cannot reintroduce an impossible share. A language count below zero, above the district total, or paired with a missing/nonpositive denominator is retained for source QA but is not treated as a resolved treatment count; the resulting EMI share is `NA` rather than capped or winsorized.

## School-quality mechanisms

The baseline 2005-06 and 2006-07 workbooks support three useful predetermined school-system diagnostics: pupils per teacher, the share of schools that are single-teacher schools, and the share of schools reporting a girls' toilet. These baseline counts are carried through the same deterministic Census-2001 lineage bridge as EMI and ratios are derived only after child-district counts have been summed. District percentages are never averaged across lineage changes.

The later raw School/Teacher sheets are not treated as a comparable mechanism panel. Inspection against the published report cards reveals district-row alignment failures (for example, 2008-09 Mahe's raw teacher total is attached to a Pondicherry-scale value) and cross-year field-definition discontinuities (including the 2009-10 girls'-toilet series). Later mechanism trajectories therefore use the published district report cards directly rather than repairing corrupted raw rows.

The tracked report reconstruction covers 2011-12 through 2014-15. It records the published all-school pupil-teacher ratio and single-teacher-school percentage for all four years. The girls'-toilet indicator is also extracted, but its published denominator changes after 2011-12: the later reports define it over girls'/coeducational schools rather than all schools. The longitudinal infrastructure specification therefore begins in 2012-13, while PTR and single-teacher-school trajectories use 2011-12 as their reference year. The report-card maintainer re-verifies the tracked numeric values from their registered PDF pages but remains outside the normal targets runtime graph.

These later publication values are already ratios, so they cannot be count-pooled across district splits without extra numerator/denominator information. The Census-2001 harmonizer consequently retains a report mechanism value only when exactly one deterministic later DISE district maps to the target in that academic year. Multiple later children are marked `multiple_source_districts_not_aggregated` and left missing rather than averaged. This is intentionally stricter than the count-based EMI harmonization.

Historical Teacher sheets still supply the 2005-06 and 2006-07 predetermined diagnostics. Those baseline counts are read through a separate mechanism target and merged only after the baseline EMI treatment reader completes, so mechanism availability cannot make the core baseline treatment target fail. Mechanism diagnostics use the preferred scalar linguistic-distance measure. Predetermined associations retain state fixed effects with state-clustered inference; later report-card trajectories use district fixed effects with either common academic-year effects or state-by-academic-year effects, with inference clustered by Census-2001 district. Later-only amenities such as electricity and computers remain excluded because they do not have a comparable predetermined baseline.

## Deferred extensions

A future HCES endpoint remains deferred to its separate feasibility/comparability phase. Historical school-level medium availability and language-taught-as-subject measures require school-level DISE/UDISE data rather than these district report-card aggregates.

## Elementary-age administrative exposure

Census C-13 single-year age returns provide the population denominator for a second administrative EMI treatment family. The pipeline sums completed ages 6 through 13, matching the eight cohorts covered by the statutory 6-14 elementary-age range, and uses total persons because the DISE numerator covers elementary schools regardless of rural/urban residence. The Census tables themselves are district-level single-year population counts for total, rural, and urban residence by sex.

The resulting construct is deliberately named a **gross enrollment ratio**, not a share of children. DISE reports enrollment in grades I-VIII rather than enrollment conditional on the pupil being age 6-13, so over-age and under-age pupils can appear in the numerator. Values above 100 are therefore possible and are retained rather than clipped. A true net age-specific EMI enrollment share would require pupil-level age-by-medium administrative data that these district report cards do not provide.

The 2001 C-13 denominator is already on the analytical Census-2001 geography. The 2011 denominator is harmonized backward only through the existing `district_transition_2001_2011` rows classified as `official_lgd_census_code_bridge`, `deterministic_containment`, or `reviewed_single_parent_ancestry` with complete population, area, and coverage flags. Reviewed one-parent ancestry is used for documented post-reference-date district creations such as Lower Dibang Valley, Jamtara, Simdega, and Ashoknagar. Valid official LGD Census-code bridges retain highest priority; reviewed ancestry fills sources without an LGD bridge and replaces an LGD row only when that row points outside the authoritative Census-2001 registry. The Census 2011 Andhra Pradesh Administrative Atlas supplies the reviewed 23-district 2001-to-2011 concordance used before tracing later single-parent Telangana descendants backward. The final transition is subject to a hard registry-validity gate. Multiple 2011 child districts wholly assigned to one 2001 parent are summed before any ratio is formed. A 2001 parent receives a 2011 anchor only when every 2011 source district contributing territory to that parent is itself deterministically assigned back to the same parent; this prevents a single deterministic child from being mistaken for the population of the whole pre-split district. Non-nested or incompletely covered 2011 districts are not split using generic population shares because district-level C-13 does not reveal the age composition of the territorial fragments.

For Census-2001 districts with both valid anchors, the annual ages-6-13 denominator follows constant compound growth between the two Censuses: the pipeline linearly interpolates log population from 2001 to 2011 and evaluates it at the midpoint of each academic year. Academic years after 2011 use the same district-specific 2001-2011 log growth rate as an explicit extrapolation. The output records whether each value is an interpolation or post-2011 extrapolation. Districts without a deterministic 2011 anchor remain missing rather than receiving a state-growth or fractional-boundary imputation.

Two baseline treatment variants enter the same IV-permutation machinery as the existing DISE treatments: the 2007-08 English-medium gross enrollment ratio and a pooled 2005-06 to 2007-08 person-year ratio. The pooled measure sums English enrollment and projected ages-6-13 population over the three academic years before dividing. The longitudinal event-study suite also reruns the scalar linguistic-distance-by-year specifications using the annual gross English-medium enrollment ratio as the outcome.

## Census-2001 geographic harmonization

Baseline DISE counts are harmonized to the same Census-2001 district units used by the main analysis before EMI shares are constructed. The bridge reuses reviewed district-lineage evidence rather than maintaining a DISE-specific district history.

A DISE district is eligible when its canonical state/district identity either matches a Census-2001 district directly or appears in the reviewed NSS lineage with a deterministic weight-one mapping to exactly one Census-2001 target. Conflicting candidate targets and mappings that require fractional allocation remain unresolved. Population-allocation weights are never applied to school counts.

The bridge treats an empty candidate source as an ordinary no-evidence condition: an empty Census registry slice or an empty deterministic reviewed-lineage slice yields a typed zero-row candidate table, not an error. A DISE identity with no surviving candidate is explicitly labeled `unresolved_no_deterministic_lineage`.

When multiple DISE districts deterministically map to the same Census-2001 parent, the pipeline sums English enrollment and total enrollment first and recomputes EMI from those pooled counts. It never averages child-district percentages. The pooled 2005-06 to 2007-08 treatment is then constructed from the harmonized annual counts and still requires all three academic years.

The extended DISE output writes both `dise_lineage_bridge.csv` and `dise_district_year_2001.csv` so the geographic recovery is reviewable independently of the regression results.

### Diagnostic cache isolation

The expensive alternative-distance and DISE IV diagnostics are projected onto
minimal analysis panels before estimation and are executed as `{targets}`
dynamic branches (alternative-distance specification by specification and DISE
construct by construct). Alternative-distance diagnostics use two projections:
a common-support-filtered first-stage view and a row-preserving augmentation
view that retains the welfare outcome needed by weak-IV diagnostics. Unrelated
columns added to the main district panel, including future HCES outcomes, are
excluded from both projections, so downstream diagnostic hashes remain stable
when those unrelated columns change. The sequential diagnostic wrapper
functions remain available for tests and direct use; the targets graph schedules
the same estimators at finer granularity.
