# Linguistic-distance and EMI constructions

## Public specification

The pipeline preserves the full mutually exclusive Census 2001 C-16 mother-tongue distribution. C-16 language-group rows whose codes end in `000` are subtotals and are not observations in the analytical distribution. Their labels are carried to child rows as contextual Census group metadata, but distance assignment is mother-tongue-first. This distinction is essential for the Census `Hindi` group, which contains many distinct mother tongues: a Bhojpuri, Rajasthani, or other non-Hindi leaf never inherits Hindi's zero distance merely because its parent Census group is `Hindi`.

`data/metadata/shastry_language_distance.csv` is the maintained published-category concordance. A mother tongue is matched to that concordance first. Its broader Census language group is used only as a fallback when doing so cannot turn a distinct leaf inside the Census Hindi/Urdu umbrellas into a false zero-distance observation. The concordance records the explicit zero-to-four categories shown by Shastry, treats native Hindi and Urdu as the zero-distance reference, assigns degree five only to supported non-Indo-European classifications, and leaves unsupported leaves unmapped. Unmapped speaker mass remains in share denominators and is reported; it is never silently assigned to degree five or renormalized away.

District constructions include:

- `ling_distance_nonzero_mean`: speaker-weighted mean distance among mapped speakers with distance above zero;
- `ling_share_distance_0` through `ling_share_distance_5`;
- `ling_share_distance_ge3`;
- `hindi_share`, `urdu_share`, `hindi_urdu_share`, and `native_english_share`;
- mapped and unresolved non-English speaker coverage, with native English speakers reported separately;
- `ling_distance_top3_legacy` and its retained-speaker coverage for descriptive comparison.

The public scalar instrument is `ling_distance_nonzero_mean`, the speaker-weighted mean Shastry distance among mapped speakers with positive distance from Hindi. The five nonzero distance shares remain an extended diagnostic set, with distance zero omitted as the compositional reference. `wavg_ling_degrees` is retained only as a compatibility alias for the top-three legacy construction used in historical comparisons.

Native English speakers are an intentional special category rather than unresolved language mass. They do not enter the numerator or denominator of Shastry-style weighted-distance means, even if a genealogical distance is available from Glottolog, and their district share is reported separately for composition adjustment. Hindi and Urdu remain the zero-distance reference categories.

## Census 2001 C-17 mechanism source

The 35 state/UT C-17 workbooks are now ingested only as a validated mechanism source; no C-17 regression enters the public paper yet. C-17 is hierarchical rather than a flat language table. For each state × native-language × sex cell, the native-speaker total is the outer denominator, first-subsidiary-language counts partition multilingual speakers, and second-subsidiary-language counts are subsets of their first-subsidiary parent. The reader therefore defines multilingual speakers as the sum of first-subsidiary counts. English acquisition among multilingual speakers is the first-subsidiary English count plus second-subsidiary English counts whose first subsidiary is not English, divided by that multilingual denominator. The analogous Hindi measure uses the same rule. Trilingual counts are never added to the denominator a second time.

The ingestion contract enforces `0 <= English/Hindi acquisition <= multilingual <= native speakers`, requires every second-subsidiary subtotal to fit within its first-subsidiary parent, checks Persons = Males + Females wherever all three counts are reported, and requires deterministic language-code labels. As an independent cross-table check, C-17 native-speaker totals must reconcile exactly to the corresponding C-16 state language-group totals before the extended target succeeds. C-17 will reuse the same language-identity and Shastry-distance resolver as C-16; it will not acquire a parallel language crosswalk.

C-17, NSS schooling, DISE administration, and district welfare have different observational units. Their eventual juxtaposition is mechanism evidence, not a sequential mediation design and not evidence that the same individuals are followed across sources.

## Glottolog 5.3 source layer

The versioned Glottolog 5.3 source bundle is validated before downstream language-crosswalk work. The direct `languoid.csv` parent graph is the canonical genealogy source because it supplies stable Glottocodes, parent IDs, family IDs, and languoid levels without requiring a Newick parser. The CLDF archive supplies `languages.csv` and `names.csv` for primary and alternative names; `languages_and_dialects_geo.csv` is disambiguation-only, and `tree_glottolog_newick.txt` is an independent representation for later validation. The pipeline anchors Hindi at Glottocode `hind1269`/ISO `hin`, rejects missing parents or parent cycles, resolves dialects to language-level nodes, and defines cross-family robustness distance through one synthetic super-root. No Glottolog taxonomy row automatically overrides the maintained Shastry/Jasanoff concordance.

Extended diagnostics generate `census_glottolog_match_candidates.csv`, a non-authoritative review queue keyed to Census mother-tongue leaf identities. Candidate generation uses an explicit evidence hierarchy: full mother-tongue primary names, mother-tongue components, mother-tongue aliases, and only then broader Census-group names/aliases. Only candidates from the strongest nonempty tier survive. It does not use fuzzy similarity and never promotes a candidate into production automatically. Ambiguous candidates within the same strongest tier remain ambiguous, and every candidate remains `unreviewed` until a separate reviewed metadata decision is made. National speaker mass and district coverage are attached to the queue so manual adjudication can prioritize consequential languages rather than alphabetical convenience.

## Education exposure

All district education-exposure margins are derived from the same weighted NSS child universe and common sufficient statistics. The primary age range is 5–19; 6–17 and 6–14 are supplementary constructions.

- `enrollment_rate_0708`: enrolled children divided by all age-eligible children;
- `emi_share_enrolled_0708`: English-medium children divided by enrolled children with known medium;
- `emi_exposure_all_children_0708`: English-medium enrolled children divided by all age-eligible children;
- `unknown_medium_share_enrolled_0708`: enrolled children whose medium is unavailable;
- `private_share_enrolled_0708`: private aided or unaided enrollment among children with known institution management;
- `emi_share_enrolled_public_0708` and `emi_share_enrolled_private_0708`: English-medium shares within public and private enrolled children whose medium is observed;
- `emi_share_enrolled_private_aided_0708` and `emi_share_enrolled_private_unaided_0708`: finer private-sector sensitivity measures;
- four all-child public/private × EMI/non-EMI exposure cells, plus `unknown_school_classification_share_all_children_0708`.

NSS-64 Schedule 25.2 Block 5 codes institution type as government (1), local body (2), private aided (3), private unaided (4), and not known (5). The mechanism decomposition groups codes 1--2 as public and 3--4 as private. Code 5 and missing institution type remain unknown; they are never silently assigned to public schooling. Medium and management missingness are likewise retained as explicit classification mass. The four observed management × medium cells therefore satisfy the accounting identity

`enrollment rate = public EMI + public non-EMI + private EMI + private non-EMI + unknown classification`,

all expressed as shares of the same eligible-child population. These are realized household-side enrollment choices, not structural demand or school-supply measures.

The public treatment is `emi_exposure_all_children_0708`, the survey-weighted share of children ages 5-19 who are both enrolled and observed in English-medium instruction. The historical field `EMIE` remains only for compatibility with legacy comparisons and measures EMI among enrolled children. The exact decomposition into enrollment and the intensive EMI margin is validated only when medium is fully observed; unknown medium is reported rather than classified as non-English.

## Alternative first-stage specifications

Extended diagnostics estimate the preferred all-child EMI treatment against every
Distance constructions on one common district support. The registry covers
the nonzero weighted mean, the share at distance three or higher, the legacy
top-three mean, scalar specifications with combined or separate Hindi and Urdu
composition controls, a Shastry-adjusted scalar specification that also controls
for native-English share, and a joint first stage for the five nonzero distance shares.
Distance zero is the omitted compositional reference in the joint specification.

Each construction is estimated unadjusted and with six-region or state fixed effects
combined with the main and expanded Census control sets. The absorption ladder also
repeats every sequential thematic control block under six-region fixed effects, so
the human-capital step can be compared directly with the preceding specification.
All coefficient standard errors and joint excluded-instrument tests cluster by
Census 2001 state. These remain diagnostic specifications and do not change the
public IV model.

## Mapping coverage and Shastry comparability

`ling_mapped_speaker_share` is the percentage of a district's mutually exclusive C-16 speaker mass whose mother-tongue leaf can be assigned to one of Shastry's degree categories under the mother-tongue-first resolver. It is a classification-coverage measure, not Census response coverage and not district-panel match coverage.

The project preserves unmapped mass rather than silently assigning it a degree. It now reports three complementary nonlinear specifications:

- all-speaker distance shares, with unmapped mass left visible;
- all-speaker distance shares with unresolved non-English and native-English shares included as controls, so distance zero is the omitted mapped category;
- mapped-speaker shares renormalized to sum to 100, used only as a sensitivity because renormalization hides unmapped mass.

Results are repeated with minimum mapped-speaker shares of 0, 90, 95, and 99 percent. Distance-four and unmapped-language diagnostics are decomposed by mother-tongue leaf, with the parent Census language group retained as context.

Shastry used the 1991 Census classification of 114 languages, assigned all non-Indo-European languages to degree five, and assigned unlisted Indo-European languages the value of the closest language on a language tree. She explicitly preferred 1991 to 1961 because the latter listed 1,652 languages that were difficult to classify. The present project uses the more detailed Census 2001 C-16 hierarchy but only the published degree categories from Shastry's article; consequently, its conservative concordance leaves some detailed categories unmapped instead of reconstructing her unpublished closest-language-tree assignments.

The exploratory outcome output reports conventional clustered 2SLS alongside the reduced-form joint test and an Anderson-Rubin test of a zero treatment effect. Because the scalar state-fixed-effect first stages and the five-share joint first stages are weak, the conventional 2SLS coefficient is not treated as decisive.


## Reviewed Glottolog crosswalk and alternative distance basis

`data/metadata/census_language_glottolog_crosswalk.csv` is the production identity layer for the Glottolog robustness basis. It contains one row per Census mother-tongue code. A deterministic row is accepted only when the strongest candidate is a unique Glottolog primary-name match, the candidate is documented for India, and the full ancestry is non-bookkeeping. Ambiguous, alias-only, generic, foreign-only, or bookkeeping-derived candidates remain unresolved unless a reviewed manual row documents the disambiguation.

The district robustness variable `ling_distance_glottolog_nonhindi_mean` is an unweighted Glottolog-5.3 edge-distance construction, aggregated with Census speaker weights. Native Hindi, Urdu, and English are reference/special categories and do not enter this weighted-mean denominator. `ling_glottolog_mapped_speaker_share` measures coverage among the remaining distance-bearing speaker mass. The Glottolog measure is never rescaled to the Shastry 0–5 scale and is never described as the historical Ethnologue node count.

The canonical IV registry now carries a construction-specific mapping-coverage variable. Existing Shastry specifications use `ling_mapped_speaker_share`; Glottolog specifications use `ling_glottolog_mapped_speaker_share`. The same first-stage, weak-IV, balance, overidentification, and monotonicity machinery therefore operates on both distance bases without a parallel estimation implementation.


For the Shastry 0–5 basis, accepted Glottolog identities outside Indo-European receive degree five directly because this is Shastry's stated family-level convention. Indo-European extensions no longer use nearest Glottolog nodes. `shastry_extension_candidates.csv` is now an evidence queue for review against Shastry's Figure 6, the Historical Linguistic Survey of India, and lexical evidence. It reports whether the later Dediu Ethnologue export contains an exact label plus direct Dyen/Kogan evidence where available, but leaves the degree unset until the historical classification is adjudicated.

## Historical Ethnologue and lexical review sources

Shastry's 2008 working paper is the primary construction documentation for the two historical robustness measures. Section 4.2 states that Figure 6 is an Ethnologue extract, defines tree distance as the number of nodes between languages, reports Punjabi as five nodes and Bengali as seven nodes from Hindi, and uses Dyen et al. cognate judgments as the second independent measure. It also states that doubtful cognations do not determine the reported Dyen percentages indirectly: reproducing the source matrix requires excluding doubtful judgments from the cognate/non-cognate denominator. The raw Dyen file itself contains 200 meanings even though the working-paper prose refers to 210 core words; the project follows the archived data and verifies the published percentages rather than altering the source.

`data/raw/ethnologue/ethnologue-newick-proportional=1.00.csv` is a Dediu `lgfam-newick` output. Its `.csv` suffix is misleading: the repository documents these outputs as tab-separated files whose fourth field is a Newick tree, which is why GitHub's CSV preview reports an apparent column-count error. This export was produced later than Shastry's working paper and is therefore a machine-readable review proxy, not the authoritative 2008 topology. It may support identity and branch review, but it does not automatically assign a Shastry degree.

`data/raw/cognates/dyen_kruskal_black_1997.txt` is parsed from its documented fixed-width card representation. The file contains a table of contents, prose, examples, and a table of lists before the real observations. Because the table of contents itself repeats the title `5. THE DATA`, the reader starts after the final exact occurrence of that section title and then recognizes the documented `a` header, `b` subheader, `c` relationship, and blank-leading form record layouts. This prevents both the contents entry and prose beginning with `b` or `c` from being interpreted as observations. The source contract then requires 200 meanings, 95 speech-variety lists, and 19,000 meaning-list form records. Pairwise percentages are `cognate / (cognate + not cognate)`; missing and doubtful judgments are excluded. The parser is required to reproduce Shastry's reported percentages with Hindi after rounding to one decimal: Bengali 64.1, English 14.6, Gujarati 64.6, Kashmiri 42.4, Marathi 56.4, Nepali 64.2, and Punjabi 74.5.

Kogan (2017) is retained as a separate 100-item New Indo-Aryan lexical source. `data/metadata/lexical_language_index.csv` records the language codes represented in its appendix and the direct Dyen aliases needed for review. Kogan values are not merged numerically with Dyen because the list length, source dictionaries, and cognacy system differ. The next adjudication layer should use Kogan and ASJP only as tie-breaking/validation evidence after Figure 6 and LSI classification have been recorded.


## Dyen/Shastry cognate-distance robustness basis

The pipeline now implements Shastry's second historical robustness construction separately from both the 0--5 expert scale and Glottolog. For a language observed in the Dyen et al. database, the language-level distance is `100 - percent cognates with Hindi`. Shastry's working-paper footnote 20 states that languages outside that Indo-European cognate database are assumed to have 5 percent of words in common with Hindi, so accepted non-Indo-European identities receive distance 95. Indo-European mother tongues that do not have a direct Dyen mapping remain unresolved rather than borrowing Kogan, ASJP, or a modern tree value.

At district level, `ling_distance_dyen_noncognate_pct` is the Census-speaker-weighted mean among mapped non-Hindi/Urdu, non-English mother tongues. `ling_dyen_mapped_speaker_share` and `ling_dyen_unmapped_speaker_share` use the same nonreference population as their denominator. This construction has its own registry entries and coverage sensitivity, so it is evaluated by the same first-stage, weak-IV, Anderson--Rubin, balance, and monotonicity machinery as the Shastry 0--5 and Glottolog bases.

Kogan (2017) remains review/tie-breaking evidence and is not numerically spliced into this Dyen variable. ASJP likewise remains a separate possible phonetic-distance diagnostic rather than a cognate-percentage substitute.


## Reviewed Shastry/Jasanoff adjudication ledger

`data/metadata/shastry_language_adjudications.csv` is the production review layer for identifiable Indo-European C-16 leaves that are absent from the published 0--5 concordance. Accepted rows require a mother-tongue code, an integer degree, a named Shastry anchor, an LSI classification, volume/year/page information, a stable source URL, an evidence summary, decision basis, and confidence. Rows whose historical classification is clear but whose corresponding Shastry anchor is not are retained as `review_required` with explicit sensitivity degrees and do not enter the preferred construction.

The first accepted tranche deliberately covers cases where the historical classification is strong and agrees with the historical-Ethnologue review proxy: Bhojpuri, Magahi and Sadri inherit the published Bihari degree; Marwari, Malvi, Mewari, Mewati and Harauti inherit the published Rajasthani degree; Haryanvi, Bundeli and Khari Boli inherit the Hindi/Western-Hindi anchor. Eastern-Hindi varieties (including Awadhi, Bagheli and Chhattisgarhi), the Bhil transition complex, and Nimadi remain unresolved where the evidence does not identify one unique Figure-5/Table-1 anchor.

`resolve_shastry_language_degrees()` is the single production resolver. The district builder, decomposition diagnostics, and extension review queue all call it, so accepted manual decisions cannot silently diverge across downstream analyses.


### Reviewed Indo-European adjudication ledger

`data/metadata/shastry_language_adjudications.csv` is the sole manual override layer for the preferred 0--5 basis. Accepted rows must record exact classification evidence; rows whose decision basis uses Kogan must additionally record the Kogan page, URL, and lexical comparison. The resolver applies published Shastry/Jasanoff scores first, accepted mother-tongue adjudications second, and Shastry's non-Indo-European degree-five rule last when a reviewed Glottolog family identity is available.

The second conservative tranche adds Khortha/Khotta and Nagpuria to the Bihari degree-three anchor; Dogri and Multani to the Punjabi degree-one anchor; Gojri to degree one because its historical Mewati/Rajasthani and Kogan Hindko/Punjabi classifications disagree genealogically but imply the same Shastry degree; and Banjari/Lamani/Lambadi to degree one because Kogan's lexical matrix places the shared Banjari/Lambadi list closest among published anchors to Rajasthani and Gujarati. Eastern Hindi (Awadhi, Bagheli, Chhattisgarhi), the Bhil transition complex, Nimadi, and broad/composite labels remain unresolved unless evidence distinguishes their adjacent plausible degrees.

The extension queue now contains only unresolved Indo-European rows. Non-Indo-European rows with accepted Glottolog identities are resolved centrally to degree five and therefore cannot simultaneously appear as production-mapped and diagnostic-unmapped.


## Frozen preferred 0--5 mapping and ASJP v21

The preferred Shastry/Jasanoff 0--5 mapping is now frozen. The adjudication ledger has only `accepted` and `frozen_unresolved` production states. A frozen unresolved row carries no degree and records why no unique published anchor is defensible. The extension queue excludes both states, so a nonempty queue now denotes a genuinely unreviewed accepted identity.

ASJP v21 is retained as the untouched Zenodo archive `data/raw/cognates/asjp-v21.zip`; the reader opens the archive's unique `raw/lists.txt` member directly. This keeps the downloaded raw artifact immutable and removes a manual extraction step. The review module uses the 40 Holman core meanings, excludes `%`-marked loans and `XXX` missing forms, retains at most two synonyms, and applies the ASJP minimum of 28 attested core items. LDND is the average normalized Levenshtein distance for same-meaning forms divided by the different-meaning baseline. `asjp_review_anchor_distances.csv` preserves candidate-to-published-anchor comparisons and `asjp_review_summary.csv` reports the nearest and runner-up anchors. ASJP is evidence only: it never enters the district IV and never fills Dyen observations.

The attached Kogan article is represented by `data/metadata/kogan_2017_anchor_similarity.csv`, a transcription of only the Table 1 candidate-to-published-anchor cells needed for adjudication. Production therefore does not parse a PDF and does not pretend the extracted table is a new lexical series.

The final mapping accepts direct Rajasthani classifications (Bagri, Dhundhari, Nimadi); uses concordant Kogan/ASJP evidence from Awadhi to anchor the LSI Eastern-Hindi branch to Hindi while retaining degree 1 as the adjacent branch sensitivity; and uses historical classification plus clear ASJP tie-breaks for Bhili, Bhilali, and Khandeshi/Ahirani. Pahari aggregates, close Hindi/Punjabi/Nepali ties, Wagdi, Shina, Sanskrit, Halabi without an exact historical adjudication, and identity-ambiguous labels are deliberately frozen unresolved. Literal 100-percent row coverage is not a target.


### Frozen-adjudication sensitivity scenarios

The preferred 0--5 construction remains the published Shastry/Jasanoff mapping
plus accepted reviewed adjudications; `frozen_unresolved` rows remain missing
there. The ledger's `sensitivity_degrees` field is now operational rather than
descriptive. Two deliberately joint stress tests are constructed:

- `ling_distance_nonzero_mean_sensitivity_low` assigns each reviewed ambiguous
  mother tongue the minimum finite value among its accepted primary degree (if
  any) and its recorded sensitivity degrees.
- `ling_distance_nonzero_mean_sensitivity_high` analogously assigns the maximum.

Frozen-unresolved rows therefore enter only these stress tests, never the
preferred instrument. The two scenarios use identical mapped support and are
registered in the same first-stage, weak-IV, Anderson--Rubin, monotonicity, and
coverage machinery as the preferred scalar. They are bounding stress tests, not
additional preferred linguistic-distance measures and not tuned to first-stage
strength.

An empty `shastry_extension_candidates.csv` now has a stable typed header rather
than a one-column empty CSV. This is the expected release state: every accepted
crosswalk identity is either assigned or explicitly frozen unresolved.

## Census 2001 C-17 English-acquisition mechanism

The extended diagnostics now use Census 2001 C-17 at the state × native-language
level to test the behavioral mechanism that motivates linguistic distance. This is
not a district IV and does not enter the production treatment or welfare model.
For each state-language cell, the outcome is the share of multilingual native
speakers who report English as either their first subsidiary language or, nested
under a non-English first subsidiary language, their second subsidiary language.
The multilingual denominator is the sum of first-subsidiary-language counts, so
trilingual speakers are counted once.

The preferred C-17 specification follows Shastry's language-acquisition equation as
closely as a single 2001 cross-section permits: state fixed effects, native-language
share in the state, an indicator for the state's modal native language, native-speaker
weights, a linear Shastry/Jasanoff distance term, and a separate distance-zero
indicator for the Hindi/Urdu nonlinearity. Flexible distance bins, a distant-language
indicator, Hindi acquisition, multilingualism, and male/female splits are retained as
a deliberately small diagnostic registry. All distance assignments use
`resolve_shastry_language_degrees()`; C-17 has no parallel language crosswalk.

Shastry clusters by state-language in her pooled 1961/1991 language regressions to
allow serial correlation within a local ethnic group. Census 2001 C-17 supplies only
one observation per state-language cell, so that serial-cluster dimension is absent.
The single-year C-17 diagnostic therefore reports HC1 heteroskedasticity-robust
standard errors. This difference is explicit rather than imitating a cluster scheme
whose repeated dimension is not present in the data.

## Cross-source mechanism reporting contract

`data/metadata/english_opportunity_measures.csv` is the reporting-only registry for the compact mechanism sequence. It gives each selected C-17, NSS-64, and DISE measure a source, observational unit, numerator, denominator, population, stage, source-side interpretation, and paper role. It contains no formulas or estimation settings. C-17 state-by-language behavior, NSS household-side realized enrollment, and DISE administrative conditions remain distinct empirical objects; their ordering in a figure is descriptive mechanism organization, not a sequential mediation model. DISE rows are checked against the existing DISE construct and school-quality registries so that paper labels cannot silently drift from the source-specific semantic authority.

## Compact district mechanism grid

The paper-facing district mechanism diagnostic uses the reporting registry only
to select preferred district measures and attach labels. Estimation continues to
use the canonical IV adjustment definitions and the shared first-stage residual
and clustered-inference helpers. Exactly three specifications are reported:
unadjusted, six-region fixed effects plus the predetermined main Census controls,
and state fixed effects plus the same controls. No expanded-control or instrument-
construction permutation enters this compact grid.

For each mechanism outcome, the complete-case sample is fixed across those three
columns before estimation. The diagnostic reports the raw linguistic-distance
coefficient, state-clustered standard error, excluded-instrument Wald statistic,
partial R-squared, and the signed standardized partial association. The latter is
computed by scaling the coefficient with the residual standard deviations of the
instrument and outcome after the specification's nuisance terms; its square equals
the reported partial R-squared. This supports cross-row visualization without
pretending that C-17 state-language outcomes and district NSS/DISE outcomes share
a common unit or form a sequential mediation panel.

The 2007-08 DISE baseline attachment now carries the already-constructed school-
quality measures (pupils per teacher, single-teacher-school share, and girls'-toilet
share) alongside medium and management measures. This removes a parallel quality-
data path for the compact district comparison while leaving later report-card
dynamics in their existing quality diagnostic.

## Mechanism-stage figure

The extended mechanism figure combines the preferred C-17 language-acquisition
model with the compact district grid using one common descriptive signal: the
signed square root of the partial R-squared for linguistic distance. For the
single linear C-17 distance term, partial R-squared is the proportional reduction
in the native-speaker-weighted residual sum of squares when that term is added to
the otherwise identical preferred model. For district outcomes, the existing
standardized partial coefficient already has the same signed-square-root
interpretation.

The figure therefore compares strength and sign without placing percentage-point,
enrollment-share, school-stock, and school-quality coefficients on a false common
scale. It shows no causal arrows and no mediation estimate. The C-17 row appears
only in the within-state column because its nuisance controls are language-cell
controls (native-language state share, modal-language status, and the distance-zero
indicator), whereas district region/state columns use the predetermined Census-2001
control set. The underlying numerical and specification CSVs remain the audit trail;
the plot is a paper-organization diagnostic until the empirical pattern is reviewed.

The cross-source mechanism figure and compact companion table use the same validated
signed-partial-correlation data. The table contains only the predeclared unadjusted,
region-plus-controls, and within-state columns. C-17 appears only in the within-state
column because its state-by-language design has a different observational unit and
nuisance-control set; neither output should be read as a sequential mediation model.
