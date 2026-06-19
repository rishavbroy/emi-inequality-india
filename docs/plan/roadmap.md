## Top priorities

- Turn percent change consumption into difference of log consumption.

- Harmonize districts better. Avoids rank wreckage in my current dataset

  - Aggregate 2017-18 design-consistent, survey-weighted district estimates into 2007-08 districts using 2011 census population weights

    - Download 2011 district-wise polgyons and population data 

  <!-- -->

  - Aggregate 2007-08 design-consistent, survey-weighted district estimates into 2001 districts using 2001 census population weights

    - Download 2001 district-wise polgyons and population data

<!-- -->

- Incorporate new data sources

  -  Expand window of time using school-reported DISE data 

       - The PhD student's data looks very wrong; confirm by refining the email draft [here](https://chatgpt.com/c/69c5dcf3-e558-8328-82f1-07270e9ba7a6).
       - Is best method to go through the archived PDFs and use Tabula to extract data? Cumbersome but... what else is there?

  - 2001 geometry

<!-- -->

- Use EMIE + enrollment as endogenous regressors

  - Enrollment describes one mechanism: selection into school (extensive margin)

    - Education “freebies” IVs

      - District-level weighted mean of each binary variable IS_EDU_FREE, TUTION_FEE_WAIVED, RECD_SCHOLARSHIP_STIPEND, RECD_TXT_BOOKS, RECD_STATIONERY, MID_DAY_MEAL_ETC_RECD among enrolled children as its own IV. 

      <!-- -->

      - Run overidentification tests if needed

  <!-- -->

  - EMIE describes selection into EMIE among those in school (intensive margin)

    - LingDist IV

      - Acknowledge relevance (district-level) in the writing: within a state, districts with higher LingDist face lower cross-language coordination costs for English (vs. Hindi), which raises EMI supply and the probability of selecting English within school. The state-level result suggests this channel; you’re assuming it also holds within state across districts.

      <!-- -->

      - Acknowledge exclusion in the test: conditional on controls and fixed effects, 2001 LingDist affects 2007→2017 consumption growth only through schooling composition (EMIE and/or enrollment). LingDist is predetermined and plausibly orthogonal to later consumption shocks except via schooling pathways.

- Add state FEs

  - Demean IV to force identification to within-state differences

    - Calculate weighted average for each state

    <!-- -->

    - Subtract it from the weighted average of each of its districts to get demeaned IV.

  <!-- -->

  - Add state FEs to all stages. Check for multicollinearity and weak-IV metrics

  - What Shastry does:

    - Spatial randomness: Regress 1991 district-level linguistic distance on state fixed effects and pre-1991 district-level controls. Map out the residuals; the residual variation appears to be geographically balanced.

    - Temporal stability: 1991 linguistic distance heavily correlated with 1961 linguistic distance. She also replicates all regressions using both 1991 and 1961 linguistic distance and argues against remaining sources of heterogeneity.

- Better incorporate education probit

  - Use it to contextualize the environment, mechanisms behind EMI variatio + importance of my paper.

    - Constraints (class, social group, geography) shape enrollment --> we need IV to study EMI’s macro impact.

  <!-- -->

  - School supply correlates with geography, caste, urbanization, etc. Supports my narrative about constraints beyond individual optimization.

  <!-- -->

  - Control function or 2SRI method? Probably not

    - Both work for endogenous regressors and a potentially nonlinear outcome equation. Would compute first-stage probit for enrollment, find generalized residual (Vella-Newey) for each district (e.g., averaging the residual to district). Include that residual as a control in the consumption regression if enrollment enters linearly and is endogenous. 

    <!-- -->

    - Probit first-stage under these methods isn’t better than instrumenting enrollment because probit outcome (enrollment share) is not binary and 2SLS is a lot cleaner. Should be asymptotically equivalent to 2SLS too.

<!-- -->



## Comprehensive plan

Should goal actually be FD 2SLS with baseline exposure and SEs clustered at state level?

1.  Add notes

    1.  @hanushek1997, @motiram2012a: School supply factors affect education outcomes way more than family, household factors

    2.  Flaws in clustering standard errors: @abadie2022a

2.  Acknowledge flaws in Heckman correction

    1.  Instrumental variables on enrollment

        1.  Historical school construction intensity, teacher hiring, exogenous program rollout, etc.

    <!-- -->

    2.  Change summary tables to reflect instrument aggregation

    3.  Cluster probit’s SE at district level?

3.  Acknowledge flaws in IV and my explanation

    1.  Need to replicate her demonstration of exogeneity and relevance

        1.  In her final analysis (on the effect of district-level measures of linguistic distance on benefits from globalization e.g., the number of IT firms), she only uses one year's data for language, 1991, "because it is more precise than 1961. The 1961 data lists 1,652 languages, many of which are difficult to assign a distance from Hindi (in contrast, there are only 114 in 1991 due to prior Census classification). Many districts have been divided since 1961, adding further noise." (p. 299). However, she still uses 1961 when trying to show exogeneity and relevance because it's strong correlation with linguistic distance in 1991 indicates a small effect of migration, the spatial spillover I discuss in Section \\ref(spa). "Some local migrants assimilate, but the local diversity shows that these groups retain a separate identity. Linguistic distance to Hindi in 1961 and 1991 are strongly correlated, demonstrating this persistence" (p. 305). Also to address bias from forward-looking agents, changes in preferences for education, endogeneity between state and language lines, and migration (p. 305). I only ever use 2001.

    2.  Alternative IVs:

        1.  Historical missionary-school density, colonial-era English-school endowments, timing of state-mandated EMI policies

    3.  Shastry also clusters standard errors at the state-language level because “serial correlation will be mostly within local ethnic group: individuals will be more likely to learn English if their parents and other relatives speak English” (p. 298).

4.  Acknowledge need for further fixed effects

    1.  State fixed effects

        1.  Shastry’s regression of weighted average linguistic distance on percent EMI taught in each state-grade/school level has “region22 and grade-level fixed effects (primary, upper primary, secondary, and higher secondary)” (p. 299)

            1.  “22. Data on language instruction in school is at the state level since district-level data is unavailable; thus, I am unable to include state fixed effects. I also cluster by state and, due to the small number of states (29), adjust my critical values according to Cameron, Miller, and Gelbach (2007): the critical values for the 1 percent, 5 percent, and 10 percent significance levels, drawn from a t-distribution with 27 ( 29 - 2) degrees of freedom, are 2.77, 2.05, and 1.70, respectively”

5.  Acknowledge further control variables are needed

    1.  Make sure HH size is in there. (Could HH size be used in the 2SLS equations if it is used in the Heckman probit selection?)

    2.  TechFirms

        1.  @office2019a: 2013-14 Economic Census, Number of rows per district whose BACT is in 15-18 or something like that. See the folder to make district name matching easier

            1.  Would be endogenously determined by EMI in 2007-08; doesn’t work as a control

        2.  @centralstatisticsofficeIndiaFifthEconomic2008: 2005 Economic Census. See Report file for parsing the txt files. See Directory file for district codes, see <https://microdata.gov.in/NADA/index.php/catalog/46/data-dictionary> to orient yourself

            1.  Use this one

        3.  If can’t find anything, maybe use change in industry and agriculture to; maybe economic activity growth = agricultural growth – industrial growth + service growth, so having the other three variables for growth could let me calculate economic activity growth?

        4.  @shastry2012a: Though the EMI premium does exist, districts with the greatest intensity of EMI, and thus with the greatest growth of IT jobs and school enrollment, also experienced the smallest wage premia for English skills. It could be the case, then, that there is a negative association between district-level economic growth and EMI intensity

    3.  @shastry2012a: State fixed effects should absorb many predictors of IT firm location!

        1.  “The state fixed effects account for many other predictors of IT firm location, such as state business policies. For example, IT firms may be influenced by labor regulation (Besley and Burgess 2004). This is unlikely because turnover in IT is remarkably high with firms raiding each other and employees migrating abroad. Nevertheless, allowing the effect of labor regulation to differ over time does not alter my results. 29 (p. 306)”

    4.  Engineering colleges

        1.  “In measuring engineering college presence, I count only the 26 elite engineering colleges, because district-level data on all engineering colleges are not of the same quality. Another measure is from the list of accredited engineering programs from the National Board of Accreditation of the All India Council for Technical Education. Each program was assigned to a district based on the address of the affiliated college. I only include colleges established prior to 1990. Controlling for this measure does not alter my results” (p. 306)

    5.  Infrastructure measures beyond prop_pucca; perhaps Internet access or electricity access?

    6.  Corruption indices? Hindi-speaking districts of Central India may be far more corrupt, which likely proxies for having far more false advertising my schools, than other regions of India.

        1.  Current framework implicitly assumes corruption, chronic teacher absenteeism, etc. are similar across districts. See Compacted notes for more e.g.: Regardless, a desire for EMI alongside better teacher quality amongst poorer households drove a 30% increase in the number of private schools from 2012 to 2020, comprised mostly of low-cost private schools \[@gooptu2023a, p. 2\]. This change, however, did not seem to change the fact that private schools attract richer households than government schools on average \[@muralidharan2015a\]

        2.  @shastry2012a, p. 299: “To ensure that these results are not driven by native Hindi populations in the “Hindi Belt” states with high levels of corruption and government inefficiency, I include an indicator variable for the following states: Bihar, Uttar Pradesh, Uttaranchal, Madhya Pradesh, Chhattisgarh, Haryana, Punjab, Rajasthan, Himachal Pradesh, Jharkhand, Chandigarh, and Delhi.” For regression to show relevance, exclusion restriction of weighted average linguistic distance on percent EMI for each state. “In addition, I focus on urban areas and include region and grade-level fixed effects (primary, upper primary, secondary, and higher secondary).”

    7.  Changes in composition over 2007-2018 (may be particularly important)

        1.  Change in urban population, change in caste make-up, etc.

    8.  Other mechanisms and confounders of EMI’s effect on economic well-being?

6.  Acknowledge further mistakes

    1.  Flaws in district splitting

        1.  Districts which get split between 2007 and 2017 have two rows representing their 2007 state in my final dataset i.e., have to units of analysis representing their initial condition. Is that ok? Is this a correct way to have one-to-many splits in my units of analysis? Maybe I need to manually create my own estimator to account for this?

        2.  Harmonize districts better. Avoids rank wreckage in my current dataset

            1.  Aggregate 2017-18 design-consistent, survey-weighted district estimates into 2007-08 districts using 2011 census population weights

                1.  Download 2011 district-wise polgyons and population data

            2.  Aggregate 2007-08 design-consistent, survey-weighted district estimates into 2001 districts using 2001 census population weights

                1.  Download 2001 district-wise polgyons and population data

    2.  Flaws in district adjacency

        1.  Is historical district adjacency being properly reflected by using 2020 district adjacency? This is perhaps a huge reason why I’m getting vast levels of multicollinearity and spatial autocorrelation.

            1.  \*\*\*Try to find or construct shapefiles for districts in 2007-08‼ Then I could make my units of analysis 2007-08 districts instead of 2020 districts, as it is currently. Weighted average of 2017-18 measures for each district in each row of the dataframe in which I run my regression?

        2.  Could this district matching introduce errors in, say, the way linguistic distance is measured which correlate with the way consumption growth is measured? 2007 districts which were predisposed by some confounder X to split more have spatially autocorrelated linguistic distance, consumption growth, EMI, etc.?

        3.  Is this something which would only affect spatial autocorrelation metric? Would it also affect multicollinearity?

    3.  Flaws in measures

        1.  Nominal measures: Consumption growth measure is not robust to geographic variation in inflation rates

        2.  Percent change measures e.g., %ΔConsumption, as dependent variables may be flawed; perhaps replace with better measures? See [Using percentage change as a dependent variable? - Cross Validated](https://stats.stackexchange.com/questions/491114/using-percentage-change-as-a-dependent-variable), which links to [Linear Regression with a Dependent Variable that is a Ratio - Cross Validated](https://stats.stackexchange.com/questions/59145/linear-regression-with-a-dependent-variable-that-is-a-ratio), which links to [Ratios in Regression, aka Questions on Kronmal - Cross Validated](https://stats.stackexchange.com/questions/58664/ratios-in-regression-aka-questions-on-kronmal) and, more importantly, [Spurious Correlation and the Fallacy of the Ratio Standard Revisited](https://www.jstor.org/stable/2983064). Also see [Prediction of Percent Change in Linear Regression by Correlated Variables](https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2369&context=jmasm)

            1.  %Δconsumption should be difference between log of avg consumption in 2017-18 and 2007-08

    4.  Findings not robust across multiple specifications

        1.  Report multiple specifications: First have no IV and no controls, then no IV and all controls except for the average IMR, then no IV and all controls including the average IMR, then IV and no controls, then IV and all controls except for the average IMR, then IV and all controls including the average IMR? Would six specifications like this fit best vertically or horizontally on a page? How could I make it so that the table neatly flows onto multiple pages as needed?

7.  Acknowledge further flaws in formatting

    1.  Those replicating the code for this paper are encouraged to skim the chunk \` Define functions to read in short 8.3 filenames\`, where the functions \`read_sav_short()\`, \`read_csv_short()\`, and \`read_excel_short()\` are defined and troubleshooting methods are explained

    2.  Standardize tables

8.  Go through code

    1.  Go through code, take note of what to add to paper (e.g., graph of districts which weren’t clean partitions)

    2.  Copy and paste code from each chunk into Chat. Confirm it all is good, accurate, would read well to econ pre-doc application committee, and conforms with how I describe the paper in my CV and cover letter.

9.  Go through writing

    1.  Copy and paste writing from each section into Chat. Confirm it all is good, accurate, would read well to econ pre-doc application committee, and conforms with how I describe the paper in my CV and cover letter.

10. Proofread

    1.  Read through what’s currently there and proofread, edit sentences

    2.  Feed into Quillbot or something for further proofreading. Fix typos

    3.  Make sure all equations look nice

Most important changes to make

11. Code: Fix district matching more

    1.  Go through “#### Correct even more NAs \####” section of code and correct remaining NAs

        1.  92 rows with NAs excluded from joined_df_tracker,

        2.  Seems like many remaining errors come from districts which were not clean partitions or mergers, an issue especially prominent in Arunachal Pradesh

        3.  Maybe merge district_timeseries and district_tracker into a dataframe called tracker?

    2.  Test different matching methods by putting troublesome district name pairs into the “pairs” character vector

    3.  Districts which get split between 2007 and 2017 have two rows representing them in 2007 i.e., have to units of analysis representing their initial condition. Is that ok? Is this a correct way to have one-to-many splits in my units of analysis? Maybe I need to manually create my own estimator to account for this?

    4.  Is historical district adjacency being properly reflected by using 2020 district adjacency? This is perhaps a huge reason why I’m getting vast levels of multicollinearity and spatial autocorrelation.

        1.  \*\*\*Try to find or construct shapefiles for districts in 2007-08‼ Then I could make my units of analysis 2007-08 districts instead of 2020 districts, as it is currently. Weighted average of 2017-18 measures for each district in each row of the dataframe in which I run my regression?

    5.  Could this district matching introduce errors in, say, the way linguistic distance is measured which correlate with the way consumption growth is measured?

    6.  Make sure all measured districts are included in the regression and that no many-to-many relationships exist

12. I just found a dataset with district-level enrollment by medium of instruction. Could this be used to make a better framework somehow?

13. Fix multicollinearity (if it even remains an issue)

    1.  First, check if multicollinearity still exists (really high joint multicollinearity, high kappa overall; but relatively low kappa for all individual variables)

        1.  If it is not longer nearly multicollinear with state fixed effects, then add state fixed effects back in.

    2.  See alternative methods to 2SLS and Heckit estimator at <https://chatgpt.com/c/680842a9-d2c8-8012-adb1-4fdcd9b34492>: dimensionality reduction (PCA, ridge IV, lasso IV), LIML, orthogonalizing, etc.

14. Deflate variables

    1.  Check which year’s dollars each relevant variable (e.g., the consumption expenditures) is given in

    2.  Download and apply deflators to adjust for inflation over regions and time

    3.  <https://pubs-aeaweb-org.ezproxy.library.wisc.edu/doi/pdfplus/10.1257/app.2.4.1>: “defective price indices” p. 9

    4.  Chakraborty and Bakshi: “The labor market outcomes that we consider are wages and occupational choice. We deflate the weekly wages from NSS 55th and NSS 61st rounds in terms of 1982 [Indian rupees](https://www-sciencedirect-com.ezproxy.library.wisc.edu/topics/economics-econometrics-and-finance/indian-rupee) using the [consumer price index](https://www-sciencedirect-com.ezproxy.library.wisc.edu/topics/social-sciences/consumer-price-index) for industrial workers to be able to compare the wage data from the two rounds. Wages are expressed in terms of total real weekly earnings.” (p. 9)

15. IV: Replicate Shastry’s demonstration of exogeneity and relevance

    1.  See her regression on p. 299 with results on relevance from p. 302, supported by her regression on p. 298 with relevance results in pp. 300-301 and exogeneity demonstration p. 304.

    2.  LATE interpretation:

        1.  To do LATE interpretation, would need to show that increasing distance from Hindi doesn’t increase EMIE in some districts while decreasing it for others (i.e., want to make sure linguistic distance has monotonic effect on EMIE)

        2.  See @klineHeckitsLATENumerical2019 for (possibly) more

16. Selection: Change the Heckman implementation and approach

    1.  Study existence of aggregation bias: Is the mean IMR a sufficient statistic for selection correlations in my main 2SLS framework? Perhaps, but only with large enough samples for each district

    2.  Standard errors: How do I get accurate standard errors which account for the extra noise added in via aggregation? Bootstrap?

    3.  Potential issues:

        1.  The IMR is a non-linear function of individual covariates and observables. By averaging at the *district* level, I effectively aggregate by the wrong group, and lose the true group-level selection terms. See cohort-district pseudo-panel below

        2.  Aggregation bias via ecological fallacy: aggregating micro relationships can lead to misleading macro associations (the ecological fallacy). Individual selection and variance in district size or within-district heterogeneity completely lost.

    4.  Alternative methods for controlling selection bias might be better:

        1.  Propensity-score methods: propensity score for high vs. low EMI adoption using district covariates, then match districts based on that score, then compare percent change in consumption

    5.  Fix my current dropping of non-randomly distributed NAs

        1.  What’s up with the NAs from the database of 5-19 year olds i.e., selection_df before its NAs are dropped?

    6.  Fix specious choice about assigning 0/No to children not enrolled in school for some variables

        1.  Issue: Variable should reflect what the child *would* get if they were enrolled in school (e.g., if IS_EDU_FREE would actually be “Yes” if they were enrolled, or if ENROLLMENT_COST would be 0)

        2.  Use district‐level aggregates (e.g., the proportion of enrolled children with “Yes” for IS_EDU_FREE or the average ENROLLMENT_COST of enrolled children in each district) as instruments

            1.  Would satisfy exogeneity requirement of the individual-level probit. For example:

            2.  Construct avg_df0708 with one row per district, and averages of each columns.

            3.  Estimate \$\Pr(\text{enroll}\_i=1\mid X_i,Z_d) \\=\\\Phi\bigl(\beta X_i \\+\\\gamma Z_d\bigr)\$, for child-level controls \$X_i\$ and district-level instruments \$Z_d\$. Then set \$\hat\eta_i = X_i\hat\beta + Z_d\hat\gamma\$ and the IMR becomes:
                \$\$
                \widehat\lambda_i
                =
                \begin{cases}
                \dfrac{\phi(\hat\eta_i)}{\Phi(\hat\eta_i)}
                &\text{if enrolled}\_i=1,\\\[6pt\]
                \dfrac{\phi(\hat\eta_i)}{1-\Phi(\hat\eta_i)}
                &\text{if enrolled}\_i=0,
                \end{cases}
                \$\$

            4.  Aggregate this IMR at the district level for the district-level regression

        3.  Or use multiple imputation to impute the values of each of these for the unenrolled. Maybe run the probit estimation with each imputed dataset, combine the estimated coefficients and \hat\eta_i values via Rubin’s rules to form IMR values for each child. Aggregate these at the district level

    7.  My first thought, to find a close enrolled child match for each unenrolled child and give them the values of the enrolled child, is effectively single-imputation by predictive mean matching, which may understate the uncertainty of imputation

17. Narrative/writing

    1.  Tighter introduction

        1.  Maybe separate introduction and literature review? Have introduction be just like a more thorough abstract

    2.  Make abstract pop

        1.  Highlight empirical methods, findings

18. Make better maps for probit

    1.  Make a map of predicted enrollment propensity by district (maybe via district FE or predicted probabilities).

    2.  Make a map of IMR or missingness structure (helps explain measurement/coverage).

19. Fix EMI and response variable measures

    1.  Percent change measures e.g., %ΔConsumption, as dependent variables may be flawed; perhaps replace with better measures? See [Using percentage change as a dependent variable? - Cross Validated](https://stats.stackexchange.com/questions/491114/using-percentage-change-as-a-dependent-variable), which links to [Linear Regression with a Dependent Variable that is a Ratio - Cross Validated](https://stats.stackexchange.com/questions/59145/linear-regression-with-a-dependent-variable-that-is-a-ratio), which links to [Ratios in Regression, aka Questions on Kronmal - Cross Validated](https://stats.stackexchange.com/questions/58664/ratios-in-regression-aka-questions-on-kronmal) and, more importantly, [Spurious Correlation and the Fallacy of the Ratio Standard Revisited](https://www.jstor.org/stable/2983064). Also see [Prediction of Percent Change in Linear Regression by Correlated Variables](https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2369&context=jmasm)

20. Regressions: Report multiple specifications

    1.  Research how to report multiple regression specifications in the same table.

    2.  First have no IV and no controls, then no IV and all controls except for the average IMR, then no IV and all controls including the average IMR, then IV and no controls, then IV and all controls except for the average IMR, then IV and all controls including the average IMR? Would six specifications like this fit best vertically or horizontally on a page? How could I make it so that the table neatly flows onto multiple pages as needed?

        1.  See Azam et al. (2013) for how they run and report different regression specifications with different interactions

        2.  How to make it so that the no IV specifications have their EMI value in an “EMI” row and the IV specifications have their EMI value in an “EMI (fitted) row”?

    3.  What if the “best” (e.g., highest AIC) set of controls for the non-IV regression is the same as with the IV regression?

Less important changes

21. Get more control variables into R

    1.  In Compacted Notes doc, make note of controls used by relevant papers

        1.  Due to nonmonotonicity in the linguistic distance IV, Shastry (2012) adds in controls which are given in the bottom of p. 298

        2.  How did other papers account for migration?

    2.  How to account for cohort effects?

        1.  Age had strong and very significant negative effect in selection equation

    3.  Enrollment effects: Overall enrollment rates (and/or an EMI × enrollment interaction)?

    4.  TechFirms

        1.  @office2019a: 2013-14 Economic Census, Number of rows per district whose BACT is in 15-18 or something like that. See the folder to make district name matching easier

            1.  Would be endogenously determined by EMI in 2007-08; doesn’t work as a control

        2.  @centralstatisticsofficeIndiaFifthEconomic2008: 2005 Economic Census. See Report file for parsing the txt files. See Directory file for district codes, see <https://microdata.gov.in/NADA/index.php/catalog/46/data-dictionary> to orient yourself

            1.  Use this one

        3.  If can’t find anything, maybe use change in industry and agriculture to; maybe economic activity growth = agricultural growth – industrial growth + service growth, so having the other three variables for growth could let me calculate economic activity growth?

        4.  @shastry2012a: Though the EMI premium does exist, districts with the greatest intensity of EMI, and thus with the greatest growth of IT jobs and school enrollment, also experienced the smallest wage premia for English skills. It could be the case, then, that there is a negative association between district-level economic growth and EMI intensity

    5.  @shastry2012a: State fixed effects should absorb many predictors of IT firm location!

        1.  “The state fixed effects account for many other predictors of IT firm location, such as state business policies. For example, IT firms may be influenced by labor regulation (Besley and Burgess 2004). This is unlikely because turnover in IT is remarkably high with firms raiding each other and employees migrating abroad. Nevertheless, allowing the effect of labor regulation to differ over time does not alter my results. 29 (p. 306)”

    6.  Engineering colleges

        1.  “In measuring engineering college presence, I count only the 26 elite engineering colleges, because district-level data on all engineering colleges are not of the same quality. Another measure is from the list of accredited engineering programs from the National Board of Accreditation of the All India Council for Technical Education. Each program was assigned to a district based on the address of the affiliated college. I only include colleges established prior to 1990. Controlling for this measure does not alter my results” (p. 306)

    7.  Infrastructure measures beyond prop_pucca; perhaps Internet access or electricity access?

    8.  Corruption indices? Hindi-speaking districts of Central India may be far more corrupt, which likely proxies for having far more false advertising my schools, than other regions of India.

        1.  Current framework implicitly assumes corruption, chronic teacher absenteeism, etc. are similar across districts. See Compacted notes for more e.g.: Regardless, a desire for EMI alongside better teacher quality amongst poorer households drove a 30% increase in the number of private schools from 2012 to 2020, comprised mostly of low-cost private schools \[@gooptu2023a, p. 2\]. This change, however, did not seem to change the fact that private schools attract richer households than government schools on average \[@muralidharan2015a\]

        2.  @shastry2012a, p. 299: “To ensure that these results are not driven by native Hindi populations in the “Hindi Belt” states with high levels of corruption and government inefficiency, I include an indicator variable for the following states: Bihar, Uttar Pradesh, Uttaranchal, Madhya Pradesh, Chhattisgarh, Haryana, Punjab, Rajasthan, Himachal Pradesh, Jharkhand, Chandigarh, and Delhi.” For regression to show relevance, exclusion restriction of weighted average linguistic distance on percent EMI for each state. “In addition, I focus on urban areas and include region22 and grade-level fixed effects (primary, upper primary, secondary, and higher secondary).”

            1.  “22. Data on language instruction in school is at the state level since district-level data is unavailable; thus, I am unable to include state fixed effects. I also cluster by state and, due to the small number of states (29 ), adjust my critical values according to Cameron, Miller, and Gelbach (2007): the critical values for the 1 percent, 5 percent, and 10 percent significance levels, drawn from a t-distribution with 27 ( 29 - 2) degrees of freedom, are 2.77, 2.05, and 1.70, respectively”

    9.  Changes in composition over 2007-2018 (may be particularly important)

        1.  Change in urban population, change in caste make-up, etc.

    10. Other mechanisms and confounders of EMI’s effect on economic well-being?

    11. @shastry2012a find educational attainment and economic returns higher for those in EMI

22. Get more response variables

    1.  In emp0708b5:

        1.  Average wage in 2007-08

        2.  Proportion employed in 2007-08

    2.  See Undergraduate Research Symposium (URS) presentation for full list of potential response variables

    3.  Get wages, Gini coefficient of wages, employment, HDI

        1.  Once HDI data has been entered in and matched via districts; if there are differences between the 2005-06 and 2019-20-21 districts, see which ones match the HDI data. Create a loop which, for example, takes the districts without matches in 2005 and looks for matches in 2006.

        2.  In general, check to see if the right number of districts are there

        3.  Check to see if HDI still inaccessible: <https://dhsprogram.com/data/new-user-registration.cfm>, Attention Users: Due to the on-going review of US foreign assistance programs, The DHS Program is currently on pause. We are unable to respond to any data or other requests at this time. We ask for your patience.

    4.  How to better incorporate informal, agricultural, self-employed workers beyond wages?

    5.  Lot more observations of variables in other NSS 64<sup>th</sup> round datasets. But also lot more NAs

23. Fix details of the Heckman correction variable selection

    1.  See <https://en.wikipedia.org/wiki/Heckman_correction#Statistical_inference>: The covariance matrix generated by the OLS estimation of the Heckman correction’s second stage is inconsistent. Bootstrapping might be better.

    2.  See <https://en.wikipedia.org/wiki/Heckman_correction#Disadvantages>: Semiparametric, others could get around joint normality assumption.

    3.  Change reference levels of variables like IS_EDU_FREE in participation equation into “No”, so that the coefficients represent the effect of “Yes”

    4.  Make sure DIST_FROM_NEAREST_PRIMARY_CLASS in participation equation is either properly labeled (so readers know it is not distance from nearest school overall) or replace it with distance to the nearest school or the nearest school which would be where the student would go if they were to enroll right now

    5.  Maybe include parental education == “Other” in reference group alongside “Illiterate”

    6.  Instead of just distance to nearest primary class, include distance nearest school of the level kids their age would normally be attending?

        1.  Distance from nearest upper primary and secondary class also have much more variation (but also a few more NAs) than from nearest primary class, where 91.7% of rows have a value of 1

    7.  Tuition was not waived for 94% of observations, scholarships were not received for 90% of observations, and free stationary was not given for 92% of observations. See what happens when these variables are removed; does anything change?

    8.  Further dimensionality reduction impossible? For example, in selection equation maybe just have DIST \> 1 and DIST \<= 1?

    9.  Does maternal education predict child’s education better than paternal education? Are mothers more involved in their child’s education and in making schooling decisions than fathers?

24. Use better measures of linguistic distance IV

    1.  Have access to m_spkr_urban, f_spkr_rural, etc. Could that be used to construct a better measure than by using spkr_tot?

    2.  Create better measure of linguistic distance using corpuses and perplexity of the corpus-based *n*-grams

        1.  If I don’t do this, then ask myself which languages were incorrectly measured. Rank the languages most spoken in each district. Once grouped by district, filter so that only languages not explicitly named in Shastry’s Table 1 (and thus implicitly given a measure of degree 5) remain

    3.  Use multiple measures: Weighted average of linguistic distance plus percent of speakers who spoke a “distant” language, however Shastry (2012) defined that, plus the perplexity of corpus-based *n*-grams

        1.  Are there any benefits to having multiple IV’s? If so, run an overidentification test

25. See if spatial spillovers (migration, commuting students, commuting workers) matter

    1.  Was I able to find 2007-08 shapefiles or other solutions to fix my district matching methodology? Maybe merge polygons of 2020 districts which have the same district and state name in district_08 to create new polygon list and then construct neighbors list from there?

    2.  Migration as regressor using NSS 64th Round, Employment & Unemployment and Migration Survey data? Out-migration, place of last residence, remittances sent back

    3.  Moran’s I calculation: Repeat for controls which may have a strong degree of spatial autocorrelation (infrastructure, poverty, etc.)

    4.  Redo the SDM-2SLS model with gstsls() from the spatialreg package (note that GMM equals IV with the right moment equation) or with spreg() from the sphet package or some other way to fix the weird two-step lag instrument needed to have 3 instruments and the massive multicollinearity issues. How can I effectively estimate a spatial Durbin model when my errors, explanatory variables, and instrumental variables are all spatially autocorrelated?

    5.  Turn standard errors into Conley SEs i.e., HAC SEs.

        1.  How well will those work for islands like the Andaman and Nicobar islands or Lakshadweep?

    6.  Estimate impact functions for proper interpretability, perhaps using the spatialreg package

    7.  Test the common factor restriction via likelihood-ratio or Wald test to see if our current spatial Durbin models reduce to spatial error models (SEMs).

    8.  Use lm.LMtests() to choose between an SAR, SEM, SLX, SDM, etc. specification

    9.  Re-check Moran’s I on my two response variables, my primary explanatory variable (EMIE), my instrument, the first-stage model’s residuals, and my two second-stage models’ residuals

    10. Moran’s I calculation: Repeat for these new response variables

26. Update presentation slides with updated results

Least important changes

27. Sensitivity tests

    1.  Drop districts which have very large or very small values. How much do results change?

28. Fix figures

    1.  Make ILO-fig images bigger

29. Fix structure of the paper

    1.  Go through Compacted Notes and flesh out missing details

    2.  Go through Thorough Notes and flesh out missing details

30. Implement Benjamini-Hochberg procedure in R

    1.  Multiple comparisons problem—how big of a deal is it? If big, how can I implement the procedure?

\*\*\*Turn read_sav_short() type files into an R package for reading in 8.3 filenames?

[4.19 Put together all code in the appendix (\*) \| R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/code-appendix.html)

Model selection

[10.3 - Best Subsets Regression, Adjusted R-Sq, Mallows Cp \| STAT 501](https://online.stat.psu.edu/stat501/lesson/10/10.3)

[Mallows's Cp - Wikipedia](https://en.wikipedia.org/wiki/Mallows's_Cp)

[Bayesian information criterion - Wikipedia](https://en.wikipedia.org/wiki/Bayesian_information_criterion)

Moran test from [Maximum spacing estimation - Wikipedia](https://en.wikipedia.org/wiki/Maximum_spacing_estimation#Moran_test)

[Akaike information criterion - Wikipedia](https://en.wikipedia.org/wiki/Akaike_information_criterion)

Nested models from [Statistical model - Wikipedia](https://en.wikipedia.org/wiki/Statistical_model#Nested_models)

[A new look at the statistical model identification](https://ieeexplore-ieee-org.ezproxy.library.wisc.edu/document/1100705), IEEE

[Stepwise regression - Wikipedia](https://en.wikipedia.org/wiki/Stepwise_regression)

Documentation for [stepwise: Main wrapper function for stepwise regression](https://www.rdocumentation.org/packages/StepReg/versions/1.5.0/topics/stepwise)

[A Complete Guide to Stepwise Regression in R \| R-bloggers](https://www.r-bloggers.com/2023/12/a-complete-guide-to-stepwise-regression-in-r/)

[regsubsets function - RDocumentation](https://www.rdocumentation.org/packages/leaps/versions/3.1/topics/regsubsets)

[Best Subsets Regression Essentials in R - Articles - STHDA](http://sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r)  

[How to Use regsubsets() in R for Model Selection](https://www.statology.org/regsubsets-in-r/)

Multicollinearity

[A Guide to Using the R Package “multiColl” for Detecting Multicollinearity](https://link-springer-com.ezproxy.library.wisc.edu/article/10.1007/s10614-019-09967-y)

[collinear: R Package for Seamless Multicollinearity Management](https://blasbenito.github.io/collinear/)

[How to deal with multicollinearity when performing variable selection? - Cross Validated](https://stats.stackexchange.com/questions/25611/how-to-deal-with-multicollinearity-when-performing-variable-selection)

[How to Standardize Data in R (With Examples)](https://www.statology.org/standardize-data-in-r/)

Citing R packages

[Citing R packages - Zotero Forums](https://forums.zotero.org/discussion/77517/citing-r-packages)

[Available CRAN Packages By Name](https://cran.r-project.org/web/packages/available_packages_by_name.html)

Different bibliography for main text and appendix

[multiple independent reference sections in Rmarkdown - Stack Overflow](https://stackoverflow.com/questions/61737494/multiple-independent-reference-sections-in-rmarkdown)

[biblatex - Section bibliographies - TeX - LaTeX Stack Exchange](https://tex.stackexchange.com/questions/19326/section-bibliographies)

[Two Bibliographies: one for main text and one for appendix - TeX - LaTeX Stack Exchange](https://tex.stackexchange.com/questions/98660/two-bibliographies-one-for-main-text-and-one-for-appendix)

[R Markdown: place an Appendix after the "References" section? - Stack Overflow](https://stackoverflow.com/questions/58187514/r-markdown-place-an-appendix-after-the-references-section)

APA formatting

- [Paper](https://www.ericchyn.com/files/Chyn_2018_AER_Moved_to_Opportunity.pdf) with a good introduction—possible model for my intro

- APA Headings and Subheadings [guide](https://www.scribbr.com/apa-style/apa-headings/)

Keys for data

- UDISE [metadata](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/UDISE+%202018-22%20Unified%20District%20Information%20System%20for%20Education%20Plus/Metadata%20USIDE+%20DSP_Schema.pdf)

- NSS 2017-18 Household Education Expenditure [definitions](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/NSS%202017-18%20Household%20Social%20Consumption%20Education%2075th%20Round%20Data%20July%202017%20-%20June%202018/NSS%2075th%20Round%20METADATA%20Definitions.pdf)

- NSS 2014-15 Social Consumption Education [data download](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/NSS%202014%20Education%2071st%20Round%20Data/survey0/index.html)

  - var34, var8, dataeFile1, dataFile2, dataFile3, dataFile5

- NSS 2007-08 Social Consumption Education [data download](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/NSS%202007-08%20Participation%20and%20Expenditure%20in%20Education%2064th%20Round/NIC%20-%20National%20Industrial%20Classification%202004%20code%20list.pdf)

- PLFS 2020-21 Migration in India [survey questions](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/PLFS%202020-21%20Migration%20in%20India%20Unit%20Level%20Data%20of%20Periodic%20Labour%20Force%20Survey%20July%202020-June%202021/Schedule10.4_FIRSTVISIT%20PLFS.pdf)

- Time Use Survey 2019 [read me](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/TUS%202019%20Time%20Use%20Survey%20January%202019-December%202019/README%20TUS106.pdf)

- National Achievement Survey 2017-21 [read me](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623%20Research%20Paper/NAS%202017-2021%20National%20Achievement%20Survey%20Learning%20Outcomes%20Data/NAS%20District%20report%20for%20Class%203/readme_NAS%20Class%203.pdf)

- Annual Status of Education Report (Rural) 2022 [report](https://img.asercentre.org/docs/ASER%202022%20report%20pdfs/All%20India%20documents/aserreport2022.pdf)

Code model

- 623-HW2-With-Code-ECON-623 [assignment](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Spring%202024/ECON%20623%20Population%20Economics/623-HW2-With-Code-ECON-623.pdf)

- [STAT 240 Introduction to the dplyr package](https://uwprod-my.sharepoint.com/personal/rroy22_wisc_edu/Documents/Classes/Old%20Classes/Summer%202023/STAT%20240%20Data%20Science%20Modeling%20I/lecture/wk%202/03-intro-dplyr.html)

Other helpful datasets

- Debt and Investment 77th Round:JANUARY 2019 – DECEMBER 2019-Visit 1 and Visit 2 [download](https://microdata.gov.in/nada43/index.php/catalog/156)

R and R Markdown

Equations and math

- equatiomatic package to plug coefficients into a model’s equation [guide](https://bookdown.org/yihui/rmarkdown-cookbook/equatiomatic.html), [download](https://github.com/datalorax/equatiomatic?tab=readme-ov-file)

R Markdown

- Output raw text in Markdown [guide](https://bookdown.org/yihui/rmarkdown-cookbook/results-asis.html#results-asis)

- Markdown syntax (inline formatting, block formatting, expressions) [guide](https://bookdown.org/yihui/bookdown/markdown-syntax.html)

- [how to include an abstract in a rmakdown Rmd file](how%20to%20include%20an%20abstract%20in%20a%20rmakdown%20Rmd%20file)

knitr

- Code chunks, cache, plot chunks, code chunks [guide](https://yihui.org/knitr/options/)

bookdown

- Number and reference expressions, theorems, headers, and text [guide](https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#equations)

- In-text cross-references [guide](https://bookdown.org/yihui/bookdown/cross-references.html)

- Figures and plots [guide](https://bookdown.org/yihui/bookdown/figures.html#figures)

  - [Control the placement of figures](https://bookdown.org/yihui/rmarkdown-cookbook/figure-placement.html)

  - [Rstudio rmarkdown: both portrait and landscape layout in a single PDF](https://stackoverflow.com/questions/25849814/rstudio-rmarkdown-both-portrait-and-landscape-layout-in-a-single-pdf#27334272)

- Table using kable in knitr [guide](https://bookdown.org/yihui/bookdown/tables.html#tables)

Regression output table

- Using modelsummary [guide](https://tilburgsciencehub.com/topics/visualization/data-visualization/regression-results/model-summary/) with other packages [guide](https://tilburgsciencehub.com/topics/visualization/reporting-tables/reportingtables/kableextra/) [forum](https://stackoverflow.com/questions/66275656/creating-regression-output-table-using-modelsummary)

kable

- Formatting and styling [guide](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html#html_only_features)

<!-- -->

- [Creating nice tables using R Markdown \| R-bloggers](https://www.r-bloggers.com/2015/11/creating-nice-tables-using-r-markdown/)

- [10.1 The function knitr::kable() \| R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/kable.html)

- [10.2 The kableExtra package \| R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html)

[India’s Statistical System: Past, Present, Future](https://carnegieendowment.org/research/2023/06/indias-statistical-system-past-present-future?lang=en)

More interesting sources

- [Underemployment in India: Measurement and Analysis](https://idsk.edu.in/wp-content/uploads/2018/03/OP-58.pdf)

- More current look at the economy:

  - [Budget 2025-26: India has a non-paid employment crisis](https://www.downtoearth.org.in/economy/budget-2025-26-india-has-a-non-paid-employment-crisis)

- Missing issue: Female labor

  - ['Fewer women now doing unpaid domestic work': First Time Use Survey reveals how India's workforce dynamics have shifted](https://economictimes.indiatimes.com/news/economy/indicators/fewer-women-now-doing-unpaid-domestic-work-first-time-use-survey-reveals-how-indias-workforce-dynamics-have-shifted/articleshow/118560424.cms)

  - [Indian women are moving from unpaid domestic work to employment: NSO survey](https://yourstory.com/herstory/2025/02/govt-survey-finds-more-indian-women-moving-to-paid-employment)

  - [Held back by homes: effects of domestic work on occupational choices of women in India](https://link.springer.com/article/10.1007/s41775-024-00237-9)

- More current look at issues relevant to my paper:

  - [Talent shortage — global challenge, India’s opportunity](https://www.thehindu.com/opinion/op-ed/talent-shortage-global-challenge-indias-opportunity/article69255324.ece)

  - Effect of COVID

    - [How a low income state of India managed the unemployment situation during COVID-19? Lessons for future pandemic management](https://doi.org/10.1177/22799036231208425)

    - [Jobless and Stuck: Youth Unemployment and COVID-19 in India](https://link.springer.com/article/10.1057/s41308-023-00205-y)

<u>How datasets were merged:</u>

In R, I have four dataframes: district_tracker, mother_tongues_01, df0708, and df1718. I would like to merge the latter three dataframes into district_tracker to produce a final output called joined_df. However, matches between district_tracker and each of these other dataframes may fail for one of three reasons: rows have been improperly encoded (i.e., have incorrect information), rows have typographical differences (e.g., the “state_01” column in mother_tongues_01 has rows with “Jammu & Kashmir” whereas the “state_01” column in district_tracker has “Jammu and Kashmir”), or rows in district_tracker genuinely do not have corresponding rows in the other dataframes. We aim to merge in such a way that accounts for these first issues.

The following vectors have been previously defined (meaning you do not have to define them in your code) and will be referenced later on in this prompt:

years_of_interest \<- c("2001", "2005", "2006", "2007", "2008", "2011", "2017", "2018", "2019", "2020")

year_suffixes \<- substr(years_of_interest, 3, 4) \# "01", "05", etc.

\# Order columns by year

state_cols \<- paste0("state\_", year_suffixes) \# paste0() is more efficient than paste()

district_cols \<- paste0("district\_", year_suffixes)

The following vectors will also be referenced later on in this prompt, but your code will need to start by defining them like so:

methods \<- c("lcs","osa"),

thresholds \<- c(3,3)

We define one fuzzy join sequence as a function which loops through fuzzy joins with parameter mode = “full” and satisfies the following: for each i in the length of the “methods” vector, the ith iteration of this loop will fuzzy join rows where the string distance metric specified by the ith element of “methods” yields a distance less than or equal to the ith element of “thresholds”. The (i+1)th iteration of the loop then takes all the rows from either df \*which were unmatched in the previous iteration\* and attempts to match them using the (i+1)th element of “methods” as the distance metric and the (i+1)th element of “thresholds” as the threshold. This repeats for the length of the “methods” vector. It should output three dataframes: the result of these fuzzy (full) joins, all rows from the first df it was given which remain unmatched by the end of the loop, and all rows from the second df it was given which remain unmatched by the end of the loop.

We will apply this fuzzy join sequence iteratively. I want another function which will take as input a vector of names of dataframes. First, it will define a dataframe joined_df as being equivalent to district_tracker (joined_df \<- district_tracker). Then, for each dataframe d in the vector of dataframe names, I want the function to extract the last two digits of dataframe d’s name. These two digits will correspond to one of the elements in the previously defined vector year_suffixes (year_suffixes \<- substr(years_of_interest, 3, 4)).

It will then do all of the following:

Given dataframe d, take the one column within it which begins with “district\_” and does not contain the substring “code”; then take the one column within it which begins with “state\_” and does not contain the substring “code”.

Given dataframe joined_df, identify all the columns within it which begin with “district\_” followed by the last two digits of dataframe d’s name, then identify all the columns within it which begin with “state\_” followed by the last two digits of dataframe d’s name. There may be multiple such columns, each ending with different substrings following the last two digits of dataframe d’s name. Each such “district\_” column should share its ending with one such “state\_” column.

For each of these “district\_”/“state\_” pairs, we will run the fuzzy join sequence on dataframe d and joined_df, matching dataframe d’s aforementioned “district\_” column with this joined_df pair’s “district\_” column as well as dataframe d’s aforementioned “state\_” column with this joined_df pair’s “state\_” column. All rows from dataframe d unmatched by the end of this sequence will go through the same process with another “district\_”/“state\_” pair, and so on until all “district\_”/“state\_” pairs which end with the last two digits of dataframe d’s name have been used.

Take all rows from dataframe d which, after all this, are still unmatched. Recall the vector year_suffixes: identify the element which precedes the element equivalent to the last two digits of dataframe d. We will now repeat this process given the two digits which this element will consist of: Given dataframe joined_df, identify all columns which begin with “district\_” followed by these two digits, then all columns which begin with “state\_” followed by these two digits. Identify each pair of these “district\_”/“state\_” columns which have the same characters after the two digits. For each such pair, run the fuzzy join sequence with dataframe d’s “district\_” and “state\_” columns matched to this pair’s “district\_” and “state\_” columns respectively. Repeat for all of the other “district\_”/“state\_” pairs which include the two digits from year_suffix. Identify the element from year_suffix which precedes these two digits and repeat using that element as our two digits of interest. Repeat until the first element of year_suffixes has been used.

The function should then store the result of this process as joined_df. This joined_df will become the starting point for repeating all of this for the next element d in the vector of dataframe names. The final output should be joined_df.

<u>How I asked for manual district corrections</u>

See <https://chatgpt.com/c/68040d36-63c8-8012-97d1-60fb60d054f9>

In R, in the dataframe mother_tongues_01, under the column district_01, recode the following:

- Sant Ravidas Nagar Bhadohi to Bhadohi

- Kanker to Uttar Bastar Kanker

In mother_tongues_01, under the column state_01, recode the following:

- N.c.t. Of Delhi to Delhi

In df0708, under the column district_0708, recode the following:

- Sahib Mansa to Sahibzada Ajit Singh Nagar

- J Phule Nagar to Jyotiba Phule Nagar

- G. Buddha Nagar to Gautam Buddha Nagar

- S. Kabir Nagar to Sant Kabir Nagar

- S R Nagar (Bhadohi) to Bhadohi

- Champaran (W) to Pashchim Champaran

- Champaran (E) to Purba Champaran

- North (Mongam) to North

- West (Gyalshing) to West

- South (Nimachai) to South

- East (Gangtok) to East

- West Dinajpur to Dakshin Dinajpur

- 24-Parganas ( North ) to North Twenty Four Parganas

- 24-Parganas ( South ) to South Twenty Four Parganas

- Singhbhum(W) to Pashchimi Singhbhum

- Singhbhum(E) to Purbi Singhbhum

- W. Nimar ( Khargoan ) to West Nimar

- E. Nimar ( Khandwa ) to East Nimar

- Kanker to Uttar Bastar Kanker

In df1718, under the column district_1718, recode the following:

- Sant Ravidas Nagar(Bhadohi) to Bhadohi

- North District to North

- West District to West

- South District to South

- East District to East

- Khargone (West Nimar) to West Nimar

- Khandwa (East Nimar) to East Nimar

- Leh to Leh (Ladakh)

- Y.S.R. (Cuddapah) to Cuddapah

- Rajanna to Rajanna Sircilla

In districts_20, under the column district_20

- North District to North \[note the two spaces in “North District”\]

- West District to West

- South District to South

- East District to East

- Leh to Leh (Ladakh)

- Cooch Behar to Koch Bihar

- Y.S.R. to Y.S.R. Kadapa

In district_timeseries, under the column…

- “district_24”:

  - Shamli (Prabuddhanagar) to Shamli

  - Y.S.R. to Y.S.R. Kadapa

- “district_01”:

  - Kanker to Uttar Bastar Kanker

In district_tracker, under “district_01”

- West Champaran to Pashchim Champaran

- East Champaran to Purba Champaran

- North District to North

- West District to West

- South District to South

- East District to East

- North 24 Parganas to North Twenty Four Parganas

- South 24 Parganas to South Twenty Four Parganas

- West Singhbhum to Pashchimi Singhbhum

- East Singhbhum to Purbi Singhbhum

- Leh to Leh (Ladakh)

- Cooch Behar to Koch Bihar

- Dima Hasao to North Cachar Hills

- Kaimur to Kaimur (Bhabua)

- Kabeerdham to Kawardha

- Kutch to Kachchh

- Dang to The Dangs

- Belagavi to Belgaum

- Shivamogga to Shimoga

- Khandwa to East Nimar

- Khargone to West Nimar

- Subarnapur to Sonapur

- Amroha to Jyotiba Phule Nagar

- Lakhimpur Kheri to Kheri

- Central Delhi to Central

- North Delhi to North

- North East Delhi to North East

- North West Delhi to North West

- East Delhi to East

- South Delhi to South

- South West Delhi to South West

- West Delhi to West

In district_tracker:

- Under the column “district_06”:

  - S.A.S. Nagar to Sahibzada Ajit Singh Nagar

- Under “district_08”:

  - S.P.S. Nellore to Sri Potti Sriramulu Nellore

  - Aizawl to Saitual

  - Pauri Garhwal to Garhwal

In df1718, under the column state_1718, recode the following:

- A & N Islands to Andaman and Nicobar Islands

In districts_20, under the column state_20, recode the following:

- Ladakh to Jammu and Kashmir
