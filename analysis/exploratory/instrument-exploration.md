# Instrument Exploration


## Legacy comments

### Legacy Chunk 15: preliminary IV-strength exploration

Preliminary test of IV strength: Dotplot of EMIE values by district_code
EMIE has three peaks: in Jammu and Kashmir; in Sikkim, Arunachal
Pradesh, Nagaland, Manipur, Mizoran, Tripura, maybe Meghalaya; and in
Andhra Pradesh, Karnataka, Goa, Lakshadweep, Kerala, Tamil Nadu,
Pondicheri, and Andaman & Nicobar! The regions which historically were
the furthest from Hindi! Many districts in the second group seem to have
EMIE around 1, and the range of EMIE outside peaks is between 0.4 and
0.1. Justification for looking at smaller units of analysis?

Process the dataframe and compute the weighted average ling_distance for
each (state, district) group. Group by state and district to ensure that
same district names in different states are treated separately For each
group, keep only the top three rows by spkr_tot Ungroup before applying
new transformations Create the ling_distance column based on the
mother_tongue values and @shastry2012a’s 0-5 measure of degrees of
linguistic distance Group again by state and district to compute group
summaries Calculate the weighted average ling_distance for each group.
For each langauge $\ell$ which is among the top three most spoken in
district $d$:
$$\frac{\sum_{\ell}(\text{linguistic distance of }\ell \times \text{num. speakers of }\ell\text{ in }d)}{\sum_{\ell}(\text{num. speakers of }\ell)}$$

``` r
census01 %>% mutate(StateDistrict = paste0(state_code, district_code)) %>% distinct(StateDistrict) %>% nrow() - mother_tongues_01 %>% mutate(StateDistrict = paste0(state_01, district_01)) %>% distinct(StateDistrict) %>% nrow()
Number of districts did not change in this operation
```

**Deviation note.** The prose and exploratory code above are rendered
from the legacy comments. The current tables below use target outputs
from the active IV panel diagnostics rather than reproducing GUI
inspection or a standalone dotplot in the build path.

## Current targets-backed results

| .matched_2001 | .matched_2007 | .matched_2017 | n_rows | mean_EMIE | mean_wavg_ling_degrees | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| TRUE | TRUE | TRUE | 482 | 18.973 | 2.013 | 1700680 | 850.209 | 60.264 |

Current IV-panel match summary

| state | n_rows | mean_EMIE | mean_wavg_ling_degrees | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|:---|:---|:---|:---|
| Andhra Pradesh | 11 | 23.644 | 4.73744 | 3565360 | 858.69 | 47.598 |
| Arunachal Pradesh | 13 | 95.0157 | 4.64729 | 76279.8 | 906.331 | 61.0798 |
| Assam | 20 | 4.86271 | 3.07233 | 1147550 | 801.401 | 57.151 |
| Bihar | 34 | 3.50846 | 0.737019 | 1981690 | 602.665 | 84.8703 |
| Chandigarh | 1 | 58.3319 | 0.289864 | 837516 | 2923.14 | 36.783 |
| Chhattisgarh | 13 | 3.36952 | 0.591175 | 1626490 | 588.203 | 57.6972 |
| Goa | 2 | 62.2874 | 2 | 698016 | 1427.58 | 35.721 |
| Gujarat | 16 | 4.43579 | 1.27061 | 1824360 | 994.49 | 49.8058 |
| Haryana | 19 | 20.1701 | 0.126072 | 1142010 | 1056.35 | 53.1687 |
| Himachal Pradesh | 11 | 16.3984 | 0.711727 | 564285 | 1133.87 | 54.6767 |
| Jammu and Kashmir | 8 | 74.5374 | 3.6644 | 789480 | 959.896 | 51.2147 |
| Jharkhand | 14 | 5.85633 | 1.02389 | 1454030 | 671.741 | 71.0617 |
| Karnataka | 16 | 14.0909 | 4.34573 | 1430980 | 831.971 | 50.7501 |
| Kerala | 14 | 43.4498 | 4.9984 | 2129850 | 1206.29 | 48.0393 |
| Lakshadweep | 1 | 32.4513 | 5 | 57165.4 | 1535.94 | 47.3128 |
| Madhya Pradesh | 42 | 6.24754 | 0.573043 | 1314870 | 653.244 | 63.8122 |
| Maharashtra | 29 | 11.483 | 1.9522 | 2841020 | 889.465 | 52.3245 |
| Manipur | 8 | 63.6689 | 4.9857 | 227266 | 870.904 | 49.6512 |
| Meghalaya | 6 | 63.7426 | 4.85968 | 344809 | 941.6 | 64.6292 |
| Mizoram | 7 | 49.8826 | 4.70968 | 81444.7 | 1150.98 | 62.8539 |
| Nagaland | 8 | 99.7003 | 4.7431 | 118572 | 1235.28 | 41.5966 |
| Odisha | 23 | 7.07565 | 3.11402 | 1244500 | 619.2 | 52.6754 |
| Puducherry | 4 | 53.7704 | 4.97028 | 207734 | 1333.57 | 44.4357 |
| Punjab | 15 | 30.8111 | 0.932473 | 1503360 | 1237.25 | 49.576 |
| Rajasthan | 27 | 5.38824 | 0.369378 | 1861620 | 818.178 | 68.8614 |
| Tamil Nadu | 27 | 22.696 | 4.98566 | 2169280 | 976.786 | 45.8385 |
| Telangana | 8 | 28.7588 | 4.01688 | 2923320 | 991.441 | 46.7537 |
| Tripura | 4 | 3.23891 | 3.58325 | 883490 | 789.775 | 50.5184 |
| Uttar Pradesh | 62 | 5.68862 | 0.017948 | 2518110 | 700.163 | 76.9319 |
| Uttarakhand | 11 | 14.6636 | 0.0624887 | 589312 | 924.281 | 66.6122 |
| Table truncated in rendered note; full CSV has 31 rows. |  |  |  |  |  |  |

Current IV-panel state summary

| group | variable | Variable | Description | Min | 1Q | Med | 3Q | Max | Mean | SD | N |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|
| 2001 | wavg_ling_degrees | Ling. Distance | Average linguistic distance of mother tongue from Hindi | 0.00 | 0.04 | 1.44 | 4.02 | 5.00 | 2.01 | 1.98 | 482 |
| 2007-08 | EMIE | EMIE | EMI exposure | 0.00 | 2.02 | 8.54 | 22.89 | 100.00 | 18.97 | 25.27 | 482 |
| 2007-08 | npeople_0708 | Population | Estimated via NSS sample weights | 12,285 | 823,676 | 1,396,516 | 2,317,118 | 9,922,640 | 1,700,682 | 1,307,716 | 482 |
| 2007-08 | consumption_0708 | Consumption | Average household monthly consumption expenditures (Rs.) | 330.09 | 626.88 | 768.60 | 999.13 | 2923.14 | 850.21 | 319.75 | 482 |
| 2007-08 | gini_cons_0708 | Gini of Consumption | Gini coefficient of consumption | 0.06 | 0.22 | 0.26 | 0.30 | 0.56 | 0.26 | 0.07 | 482 |
| 2007-08 | Pct. Urban | Pct. Urban | Percentage of people in an urban area | 0.00 | 8.60 | 15.69 | 28.13 | 100.00 | 21.09 | 17.91 | 482 |
| 2007-08 | Avg. HH Size | Avg. HH Size | Average household size | 3.85 | 5.10 | 5.60 | 6.23 | 8.94 | 5.66 | 0.85 | 482 |
| 2007-08 | dependency_ratio | Dependency Ratio × 100 | Ratio of dependents (0-14, 65+) to labor force (15-64), × 100 | 23.69 | 48.43 | 57.43 | 71.22 | 110.65 | 60.26 | 15.45 | 482 |
| 2007-08 | pct_fem_head | Pct. Female Head | Percentage of households with a female head | 9.81 | 17.56 | 19.04 | 20.89 | 28.98 | 19.29 | 2.64 | 482 |
| 2007-08 | Pct. Hindu | Pct. Hindu | Percentage of Hindus | 0.00 | 71.62 | 87.94 | 95.10 | 100.00 | 76.78 | 27.73 | 482 |
| 2007-08 | Pct. Muslim | Pct. Muslim | Percentage of Muslims | 0.00 | 1.55 | 6.14 | 13.64 | 100.00 | 11.15 | 16.00 | 482 |
| 2007-08 | Pct. Other | Pct. Other | Percentage not Hindu/Muslim | 0.00 | 0.00 | 1.02 | 6.65 | 100.00 | 12.07 | 26.56 | 482 |
| 2007-08 | Pct. ST | Pct. ST | Scheduled Tribe | 0.00 | 0.00 | 2.93 | 18.06 | 100.00 | 16.97 | 28.12 | 482 |
| 2007-08 | Pct. SC | Pct. SC | Scheduled Caste | 0.00 | 9.45 | 17.68 | 25.55 | 46.42 | 17.61 | 10.86 | 482 |
| 2007-08 | Pct. OBC | Pct. OBC | Other Backward Class | 0.00 | 18.15 | 43.12 | 59.18 | 96.69 | 39.41 | 24.28 | 482 |
| 2007-08 | Pct. Small Land-Owner | Pct. Small Land-Owner | Owns 0.005–0.40 hectares | 4.89 | 32.36 | 45.80 | 59.45 | 95.61 | 46.49 | 18.52 | 482 |
| 2007-08 | Pct. Med. Land-Owner | Pct. Med. Land-Owner | Owns 0.41–3.00 hectares | 0.00 | 19.92 | 31.71 | 44.77 | 94.64 | 33.25 | 17.70 | 482 |
| 2007-08 | Pct. Large Land-Owner | Pct. Large Land-Owner | Owns $\geq$ 3.01 hectares | 0.00 | 0.00 | 1.67 | 6.11 | 43.73 | 4.20 | 6.07 | 482 |
| 2007-08 | Pct. Head Educ., Illiterate | Pct. Head Educ., Illiterate | Percentage of household heads with educ. level: illiterate | 0.00 | 23.95 | 34.35 | 46.16 | 78.68 | 34.88 | 15.87 | 482 |
| 2007-08 | Pct. Head Educ., Lit.-Primary | Pct. Head Educ., Lit.-Primary | Percentage of heads with educ. level: literate-primary | 3.28 | 19.67 | 26.46 | 33.77 | 77.63 | 27.74 | 11.02 | 482 |
| 2007-08 | Pct. Head Educ., Secondary+ | Pct. Head Educ., Secondary+ | Percentage of heads with educ. level: above secondary | 0.58 | 26.90 | 35.20 | 46.43 | 79.43 | 37.32 | 14.40 | 482 |
| 2007-08 | Pct. Pucca | Pct. Pucca | Percentage in pucca (permanent) homes | 0.00 | 27.51 | 55.63 | 80.92 | 100.00 | 53.95 | 29.42 | 482 |
| 2017-18 | npeople_1718 | Population | Estimated via NSS sample weights | 30,094 | 807,081 | 1,481,289 | 2,342,633 | 12,274,837 | 1,788,321 | 1,476,278 | 482 |
| 2017-18 | consumption_1718 | Consumption | Average household monthly consumption expenditures (Rs.) | 850.53 | 1544.12 | 2038.39 | 2618.70 | 6764.46 | 2214.82 | 902.80 | 482 |
| 2017-18 | gini_cons_1718 | Gini of Consumption | Gini coefficient of consumption | 0.11 | 0.20 | 0.24 | 0.29 | 0.55 | 0.25 | 0.07 | 482 |
| 2007-08 to 2017-18 | consumption_pct_change | Percent change in consumption | Percent change in consumption | 12.25 | 124.22 | 157.62 | 192.18 | 446.24 | 164.61 | 61.56 | 482 |
| 2007-08 to 2017-18 | Change in Gini of consumption | Change in Gini of consumption | Change in the Gini coefficient of consumption | -0.30 | -0.07 | -0.02 | 0.03 | 0.29 | -0.02 | 0.08 | 482 |

Current keyed IV summary rows
