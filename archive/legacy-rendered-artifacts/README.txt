---On long file paths---

Many of the files in this folder have incredibly long path names. This was done intentionally, to both ensure consistency between my code and the original data sources, and to let you replicate my findings with newly-downloaded data easily, without having to worry about changing file names or my R code. I did not realize this was not good practice at the time.

"580-Draft-ECON-580.Rmd" contains all of the code and text used in this project. The chunk titled "Define functions to read in short 8.3 filenames" contains functions which will read file names that exceed the Windows character limit. Should issues arise, background information and troubleshooting tips can be found in this chunk.




---Folder and file information---

The folder "NSS 2007-08 Participation and Expenditure in Education 64th Round" contains the following files, all taken from the titular survey:
- "Block-3  Household  characteristics.sav"
- "Block-4  Demographic and other particulars of household members.sav"
- "Block-5  Education particulars of those aged 5-29 years who are currently attending primary level and above.sav"
- "Block-6  Particulars of private expend.sav"
- "DDI Metadata from Nesstar XML.xlsx"

"NSS 2007-08 Household Consumer Expenditure Survey 64th Round" contains the following:
- "Household Characteristics.sav"

""NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018" contains:
- "Block 3 - Household characteristics.sav"
- "List of Districts NSS 2017-18.csv"
- "State Codes.csv"

"Indian Census 2001" contains:
- 35 files of the form "PC01_C16_....xls", one per state or union territory

"District Boundaries 2020" contains:
- A folder "district" with a file called "in_district.shp"

"District Changes Data" contains:
- "Time series- State and Districts Changes -Alluvial 1951-2024.xls"
- "District Carve-Outs and Renamings 1961-2001.csv"
- "IndiaDistrictTracker2001to2020.ods"

The following only need to be downloaded if one is interested in replicating my .Rmd file exactly:
"580 Paper Images" contains:
- "Average Monthly Real Earnings Over Time - Total.png"
- "LFPR WPR and Unemployment for All Over Time.png"



I have tried my best to have descriptive folder names for each file. See my .Rmd file, "580-Draft-ECON-580.Rmd", for more information on them and their source.