# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' read nss 2007 education
#'
#' @return A tibble, model object, list, or file path depending on context.
read_nss_2007_education <- function(paths) {
  base <- path_raw(paths, "NSS 2007-08 Participation and Expenditure in Education 64th Round")
  list(block3 = haven::read_sav(file.path(base, "Block-3  Household  characteristics.sav")), block4 = read_sav_short(file.path(base, "Block-4  Demographic and other particulars of household members.sav")), block5 = read_sav_short(file.path(base, "Block-5  Education particulars of those aged 5-29 years who are currently attending primary level and above.sav")), block6 = haven::read_sav(file.path(base, "Block-6  Particulars of private expend.sav")), metadata = readxl::read_xlsx(file.path(base, "DDI Metadata from Nesstar XML.xlsx")))
}

#' read nss 2007 consumption
#'
#' @return A tibble, model object, list, or file path depending on context.
read_nss_2007_consumption <- function(paths) {
  base <- path_raw(paths, "NSS 2007-08 Household Consumer Expenditure Survey 64th Round")
  list(household_characteristics = haven::read_sav(file.path(base, "Household Characteristics.sav")))
}

#' read nss 2017 education
#'
#' @return A tibble, model object, list, or file path depending on context.
read_nss_2017_education <- function(paths) {
  base <- path_raw(paths, "NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018")
  list(block3 = read_sav_short(file.path(base, "Block 3 - Household characteristics.sav")), districts = read_csv_short(file.path(base, "List of Districts NSS 2017-18.csv"), col_names = FALSE), state_codes = read_csv_short(file.path(base, "State Codes.csv")))
}

#' read census 2001 mother tongue
#'
#' @return A tibble, model object, list, or file path depending on context.
read_census_2001_mother_tongue <- function(paths) {
  base <- path_raw(paths, "Indian Census 2001")
  files <- file.path(base, sprintf("PC01_C16_%02d.xls", 1:35))
  setNames(lapply(files, readxl::read_excel, skip = 6, col_names = FALSE), basename(files))
}

#' read district boundaries 2020
#'
#' @return A tibble, model object, list, or file path depending on context.
read_district_boundaries_2020 <- function(paths) {
  sf::st_read(path_raw(paths, "District Boundaries 2020", "district", "in_district.shp"), quiet = TRUE)
}

#' read district change sources
#'
#' @return A tibble, model object, list, or file path depending on context.
read_district_change_sources <- function(paths) {
  base <- path_raw(paths, "District Changes Data")
  list(alluvial = readxl::read_xlsx(file.path(base, "Time series- State and Districts Changes -Alluvial 1951-2024.xlsx")), carveouts = readr::read_csv(file.path(base, "District Carve-Outs and Renamings 1961-2001.csv"), show_col_types = FALSE), tracker = readODS::read_ods(file.path(base, "IndiaDistrictTracker2001to2020.ods")), new_districts = readxl::read_xlsx(file.path(base, "New Districts Created between 1951-2024.xlsx")), name_changes = readxl::read_xlsx(file.path(base, "Name Changes_Districts_Indian States_1951-2021.xlsx")), splits = readxl::read_xlsx(file.path(base, "District Splits and Carve outs-decadewise  1951-2024.xlsx")))
}

#' list ilo figure paths
#'
#' @return A tibble, model object, list, or file path depending on context.
list_ilo_figure_paths <- function(paths) {
  files <- c("average_monthly_real_earnings_total.png", "lfpr_wpr_unemployment_all.png", "unemployment_rate_by_general_education.png")
  file.path(paths$assets, "ilo_figures", files)
}

