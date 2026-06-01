# The districts listed in 2007-08 vs. 2017-18 vary for two reasons: legitimate political changes (districts may have been created, destroyed, or renamed) and typos. I address both here.


# Import data on district changes
data_folder = "District Changes Data"

district_timeseries <- read_excel_short(file.path(data_folder, "Time series- State and Districts Changes -Alluvial 1951-2024.xlsx")) 
# Though its extensive time range is informative, there seem to be some typos and factual errors here (e.g., the 2019 merger of Dadra and Nagar Haveli with Daman and Diu is not reflected here, and "Dadra" is twice misspelled as "Dadara")
# But not as many mistakes as my original district_tracker, turns out! :D

years_of_interest <- c(2001, 2011, 2024)

district_timeseries <- district_timeseries %>%
  # Select only XYYYY-State and XYYYY-District for those years
  select(matches(paste0("^(", paste(years_of_interest, collapse="|"), ")[-–](State|District)$"))) %>%
  # Rename to state_yy / district_yy
  rename_with(~ tolower(
    str_replace(.x, "^(?:\\d{2})(\\d{2})[-–](State|District)$", "\\2_\\1")
  ))
  

#district_creations <- read_excel_short(file.path(data_folder, "New Districts Created between 1951-2024.xlsx")) %>% select(!c.............................................................) # Some files gained an empty column upon importing

#district_renamings <- read_excel_short(file.path(data_folder, "Name Changes_Districts_Indian States_1951-2021.xlsx")) %>% select(!c.............................................................)

#district_splits <- read_excel_short(file.path(data_folder, "District Splits and Carve outs-decadewise  1951-2024.xls")) %>% select(!c.............................................................)


# ---


# Justify the district_tracker df method of matching 

# We want to make it so that for each 2017-18 district, either it can be directly matched to a single 2007-08 district, or a single 2007-08 district can be matched to it. To do so, all district changes must have been either clean partitions or name changes of old districts--new districts being carved out of multiple old districts and/or new borders being drawn between old districts make it far harder to equate or claim equality between units of analysis across time.


# Up to 2001, @kumarCreatingLongPanels2016 are able to measure the *proportion* of each district's population that was allocated into a new district as a result of district changes, specifically those which are not clean partitions: so carve-outs (of new districts from old districts), mergers, renamings, and border shifts. Carve-outs from multiple districts at once and border shifts are the most troublesome for us, as they prevent us from directly matching new districts to a single old district. There does not seem to be any data on how such district carve-outs or border shifts have allocated populations between districts since 2001.

district_carveouts_shifts_9101 <- read_csv_short(
  file.path(data_folder, "District Carve-Outs and Renamings 1961-2001.csv"),
  col_names = c("district_1991", "pop_1991", "district_2001", "pct_01in91", "pct_91in01"),
  col_types = "cccdd"
  ) %>% 
  fill(district_1991, pop_1991, .direction = c("down")) %>% 
  filter(!is.na(pct_91in01))

# So we justify directly matching districts by showing that most district changes from 1991 to 2001 were equivalent to renamings, partitions, or mergers.

plt_district_carveouts_shifts <- district_carveouts_shifts_9101 %>% 
  ggplot(aes(x = pct_91in01)) +
  geom_histogram(binwidth = diff(range(district_carveouts_shifts_9101$pct_91in01, na.rm = TRUE))/40, fill = "goldenrod") +
  guides(fill = "none") + 
  labs(
    y = "Number of 2001 Districts",
    x = "Percentage of a 1991 District's Population in the 2001 District"
    # title = "Population Reallocations From District Shifts and Carve-Outs, 1991-2001",
    # subtitle = "Number of 2001 Districts Which Absorbed a Percentage of a 1991 District's Population"
    )
# From 1991 to 2001, it's evident that district changes which did not involve clean partitions were rarely associated with changes in district populations. In other words, most of these changes were approximately equal to name changes. ^[While most of these changes were actually *precisely* name changes, some were not. See @kumarCreatingLongPanels2016, for example, to see that certain changes only involved transfers of uninhabited land.]


# To get the exact proportion of "other" district changes which are not equivalent to renamings:
# Check pctortion of district changes which allocated less than 100/c or 100 - 100/c of the old district's population to the new district 
c = 40

int_bnds <- district_carveouts_shifts_9101 %>%
  summarise(
    min = min(pct_91in01),
    max = max(pct_91in01),
    mesh = (max - min) / c,
    int_lower = min + mesh,
    int_upper = max - mesh,
    n = n(),
    n_ext = sum(
      pct_91in01 <= min + mesh | 
        pct_91in01 >= max - mesh),
    pct_ext = n_ext/n
  )

# (Set c = 40) 86% of allocations from a parent district to a child district upon an "other" type of district change involved a transfer of more than 97.5% or less than 2.5% of the population. Meaning 86% of the time, the new district almost precisely matched the old district--the change was effectively a name change. 

carveshift_count <- district_carveouts_shifts_9101 %>% 
  group_by(district_2001) %>% 
  arrange(district_2001) %>% 
  filter(n() > 1) %>% 
  summarize(
    max_pct = max(pct_91in01),
    min_pct = min(pct_91in01),
    num_parents = n()
  ) %>% 
  mutate(
    carveshift = if_else(max_pct >= int_bnds$int_upper & min_pct <= int_bnds$int_lower, 0, 1)
  ) %>%
  ungroup()

# mean(carveshift_count$carveshift)
pct_ok_annoying_districts <- 1 - sum(carveshift_count$carveshift)/int_bnds$n
# = 0.9335106 if c=40, so 93.4% of district changes that were not clean partitions were equivalent to a name change (allocating more than 97.5% or less than 2.5% of the old district's population).

# We use this as justification to assume away other types of district changes (namely district carve-outs and border shifts), allowing us to match each new district to a single old district or vice versa. 

# This is equivalent to what @jaacksIndiaDistrictChanges2020 did in their tracking of district changes from 2001 to 2020, with the children of district carve-outs only matched with the one parent who contributed the most to their land area; likewise for districts following border shifts. We thus use their data below.


# ---


#### Construct district_tracker df ####


# Import data, do preliminary data cleaning

data_folder = "District Changes Data"
temp <- read_ods(file.path(data_folder, "IndiaDistrictTracker2001to2020.ods"), col_names = FALSE)

temp_names <- tibble(year = as.character(temp[1,])) %>% 
  fill(year) %>% 
  pull(year)

colnames(temp) <- paste(temp_names, as.character(temp[2,]), sep = "_")


# Set column names as interleaved named vector

years_of_interest <- c("2001", "2005", "2006", "2007", "2008", "2011", "2017", "2018", "2019", "2020") # 2021 excluded due to no data
year_suffixes <- substr(years_of_interest, 3, 4)  # "01", "05", etc.

# Order columns by year
state_cols <- paste0("state_", year_suffixes) # paste0() more efficient than paste()
district_cols <- paste0("district_", year_suffixes)

# Interleave columns
interleaved_renamings <- state_cols %>% 
  rbind(district_cols) %>% # rbind()
  as.vector()
interleaved_old_names <- paste0(years_of_interest, "_STATENAME") %>% 
  rbind(paste0(years_of_interest, "_DISTNAME")) %>%
  as.vector()

# Rename columns
column_renamings <- setNames(
  interleaved_old_names, 
  interleaved_renamings
  )
district_tracker <- temp[-c(1,2), ] %>%
  rename(!!!column_renamings) %>% # `!!!` is the splice operator, to splice and inject arguments from a list into a function call. Note: Base R often needs inject() to accept `!!!`
  select(all_of(names(column_renamings)))


#### Diagnose and correct mistakes ####

#### ...in states/UTs

# View state/UT changes recorded in original data
district_tracker %>% 
  rowwise() %>% 
  filter(length(unique(c_across(all_of(state_cols)))) > 1) %>% 
  ungroup() %>% 
  select(all_of(state_cols)) %>% 
  invisible()
# Two changes, both from 2019, both first reflected in the 2019 data: the union territory (UT) of Ladakh split from Jammu and Kashmir, and the UT Dadra and Nagar Haveli and Daman and Diu formed from the merger of Dadra and Nagar Haveli with Daman and Diu
# This means four changes were not recorded in the database: renaming Pondicherry (both the district and its UT) to Puducherry in 2006, renaming Uttaranchal to Uttarakhand in 2007, renaming Orissa to Odisha in 2011, and cleaving Telangana out of Andhra Pradesh in 2014.

# Add remaining state/UT changes
district_tracker <- district_tracker %>%
  # For state_xx columns with suffix < "06"
  mutate(across(c(state_01, district_01, state_05, district_05), 
                ~ ifelse(. == "Puducherry", "Pondicherry", .))) %>%
  # For state_xx columns with suffix < "07"
  mutate(across(c(state_01, state_05, state_06), 
                ~ ifelse(. == "Uttarakhand", "Uttaranchal", .))) %>%
  # For state_xx columns with suffix < "11"
  mutate(across(c(state_01, state_05, state_06, state_07, state_08), 
                ~ ifelse(. == "Odisha", "Orissa", .))) %>%
  # For state_xx columns with suffix < "14"
  mutate(across(c(state_01, state_05, state_06, state_07, state_08, state_11), 
                ~ ifelse(. == "Telangana", "Andhra Pradesh", .)))
# NOTE: The 07-08 NSS data uses Pondicherry instead of the 2006 name change of Puducherry!! They also (more understandably) use Uttaranchal instead of the 2007 name change Uttarakhand. 
#Fix: To join data from 07-08 NSS, must search for matches beyond the sample years.


#### ...in districts

# View number of districts which changed names during a dataset's sampling period
inperiod_district_changes <- district_tracker %>% 
  filter(
    district_05 != district_06 |
    district_07 != district_08 |
    district_17 != district_18 |
    district_19 != district_20
  )
# 16 districts :(
# Fix: To join data from multi-year surveys, will need to check for matches across all sampling years at least



# View number of districts with shared names in each year

# 1. Filter rows
# For each suffix, we flag rows where the district (for that suffix) appears with more than one distinct state. Then we keep rows where at least one of the suffix comparisons is TRUE.

# Create a list of logical vectors (one per suffix)
keep_list <- lapply(year_suffixes, function(sfx) {
  
  # Build the column names for this suffix
  state_col <- paste0("state_", sfx)
  district_col <- paste0("district_", sfx)
  
  # Group the data by district for this suffix and count distinct states
  temp <- district_tracker %>%
    group_by(district = .data[[district_col]]) %>%
    summarise(n_states = n_distinct(.data[[state_col]]),
              .groups = "drop")
  # Districts that occur with more than one unique state
  duplicate_districts <- temp %>%
    filter(n_states > 1) %>%
    pull(district)
  # Return a logical vector: TRUE if the district value in the row is in duplicate_districts
  district_tracker[[district_col]] %in% duplicate_districts
})
# Combine the logical vectors by taking the rowwise OR (i.e., TRUE if condition holds for any suffix)
keep_indicator <- Reduce(`|`, keep_list)
# Filter the original data
same_name_districts <- district_tracker[keep_indicator, ]

# 2. Summarize by district for each suffix
# For each suffix, group the filtered data by the district column and count:
# a. the number of rows (n_rows) sharing that district value, and 
# b. the number of distinct state values (n_states) in that group.
# Then keep only groups that have more than one unique state.
temp <- lapply(year_suffixes, function(sfx) {
  state_col <- paste0("state_", sfx)
  district_col <- paste0("district_", sfx)
  same_name_districts %>%
    group_by(district_name = .data[[district_col]]) %>%
    summarise(n_districts = n(),
              n_states = n_distinct(.data[[state_col]]),
              .groups = "drop") %>%
    filter(n_states > 1) %>%
    mutate(year_suffix = sfx)
})
# Combine summaries from all suffixes into one data frame
temp <- bind_rows(temp)
# Make it easy to get summary stats
n_same_name_districts <- temp %>% 
    group_by(year_suffix) %>% 
    summarize(n = 2 * n())
# Between min(n_same_name_districts$n) = 6 and max(n_same_name_districts$n) = 10 districts with shared names in each year of interest
