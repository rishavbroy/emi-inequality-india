# From NSS 2007-08 Participation and Expenditure in Education, 64th Round

data_folder <- "NSS 2007-08 Participation and Expenditure in Education 64th Round"

temp <- read_xlsx(file.path(data_folder, "DDI Metadata from Nesstar XML.xlsx")) %>% 
  suppressWarnings()

districts_only_0708 <- temp %>% 
  filter(name == "district_code") %>% 
  select(`ns1:catValu`, `ns1:labl25`) %>% 
  distinct() %>% 
  filter(str_detect(`ns1:catValu`, "^\\d{5}$")) %>% # ^ = start of string, \\d{5} = exactly five digits, $ = end of string. Filters out codes which are actually for states (2 digits long) or regions (3 digits), keeping only true district codes (5 digits).
  rename(district_code_0708 = `ns1:catValu`, district_0708 = `ns1:labl25`) %>% 
  mutate(
    state_code_0708 = substr(district_code_0708, 1, 2),
    region_code_0708 = substr(district_code_0708, 1, 3)
    ) # substr vs. str_sub?

regions_only_0708 <- temp %>% 
  filter(name == "region_code") %>% 
  select(`ns1:catValu`, `ns1:labl25`) %>% 
  distinct() %>% 
  filter(str_detect(`ns1:catValu`, "^\\d{3}$")) %>% # ^ = start of string, \\d{3} = exactly three digits, $ = end of string. Filters for regions (3 digits).
  rename(region_code_0708 = `ns1:catValu`, region_0708 = `ns1:labl25`)

states_0708 <- temp %>% 
  filter(name == "STATE") %>% 
  select(`ns1:catValu`, `ns1:labl25`) %>% 
  distinct() %>% 
  filter(!is.na(`ns1:labl25`)) %>% 
  rename(state_code_0708 = `ns1:catValu`, state_0708 = `ns1:labl25`)

districts_0708 <- left_join(states_0708, districts_only_0708, by = "state_code_0708")

districts_0708 <- left_join(districts_0708, regions_only_0708, by = "region_code_0708")





# From NSS 2017-18 Household Social Consumption Education, 75th Round
data_folder <- "NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018"

# Huge thanks to Tabula for helping me turn the original PDF table into a CSV!
temp <- read_csv_short(
  file.path(data_folder, "List of Districts NSS 2017-18.csv"),
  col_names = c("sl. no (1)", "state_1718", "region_code_1718", "region_1718", "sl. no (5)", "district_1718", "district_only_code_1718"),
  col_types = "dcccdcc")

districts_1718 <- temp %>% 
  filter(!is.na(district_1718)) %>% 
  fill(region_code_1718, .direction = "down") %>% # In the original table, region_code was only given for the first district listed within each region--all other districts had an NA.
  mutate(
    district_only_code_1718 = gsub("[()]", "", district_only_code_1718), # Characters enclosed by square brackets form a character class, a pattern which matches with any character also found inside of the class. So here, we globally substitute ("gsub") an empty string for any character in the district_only_code column which matches the class "[()]" i.e., for any "(" or ")".
    district_code_1718 = paste0(region_code_1718, district_only_code_1718) # Row by row, append the newly de-parenthesized district_only_code to the end of region_code.
  ) %>% 
  select(district_code_1718, district_1718) %>% 
  mutate(state_code_1718 = substr(district_code_1718, 1, 2)) %>% 
  arrange(district_code_1718)

temp <- read_csv(
  file.path(data_folder, "State Codes.csv"),
  col_names = c("state_1718", "state_code_1718"),
  col_types = c("cc"),
  skip = 1)

districts_1718 <- right_join(districts_1718, temp, by = "state_code_1718")


# Drop geometry. Keep JID + keys
districts_20 <- district_bnds_20 %>%
  st_drop_geometry() %>%
  select(JID, district_20, state_20)


#### Diagnose and fix issues ####

# Fix 2007-08 districts which are assigned multiple codes
# Test for their presence:
# districts_0708 %>% group_by(state_0708) %>% distinct(district_0708) %>% nrow()
# Returns 595
# districts_0708 %>% group_by(state_0708) %>% distinct(district_code_0708) %>% nrow()
# Returns 598
# View the suspected culprits:
repeated_districts_0708 <- districts_0708 %>% group_by(district_0708, state_0708) %>% summarize(n = n_distinct(district_code_0708)) %>% filter(n > 1)
repeats_of_districts_0708 <- districts_0708 %>% filter(
  district_0708 %in% repeated_districts_0708$district_0708,
  state_0708 %in% repeated_districts_0708$state_0708
)
# Clear the innocent suspects:
used_repeats_of_districts_0708 <- edu0708b4 %>% filter(
  district_code %in% repeats_of_districts_0708$district_code_0708
) %>% group_by(district_code) %>% summarize(n())
# Charge the true culprits
unused_repeats_of_districts_0708 <- repeats_of_districts_0708 %>% 
  filter(
    !district_code_0708 %in% used_repeats_of_districts_0708$district_code
  )
# Convict them
districts_0708 <- districts_0708 %>% filter(
  !district_code_0708 %in% unused_repeats_of_districts_0708$district_code_0708)

# Repeat for 2017-18
# # Test for presence of guilt:
# districts_1718 %>% group_by(state_1718) %>% distinct(district_1718) %>% nrow()
# # Returns 672
# districts_1718 %>% group_by(state_1718) %>% distinct(district_code_1718) %>% nrow()
# # Returns 672
# # All good!
