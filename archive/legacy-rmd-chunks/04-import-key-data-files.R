#### From NSS 2007-08, 64th Round

data_folder <- "NSS 2007-08 Participation and Expenditure in Education 64th Round"

#edu0708b12 <- read_sav(file.path(data_folder, "Block-1 & 2 Identification of sample household.sav"))
edu0708b3  <- read_sav(file.path(data_folder, "Block-3  Household  characteristics.sav"))
edu0708b4  <- read_sav_short(file.path(data_folder, "Block-4  Demographic and other particulars of household members.sav"))
edu0708b5  <- read_sav_short(file.path(data_folder, "Block-5  Education particulars of those aged 5-29 years who are currently attending primary level and above.sav"))
edu0708b6  <- read_sav(file.path(data_folder, "Block-6  Particulars of private expend.sav"))
#edu0708b7  <- read_sav_short(file.path(data_folder, "Block-7  Particulars of currently not attending persons aged 5-29 years.sav"))



data_folder <- "NSS 2007-08 Household Consumer Expenditure Survey 64th Round"

cons0708hhchar <- read_sav(file.path(data_folder, "Household Characteristics.sav"))


# ---


#### From NSS 2017-18 Household Social Consumption Education, 75th Round

data_folder <- "NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018"

#edu1718b1211 <- read_sav_short(file.path(data_folder, "Blocks 1, 2 and 11 - Identification of sample household.sav"))
edu1718b3  <- read_sav_short(file.path(data_folder, "Block 3 - Household characteristics.sav"))
#edu1718b31  <- read_sav_short(file.path(data_folder, "Block 3.1 - Details of erstwhile household members of age 3 to 35 years currently attending education.sav"))
#edu1718b4  <- read_sav_short(file.path(data_folder, "Block 4 - Demographic and other particulars of household members.sav"))
#edu1718b5  <- read_sav_short(file.path(data_folder, "Block 5 - Education particulars on basic course of the persons of age 3 to 35 years who are currently attending education.sav"))
#edu1718b6  <- read_sav_short(file.path(data_folder, "Block 6 - Particulars of expenditure (Rs.) for persons of age 3 to 35 years who are currently attending at pre-primary and above level.sav"))
#edu1718b7  <- read_sav_short(file.path(data_folder, "Block 7 - Particulars of currently not attending persons of age 3 to 35 years.sav"))
#edu1718b8  <- read_sav_short(file.path(data_folder, "Block 8 - Particulars of formal vocational or technical training received by household members of age 12 to 59 years.sav"))


# ---


#### From the Census of India 2001

data_folder <- "Indian Census 2001"

# start.time <- Sys.time()
# There are 35 files to be read in. For efficiency, preallocate a list to store the cleaned data from each file
result_list <- vector("list", length = 35)

for(i in 1:35) {
  # Construct the file name using sprintf to pad with zeros
  file_name <- sprintf("PC01_C16_%02d.xls", i)
  file_path <- file.path(data_folder, file_name)
  
  # Read in the Excel file skipping the first 6 rows (data starts on row 7)
  temp <- read_excel(file_path, skip = 6, col_names = FALSE)
  
  # Explicitly set the column names as required
  colnames(temp) <- c("table", "state_code", "district_code", "tehsil_code", "area_name",
                      "mother_tongue_code", "mother_tongue", "spkr_tot", "m_spkr_tot",
                      "f_spkr_tot", "spkr_urban", "m_spkr_urban", "f_spkr_urban",
                      "spkr_rural", "m_spkr_rural", "f_spkr_rural")
  
  # Extract the state name:
  # Find the first row in the area_name column that begins with "State - "
  state_val <- temp$area_name[grep("^State - ", temp$area_name)]
  if(length(state_val) == 0) {
    state <- NA_character_
  } else {
    state <- state_val[1] %>% 
      # Remove the "State - " prefix
      str_remove("^State - ") %>% 
      # Remove the last four characters from the string
      str_sub(end = -5) %>% 
      # Convert to title case
      str_to_title()
  }
  
  # Filter the dataframe for rows where both:
  # 1. "mother_tongue_code" ends with "0"
  # 2. "area_name" begins with "District - "
  temp <- temp %>% 
    filter(str_detect(mother_tongue_code, "0$"),
           str_detect(area_name, "^District - "))
  
  # Create a new column "district" by cleaning "area_name"
  temp <- temp %>% 
    mutate(
      district = area_name %>% 
        # Remove the "District - " prefix
        str_remove("^District - ") %>% 
        # Remove the last four characters from the string
        str_sub(end = -5) %>% 
        # Remove leading/trailing spaces and "squish" internal spaces
        str_squish() %>% 
        # Remove any trailing characters that are not letters e.g., spaces, asterisks
        str_replace("[^[:alpha:]]+$", ""),
      
      # Add a new column "state" with the extracted state name for every row
      state = state
    )
  
  # Clean the "mother_tongue" column by removing any leading 1-3 digits and a space, then convert the remaining string to title case.
  temp <- temp %>% 
    mutate(mother_tongue = mother_tongue %>% 
             str_remove("^\\d{1,3}\\s+") %>% 
             str_to_title())
  
  # Save the cleaned data frame into the list
  result_list[[i]] <- temp
}

# Combine all cleaned data frames into one using dplyr's bind_rows (or an equivalent function)
census01 <- bind_rows(result_list)
# end.time <- Sys.time()
# end.time - start.time # 1.48 secs faster than binding before doing all the cleaning sans the "state" column formation

# mother_tongues_01 %>% select(-c(table, tehsil_code, area_name, mother_tongue_code)) %>% group_by(state) %>% distinct(district) %>% View()


# Rows which include "(...)" had the closing ")" cropped off
# census01 %>% filter(grepl("\\(", district)) %>% View



# ---


#### District boundaries and regions ####
# Data from @bhatiaMergingUpdatedDistrictlevel2020, itself an adaptation of @meyersIndiaOfficialBoundaries2020



data_folder <- "District Boundaries 2020"

district_bnds_20 <- st_read(file.path(file.path(data_folder, "district", "in_district.shp"))) %>% 
  rename(district_20 = dtname, 
         state_20 = stname) %>% 
  mutate(state_20 = str_to_title(state_20))




# Lookup table: state --> region
region_df <- tribble(
  ~state_20, ~region,
  "Andaman and Nicobar Islands", "South",
  "Andhra Pradesh", "South",
  "Arunachal Pradesh", "Northeast",
  "Assam", "Northeast",
  "Bihar", "East",
  "Chandigarh", "North",
  "Chhattisgarh", "Central",
  "Dadra and Nagar Haveli and Daman and Diu", "West",
  "Delhi", "North",
  "Goa", "West",
  "Gujarat", "West",
  "Haryana", "North",
  "Himachal Pradesh", "North",
  "Jammu and Kashmir", "North",
  "Ladakh", "North",
  "Jharkhand", "East",
  "Karnataka", "South",
  "Kerala", "South",
  "Lakshadweep", "South",
  "Madhya Pradesh", "Central",
  "Maharashtra", "West",
  "Manipur", "Northeast",
  "Meghalaya", "Northeast",
  "Mizoram", "Northeast",
  "Nagaland", "Northeast",
  "Odisha", "East",
  "Puducherry", "South",
  "Punjab", "North",
  "Rajasthan", "West",
  "Sikkim", "Northeast",
  "Tamil Nadu", "South",
  "Telangana", "South",
  "Tripura", "Northeast",
  "Uttar Pradesh", "North",
  "Uttarakhand", "North",
  "West Bengal", "East"
)

region_df <- region_df %>% 
  mutate(region = as.factor(region))
