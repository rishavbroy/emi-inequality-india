# Errors were diagnosed in the chunk "Match districts: Diagnose errors"

# library(dplyr) # Was once needed to avoid car::recode() from being used below; I forgot why this was the case + why it is no longer the case! So I've left it just in case.


mother_tongues_01 <- mother_tongues_01 %>%
  mutate(
    district_01 = recode(district_01,
      "Sant Ravidas Nagar Bhadohi" = "Bhadohi",
      "Kanker" = "Uttar Bastar Kanker"
      ),
    state_01 = recode(state_01,
      "N.c.t. Of Delhi" = "Delhi"
      )
    )


df0708 <- df0708 %>%
  mutate(
    district_0708 = recode(district_0708,
      "Sahib Mansa" = "Sahibzada Ajit Singh Nagar",
      "J Phule Nagar" = "Jyotiba Phule Nagar",
      "G. Buddha Nagar" = "Gautam Buddha Nagar",
      "S. Kabir Nagar" = "Sant Kabir Nagar",
      "S R Nagar (Bhadohi)" = "Bhadohi",
      "Champaran (W)" = "Pashchim Champaran",
      "Champaran (E)" = "Purba Champaran",
      "North (Mongam)" = "North",
      "West (Gyalshing)" = "West",
      "South (Nimachai)" = "South",
      "East (Gangtok)" = "East",
      "West Dinajpur" = "Dakshin Dinajpur",
      "24-Parganas ( North )" = "North Twenty Four Parganas",
      "24-Parganas ( South )" = "South Twenty Four Parganas",
      "Singhbhum(W)" = "Pashchimi Singhbhum",
      "Singhbhum(E)" = "Purbi Singhbhum",
      "W. Nimar ( Khargoan )" = "West Nimar",
      "E. Nimar ( Khandwa )" = "East Nimar",
      "Kanker" = "Uttar Bastar Kanker"
      )
    )


df1718 <- df1718 %>%
  mutate(
    district_1718 = recode(district_1718,
      "Sant Ravidas Nagar(Bhadohi)" = "Bhadohi",
      "North District" = "North",
      "West District" = "West",
      "South District" = "South",
      "East District" = "East",
      "Khargone (West Nimar)" = "West Nimar",
      "Khandwa (East Nimar)" = "East Nimar",
      "Leh" = "Leh (Ladakh)",
      "Y.S.R. (Cuddapah)" = "Cuddapah",
      "Rajanna" = "Rajanna Sircilla"
      ),
    state_1718 = recode(state_1718,
      "A & N Islands" = "Andaman and Nicobar Islands"
      )
    )


districts_20 <- districts_20 %>%
  mutate(
    district_20 = recode(district_20,
      "North  District" = "North", # Note the two spaces!!
      "West District" = "West",
      "South District" = "South",
      "East District" = "East",
      "Leh" = "Leh (Ladakh)",
      "Cooch Behar" = "Koch Bihar",
      "Y.S.R." = "Y.S.R. Kadapa"
      ),
    state_20 = recode(state_20,
      "Ladakh" = "Jammu and Kashmir"
      )
    )


district_timeseries <- district_timeseries %>%
  mutate(
    district_24 = recode(district_24,
      "Shamli (Prabuddhanagar)" = "Shamli",
      "Y.S.R." = "Y.S.R. Kadapa"
      ),
    district_01 = recode(district_01,
      "Kanker" = "Uttar Bastar Kanker"
      )
    )


district_tracker <- district_tracker %>%
  mutate(
    
    district_01 = recode(district_01,
      "West Champaran" = "Pashchim Champaran",
      "East Champaran" = "Purba Champaran",
      "North District" = "North",
      "West District" = "West",
      "South District" = "South",
      "East District" = "East",
      "North 24 Parganas" = "North Twenty Four Parganas",
      "South 24 Parganas" = "South Twenty Four Parganas",
      "West Singhbhum" = "Pashchimi Singhbhum",
      "East Singhbhum" = "Purbi Singhbhum",
      "Leh" = "Leh (Ladakh)",
      "Cooch Behar" = "Koch Bihar",
      "Dima Hasao" = "North Cachar Hills",
      "Kaimur" = "Kaimur (Bhabua)",
      "Kabeerdham" = "Kawardha",
      "Kutch" = "Kachchh",
      "Dang" = "The Dangs",
      "Belagavi" = "Belgaum",
      "Shivamogga" = "Shimoga",
      "Khandwa" = "East Nimar",
      "Khargone" = "West Nimar",
      "Subarnapur" = "Sonapur",
      "Amroha" = "Jyotiba Phule Nagar",
      "Lakhimpur Kheri" = "Kheri",
      "Central Delhi" = "Central",
      "East Delhi" = "East",
      "North Delhi" = "North",
      "North East Delhi" = "North East",
      "North West Delhi" = "North West",
      "South Delhi" = "South",
      "South West Delhi" = "South West",
      "West Delhi" = "West"
    ),
    
    district_06 = recode(district_06,
      "S.A.S. Nagar" = "Sahibzada Ajit Singh Nagar"
    ),
    
    district_08 = recode(district_08,
      "S.P.S. Nellore" = "Sri Potti Sriramulu Nellore",
      "Aizawl" = "Saitual",
      "Pauri Garhwal" = "Garhwal"
    )
  )



# Alternative future strat: 

# Build a tibble of corrections
# corrections <- tribble(~old, ~new, "Sant Ravidas Nagar Bhadohi", "Bhadohi", "Kanker", "Uttar Bastar Kanker") # etc.

# mother_tongues_01 <- mother_tongues_01 %>%
#   left_join(corrections, by = c("district_01" = "old")) %>%
#   mutate(
#     district_01 = coalesce(new, district_01)
#   ) %>%
#   select(-new)
