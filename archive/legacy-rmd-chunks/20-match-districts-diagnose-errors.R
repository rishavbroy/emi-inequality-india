# Errors are fixed in the chunk "Match districts: Manually fix errors"


df_names <- c("mother_tongues_01", "df0708", "df1718", "districts_20")
# Changed for diagnostic purposes



# To extract mismatches:
unmatched_rows <- function(
    df_names = get("df_names", 
                   envir = parent.frame()),
    tracker = get("district_tracker", 
                  envir = parent.frame()),
    years_of_interest = get("years_of_interest", 
                            envir = parent.frame())
    ) {
  # Compute two‐digit suffixes
  suffixes   <- substr(as.character(years_of_interest), 3, 4)
  
  # Run merge_dfs_into_tracker on each df, extract unmatched_df, add source
  result_list <- lapply(df_names, function(nm) {
    out <- merge_dfs_into_tracker(
      df_names = nm,
      tracker = tracker,
      years_of_interest = years_of_interest
    )$unmatched_df
    
    # Add source column of length nrow(out)
    out$source <- rep(nm, nrow(out))
    out
  })
  
  # Bind their rows
  combined <- bind_rows(result_list)
  
  # Coalesce the district_/state_ columns
  state_cols <- combined %>%
    select(starts_with("state_"), -contains("code")) %>%
    names()
  dist_cols <- combined %>%
    select(starts_with("district_"), -contains("code")) %>%
    names()
  
  combined %>%
    mutate(
      district = do.call(coalesce, select(., all_of(dist_cols))),
      state = do.call(coalesce, select(., all_of(state_cols)))
    ) %>%
    select(district, state, source)
}





#### Compare outcomes with different tracker dfs ####

# Identify errors for each tracker dr
unmatched_rows_timeseries <- unmatched_rows(
  df_names,
  tracker = district_timeseries,
  years_of_interest = c("2001","2011","2024")
)
unmatched_rows_tracker <- unmatched_rows(
  df_names,
  tracker = district_tracker,
  years_of_interest
)
# Combine all errors
unmatched_rows <- full_join(unmatched_rows_timeseries, unmatched_rows_tracker, by = join_by(state, district), suffix = c("_timeseries","_tracker"))


# Create a df with all rows from original data, so I can search for close matches easily

all_rows <- c(df_names, 
              "missing_districts_1718", 
              "missing_districts_0708",
              "district_timeseries",
              "district_tracker") %>%
  set_names() %>%
  map_df(~ get(.x) %>% mutate(source = .x)) %>% 
  mutate(
    state = coalesce(
      !!!select(., starts_with("state_"), -contains("code"))
    ),
    district = coalesce(
      !!!select(., starts_with("district_"), -contains("code"))
    )
  ) %>%
  select(state, district, source)



#### Correct even more NAs ####

#flagged_df_timeseries %>% .[!complete.cases(.),] %>% nrow()
# 155 rows

#joined_df_timeseries %>% .[!complete.cases(.),] %>% nrow()
# 305 rows

#flagged_df_tracker %>% .[!complete.cases(.),] %>% nrow()
# 92 rows

#joined_df_tracker %>% .[!complete.cases(.),] %>% nrow()
# 250 rows


# ***Why the gigantic discrepancy???