# Helper vectors:
df_names <- c("mother_tongues_01", "df0708", "df1718", "districts_20")
# Also years_of_interest, year_suffixes, methods, and thresholds

# Join data with district_timeseries data
temp <- merge_dfs_into_tracker(
  df_names, 
  tracker = district_timeseries, 
  years_of_interest = c("2001", "2011", "2024"),
  flag = TRUE
  )

joined_df_timeseries <- temp$joined_df

unmatched_df_timeseries <- temp$unmatched_df

flagged_df_timeseries <- temp$flagged_df


# Join data with district_tracker
temp <- merge_dfs_into_tracker(
  df_names,
  tracker = district_tracker,
  years_of_interest = years_of_interest,
  flag = TRUE
)

joined_df_tracker <- temp$joined_df

unmatched_df_tracker <- temp$unmatched_df

flagged_df_tracker <- temp$flagged_df
