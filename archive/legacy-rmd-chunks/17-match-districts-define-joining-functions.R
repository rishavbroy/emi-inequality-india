# Fuzzy‑join sequence: 
# Loop over methods/thresholds,
# Each time full‑joining only the as‑yet‑unmatched rows, and at the end 
# Return joined (all the matches), unmatched_df1, unmatched_df2
fuzzy_join_sequence <- function(df1, df2,
                                dist1, state1,
                                dist2, state2,
                                methods, thresholds, mode = "full") {
  # Attach row IDs to track unmatched
  df1_id <- df1 %>% mutate(.id1 = row_number())
  df2_id <- df2 %>% mutate(.id2 = row_number())
  
  df1_curr <- df1_id
  df2_curr <- df2_id
  matched_all <- tibble()
  
  for(i in seq_along(methods)) {
    full_j <- stringdist_join(
      df1_curr, df2_curr,
      by = setNames(c(dist2, state2), c(dist1, state1)),
      mode = mode,
      method = methods[i],
      max_dist = thresholds[i],
      distance_col = "dist"
    )
    
    # Keep only pairs i.e., those with both sides present
    matched_i <- full_j %>% filter(!is.na(.id1) & !is.na(.id2))
    matched_all <- bind_rows(matched_all, matched_i)
    
    # Remove them from "to‑do" sets
    df1_curr <- df1_curr %>% filter(!(.id1 %in% matched_i$.id1))
    df2_curr <- df2_curr %>% filter(!(.id2 %in% matched_i$.id2))
  }
  
  list(
    joined = matched_all,
    unmatched_df1 = df1_curr %>% select(-.id1),
    unmatched_df2 = df2_curr %>% select(-.id2)
  )
}

# For each df name in input vector, extract its "01/05/..." suffix, and
# Find that suffix’s district_/state_ columns in joined_df. 
# Run fuzzy join sequence on each columns, and then 
# Repeat with the prior suffix.
merge_dfs_into_tracker <- function(
  df_names = get("df_names", envir = parent.frame()),
  tracker = get("district_tracker", envir = parent.frame()),
  years_of_interest = get("years_of_interest", envir = parent.frame()),
  flag = FALSE
) {
  # 1. Initialize both joined_df and (if flag=TRUE) flagged_df
  joined_df <- tracker
  if (flag) {
    flagged_df <- tracker
    # Prepare placeholder vector for each df-name
    matched_lists <- lapply(df_names, function(x)
      rep(NA_character_, nrow(tracker))
    )
    names(matched_lists) <- df_names
  }

  year_suffixes <- substr(as.character(years_of_interest), 3, 4)

  # 2. Loop over each auxiliary data-frame name
  for (d_name in df_names) {
    df_d <- get(d_name)
    suffix <- substr(d_name, nchar(d_name)-1, nchar(d_name))

    # Find exactly one district_XX / state_XX in df_d
    dist_d  <- grep("^district_", names(df_d), value=TRUE) %>%
                grep("code", ., invert=TRUE, value=TRUE)
    state_d <- grep("^state_",    names(df_d), value=TRUE) %>%
                grep("code", ., invert=TRUE, value=TRUE)
    stopifnot(length(dist_d)==1, length(state_d)==1)

    df_unmatched <- df_d
    if (flag) mv <- matched_lists[[d_name]]

    # suffix_chain sorted by absolute distance from said suffix
    year_nums  <- as.integer(year_suffixes)
    suffix_num <- as.integer(suffix)
    order_idx  <- order(abs(year_nums - suffix_num))
    suffix_chain <- year_suffixes[order_idx]

    # Work through suffixes, closest to furthest
    for (suf in suffix_chain) {
      dist_cols_j  <- grep(paste0("^district_", suf, "(?:$|_)"),
                           names(joined_df), value=TRUE)
      state_cols_j <- grep(paste0("^state_",    suf, "(?:$|_)"),
                           names(joined_df), value=TRUE)

      for (dist_col_j in dist_cols_j) {
        ending <- sub(paste0("^district_", suf), "", dist_col_j)
        state_col_j <- paste0("state_", suf, ending)
        if (!state_col_j %in% state_cols_j) next

        # 2a. Multi-threshold fuzzy join
        res <- fuzzy_join_sequence(
          df_unmatched, joined_df,
          dist1 = dist_d, 
          state1 = state_d,
          dist2 = dist_col_j,
          state2 = state_col_j,
          methods = methods,
          thresholds = thresholds,
          mode = "full"
        )

        # 2b. Pull other cols into joined_df
        other_cols <- setdiff(names(df_d), c(dist_d, state_d))
        if (nrow(res$joined)>0 && length(other_cols)>0) {
          extra <- res$joined %>%
            select(.id2, all_of(intersect(other_cols, names(res$joined)))) %>%
            distinct(.id2, .keep_all=TRUE)

          # Merge into joined_df
          joined_df <- joined_df %>%
            mutate(.row = row_number()) %>%
            left_join(extra, by = c(".row"=".id2")) %>%
            select(-.row)

          # If flag, merge into flagged_df
          if (flag) {
            flagged_df <- flagged_df %>%
              mutate(.row = row_number()) %>%
              left_join(extra, by = c(".row"=".id2")) %>%
              select(-.row)
          }
        }

        # 2c. If flag=TRUE, record which district got matched this iteration
        if (flag && nrow(res$joined)>0) {
          col_match <- grep(
            paste0("^", dist_d, "(?:$|\\.x$)"),
            names(res$joined), value=TRUE
          )[1]
          info <- res$joined[, c(".id2", col_match), drop=FALSE]
          colnames(info)[2] <- "match_dist"
          info <- info[!duplicated(info$.id2), ]

          idx <- info$.id2
          new_vals <- info$match_dist
          blank <- is.na(mv[idx])
          mv[idx[blank]] <- new_vals[blank]
        }

        # 2d. Update the df of unmathed
        df_unmatched <- res$unmatched_df1
        if (nrow(df_unmatched)==0) break
      }
      if (nrow(df_unmatched)==0) break
    }

    # 2e. Once done, append flag-column to flagged_df
    if (flag) {
      flagged_df[[d_name]] <- mv
    }
    if (nrow(df_unmatched)>0) {
      warning(sprintf("Some rows in %s were never matched", d_name))
    }
  }

  # 3. Finalize flagged_df if flag=TRUE
  if (flag) {
    max_suf <- year_suffixes[which.max(as.integer(year_suffixes))]
    keep_cols <- names(flagged_df)[
      (grepl("^district_", names(flagged_df)) & !grepl("code", names(flagged_df))) |
      names(flagged_df) %in% df_names |
      names(flagged_df) == paste0("state_", max_suf)
    ]
    flagged_df <- flagged_df[, keep_cols, drop=FALSE]
  }

  # 4. Return!!!
  if (flag) {
    list(
      joined_df = joined_df,
      unmatched_df = df_unmatched, 
      flagged_df = flagged_df
    )
  } else {
    list(
      joined_df = joined_df,
      unmatched_df = df_unmatched
    )
  }
}

