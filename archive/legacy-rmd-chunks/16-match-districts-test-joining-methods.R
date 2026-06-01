# Number of rows from fuzzy full joining district_tracker (734 rows) and mother_tongues_01 (593 rows) with different metrics:
# "osa": 859
# "lv": 859
# "dl": 859
# "hamming": 872
# "lcs": 825
# "qgram": 829
# "cosine":  435,262
# "jaccard": 435,262
# "jw": 435,262
# See https://cran.r-project.org/web/packages/stringdist/stringdist.pdf#page=23 for more



# Testing different methods of fuzzy joining

# With ("lcs","osa"), (3, 3):
# colSums(is.na(joined_df))
# 75 NAs in 01 columns, 124 in 07-08, 105 in 17-18
# sum(apply(joined_df, 1, anyNA))
# 189/734 rows have an NA

# With ("jw", "dl", "osa", "lcs"), (0.10, 2, 3, 5)
# colSums(is.na(joined_df))
# 70 in 01, 119 in 07-08, 98 in 17-18
# sum(apply(joined_df, 1, anyNA))
# 180/734

# methods <- c("soundex", "jw", "dl", "osa", "lcs")
# thresholds <- c(0, 0.15, 2, 1, 5)
# colSums(is.na(joined_df))
# 47 in 01, 105 in 07-08, 83 in 17-18
# sum(apply(joined_df, 1, anyNA))
# 158/734
# With lcs, the higher the better. Would need normalized version like 1 - lcs(a,b)/min(nchar(a),nchar(b))



# See ?roxygen2::`tags-rd` for info on how to do better comments before functions
evaluate_distances <- function(pairs,
                               methods, thresholds,
                               col1 = "str1", col2 = "str2") {
  if(length(methods) != length(thresholds)) {
    stop("The character vectors \"methods\" and \"thresholds\" must have the same length.")
  }
  # Ensure chr type
  pairs <- pairs %>%
    mutate(
      !!col1 := as.character(.data[[col1]]),
      !!col2 := as.character(.data[[col2]])
    )
  
  # Compute distances for each method
  out <- bind_rows(
    lapply(seq_along(methods), function(i) {
      m  <- methods[i]
      th <- thresholds[i]
      
      pairs %>%
        transmute(
          str1 = .data[[col1]],
          str2 = .data[[col2]],
          method = m,
          distance = stringdist(str1, str2, method = m),
          threshold = th,
          match = distance <= th
        )
    })
  )
  
  # Keep a consistent ordering
  out %>% arrange(str1, str2, method)
}

# Some sample pairs
pairs <- tribble(
  ~str1, ~str2,
  "Baleshwar", "Balasore",
  "Jammu & Kashmir", "Jammu and Kashmir",
  "East Godavari", "Godavari East",
  "Sikim", "Sikkim",
  "Mumbai", "Mumbai",
  "24-Parganas ( North )", "North Twenty Four Parganas",
  "North Twenty Four Pargan*", "North Twenty Four Parganas",
  "Sahibzada Ajit Singh Nag*", "Sahibzada Ajit Singh Nagar",
  "Sri Potti Sriramulu Nell*", "Sri Potti Sriramulu Nellore"
)


#### Helper vectors ####
methods <- c("soundex", "qgram", "jw", "dl", "osa")
thresholds <- c(0, 0, 0.15, 2, 1)
# soundex=0 --> Phonetic variants in anglicization allowed
# qgram = 0 --> Rearrangements of words allowed
# jw<=0.15 --> Respellings + vowel swaps with 0.85 similarity allowed
# dl<=2 --> <=2 insertions + deletions + substitutions + transpositions allowed
# osa<=1 --> 1 typo allowed


# colSums(is.na(joined_df))
# 53 in 01, 112 in 07-08, 89 in 17-18
# sum(apply(joined_df, 1, anyNA))
# 166/734


# Run it
# evaluate_distances(pairs, methods, thresholds) %>% View()


# stringdist("East Godavari", "Godavari East", method = "qgram")
# 0
# stringdist("East Godavari", "Godavari East", method = "jaccard")
# 0


