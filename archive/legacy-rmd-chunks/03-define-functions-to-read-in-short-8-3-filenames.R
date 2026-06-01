# ---TROUBLESHOOTING---
# If getting "file does not exist" or similar errors:
# Switch the function used to read in the file e.g., if the relevant file is being read in using read_sav() or read_sav_short(), switch to read_sav_short() or read_sav() respectively.

# If that didn't work and you're not using Windows: 
# Either modify this code chunk for your OS (note the "if" statement at the start of each function) or manually shorten the error-inducing file paths.

# ---

# Purpose of this chunk: 
# To ensure R can identify and read in all necessary files.
# This project found me making frequent use of file paths over 260 characters in length. Out of my naivete, I didn't realize Windows File Explorer automatically shortens such file paths by rewriting them in the 8.3 filename convention---meaning R can no longer access their standard "long name." Conversely, functions like read_sav() automatically lengthen 8.3 filenames via functions like normalizePath(), turning the true 8.3 file path into a long name which, from R's and (thus our) perspective, is useless.
# This code chunk turns our long name inputs into 8.3 filenames while tricking the read_*()-type functions into not turning them back.


# Context for nerds:
# In my Registry Editor, under Computer\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\FileSystem, I had set the LongPathsEnabled value to 1 (which enables Windows to use long file paths) and the NtfsDisable8dot3NameCreation value to 2. I had also changed Group Policy to "Enable Win32 long paths".
# But none of that mattered! Windows File Explorer lacks a longPathAware entry in its .exe file's manifest--so despite LongPathsEnabled=1, my file paths were still capped at 260 characters. Ergo this code chunk.
# It's possible that setting NtfsDisable8dot3NameCreation=0 would've prevented all of this code from being necessary. But I'd rather not force replicators to change their registry settings.

# ---

# Goal: Make an alternative read_sav() which accepts 8.3 filenames

read_sav_short <- function(long_path, ...) {
  
  # shell() only exists in Windows; so if not running on Windows, just use read_sav() normally
  if(Sys.info()['sysname'] != "Windows"){
    return(read_sav(long_path, ...))
  }
  
  # Get the 8.3 alias of the file's absolute path from shell
  short_path <- shell(
    sprintf('for %%I in ("%s") do @echo %%~sI', long_path), 
    intern = TRUE
    ) # Note: Use double percent signs (%%) to prevent CMD shell from thinking each percent sign is a delimiter for a variable name.
 
  # Open a connection ("con") in binary mode ("rb" = "read binary"; SPSS files are binary i.e. non-text files, and thus best read in as such) to the 8.3 filename
  con <- file(short_path, "rb")
  
  # Save space by closing the connection when this function finishes executing
  on.exit(close(con))
  
  # Pass this open connection to read_sav() so it doesn't convert the 8.3 filename back into the long filename
  data <- read_sav(con, ...)
  return(data)
  
  # There's something weird happening here. In my File Explorer, the entire absolute path is not an 8.3 alias: only the relative path (the path from the working directory, or here, the names of the file and the folder containing it) is. Yet this code only works because it feeds a connection, opened via file() using an absolute path's 8.3 alias, directly into read_sav(). This should *automatically happen in read_sav() anyways*!!! read_sav(path) calls datasource(path) calls readr:::standardise_path(path) (an internal function, see ?`:::` and https://github.com/tidyverse/readr/blob/96ddac314b47402bc63e1f81c149c463cf58e3da/R/source.R#L119) calls readr:::detect_compression(path) calls readBin(path) calls file(path, "rb") when 'path' is a string e.g., when 'path' is a file path instead of a connection. This file(..., "rb") call is exactly what happens to 'con' in read_sav_short()! Yet it is precisely at file(..., "rb") where read_sav() fails when called on an 8.3 file path! This is true when the whole absolute path is 8.3 (as in read_sav_short()) or when only the relative path is (as in my File Explorer).
}

# ---

# Goal: Likewise, now for reading in CSV files

read_csv_short <- function(long_path, ...){
  
  # shell() only exists in Windows
  if(Sys.info()['sysname'] != "Windows"){
    return(read_csv(long_path, ...))
  }
  
  short_path <- shell(
    sprintf('for %%I in ("%s") do @echo %%~sI', long_path),
    intern = TRUE
  )
  
  con <- file(short_path, "rb") # CSVs are text, not binary, files. But read_csv() feeds its inputs into vroom() (https://github.com/tidyverse/readr/blob/96ddac314b47402bc63e1f81c149c463cf58e3da/R/read_delim.R#L253), which apparently can only read from a binary connection? Otherwise vroom() calls vroom_() (https://github.com/tidyverse/vroom/blob/73c90c4fe490c0588b20ac527c40fcb1c683683e/src/vroom.cc) which in turn calls a function which looks very similar to the source code of readBin() (https://github.com/SurajGupta/r-source/blob/a28e609e72ed7c47f6ddfbb86c85279a0750f0b7/src/library/base/R/connections.R#L219). The vroom package seems to only call readBin() at vroom:::R_ReadConnection() (https://github.com/tidyverse/vroom/blob/73c90c4fe490c0588b20ac527c40fcb1c683683e/src/connection.h#L52), though I'm not sure how the call chain could go from vroom_() to R_ReadConnection()--nor am I sure as to why read_csv() reads in text files as binary. Perhaps read_csv() reads in the raw bytes using readBin() before applying its own parser to the text?
  on.exit(close(con))
  
  data <- read_csv(con, ...)
  return(data)
}

# ---

# For Excel files

# Note: read_excel() only accepts file paths as strings, not connections (see https://github.com/tidyverse/readxl/blob/main/R/read_excel.R#L183 and https://github.com/tidyverse/readxl/blob/2a7f6efd8562ca5beba0b34933c8e4a9332ed6a8/src/Read.cpp#L86). It seems like all similar functions are the same, too.

read_excel_short <- function(long_path, sheet = 1, ...){ # We want sheet = 1 to be the default for all commands within this function 
  
  # shell() only exists in Windows
  if(Sys.info()['sysname'] != "Windows"){
    return(read_excel(long_path, sheet = sheet, ...))
  }
  
  # Split long_path into its two components: the folder and the file
  long_folder <- dirname(long_path)
  long_file <- basename(long_path)
  
  # Extract the 8.3 alias of the folder's absolute path
  short_folder <- shell(
    sprintf('for %%I in ("%s") do @echo %%~sI', long_folder), 
    intern = TRUE
    )
  
  # There seems to be no similar way to get the file's 8.3 alias safely. To bypass this, we run a directory call on the folder's 8.3 name, which lists out all the files in the folder, identified by both their long and short names.
  dir_output <- shell(
    sprintf('dir /x "%s"', short_folder),
    intern = TRUE
  )
  
  # Identify the relevant file's row as the one which includes an exact match (ergo "fixed", meaning no regex parsing) to long_file and then store it (ergo "value"). 
  candidate_line <- grep(long_file, dir_output, value = TRUE, fixed = TRUE) # grep() searches through a vector of strings and returns the indices or values of elements which contain a match.

  
  
  # To identify the file's 8.3 name from this row, see https://tomgalvin.uk/blog/gen/2015/06/09/filenames/: a file name of 8 characters or more is upper-cased, stripped of its invalid characters (e.g., spaces), truncated to the first 6 characters of its basename, and then followed by a tilde, a single digit, and the first 3 characters of its upper-cased file extension en route to becoming an 8.3 filename.
  
  pattern <- "([A-Z0-9-]{1,8}~[0-9]\\.[A-Z0-9]{1,3})" # "(...)" = capturing group, though only used here for cleanliness; "[A-Z0-9]{1,8}" = match with 1-8 uppercase letters or digits; "~" = match with a tilde; "[0-9]" = match with exactly one digit; "\\." = match with a period (as "\" is an escape character in both R and regex, meaning "\\." in R = "\." in regex = "." in text)
  # Note that, excluding the working directory, 8.3 filenames must be weakly less than 8 characters. Ergo "{1,8}"
  
  match <- regexpr(pattern, candidate_line, perl = TRUE) #regexpr() searches a single string and returns an integer vector indicating the starting position of the first match and the length of the matched text. Perl-style matching is used for its speed (per the regexpr() documentation) and flexibility (see https://gist.github.com/CMCDragonkai/6c933f4a7d713ef712145c5eb94a1816) over the default POSIX-style matching.
  
  # I'm not entirely sure why this code is working so well; just in case, I'm including error codes for your benefit.
  if (match[1] == -1){
    stop("Could not find the file's 8.3 alias from the folder's directory listing.")
  }
  
  # Extract the file's 8.3 alias from this match data
  short_file <- regmatches(candidate_line, match)
  
  # Construct the absolute short path using short names
  abs_short <- file.path(short_folder, short_file)
  
  # Copy the file to a temporary one so that our file readers won’t re-normalize its path
  ext <- tolower(tools::file_ext(short_file)) # Extract the file's extension
  temp_file <- tempfile(fileext = paste0(".", ext))
  
  # In case your computer is messing up in ways mine wasn't.
  if (!file.copy(abs_short, temp_file, overwrite = TRUE)){
    stop("File failed to copy from its short path to a temporary location.")
  }
  
  # Using read_excel() would return an error from libxls, the C library. So we turn to other packages instead.
  # Only use xlsx package to read in .xls to limit the need for Java
  # Load the packages via namespace so they only need to be installed if needed. Use xlsx::read.xlsx for .xls files.
  
  # Read in from the temporary file
  if (ext %in% c("xls", "xlsm")) {
    
    # If needed namespace is installed, read in the file
    if (!requireNamespace("xlsx", quietly = TRUE)) 
      stop("Install the \"xlsx\" package and Java (see https://www.java.com/en/download/manual.jsp).")
    data <- xlsx::read.xlsx(temp_file, sheetIndex = sheet, ...)
  } 
  
  # Repeat
  else if (ext == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)){
      stop("Install the \"openxlsx\" package.")
    }
    data <- openxlsx::read.xlsx(temp_file, sheet = sheet, ...)
  } 
  
  return(data)
  
}

