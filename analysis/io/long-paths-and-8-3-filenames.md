# Long Paths and 8.3 Filename Troubleshooting


## Legacy comments

### Legacy Chunk 3: long-path troubleshooting and design notes

—TROUBLESHOOTING— If getting “file does not exist” or similar errors:
Switch the function used to read in the file e.g., if the relevant file
is being read in using read_sav() or read_sav_short(), switch to
read_sav_short() or read_sav() respectively.

If that didn’t work and you’re not using Windows: Either modify this
code chunk for your OS (note the “if” statement at the start of each
function) or manually shorten the error-inducing file paths.

------------------------------------------------------------------------

Purpose of this chunk: To ensure R can identify and read in all
necessary files. This project found me making frequent use of file paths
over 260 characters in length. Out of my naivete, I didn’t realize
Windows File Explorer automatically shortens such file paths by
rewriting them in the 8.3 filename convention—meaning R can no longer
access their standard “long name.” Conversely, functions like read_sav()
automatically lengthen 8.3 filenames via functions like normalizePath(),
turning the true 8.3 file path into a long name which, from R’s and
(thus our) perspective, is useless. This code chunk turns our long name
inputs into 8.3 filenames while tricking the read\_\*()-type functions
into not turning them back.

Context for nerds: In my Registry Editor, under Computer\_LOCAL_MACHINE,
I had set the LongPathsEnabled value to 1 (which enables Windows to use
long file paths) and the NtfsDisable8dot3NameCreation value to 2. I had
also changed Group Policy to “Enable Win32 long paths”.

``` r
But none of that mattered! Windows File Explorer lacks a longPathAware entry in its .exe file's manifest--so despite LongPathsEnabled=1, my file paths were still capped at 260 characters. Ergo this code chunk.
It's possible that setting NtfsDisable8dot3NameCreation=0 would've prevented all of this code from being necessary. But I'd rather not force replicators to change their registry settings.
```

------------------------------------------------------------------------

Goal: Make an alternative read_sav() which accepts 8.3 filenames

``` r
shell() only exists in Windows; so if not running on Windows, just use read_sav() normally
```

Get the 8.3 alias of the file’s absolute path from shell

Open a connection (“con”) in binary mode (“rb” = “read binary”; SPSS
files are binary i.e. non-text files, and thus best read in as such) to
the 8.3 filename

Save space by closing the connection when this function finishes
executing

Pass this open connection to read_sav() so it doesn’t convert the 8.3
filename back into the long filename

``` r
There's something weird happening here. In my File Explorer, the entire absolute path is not an 8.3 alias: only the relative path (the path from the working directory, or here, the names of the file and the folder containing it) is. Yet this code only works because it feeds a connection, opened via file() using an absolute path's 8.3 alias, directly into read_sav(). This should *automatically happen in read_sav() anyways*!!! read_sav(path) calls datasource(path) calls readr:::standardise_path(path) (an internal function, see ?`:::` and https://github.com/tidyverse/readr/blob/96ddac314b47402bc63e1f81c149c463cf58e3da/R/source.R#L119) calls readr:::detect_compression(path) calls readBin(path) calls file(path, "rb") when 'path' is a string e.g., when 'path' is a file path instead of a connection. This file(..., "rb") call is exactly what happens to 'con' in read_sav_short()! Yet it is precisely at file(..., "rb") where read_sav() fails when called on an 8.3 file path! This is true when the whole absolute path is 8.3 (as in read_sav_short()) or when only the relative path is (as in my File Explorer).
```

------------------------------------------------------------------------

Goal: Likewise, now for reading in CSV files

``` r
shell() only exists in Windows
```

------------------------------------------------------------------------

For Excel files

Note: read_excel() only accepts file paths as strings, not connections
(see https://github.com/tidyverse/readxl/blob/main/R/read_excel.R#L183
and
https://github.com/tidyverse/readxl/blob/2a7f6efd8562ca5beba0b34933c8e4a9332ed6a8/src/Read.cpp#L86).
It seems like all similar functions are the same, too.

``` r
shell() only exists in Windows
```

Split long_path into its two components: the folder and the file

Extract the 8.3 alias of the folder’s absolute path

There seems to be no similar way to get the file’s 8.3 alias safely. To
bypass this, we run a directory call on the folder’s 8.3 name, which
lists out all the files in the folder, identified by both their long and
short names.

Identify the relevant file’s row as the one which includes an exact
match (ergo “fixed”, meaning no regex parsing) to long_file and then
store it (ergo “value”).

To identify the file’s 8.3 name from this row, see
https://tomgalvin.uk/blog/gen/2015/06/09/filenames/: a file name of 8
characters or more is upper-cased, stripped of its invalid characters
(e.g., spaces), truncated to the first 6 characters of its basename, and
then followed by a tilde, a single digit, and the first 3 characters of
its upper-cased file extension en route to becoming an 8.3 filename.

Note that, excluding the working directory, 8.3 filenames must be weakly
less than 8 characters. Ergo “{1,8}”

I’m not entirely sure why this code is working so well; just in case,
I’m including error codes for your benefit.

Extract the file’s 8.3 alias from this match data

Construct the absolute short path using short names

Copy the file to a temporary one so that our file readers won’t
re-normalize its path

In case your computer is messing up in ways mine wasn’t.

Using read_excel() would return an error from libxls, the C library. So
we turn to other packages instead. Only use xlsx package to read in .xls
to limit the need for Java

``` r
Load the packages via namespace so they only need to be installed if needed. Use xlsx::read.xlsx for .xls files.
```

Read in from the temporary file

If needed namespace is installed, read in the file

Repeat

**Deviation note.** The prose above is rendered from the legacy
comments, with comment markers removed. The code below shows the current
production helper implementation in `R/io/read_long_paths.R`, so the
long explanation can live in `analysis/` rather than as lengthy inline
comments in production code.

## Current helper implementation

``` r
# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# ---TROUBLESHOOTING---
# If getting "file does not exist" or similar errors:
# Switch the function used to read in the file e.g., if the relevant file is being read in using read_sav() or read_sav_short(), switch to read_sav_short() or read_sav() respectively.
# If that didn't work and you're not using Windows: Either modify this code chunk for your OS (note the "if" statement at the start of each function) or manually shorten the error-inducing file paths.
# Purpose of this chunk: To ensure R can identify and read in all necessary files.
# This project found me making frequent use of file paths over 260 characters in length.
# Out of my naivete, I didn't realize Windows File Explorer automatically shortens such file paths by rewriting them in the 8.3 filename convention---meaning R can no longer access their standard "long name."

#' read with short path
#'
#' @return Reader output from the supplied path.
read_with_short_path <- function(path, reader, ..., binary_connection = TRUE) {
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  if (Sys.info()[["sysname"]] != "Windows") return(reader(path, ...))
  short <- get_windows_short_path(path)
  if (!binary_connection) return(reader(short, ...))
  # Open a connection ("con") in binary mode ("rb" = "read binary"; SPSS files are binary i.e. non-text files, and thus best read in as such) to the 8.3 filename
  con <- file(short, "rb")
  on.exit(close(con), add = TRUE)
  reader(con, ...)
}

#' read sav short
#'
#' @return Data frame read from an SPSS file.
read_sav_short <- function(long_path, ...) {
  need_pkg("haven", "SPSS files")
  read_with_short_path(long_path, haven::read_sav, ...)
}

#' read csv short
#'
#' @return Data frame read from a CSV file.
read_csv_short <- function(long_path, ...) {
  if (requireNamespace("readr", quietly = TRUE)) {
    read_with_short_path(
      long_path,
      readr::read_csv,
      ...,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )
  } else {
    utils::read.csv(long_path, stringsAsFactors = FALSE, colClasses = "character", ...)
  }
}

#' read excel short
#'
#' @return Data frame read from an Excel file.
read_excel_short <- function(long_path, sheet = 1, col_types = "text", ...) {
  need_pkg("readxl", "Excel files")
  readxl::read_excel(
    normalize_path_for_os(long_path),
    sheet = sheet,
    col_types = col_types,
    .name_repair = "unique",
    ...
  )
}

#' read ODS using a path normalized for this OS
#'
#' @return Data frame read from an ODS file.
read_ods_short <- function(long_path, ...) {
  need_pkg("readODS", "ODS files")
  readODS::read_ods(long_path, ...)
}

#' normalize path for os
#'
#' @return Normalized path string.
normalize_path_for_os <- function(path) {
  normalizePath(path, mustWork = FALSE)
}

#' get windows short path
#'
#' @return Windows 8.3 short path on Windows, otherwise the original path.
get_windows_short_path <- function(long_path) {
  if (Sys.info()[["sysname"]] != "Windows") return(long_path)
  shell(sprintf('for %%I in ("%s") do @echo %%~sI', long_path), intern = TRUE)[[1]]
}
```
