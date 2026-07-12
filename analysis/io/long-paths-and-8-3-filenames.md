# Long Paths and 8.3 Filename Troubleshooting


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

—TROUBLESHOOTING— If getting “file does not exist” or similar errors:
switch the function used to read in the file; for example, if the
relevant file is being read in using `read_sav()` or `read_sav_short()`,
switch to `read_sav_short()` or `read_sav()` respectively.

If that didn’t work and you’re not using Windows: either modify this
code chunk for your OS or manually shorten the error-inducing file
paths.

Purpose of this chunk: to ensure R can identify and read in all
necessary files. This project found me making frequent use of file paths
over 260 characters in length. Out of my naivete, I didn’t realize
Windows File Explorer automatically shortens such file paths by
rewriting them in the 8.3 filename convention—meaning R can no longer
access their standard “long name.” Conversely, functions like
`read_sav()` automatically lengthen 8.3 filenames via functions like
`normalizePath()`, turning the true 8.3 file path into a long name
which, from R’s and thus our perspective, is useless. This code chunk
turns our long name inputs into 8.3 filenames while tricking the
`read_*()`-type functions into not turning them back.

Goal: make alternative readers which accept 8.3 filenames. The current
production implementation is in `R/io/read_long_paths.R`; this analysis
note keeps the troubleshooting material out of production code while
rendering runnable current-code analogs directly.

``` r
analysis_deviation_note("The legacy comment included extensive Windows Registry and readr/vroom internals notes. The rendered note preserves the operational troubleshooting prose and runs the current production reader contract; full implementation details live in R/io/read_long_paths.R.")
```

**Deviation note.** The legacy comment included extensive Windows
Registry and readr/vroom internals notes. The rendered note preserves
the operational troubleshooting prose and runs the current production
reader contract; full implementation details live in
R/io/read_long_paths.R.

``` r
source(analysis_path("R", "io", "read_long_paths.R"))
read_with_short_path
```

    function (path, reader, ..., binary_connection = TRUE) 
    {
        if (!file.exists(path)) 
            stop("File does not exist: ", path, call. = FALSE)
        if (Sys.info()[["sysname"]] != "Windows") 
            return(reader(path, ...))
        short <- get_windows_short_path(path)
        if (!binary_connection) 
            return(reader(short, ...))
        con <- file(short, "rb")
        on.exit(close(con), add = TRUE)
        reader(con, ...)
    }

``` r
get_windows_short_path
```

    function (long_path) 
    {
        if (Sys.info()[["sysname"]] != "Windows") 
            return(long_path)
        shell(sprintf("for %%I in (\"%s\") do @echo %%~sI", long_path), 
            intern = TRUE)[[1]]
    }

``` r
normalize_path_for_os
```

    function (path) 
    {
        normalizePath(path, mustWork = FALSE)
    }

``` r
tmp <- tempfile(fileext = ".csv")
write.csv(data.frame(x = 1:2), tmp, row.names = FALSE)
read_csv_short(tmp)
```

    # A tibble: 2 × 1
      x    
      <chr>
    1 1    
    2 2    

``` r
data.frame(
  current_code_analog = c(
    "normalize_path_for_os(tmp)",
    "get_windows_short_path(tmp)",
    "read_csv_short(tmp)"
  ),
  result = c(
    normalize_path_for_os(tmp),
    get_windows_short_path(tmp),
    paste(names(read_csv_short(tmp)), collapse = ", ")
  )
)
```

              current_code_analog
    1  normalize_path_for_os(tmp)
    2 get_windows_short_path(tmp)
    3         read_csv_short(tmp)
                                                                                        result
    1 /private/var/folders/v0/7rc_jjhs6dv8gzmtnmqtpg3w0000gn/T/RtmpIoxBjc/filefd5563dfc7fb.csv
    2        /var/folders/v0/7rc_jjhs6dv8gzmtnmqtpg3w0000gn/T//RtmpIoxBjc/filefd5563dfc7fb.csv
    3                                                                                        x
