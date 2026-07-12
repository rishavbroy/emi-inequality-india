# Long Paths and 8.3 Filename Troubleshooting


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy diagnostic intent

The legacy project carried lengthy troubleshooting comments about
Windows long paths, 8.3 short filenames, and reader fallbacks. The
current production implementation is in `R/io/read_long_paths.R`; this
analysis note keeps the troubleshooting material out of production code
while rendering runnable current-code analogs directly.

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
    1 /private/var/folders/v0/7rc_jjhs6dv8gzmtnmqtpg3w0000gn/T/RtmptGqpjE/file405944c4a310.csv
    2        /var/folders/v0/7rc_jjhs6dv8gzmtnmqtpg3w0000gn/T//RtmptGqpjE/file405944c4a310.csv
    3                                                                                        x
