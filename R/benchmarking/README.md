# Benchmarking helpers

This directory is reserved for opt-in tuning and benchmarking code ported from the legacy Rmd.
Targets that use these helpers should be named `bench_*` and should write to `outputs/benchmarking/`.
They are not part of the ordinary public build unless requested with `--with-benchmarks` or `make benchmarking`.
