# rren: Renaissance Benchmark R Utilities

[![check](https://github.com/renaissance-benchmarks/utilities-r/actions/workflows/check.yaml/badge.svg)](https://github.com/renaissance-benchmarks/utilities-r/actions/workflows/check.yaml)

## Overview

rren packages various functions useful for processing measurements collected using the [Renaissance Benchmark Suite](https://renaissance.dev).

## Installation

```r
install.packages ('pak')
pak::pak ('renaissance-benchmarks/utilities-r')
```

## Usage

A simple summary report can be generated from JSON data with `Rscript -e 'rren::report_summaries ()'` run in the data path.

```console
> java -jar renaissance-gpl-0.15.0.jar --json scrabble.json scrabble
====== scrabble (functional) [default], iteration 0 started ======
GC before operation: completed in 62.114 ms, heap usage 188.729 MB -> 77.885 MB.
...
> Rscript -e 'rren::report_summaries (warmup = 0)'
ℹ Loading data from .

── OpenJDK 64-Bit Server VM 17.0.8+7 (cd7b7eea) ────────────────────────────────

── scrabble ──

metric     mean                       median
time        388 m (371 m - 415 m)      373 m (363 m - 392 m)

────────────────────────────────────────────────────────────────────────────────
```

Use `load_file_json ()` or `load_path_json ()` to load measurement data into a [tibble](https://github.com/tidyverse/tibble).
The functions support multiple measurement data file versions and normalize basic timing columns for easier processing.
