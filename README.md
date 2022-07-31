# rren: Renaissance Benchmark R Utilities

[![check](https://github.com/renaissance-benchmarks/utilities-r/actions/workflows/check.yaml/badge.svg)](https://github.com/renaissance-benchmarks/utilities-r/actions/workflows/check.yaml)
[![coverage](https://codecov.io/gh/renaissance-benchmarks/utilities-r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/renaissance-benchmarks/utilities-r?branch=main)

## Overview

rren packages various functions useful for processing measurements collected using the [Renaissance Benchmark Suite](https://renaissance.dev).

## Usage

Use `load_file_json ()` or `load_path_json ()` to load measurement data into a [tibble](https://github.com/tidyverse/tibble).
The functions support multiple measurement data file versions and normalize basic timing columns for easier processing.
