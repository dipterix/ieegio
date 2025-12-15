# Read ('BlackRock') 'NEV' 'NSx' data

Read ('BlackRock') 'NEV' 'NSx' data

## Usage

``` r
read_nsx(
  file,
  extract_path = getOption("ieegio.extract_path", NULL),
  header_only = FALSE,
  cache_ok = TRUE,
  include_waveform = FALSE,
  verbose = TRUE
)
```

## Arguments

- file:

  file path to the data file

- extract_path:

  location to where the extracted information is to be stored

- header_only:

  whether to only load header data

- cache_ok:

  whether existing cache should be reused; default is `TRUE`. This input
  can speed up reading large data files; set to `FALSE` to delete cache
  before importing.

- include_waveform:

  whether to include 'waveform' data (usually for online spike sorting);
  default is `FALSE`

- verbose:

  whether to print processing messages; default is `TRUE`

## Value

A cached object that is readily to be loaded to memory; see
[`SignalDataCache`](http://dipterix.org/ieegio/reference/SignalDataCache.md)
for class definition.
