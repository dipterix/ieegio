# Read 'EDF' or 'BDF' data file

Read 'EDF' or 'BDF' data file

## Usage

``` r
read_edf(
  con,
  extract_path = getOption("ieegio.extract_path", NULL),
  header_only = FALSE,
  cache_ok = TRUE,
  begin = 0,
  end = Inf,
  convert = TRUE,
  verbose = TRUE
)
```

## Arguments

- con:

  file or connection to the data file

- extract_path:

  location to where the extracted information is to be stored

- header_only:

  whether to only load header data

- cache_ok:

  whether existing cache should be reused; default is `TRUE`. This input
  can speed up reading large data files; set to `FALSE` to delete cache
  before importing.

- begin, end:

  begin and end of the data to read

- convert:

  whether to convert digital numbers to analog signals; default is
  `TRUE`

- verbose:

  whether to print processing messages; default is `TRUE`

## Value

A cached object that is readily to be loaded to memory; see
[`SignalDataCache`](http://dipterix.org/ieegio/reference/SignalDataCache.md)
for class definition.

## Examples

``` r
# ---- EDF/BDF(+) ---------------------------------------------------------

# Run `ieegio_sample_data("edfPlusD.edf")` to download sample data

# Tun example if the sample data exists
if(ieegio_sample_data("edfPlusD.edf", test = TRUE)) {

  edf_path <- ieegio_sample_data("edfPlusD.edf")

  data <- read_edf(edf_path)

  data$get_header()

  data$get_annotations()

  data$get_channel_table()

  channel <- data$get_channel(1)

  plot(
    channel$time,
    channel$value,
    type = "l",
    main = channel$info$Label,
    xlab = "Time",
    ylab = channel$info$Unit
  )

}


```
