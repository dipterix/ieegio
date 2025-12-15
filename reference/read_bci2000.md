# Read 'BCI2000' data file

Read 'BCI2000' data file

## Usage

``` r
read_bci2000(
  file,
  extract_path = getOption("ieegio.extract_path", NULL),
  header_only = FALSE,
  cache_ok = TRUE,
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

- verbose:

  whether to print processing messages; default is `TRUE`

## Value

A cached object that is readily to be loaded to memory; see
[`SignalDataCache`](http://dipterix.org/ieegio/reference/SignalDataCache.md)
for class definition.

## Examples

``` r

if( ieegio_sample_data("bci2k.dat", test = TRUE) ) {
  file <- ieegio_sample_data("bci2k.dat")

  x <- read_bci2000(file)
  print(x)

  channel <- x$get_channel(1)

  plot(
    channel$time,
    channel$value,
    type = "l",
    main = channel$info$Label,
    xlab = "Time",
    ylab = channel$info$Unit
  )
}
#> <ieegio::BCI2000Cache>
#> HeaderLen=  8189 SourceCh= 64 StatevectorLen= 15
#> [State Definitions]:
#>   <BCI2000 State Definitions>
#>     [BCIStateDef: Running    ] Len=8, ByteLoc=0, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: Active ] Len=8, ByteLoc=1, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: SourceTime ] Len=16, ByteLoc=2, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: RunActive  ] Len=8, ByteLoc=4, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: Recording  ] Len=8, ByteLoc=5, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: IntCompute ] Len=8, ByteLoc=6, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: ResultCode ] Len=8, ByteLoc=7, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: StimulusTime   ] Len=16, ByteLoc=8, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: Feedback   ] Len=8, ByteLoc=10, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: RestPeriod ] Len=8, ByteLoc=11, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: StimulusCode   ] Len=8, ByteLoc=12, BitLoc=0, DefaultValue=0
#>     [BCIStateDef: StimulusBegin  ] Len=8, ByteLoc=13, BitLoc=0, DefaultValue=0
#>   <BCI2000 Data Definitions>
#>     Data Type: int16
#>     # of Channels: 64
#>     Each Sample Contains: 143 Bytes
#>     # of Samples read: 19696
#> [Parameter Definitions]
#>   <BCIParameters>
#>     - Storage
#>     - Filtering
#>     - Visualize
#>     - Source
#>     - UsrTask
#>     - System
#>     - save_bcidat
#>     - Statistics
#>     - MEMFilter


```
