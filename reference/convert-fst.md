# Convert 'FST' files to other formats

'HDF5', 'CSV' are common file formats that can be read into 'Matlab' or
'Python'

## Usage

``` r
convert_fst_to_hdf5(fst_path, hdf5_path, exclude_names = NULL)

convert_fst_to_csv(fst_path, csv_path, exclude_names = NULL)
```

## Arguments

- fst_path:

  path to 'FST' file

- hdf5_path:

  path to 'HDF5' file; if file exists before the conversion, the file
  will be erased first. Please make sure the files are backed up.

- exclude_names:

  table names to exclude

- csv_path:

  path to 'CSV' file; if file exists before the conversion, the file
  will be erased first. Please make sure the files are backed up.

## Value

`convert_fst_to_hdf5` will return a list of data saved to 'HDF5';
`convert_fst_to_csv` returns the normalized 'CSV' path.
