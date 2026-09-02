## R CMD check results

0 errors | 0 warnings | 0 notes

Current `CRAN` check reported:

```
Version: 0.1.0
Check: package dependencies
Result: ERROR 
  Packages required but not available: 'hdf5r', 'readNSx'
  
  See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
  manual.
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-release-linux-x86_64
```

Two changes were made to the package to fix this issue:

1. Removed `hdf5r` as a dependency. Added multiple back-ends for `HDF5` support, including `hdf5r`, `h5lite`, and `readNSx` (just submitted to `CRAN`). The back-end is automatically detected at run time, and the user can choose which back-end to use. 

To accommodate this change, the `Suggests` field in the `DESCRIPTION` file was updated to include `h5lite`. Currently `hdf5r` is removed from `Suggests` and is disabled, but it will be added back once the upstream issue is resolved.

2. I have created a pull-request (https://github.com/hhoeflin/hdf5r/pull/245) to solve multiple bugs that prevent that package from passing the CRAN checks.
