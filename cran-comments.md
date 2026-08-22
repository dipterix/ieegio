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

This is because an upstream package `hdf5r` fails to compile on those platforms. 

I have create a pull-request (https://github.com/hhoeflin/hdf5r/pull/245) to solve this issue and fix the bugs.

This package itself contains no changes that worsen the condition.
