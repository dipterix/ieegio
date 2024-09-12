## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

Respond to the `CRAN` comment:

```
If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")
```

Currently I don't have any publication that can be referenced in the `DESCRIPTION` file. The scripts are mostly derived from my personal code base.

```
You have examples for unexported functions. Please either omit these examples or export these functions.
Examples for unexported function
  io_write_fst() in:
     read_nwb.Rd
```

Thanks, function `io_write_fst` has been exported. There is no `io_write_fst()` call in `read_nwb.Rd` now.


```
Only functions which are supposed to only run interactively (e.g. shiny) should be wrapped in if(interactive()). Please replace if(interactive()){} if possible.
-> ieegio_sample_data.Rd; low-level-read-write.Rd
```

Thanks, I removed all `if(interactive()){}` (2 sites), and wrapped with `dontrun` as these examples require downloading external data or additional configurations.


```
Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.
e.g.:
...
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar)) # code line i + 1
...
par(mfrow=c(2,2)) # somewhere after
...

e.g.: -> R/samples.R
If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.
```

Thanks, I added `on.exit` as instructed except for `ieegio_debug()`. This function is designed to change the option `ieegio.debug`, which should be controlled by this package.

```
Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
-> R/samples.R; R/edf.R
```

Thanks. Those code are commented out. More specifically function `update_sample_registry` was removed (commented). I don't find anything in `R/edf.R` that writes to home or working directories. Maybe I missed it. My doubt is some (already) commented code such as Line-648 triggered this warning?
