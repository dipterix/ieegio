# Install `'NWB'` via `'pynwb'`

Install `'NWB'` via `'pynwb'`

## Usage

``` r
install_pynwb(python_ver = "auto", verbose = TRUE)

pynwb_module(force = FALSE, error_if_missing = TRUE)
```

## Arguments

- python_ver:

  'Python' version, see
  [`configure_conda`](http://dipterix.org/rpymat/reference/conda-env.md);
  default is `"auto"`, which is suggested

- verbose:

  whether to print the installation messages

- force:

  whether to force-reload the module

- error_if_missing:

  whether to raise errors when the module fails to load; default is true

## Value

A 'Python' module `pynwb`.
