# python

# Internals
rpymat_is_setup <- function() {
  return( dir.exists(rpymat::env_path()) )
}

py_capture_output <- function(...) {
  rpymat::ensure_rpymat(verbose = FALSE)
  # rpymat hard-depends on reticulate so no need to include reticulate in imports
  asNamespace("reticulate")$py_capture_output(...)
}

# Installs
validate_python <- function(verbose = TRUE) {
  verb <- function(expr) {
    if(verbose) {
      force( expr )
    }
  }
  verb(message("Initializing python environment: "))

  rpymat::ensure_rpymat(verbose = verbose)

  verb(message("Searching installed packages..."))
  tbl <- rpymat::list_pkgs()
  pkgs <- tbl$package
  pkgs <- pkgs[grepl("^[a-zA-Z0-9]", pkgs)]

  # verb(cat("Installed packages:", paste(pkgs, collapse = ", "), "\n"))

  # Check environment
  verb(message("Validating packages..."))

  package_missing <- NULL
  for(package in c("pynwb")) {
    tryCatch({
      verb({ cat(sprintf("%s: ...", package)) })
      module <- rpymat::import(package)
      ver <-rpymat::py_to_r( module$`__version__`)
      verb({ cat("\b\b\b", ver, "\n", sep = "") })
    }, error = function(e) {
      verb({ cat("\b\b\b N/A   \n", sep = "") })
      package_missing <<- c(package_missing, package)
    })
  }

  return(invisible(package_missing))
}

.pynwb <- local({

  pynwb <- NULL

  get_pynwb <- function(force = FALSE, error_if_missing = TRUE) {
    if(!force && inherits(pynwb, "python.builtin.module")) {
      return( pynwb )
    }
    if( !rpymat_is_setup() ) {
      if( error_if_missing ) {
        stop("Please configure environment first. Run the following command:\n   ieegio::install_pynwb()")
      }
      return( NULL )
    }
    tryCatch({
      rpymat::ensure_rpymat(verbose = FALSE)
      m <- rpymat::import("pynwb", convert = FALSE, delay_load = FALSE, as = "pynwb")
      class(m) <- c('nwb.proxy', class(m))
      pynwb <<- m
      return( pynwb )
    }, error = function(e) {
      if( error_if_missing ) {
        stop(e)
      }
      return(NULL)
    })
  }

  clean_pynwb <- function() {
    pynwb <<- NULL
  }

  list(
    get = get_pynwb,
    clean = clean_pynwb
  )
})

ensure_py_package <- function(packages, python_ver = "auto", ...) {
  if(!dir.exists(rpymat::env_path())) {
    standalone <- !file.exists(rpymat::conda_bin())
    rpymat::configure_conda(python_ver = python_ver, force = TRUE, standalone = standalone)
  }
  rpymat::ensure_rpymat(verbose = FALSE)
  installed_pkgs_tbl <- rpymat::list_pkgs()
  packages <- packages[!packages %in% installed_pkgs_tbl$package]
  if(length(packages)) {
    rpymat::add_packages(packages, ...)
  }
}

import_py_module <- function(name, package = name, convert = FALSE) {
  rpymat::ensure_rpymat(verbose = FALSE, cache = TRUE)
  module <- tryCatch({
    rpymat::import(name, convert = convert)
  }, error = function(e) {
    ensure_py_package(packages = package)
    rpymat::import(name, convert = convert)
  })
  module
}

#' @name pynwb_module
#' @title Install \code{'NWB'} via \code{'pynwb'}
#' @param python_ver 'Python' version, see \code{\link[rpymat]{configure_conda}};
#' default is \code{"auto"}, which is suggested
#' @param verbose whether to print the installation messages
#' @param force whether to force-reload the module
#' @param error_if_missing whether to raise errors when the module fails to
#' load; default is true
#' @returns A 'Python' module \code{pynwb}.
#' @export
install_pynwb <- function(python_ver = "auto", verbose = TRUE) {
  ensure_py_package("pynwb")
  validate_python(verbose = verbose)
  return(invisible())
}

#' @rdname pynwb_module
#' @export
pynwb_module <- .pynwb$get


is_invalid_py_pointer <- function(x) {
  rpymat::ensure_rpymat(verbose = FALSE)
  asNamespace("reticulate")$py_is_null_xptr(x)
}

