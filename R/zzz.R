
# Internally used for RAVE
install_extras <- function() {
  check_py_flag()
  sample_names <- ieegio_sample_data()
  lapply(sample_names, ieegio_sample_data)

  rpymat::configure_conda("3.11")
  tryCatch({
    rpymat::run_command("conda tos accept --channel https://repo.anaconda.com/pkgs/r --channel https://repo.anaconda.com/pkgs/main")
  }, error = function(e) {})
  rpymat::add_packages(c("numpy", "pynwb", "pymatreader", "mat73", "vtk"))
}

setup_test_env <- function() {
  if(!identical(Sys.getenv("IEEGIO_WITH_EXAMPLES"), "")) {
    install_extras()
  }
}

get_os <- function(){
  os <- R.version$os
  if(grepl('^darwin', os, ignore.case = TRUE)){
    return('darwin')
  }
  if(grepl('^linux', os, ignore.case = TRUE)){
    return('linux')
  }
  if(grepl('^solaris', os, ignore.case = TRUE)){
    return('solaris')
  }
  if(grepl('^win', os, ignore.case = TRUE)){
    return('windows')
  }
  if(grepl("^(emscr|wasm)", os, ignore.case = TRUE)) {
    return('emscripten')
  }
  return('unknown')
}

# load_py <- local({
#
#   main <- NULL
#
#   function() {
#     if (!is.null(main)) { return(main) }
#
#     if( !rpymat_is_setup() ) {
#       return( NULL )
#     }
#
#     py <- tryCatch({
#       reticulate <- asNamespace("reticulate")
#       if(isTRUE(reticulate$is_python_initialized())) {
#         py <- reticulate$import_main(convert = TRUE)
#       } else {
#         py <- NULL
#       }
#       py
#     }, error = function(e) {
#       reticulate$py
#     })
#
#     if(!is.null(py)) {
#       main <<- py
#     }
#     main
#   }
# })

# # inject python when library(ieegio)
# .onAttach <- function(libname, pkgname) {
#   pkg <- getNamespace(pkgname)
#   makeActiveBinding("py", fun = load_py, env = pkg)
#   makeActiveBinding(
#     "nwb", env = pkg,
#     fun = function() {
#       pynwb_module(error_if_missing = FALSE)
#     }
#   )
# }


.onUnload <- function(libpath) {
  .pynwb$clean()
}

# Used for debugging functions
enable_debugging <- local({
  enabled <- FALSE
  function(x = TRUE) {
    if(!is.na(x)) {
      enabled <<- isTRUE(x)
    }
    invisible(enabled)
  }
})

