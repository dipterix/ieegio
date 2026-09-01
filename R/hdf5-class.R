py_to_r <- function(x) {
  rpymat::py_to_r(x = x)
}

py_isinstance <- function(obj, cls) {
  rpymat::py_builtin("isinstance", convert = TRUE)(obj, cls)
}

py_func <- function(fun) {
  rpymat <- asNamespace("rpymat")
  rpymat$run_package_function("reticulate", "py_func", fun)
}

py_is_null_xptr <- function(x) {
  if (is.null(x)) { return(TRUE) }
  if (!inherits(x, "python.builtin.object")) { return(FALSE) }

  rpymat <- asNamespace("rpymat")
  re <- rpymat$run_package_function("reticulate", "py_is_null_xptr", x)
  if (inherits(re, "python.builtin.object")) {
    re <- py_to_r(re)
  }
  isTRUE(re)
}

ensure_hdf5_backend <- local({

  h5py <- NULL

  function() {
    if (identical(get_os(), "emscripten") || getOption("ieegio.debug.emscripten", FALSE)) {
      return()
    }

    # Resolve system flag
    sys_flag <- Sys.getenv("IEEGIO_USE_H5", unset = "")
    if (!nzchar(sys_flag) && nzchar(Sys.getenv("IEEGIO_USE_H5PY"))) {
      # backward compatible
      sys_flag <- "h5py"
    }
    if (!nzchar(sys_flag)) {
      # Resolved default; change this line to move the default to `h5lite`
      sys_flag <- "readNSx"
    }

    # if h5lite specified, h5lite, then hdf5r, then h5py
    # if hdf5r specified (also the default), hdf5r, then h5py
    # if h5py, then h5py
    if (sys_flag != "h5py") {
      if (sys_flag == "h5lite" && nzchar(system.file(package = "h5lite"))) {
        return(asNamespace("h5lite"))
      }
      if (sys_flag == "hdf5r" && nzchar(system.file(package = "hdf5r"))) {
        # Using hdf5r
        return(asNamespace("hdf5r"))
      }

      # Using readNSx, essentially hdf5lib
      readNSx <- asNamespace("readNSx")
      if (is.function(readNSx$h5_native_write)) {
        return(readNSx)
      }

      # readNSx is old version
      if (nzchar(system.file(package = "hdf5r"))) {
        return(asNamespace("hdf5r"))
      }

      if (nzchar(system.file(package = "h5lite"))) {
        return(asNamespace("h5lite"))
      }

    }


    check_py_flag()

    if (!is.null(h5py)) { return(h5py) }
    rpymat <- asNamespace("rpymat")

    h5py_isnull <- !inherits(h5py, "python.builtin.module")

    if (h5py_isnull) {
      h5py_isnull <- tryCatch(
        {
          py_is_null_xptr(h5py)
        },
        error = function(e) {
          TRUE
        }
      )
    }

    if (h5py_isnull) {
      tryCatch({
        message("Trying to use python `h5py` as the HDF5 backend")
        ensure_py_package("h5py")
        h5py <<- rpymat$import("h5py")
      }, error = function(e) {

      })
    }

    h5py
  }
})

hdf5_backend_type <- function(h5backend = ensure_hdf5_backend()) {
  if (inherits(h5backend, "python.builtin.module")) {
    return("h5py")
  }
  if (isNamespace(h5backend)) {
    # h5lite or hdf5r
    return(unname(getNamespaceName(h5backend)))
  }
  return("filearray")
}

h5FileValid <- function(filename) {
  if (!length(filename)) { return(structure(FALSE, reasons = "file length must be one")) }
  filename <- filename[[1]]


  h5backend <- ensure_hdf5_backend()

  backend_type <- hdf5_backend_type(h5backend)

  switch(
    backend_type,
    "h5py" = {
      if (!file.exists(filename)) { return(structure(FALSE, reasons = "file must exist")) }
      if (isTRUE(file.info(filename)[["isdir"]])) { return(structure(FALSE, reasons = "file length must not be a directory")) }
      filename <- normalizePath(filename)
      return(py_to_r(h5backend$is_hdf5(filename)))
    },
    "hdf5r" = {
      if (!file.exists(filename)) { return(structure(FALSE, reasons = "file must exist")) }
      if (isTRUE(file.info(filename)[["isdir"]])) { return(structure(FALSE, reasons = "file length must not be a directory")) }
      filename <- normalizePath(filename)
      return(tryCatch({
        # hdf5r::is.h5file(filename)
        h5backend$is.h5file(filename)
      }, error = function(e) {
        # h5lite::h5_exists(file = filename, name = "/", assert = FALSE)
        structure(FALSE, reasons = e$message)
      }))
    },
    "filearray" = {
      return(dir.exists(alternative_h5_fname(filename)))
    },
    "h5lite" = {
      if (!file.exists(filename)) { return(structure(FALSE, reasons = "file must exist")) }
      if (isTRUE(file.info(filename)[["isdir"]])) { return(structure(FALSE, reasons = "file length must not be a directory")) }
      filename <- normalizePath(filename)
      # h5lite
      return(h5backend$h5_exists(file = filename, name = "/", assert = FALSE))
    },
    "readNSx" = {
      h5backend$h5FileValid(filename)
    },
    {
      stop("Invalid HDF5 backend: ", backend_type)
    }
  )

}

#' @title Lazy 'HDF5' file loader
#' @description Provides hybrid data structure for 'HDF5' file. The class is
#' not intended for direct-use. Please see \code{\link{io_read_h5}} and
#' \code{\link{io_write_h5}}.
#' @export
LazyH5 <- R6::R6Class(
  classname = "LazyH5",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    backend = character(0L),
    file = NULL,
    name = NULL,
    read_only = TRUE,
    data_ptr = NULL,
    file_ptr = NULL,
    last_dim = NULL,

    finalize = function() {
      self$close(all = TRUE)
    }
  ),
  public = list(

    #' @field quiet whether to suppress messages
    quiet = FALSE,

    #' @description garbage collection method
    #' @return none
    do_finalize = function() {
      self$close(all = TRUE)
    },

    #' @description overrides print method
    #' @return self instance
    print = function() {
      if (is.null(private$data_ptr) && private$backend != "readNSx") {
        base::cat("HDF5 file object (closed)\n")
        return(invisible(self))
      }
      switch(
        private$backend,
        "h5py" = {
          if (isTRUE(py_to_r(private$file_ptr$`__bool__`()))) {
            base::cat("HDF5 file object with <h5py> backend\n")
            base::cat(sprintf("  Dataset: %s (dim: %s)\n", private$name, paste(private$last_dim, collapse = "x")))
          } else {
            base::cat("Pointer closed. Information since last open:\nDim: ",
                      paste(private$last_dim, collapse = "x"), " \tRank: ",
                      length(private$last_dim), "\n")
          }
        },
        "hdf5r" = {
          if (isTRUE(private$data_ptr$is_valid)) {
            base::print(private$data_ptr)
          } else {
            base::cat("Pointer closed. Information since last open:\nDim: ",
                      paste(private$last_dim, collapse = "x"), " \tRank: ",
                      length(private$last_dim), "\n")
          }
        },
        "filearray" = {
          base::cat("HDF5 cache object with <filearray> backend\n")
          base::cat(sprintf("  Dataset : %s (dim: %s)\n", private$name, paste(private$last_dim, collapse = "x")))
          base::cat(sprintf("  Datatype: %s\n", private$data_ptr$object$type()))
        },
        "h5lite" = {
          base::cat("HDF5 file object with <h5lite> backend\n")
          base::print(private$data_ptr)
          base::cat("Information since last open:\nDim: ",
                    paste(private$last_dim, collapse = "x"), " \tRank: ",
                    length(private$last_dim), "\n")
        },
        "readNSx" = {
          base::cat(
            sep = "",
            "<HDF5 dataset>\n",
            sprintf("  File   : %s\n", private$file),
            sprintf("  Dataset: %s\n", private$name),
            sprintf("  Dim    : %s\n", paste(private$last_dim, collapse = "x"))
          )
        },
        {
          base::cat("HDF5 file object (closed)\n")
        }
      )
      invisible(self)
    },

    #' @description constructor
    #' @param file_path where data is stored in 'HDF5' format
    #' @param data_name the data stored in the file
    #' @param read_only whether to open the file in read-only mode. It's highly
    #' recommended to set this to be true, otherwise the file connection is
    #' exclusive.
    #' @param quiet whether to suppress messages, default is false
    #' @return self instance
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE) {

      h5backend <- ensure_hdf5_backend()

      private$backend <- hdf5_backend_type(h5backend)

      # First get absolute path, otherwise hdf5r may report file not found error
      private$file <- normalizePath(file_path, mustWork = FALSE)
      if (read_only) {
        is_h5_valid <- h5FileValid(private$file)
        if (!is_h5_valid) {
          reasons <- attr(is_h5_valid, "reasons")
          if (length(reasons)) {
            reasons <- sprintf(" Reasons: %s", paste(reasons, collapse = "\n"))
          } else {
            reasons <- ""
          }
          stop("File `", file_path, "` is not an HDF5 file.", reasons)
        }
      } else {
        private$file <- file_path
      }
      self$quiet <- isTRUE(quiet)
      private$name <- data_name
      private$read_only <- read_only
    },

    #' @description save data to a 'HDF5' file
    #' @param x vector, matrix, or array
    #' @param chunk chunk size, length should matches with data dimension
    #' @param level compress level, from 1 to 9
    #' @param replace if the data exists in the file, replace the file or not
    #' @param new_file remove the whole file if exists before writing?
    #' @param force if you open the file in read-only mode, then saving
    #' objects to the file will raise error. Use \code{force=TRUE} to force
    #' write data
    #' @param ctype data type, see \code{\link{mode}}, usually the data type
    #' of \code{x}. Try \code{mode(x)} or \code{storage.mode(x)} as hints.
    #' @param size deprecated, for compatibility issues
    #' @param ... passed to self \code{open()} method
    save = function(x, chunk = "auto", level = 7, replace = TRUE,
                    new_file = FALSE, force = TRUE, ctype = NULL, size = NULL,
                    ...) {
      # ctype and size is deprecated but kept in case of compatibility issues
      # ptr$create_dataset =
      # function (name, robj = NULL, dtype = NULL, space = NULL, dims = NULL,
      #           chunk_dims = "auto", gzip_level = 4, link_create_pl = h5const$H5P_DEFAULT,
      #           dataset_create_pl = h5const$H5P_DEFAULT, dataset_access_pl = h5const$H5P_DEFAULT)
      if (private$read_only) {
        if (!force) {
          stop('File is read-only. Use "force=TRUE"')
        } else {
          # Close current pointer
          self$close(all = TRUE)
          private$read_only <- FALSE

          on.exit({
            self$close(all = TRUE)
            private$read_only <- TRUE
          }, add = TRUE, after = FALSE)
        }
      }

      if (new_file && file.exists(private$file)) {
        self$close(all = TRUE)
        file.remove(private$file)
      }

      if (!is.null(ctype)) {

        xtype <- storage.mode(x)
        if (xtype != ctype) {

          switch(
            ctype,
            "numeric" = {
              if (xtype != "double") {
                storage.mode(x) <- "double"
              }
            },
            "character" = {
              x <- as.character(x)
            },
            "string" = {
              x <- as.character(x)
              ctype <- "character"
            },
            {
              storage.mode(x) <- ctype
            }
          )

        }

      }

      self$open(new_dataset = replace, robj = x, chunk = chunk, gzip_level = level, ...)

      self$close(all = TRUE)

    },


    #' @description open connection
    #' @param new_dataset only used when the internal pointer is closed, or
    #' to write the data
    #' @param robj data array to save
    #' @param ... backend-specific arguments; for example, passed to
    #' \code{createDataSet} in \pkg{hdf5r}
    open = function(new_dataset = FALSE, robj, ...) {

      h5backend <- ensure_hdf5_backend()

      switch(
        private$backend,
        "h5py" = {
          # Use Python backend
          # check data pointer
          # if valid, no need to do anything, otherwise, enter if clause

          pointer_valid <- TRUE
          if (
            !inherits(private$file_ptr, "python.builtin.object") || py_is_null_xptr(private$file_ptr) ||
            !isTRUE(py_to_r(private$file_ptr$`__bool__`()))
          ) {
            pointer_valid <- FALSE
          }

          if (new_dataset || !pointer_valid) {

            # check if `file_ptr` is valid
            if (!pointer_valid) {
              # if no, create new link
              mode <- ifelse(private$read_only, "r", "a")

              private$file_ptr <- h5backend$File(private$file, mode = mode)
            }

            has_data <- tryCatch({
              private$file_ptr$`__getitem__`(private$name)
              TRUE
            }, error = function(e) {
              FALSE
            })

            if (!private$read_only && (new_dataset || !has_data)) {
              # need to create new dataset
              g <- strsplit(private$name, "/", fixed = TRUE)[[1]]
              g <- g[stringr::str_trim(g) != ""]

              group_name <- paste(c("", g[-length(g)]), collapse = "/")
              group_name <- trimws(group_name)
              if (nzchar(group_name)) {
                ptr <- private$file_ptr$require_group(group_name)
              } else {
                ptr <- private$file_ptr
              }


              # delete it first as the data will be written later
              nm <- g[length(g)]

              if (missing(robj)) {
                robj <- NA_real_
              }
              data_shape <- dim(robj)
              if (!length(data_shape)) {
                data_shape <- length(robj)
              } else {
                # R is col-major and python is row-major
                robj <- aperm(robj, rev(seq_along(data_shape)))
                data_shape <- dim(robj)
              }
              py_data_shape <- do.call(rpymat::py_tuple, unname(as.list(data_shape)))
              # self$open(new_dataset = replace, robj = x, ctype=, ...)
              args <- list(...)
              ctype <- args$ctype
              if (length(ctype) != 1) {
                ctype <- storage.mode(robj)
              }

              # if(inherits(type, "H5T_STRING")) { return("character") }
              # if(inherits(type, "H5T_INTEGER")) { return("integer") }
              # if(inherits(type, "H5T_BITFIELD")) { return("raw") }
              # if(inherits(type, "H5T_FLOAT")) { return("double") }
              # if(inherits(type, "H5T_COMPLEX")) { return("complex") }
              dtype <- switch(
                ctype,
                "character" = h5backend$string_dtype(),
                "integer" = "int32",
                "logical" = "bool",
                "numeric" = "float64",
                "double" = "float64",
                "float" = "float32",
                "raw" = {
                  storage.mode(robj) <- "integer"
                  "uint8"
                },
                {
                  ctype
                }
              )

              dtpr <- tryCatch(
                {
                  ptr$require_dataset(nm, py_data_shape, dtype)
                },
                error = function(e) {
                  ptr$`__delitem__`(nm)
                  ptr$require_dataset(nm, py_data_shape, dtype)
                }
              )
              dtpr$`__setitem__`(rpymat::py_tuple(), robj)
              # ptr$`__setitem__`(nm, robj)

            } else if (!has_data) {
              stop(sprintf(
                "File [%s] has no [%s] in it.",
                private$file, private$name
              ))
            }

          }

          tryCatch(
            {
              private$data_ptr <- private$file_ptr$`__getitem__`(private$name)
              private$last_dim <- rev(unlist(py_to_r(private$data_ptr$shape)))
            },
            error = function(e) {}
          )
        },
        "hdf5r" = {
          # Use R backend
          # check data pointer
          # if valid, no need to do anything, otherwise, enter if clause
          if (new_dataset || is.null(private$data_ptr) || !private$data_ptr$is_valid) {

            # Check if file is valid,
            if (is.null(private$file_ptr) || !private$file_ptr$is_valid) {
              # if no, create new link
              mode <- ifelse(private$read_only, "r", "a")

              # Using hdf5r
              tryCatch({
                # private$file_ptr <- hdf5r::H5File$new(private$file, mode)
                private$file_ptr <- h5backend$H5File$new(private$file, mode)
              }, error = function(e) {
                # Open for writting, we should close all connections first
                # then the file can be opened, otherwise, Access type: H5F_ACC_RDONLY
                # will lock the file for writting
                # f <- hdf5r::H5File$new(private$file, "r")
                f <- h5backend$H5File$new(private$file, "r")

                if (!self$quiet) {
                  cat("Closing all other connections to [{private$file}] - {f$get_obj_count() - 1}\n")
                }

                try({ f$close_all() }, silent = TRUE)
                # private$file_ptr <- hdf5r::H5File$new(private$file, mode)
                private$file_ptr <- h5backend::H5File$new(private$file, mode)

              })

            }

            # Must stay outside of the `file_ptr` check above: the file pointer
            # can still be valid while the dataset pointer has been closed
            has_data <- private$file_ptr$path_valid(private$name)

            if (!private$read_only && (new_dataset || !has_data)) {
              # need to create new dataset
              g <- stringr::str_split(private$name, "/", simplify = TRUE)
              g <- g[stringr::str_trim(g) != ""]

              ptr <- private$file_ptr
              nm <- ""

              for (i in g[-length(g)]) {
                nm <- sprintf("%s/%s", nm, i)
                if (!ptr$path_valid(path = nm)) {
                  ptr <- ptr$create_group(i)
                  if (!self$quiet) {
                    cat(sprintf("%s => %s (Group Created)\n", private$file, nm))
                  }
                } else {
                  ptr <- ptr[[i]]
                }
              }

              # create dataset
              nm <- g[length(g)]
              if (ptr$path_valid(path = nm)) {
                # dataset exists, unlink first
                if (!self$quiet) {
                  cat(sprintf("%s => %s (Dataset Removed)\n", private$file, nm))
                }
                ptr$link_delete(nm)
              }
              # new create
              if (!self$quiet) {
                cat(sprintf("%s => %s (Dataset Created)\n", private$file, nm))
              }
              if (missing(robj)) {
                robj <- NA
              }
              ptr$create_dataset(nm, robj = robj, ...)
              if (ptr$is_valid && inherits(ptr, "H5Group")) {
                ptr$close()
              }
            } else if (!has_data) {
              stop(sprintf(
                "File [%s] has no [%s] in it.",
                private$file, private$name
              ))
            }

            private$data_ptr <- private$file_ptr[[private$name]]

          }

          private$last_dim <- private$data_ptr$dims
        },
        "filearray" = {
          # use filearray

          filebase <- file.path(alternative_h5_fname(private$file), private$name)
          has_data <- dir.exists(filebase)

          if (!private$read_only && (new_dataset || !has_data)) {
            if (file.exists(filebase)) {
              unlink(filebase, recursive = TRUE)
            }
            if (!missing(robj)) {

              # need to create new dataset
              dir.create(dirname(filebase), recursive = TRUE, showWarnings = FALSE)
              dm0 <- dim(robj)
              dm <- dm0
              if (length(dm) < 2) {
                dm0 <- length(robj)
                dm <- c(dm0, 1)
              }
              storage <- storage.mode(robj)
              if (storage %in% c("string", "character")) {
                storage <- "raw"
                robj <- paste(robj, collapse = "")
                robj <- charToRaw(robj)

                dm <- c(length(robj), 1)
                if (any(dm == 0)) {
                  arr <- filearray::filearray_create(filebase = filebase, dimension = c(1, 1), type = storage)
                  arr$set_header("original_dim", dm0)
                } else {
                  arr <- filearray::filearray_create(filebase = filebase, dimension = dm, type = storage)
                  arr$set_header("original_dim", dm0)
                  suppressWarnings({
                    arr[] <- robj
                  })
                }
              } else {
                if (any(dm == 0)) {
                  arr <- filearray::filearray_create(filebase = filebase, dimension = c(1, 1), type = storage)
                  arr$set_header("original_dim", dm0)
                } else {
                  arr <- filearray::filearray_create(filebase = filebase, dimension = dm, type = storage)
                  arr$set_header("original_dim", dm0)
                  arr[] <- robj
                }
              }

              private$data_ptr <- structure(
                class = "filearray_ptr",
                list(object = arr,
                     is_valid = TRUE)
              )
            }
          } else if (!has_data) {
            stop(sprintf(
              "File [%s] has no [%s] in it.",
              private$file, private$name
            ))
          } else {
            arr <- filearray::filearray_load(filebase = filebase, mode = ifelse(private$read_only, "readonly", "readwrite"))
            private$data_ptr <- structure(
              class = "filearray_ptr",
              list(object = arr,
                   is_valid = TRUE)
            )
          }

          private$last_dim <- dim(private$data_ptr)
        },
        "h5lite" = {
          # h5lite
          # check data pointer
          # if valid, no need to do anything, otherwise, enter if clause
          if (new_dataset || is.null(private$data_ptr) || is.null(private$data_ptr$.file)) {

            # Check if file is valid,
            # is.null(private$file_ptr$.file) is how h5lite check if file is opened
            if (is.null(private$file_ptr) || is.null(private$file_ptr$.file)) {
              # if no, create new link
              mode <- ifelse(private$read_only, "r", "a")

              # h5lite: h5lite always perform simple ops and does not hold
              # connections open
              private$file_ptr <- h5backend$h5_open(private$file)

            }
            has_data <- private$file_ptr$exists(private$name)

            if (!private$read_only && (new_dataset || !has_data)) {
              # need to create new dataset
              g <- stringr::str_split(private$name, "/", simplify = TRUE)
              g <- g[stringr::str_trim(g) != ""]

              group_name <- paste(g[-length(g)], collapse = "/")
              dataset_name <- paste(g, collapse = "/")

              ptr <- private$file_ptr
              if (nzchar(group_name) && !ptr$exists(group_name)) {
                ptr$create_group(group_name)

                if (!self$quiet) {
                  cat(sprintf("%s => %s (Group Created)\n", private$file, group_name))
                }
              }

              # create dataset
              if (ptr$exists(dataset_name)) {
                # dataset exists, unlink first
                if (!self$quiet) {
                  cat(sprintf("%s => %s (Dataset Removed)\n", private$file, dataset_name))
                }
                ptr$delete(dataset_name)
              }

              # new create
              if (!self$quiet) {
                cat(sprintf("%s => %s (Dataset Created)\n", private$file, dataset_name))
              }
              if (missing(robj)) {
                robj <- NA
              }

              data_shape <- dim(robj)
              if (!length(data_shape)) {
                data_shape <- length(robj)
              } else {
                # R is col-major and python is row-major, h5lite follows python convention
                robj <- aperm(robj, rev(seq_along(data_shape)))
                data_shape <- dim(robj)
              }
              args <- list(...)
              ctype <- args$ctype
              if (length(ctype) != 1) {
                ctype <- storage.mode(robj)
              }

              # When writing a numeric or logical vector, you can specify one of the following storage types for it:
              #
              #   Floating Point: "float16", "float32", "float64", "bfloat16"
              #
              # Signed Integer: "int8", "int16", "int32", "int64"
              #
              # Unsigned Integer: "uint8", "uint16", "uint32", "uint64"
              #
              # NOTE: NA values must be stored as float64. NaN, Inf, and -Inf must be stored as a floating point type.
              dtype <- switch(
                ctype,
                "character" = "utf8",
                "integer" = {
                  # `int32` round-trips as R integer without relying on the
                  # `R_type` attribute, but h5lite rejects NA in integer types
                  if (anyNA(robj)) { "float64" } else { "int32" }
                },
                # NA becomes NaN on write and is restored to NA by the
                # `storage.mode(re) <- r_type` step in `subset()`
                "logical" = "float32",
                "numeric" = "float64",
                "double" = "float64",
                "float" = "float32",
                "raw" = {
                  storage.mode(robj) <- "integer"
                  "uint8"
                },
                {
                  ctype
                }
              )

              # `chunk`/`chunk_dims` has no per-dimension equivalent in h5lite;
              # `compress` takes a level from 1 to 9, anything else is "none"
              gzip_level <- args$gzip_level
              if (length(gzip_level) != 1 || is.na(gzip_level)) {
                gzip_level <- "gzip"
              }

              ptr$write(data = robj, name = dataset_name, as = dtype,
                        compress = gzip_level)
              ptr$write(data = ctype, name = dataset_name, as = "ascii", attr = "R_type")
              ptr$close()
            } else if (!has_data) {
              stop(sprintf(
                "File [%s] has no [%s] in it.",
                private$file, private$name
              ))
            }

            data_ptr <- h5backend$h5_open(private$file)
            data_ptr$cd(private$name)
            private$data_ptr <- data_ptr

          }

          # h5lite follows the python convention, dimensions are reversed
          private$last_dim <- rev(private$data_ptr$dim("."))
        },
        "readNSx" = {

          has_data <- h5backend$h5_native_exists(private$file, private$name)

          if (private$read_only) {
            if (!has_data) {
              stop(sprintf(
                "File [%s] has no [%s] in it.",
                private$file, private$name
              ))
            }
          } else if (!h5backend$h5_writable(private$file)) {
            stop(sprintf("Cannot open file [%s] for writing.", private$file))
          }

          # Check if we need to write
          # self$open(new_dataset = replace, robj = x, ctype=, ...)
          if (!private$read_only && (new_dataset || !has_data)) {
            args <- list(...)
            ctype <- args$ctype
            chunk <- args$chunk %||% "auto"

            # `save()` forwards the compress level as `gzip_level`, while
            # `h5_native_write` calls it `level`; honor whichever arrives
            level <- args$gzip_level %||% args$level %||% 7
            level <- as.integer(level)[[1]]
            if (is.na(level)) { level <- 7L }

            replace <- as.logical(args$replace %||% TRUE)[[1]]

            if (missing(robj)) {
              # Same contract as the `hdf5r` and `h5lite` branches: opening for
              # write must register the dataset name even when there is no data
              # to store yet
              robj <- NA_real_
            }
            if (length(ctype) && !identical(ctype, storage.mode(robj))) {
              robj <- h5backend$coerce_ctype(robj, ctype)
            }

            h5backend$h5_native_write(
              path = private$file,
              name = private$name,
              x = robj,
              chunk = h5backend$as_h5_chunk(chunk),
              level = level,
              replace = replace
            )

            has_data <- TRUE
          }

          if (has_data) {
            private$last_dim <- h5backend$h5_native_dims(private$file, private$name)
          }

          invisible(self)

        },
        {
          stop("Unsupported HDF5 backend: ", private$backend)
        }
      )
    },


    #' @description close connection
    #' @param all whether to close all connections associated to the data file.
    #' If true, then all connections, including access from other programs,
    #' will be closed
    close = function(all = TRUE) {

      h5backend <- ensure_hdf5_backend()

      switch(
        private$backend,
        "h5py" = {
          tryCatch({
            private$file_ptr$close()
            private$data_ptr <- NULL
            private$file_ptr <- NULL
          }, error = function(e) {})
        },
        "hdf5r" = {
          try({
            # check if data link is valid
            if (!is.null(private$data_ptr) && private$data_ptr$is_valid) {
              private$data_ptr$close()
            }

            # if file link is valid, get_obj_ids() should return a vector of 1
            if (all && !is.null(private$file_ptr) && private$file_ptr$is_valid) {
              private$file_ptr$close_all()
            }
          }, silent = TRUE)
        },
        "h5lite" = {
          try({
            if (!is.null(private$data_ptr) && !is.null(private$data_ptr$.file)) {
              private$data_ptr$close()
            }

            # h5lite holds no connection; closing only invalidates the handle
            if (all && !is.null(private$file_ptr) && !is.null(private$file_ptr$.file)) {
              private$file_ptr$close()
            }
          }, silent = TRUE)
        },
        "readNSx" = {
          # Every operation opens and closes the file; nothing is held to release.
        },
        "filearray" = {
          # connections are managed by the `filearray` package itself
        },
        {
          stop("Invalid HDF5 backend: ", private$backend)
        }
      )

      invisible()
    },

    #' @description subset data
    #' @param i,j,... index along each dimension
    #' @param drop whether to apply \code{\link{drop}} the subset
    #' @param stream whether to read partial data at a time
    #' @param envir if \code{i,j,...} are expressions, where should the
    #' expression be evaluated
    #' @return subset of data
    subset = function(
      ...,
      drop = FALSE, stream = FALSE,
      envir = parent.frame()
    ) {
      self$open()
      on.exit({
        self$close(all = !private$read_only)
      })
      # dims <- self$get_dims()

      switch(
        private$backend,
        "h5py" = {
          re <- py_to_r(private$data_ptr[])
          dm <- dim(re)

          if (length(dm) > 1) {
            re <- aperm(re, perm = rev(seq_along(dm)))
          }

          re <- re[..., drop = FALSE]
          if (drop) {
            re <- base::drop(re)
          }

          if (!is.null(re) && length(dim(re)) == 1) {
            dim(re) <- NULL
          }
          return(re)
        },
        "filearray" = {
          arr <- private$data_ptr$object
          storage <- private$data_ptr$object$type()
          dm0 <- arr$get_header("original_dim", default = NULL)
          if (!length(dm0) || any(dm0 == 0)) {
            re <- logical(0L)
            dim(re) <- dm0
            if (storage == "raw") {
              storage <- "character"
            }
            storage.mode(re) <- storage
            re <- re[..., drop = drop]
          } else {
            if (storage == "raw") {
              re <- as.vector(private$data_ptr$object[drop = TRUE])
              re <- rawToChar(re)
              if (length(dm0) >= 2) {
                dim(re) <- dm0
              }
              re <- re[..., drop = drop]
            } else {
              if (length(dm0) < 2) {
                re <- as.vector(private$data_ptr$object[drop = TRUE])
                re <- re[..., drop = drop]
              } else {
                re <- private$data_ptr$object[..., drop = drop]
              }
            }
          }
          return(re)
        },
        "hdf5r" = {
          # step 1: eval indices
          dot_len <- ...length()
          args <- eval(substitute(alist(...)))
          if (dot_len == 0 || (dot_len == 1 && isTRUE(args[[1]] == ""))) {
            return(private$data_ptr$read(drop = drop))
          }
          return(private$data_ptr$read(args = args, drop = drop, envir = envir))
        },
        "readNSx" = {
          h5backend <- ensure_hdf5_backend()

          # `subset()` has already opened the dataset above, and `open()` caches
          # the dimensions; every extra `open()`/`get_dims()` here would cost
          # another pair of file open/close cycles
          dims <- private$last_dim

          # step 1: eval indices
          dot_len <- ...length()
          args <- eval(substitute(alist(...)))
          if (dot_len == 0 || (dot_len == 1 && isTRUE(args[[1]] == ""))) {
            re <- h5backend$h5_native_read(private$file, private$name)
            # return()
          } else {
            args <- lapply(args, function(x) {
              if (x == "") {
                return(x)
              } else {
                return(eval(x, envir = envir))
              }
            })

            # step 2: get allocation size
            alloc_dim <- sapply(seq_along(dims), function(ii) {
              if (is.logical(args[[ii]])) {
                return(sum(args[[ii]]))
              } else if (is.numeric(args[[ii]])) {
                return(length(args[[ii]]))
              } else {
                # must be blank "", otherwise raise error
                return(dims[ii])
              }
            })

            # step 3: get legit indices
            legit_args <- lapply(seq_along(dims), function(ii) {
              if (is.logical(args[[ii]])) {
                return(args[[ii]])
              } else if (is.numeric(args[[ii]])) {
                return(
                  args[[ii]][args[[ii]] <= dims[ii] & args[[ii]] > 0]
                )
              } else {
                return(args[[ii]])
              }
            })

            # step 4: get mapping
            mapping <- lapply(seq_along(dims), function(ii) {
              if (is.logical(args[[ii]])) {
                return(
                  rep(TRUE, sum(args[[ii]]))
                )
              } else if (is.numeric(args[[ii]])) {
                return(args[[ii]] <= dims[ii] & args[[ii]] > 0)
              } else {
                return(args[[ii]])
              }
            })

            # alloc space
            re <- array(NA, dim = alloc_dim)

            # A request for one contiguous block per dimension - the common
            # `channel$data[a:b]` case - is served straight from disk instead of
            # reading the whole dataset first.
            block <- h5backend$h5_contiguous_block(legit_args, dims)
            if (is.null(block)) {
              selected <- do.call("[", c(
                list(h5backend$h5_native_read(private$file, private$name)),
                legit_args, list(drop = FALSE)
              ))
            } else {
              selected <- h5backend$h5_native_read_slab(
                private$file, private$name,
                start = h5backend$as_h5_index(block$start),
                count = h5backend$as_h5_index(block$count)
              )
            }

            re <- do.call(`[<-`, c(list(quote(re)), mapping, list(value = quote(selected))))
          }

          if (drop) {
            re <- base::drop(re)
          }

          # `hdf5r` hands back a plain vector for a rank-1 dataset; `base::drop`
          # keeps a 1-d `dim` when the extent is not 1, so strip it here as the
          # `h5py` and `h5lite` branches do
          if (!is.null(re) && length(dim(re)) == 1) {
            dim(re) <- NULL
          }
          return(re)
        },
        "h5lite" = {

          re <- private$data_ptr$read(".")
          r_type <- attr(re, "R_type")

          if (length(r_type) == 1) {
            try(
              silent = TRUE,
              expr = {
                storage.mode(re) <- r_type
                attr(re, "R_type") <- NULL
              }
            )
          }

          dm <- private$data_ptr$dim(".")

          if (length(dm) > 1) {
            re <- aperm(re, perm = rev(seq_along(dm)))
          }

          re <- re[..., drop = FALSE]
          if (drop) {
            re <- base::drop(re)
          }

          if (!is.null(re) && length(dim(re)) == 1) {
            dim(re) <- NULL
          }
          return(re)
        },
        {
          stop("Invalid HDF5 backend: ", private$backend)
        }
      )
    },


    #' @description get data dimension
    #' @param stay_open whether to leave the connection opened
    #' @return dimension of the array
    get_dims = function(stay_open = TRUE) {
      self$open()

      h5backend <- ensure_hdf5_backend()

      private$last_dim <- switch(
        private$backend,
        "h5py" = {
          rev(unlist(py_to_r(private$data_ptr$shape)))
        },
        "filearray" = {
          dim(private$data_ptr)
        },
        "hdf5r" = {
          private$data_ptr$dims
        },
        "h5lite" = {
          rev(private$data_ptr$dim("."))
        },
        "readNSx" = {
          h5backend$h5_native_dims(private$file, private$name)
        },
        {
          stop("Invalid HDF5 backend: ", private$backend)
        }
      )

      if (!stay_open) {
        self$close(all = !private$read_only)
      }
      private$last_dim
    },

    #' @description get data type
    #' @param stay_open whether to leave the connection opened
    #' @return data type, currently only character, integer, raw,
    #' double, and complex are available, all other types will yield "unknown"
    get_type = function(stay_open = TRUE) {
      self$open()

      h5backend <- ensure_hdf5_backend()

      switch(
        private$backend,
        "h5py" = {
          type <- py_to_r(private$data_ptr$dtype$type$`__name__`)
          type <- tolower(substr(type, 1, 3))
          re <- switch(
            type,
            "flo" = "double",
            "dou" = "double",
            "boo" = "logical",
            "int" = "integer",
            "uin" = "integer",
            "uch" = "raw",
            "com" = "complex",
            "str" = "character",
            {
              "unknown"
            }
          )
          return(re)
        },
        "filearray" = {
          re <- private$data_ptr$object$type()
          if (re == "raw") {
            re <- "character"
          }
          return(re)
        },
        "hdf5r" = {
          type <- private$data_ptr$get_type()

          if (!stay_open) {
            self$close(all = !private$read_only)
          }
          if (inherits(type, "H5T_STRING")) { return("character") }
          if (inherits(type, "H5T_INTEGER")) { return("integer") }
          if (inherits(type, "H5T_BITFIELD")) { return("raw") }
          if (inherits(type, "H5T_FLOAT")) { return("double") }
          if (inherits(type, "H5T_COMPLEX")) { return("complex") }
        },
        "h5lite" = {
          attr_names <- private$data_ptr$attr_names(name = ".")
          if ("R_type" %in% attr_names) {
            type <- private$data_ptr$read(".", attr = "R_type")
            if (length(type) == 1) { return(type) }
          }
          type <- private$data_ptr$typeof(".")

          type <- switch(
            substr(type, 1, 3),
            "flo" = "double",
            "dou" = "double",
            "boo" = "logical",
            "int" = "integer",
            "uin" = "raw",
            "com" = "complex",
            "utf" = "character",
            "asc" = "character",
            {
              type
            }
          )
          return(type)
        },
        "readNSx" = {
          # `readNSx` exposes no datatype accessor, but it stores native HDF5
          # types (H5T_ENUM for logical, an {r,i} compound for complex, variable
          # length strings for character), so reading a single element back is
          # enough to recover the R type - and it only touches one chunk.
          dims <- private$last_dim
          if (!length(dims) || any(dims == 0)) {
            re <- h5backend$h5_native_read(private$file, private$name)
          } else {
            idx <- h5backend$as_h5_index(rep(1, length(dims)))
            re <- h5backend$h5_native_read_slab(
              private$file, private$name, start = idx, count = idx)
          }
          if (!stay_open) {
            self$close(all = !private$read_only)
          }
          return(storage.mode(re))
        },
        {
          stop("Invalid HDF5 backend: ", private$backend)
        }
      )


      return("unknown")
    }
  )
)

#' @export
`[.LazyH5` <- function(obj, ...) {
  on.exit({obj$close()}, add = TRUE)
  obj$subset(..., envir = parent.frame())
}

#' @export
`+.LazyH5` <- function(a, b) {
  b + a$subset()
}

#' @export
`-.LazyH5` <- function(a, b) {
  -(b - a$subset())
}

#' @export
`*.LazyH5` <- function(a, b) {
  b * (a$subset())
}

#' @export
`/.LazyH5` <- function(a, b) {
  if (inherits(b, "LazyH5")) {
    b <- b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyH5 <- function(x) {
  dim_info <- x$get_dims(stay_open = FALSE)
  if (length(dim_info) == 1) {
    dim_info <- NULL
  }
  dim_info
}


#' @export
length.LazyH5 <- function(x) {
  dim_info <- x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyH5 <- function(x, ...) {
  as.array(x$subset(), ...)
}

#' @export
Mod.LazyH5 <- function(z) {
  base::Mod(z$subset())
}

#' @export
Arg.LazyH5 <- function(z) {
  base::Arg(z$subset())
}


#' @export
exp.LazyH5 <- function(x) {
  base::exp(x$subset())
}

#' @export
`[.filearray_ptr` <- function(x, ..., drop = TRUE) {
  orig_dm <- dim(x)
  if (missing(drop)) {
    drop <- !isFALSE(x$drop)
  }
  if (length(orig_dm) < 2) {
    re <- x$object[..., drop = TRUE]
  } else {
    re <- x$object[..., drop = drop]
  }
  if (is.raw(re)) {
    re <- rawToChar(re)
  }
  re
}

#' @export
dim.filearray_ptr <- function(x) {
  dm0 <- x$object$get_header("original_dim", NULL)
  dm <- dim(x$object)
  if (is.null(dm0)) {
    # no original_dim is set
    return(dm)
  }

  if (length(dm0) < 2) {
    return(NULL)
  }
  dm0
}
