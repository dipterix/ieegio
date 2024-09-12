


#' @rdname low-level-read-write
#'
#' @examples
#'
#' # ---- Matlab .mat --------------------------------------------------------
#'
#' \dontrun{
#'
#' f <- tempfile(fileext = ".mat")
#'
#' x <- list(a = 1L, b = 2.3, c = "a", d = 1+1i)
#'
#' # save as MAT 5.0
#' io_write_mat(x, f)
#'
#' io_read_mat(f)
#'
#' # require setting up Python environment
#'
#' io_read_mat(f, method = "pymatreader")
#'
#' # MAT 7.3 example
#' sample_data <- ieegio_sample_data("mat_v73.mat")
#' io_read_mat(sample_data)
#'
#' # clean up
#' unlink(f)
#'
#' }
#'
#'
#' @export
io_read_mat <- function(con, method = c("auto", "R.matlab", "pymatreader", "mat73"),
                     verbose = TRUE, on_convert_error = c("warning", "error", "ignore"), ...) {
  method <- match.arg(method)
  on_convert_error <- match.arg(on_convert_error)

  # DIPSAUS DEBUG START
  # con <- "~/rave_data/raw_dir/YAB/008/YABDatafile008_ch1.mat"
  # method <- "pymatreader"
  # on_convert_error <- "warning"
  # verbose <- TRUE

  file <- normalizePath(con, mustWork = TRUE, winslash = "/")

  verb <- function(...) {
    if(verbose) {
      cat(..., "\n", sep = "")
    }
  }



  as_call_py <- function(fun, args, kwargs) {
    fmls <- names(formals(fun))
    args_length <- length(args)
    params <- structure(
      names = fmls[seq_len(args_length)],
      args
    )
    pnms <- names(params)
    knms <- names(kwargs)

    dup_names <- knms[knms %in% pnms]
    if(length(dup_names)) {
      warning("Duplicated input argument(s) found: ", paste(dup_names, collapse = ", "), ". The results might be unexpected.")
    }
    fmls <- fmls[fmls %in% c(pnms, knms)]
    args <- structure(
      names = fmls,
      lapply(fmls, function(nm) {
        if(nm %in% pnms) {
          v <- params[[nm]]
        } else {
          v <- kwargs[[nm]]
        }
        verb("  ", nm, ": ", deparse1(v))
        v
      })
    )
    as.call(c(list(fun), args))
  }

  convert_to_r <- function(re) {
    if(inherits(re, "python.builtin.dict")) {
      nms <- names(re)
      re <- structure(
        names = nms,
        lapply(nms, function(nm) {
          item <- re[[nm]]
          tryCatch({
            item <- rpymat::py_to_r(item)
          }, error = function(e) {

            switch(
              on_convert_error,
              "error" = stop("Unable to convert object ", sQuote(nm), " due to error: ", e$message),
              "warning" = warning("Unable to convert object ", sQuote(nm), "... Keeping its Python form.")
            )
          })
          item
        })
      )
    } else {
      tryCatch({
        re <- rpymat::py_to_r(re)
      }, error = function(e) {
        switch(
          on_convert_error,
          "error" = stop(e),
          "warning" = warning("Unable to convert object... Keeping its Python form.")
        )
      })
    }
    re
  }

  switch(
    method,
    "R.matlab" = {
      # native
      verb("Using native approach to load the matlab file (supporting MAT 5.0)...")
      # use R.matlab
      return(R.matlab::readMat(con = file, ...))
    },
    "pymatreader" = {
      verb(sprintf("Using Python `%s` to load the matlab file...", method))

      pymatreader <- import_py_module("pymatreader")
      fun <- pymatreader$read_mat

      args <- list(...)
      if(length(args$variable_names)) {
        args$variable_names <- as.list(args$variable_names)
      } else {
        args$variable_names <- NULL
      }
      if(length(args$ignore_fields)) {
        args$ignore_fields <- as.list(args$ignore_fields)
      } else {
        args$ignore_fields <- NULL
      }

      call <- as_call_py(fun, file, args)
      call[[1]] <- quote(fun)
      re <- eval(call)
      # try to convert to R
      re <- convert_to_r(re)

      return(re)
    },
    "mat73" = {
      verb(sprintf("Using Python `%s` to load the matlab file (supporting MAT v7.3)...", method))

      mat73 <- import_py_module("mat73")
      fun <- mat73$loadmat

      args <- list(...)
      if(length(args$only_include)) {
        args$only_include <- as.list(args$only_include)
      } else {
        args$only_include <- NULL
      }
      if( verbose ) {
        args$verbose <- TRUE
      } else {
        args$verbose <- FALSE
      }

      call <- as_call_py(fun, file, args)
      call[[1]] <- quote(fun)
      re <- eval(call)
      # try to convert to R
      re <- convert_to_r(re)

      return(re)
    },
    {
      args <- list(
        con = con, method = "R.matlab",
        verbose = verbose, on_convert_error = on_convert_error, ...
      )
      # auto
      tryCatch({
        do.call(io_read_mat, args)
      }, error = function(e) {
        verb("Failed to load matlab using native approach.")
        args$method <- "pymatreader"
        do.call(io_read_mat, args)
      })
    }
  )
}

#' @rdname low-level-read-write
#' @export
io_write_mat <- function(x, con, method = c("R.matlab", "scipy"), ...) {

  method <- match.arg(method)

  if(!is.list(x)) {
    stop("`write_mat`: `x` must be a named list.")
  }
  nms <- names(x)
  if(!length(nms) || "" %in% nms) {
    stop("`write_mat`: `x` must be a named list.")
  }

  reserved <- c('con', 'fixNames', 'matVersion', 'onWrite', 'verbose')
  reserved <- reserved[reserved %in% nms]
  if(length(reserved)) {
    stop("`write_mat`: `x` must NOT contain the following reserved names: ", paste(sQuote(reserved), collapse = ", "))
  }

  con <- normalizePath(con, winslash = "/", mustWork = FALSE)

  switch(
    method,
    "R.matlab" = {
      args <- c( list( con = con ), x )

      do.call(R.matlab::writeMat, args)
    },
    "scipy" = {
      scipy_io <- import_py_module("scipy.io", "scipy")
      scipy_io$savemat(con, x)
    }
  )

  invisible(con)
}
