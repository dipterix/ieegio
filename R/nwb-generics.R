#' @export
`$.nwb.proxy` <- function(x, name) {
  re <- NextMethod("$")
  if(inherits(re, c(
    "python.builtin.type",
    "python.builtin.function",
    "python.builtin.module"
  ))) {
    cls <- class(re)
    if(!"nwb.proxy" %in% cls) {
      class(re) <- c("nwb.proxy", cls)
    }
    py_names <- attr(re, "py_names")
    attr(re, "py_names") <- c(py_names, name)
  }
  re
}

#' @export
print.nwb.proxy <- function(x, max_lines = getOption("rnwb.max_lines", 50), ...) {

  max_lines <- as.integer(max_lines)
  if( length(max_lines) != 1 || !is.finite(max_lines) || max_lines <= 0 ) {
    max_lines <- 50
  }

  addendums <- c()
  doc_string <- tryCatch({
    str <- py_capture_output(
      type = "stdout",
      {
        base <- asNamespace("reticulate")$import_builtins()
        base$help(x)
      }
    )
    str <- trimws(str)
    str <- strsplit(paste(str, collapse = "\n"), "\n")[[1]]
    if(length(str) > max_lines) {
      str <- c(str[seq_len(max_lines)], sprintf("... (Max lines reached, Limit: %d lines)", max_lines))

      py_names <- attr(x, "py_names")
      addendums <- c(sprintf("Please use the following command to see the full documentation: {.run rnwb::py_help(%s)}", paste(c("nwb", py_names), collapse = "$")))
    }
    str <- paste(str, collapse = "\n")

    addendums <- c(addendums, "Above documentation is for Python. Please use `$` instead of `.` for modules and functions in R (e.g. nwb$NWBHDF5IO instead of nwb.NWBHDF5IO)")
    str
  }, error = function(e) {
    "*** Cannot retrieve help function..."
  })

  cat_h1 <- function(s) {
    s <- paste(s, collapse = "")
    if(!nzchar(s)) {
      fill <- rep("-", getOption("width", 80) - 1)
      cat(fill, "\n", sep = "")
    } else {
      nfill <- getOption("width", 80) - nchar(s) - 7
      if(nfill > 0) {
        fill <- sprintf(" %s", paste(rep("-", nfill), collapse = ""))
      } else {
        fill <- ""
      }
      cat(sprintf("---- %s%s\n", s, fill))
    }
  }

  cat_h1("Original python documentation")
  cat(doc_string, "\n", sep = "\n")

  NextMethod()

  if(length(addendums)) {
    cat_h1("Footnotes")

    lapply(seq_along(addendums), function(ii) {
      cat_h1(addendums[[ii]])
    })

  }
  cat_h1(NULL)
  invisible(x)
}


easy_subset <- function(x, ...) {
  ndots <- ...length()
  if(!length(ndots)) {
    return(rpymat::py_get_item(x = x, key = rpymat::py_slice(NULL)))
  }
  call <- match.call(expand.dots = FALSE)
  call <- as.list(call)[-1]
  if("..." %in% names(call)) {
    call <- call[["..."]]
  } else {
    call <- rep(NA, ndots)
  }
  idx_call_args <- list()
  for(ii in seq_len(ndots)) {
    tryCatch({
      idx_set <- FALSE
      idx <- ...elt(ii)
      if(inherits(idx, "python.builtin.slice")) {
        idx_call_args[[ ii ]] <- idx
        idx_set <- TRUE
      } else if(is.null(idx)) {
        idx_call_args[[ ii ]] <- rpymat::py_slice(NULL)
        idx_set <- TRUE
      } else if (is.logical(idx)) {
        idx <- which(idx)
      }
      # want to prevent fancy indexing
      if( !idx_set && length(idx) == 1 ) {
        idx <- as.integer(idx)
        idx_call_args[[ ii ]] <- rpymat::py_slice(idx - 1L, idx)
        idx_set <- TRUE
      }
      if( !idx_set ) {
        # check if stride is the same
        stride <- unique(idx[-1] - idx[-length(idx)])
        if(length(stride) == 1) {
          idx_call_args[[ ii ]] <- rpymat::py_slice(as.integer(idx[[1]] - 1L), as.integer(idx[[length(idx)]]), as.integer(stride))
          idx_set <- TRUE
        } else {
          idx_call_args[[ ii ]] <- do.call(rpymat::tuple, as.list(as.integer(idx) - 1L))
          idx_set <- TRUE
        }
      }
      # if(length(idx) == 1) {
      #   idx_call_args[[ ii ]] <- as.integer(idx) - 1L
      # } else {
      #   idx_call <- call[[ ii ]]
      #   if(is.call(idx_call)) {
      #     if( idx_call[[1]] == ":" ) {
      #       # this can be turned to slices since user puts 1:n
      #       idx1 <- as.integer(idx[[1]])
      #       idx2 <- as.integer(idx[[length(idx)]])
      #       if( idx1 <= idx2 ) {
      #         idx_call_args[[ ii ]] <- rpymat::py_slice(idx1 - 1L, idx2)
      #       } else {
      #         idx_call_args[[ ii ]] <- rpymat::py_slice(idx1 - 1L, idx2, -1L)
      #       }
      #     } else if (
      #       idx_call[[1]] == "seq_along" ||
      #       idx_call[[1]] == "seq_len"
      #     ) {
      #       idx_call_args[[ ii ]] <- rpymat::py_slice(as.integer(idx[[1]] - 1), as.integer(idx[[length(idx)]]))
      #     } else if (
      #       idx_call[[1]] == "seq" ||
      #       idx_call[[1]] == "seq.int"
      #     ) {
      #       start <- as.integer(idx[[1]] - 1)
      #       end <- as.integer(idx[[length(idx)]])
      #       # I can trust length(idx) > 1
      #       by <- idx[[2]] - idx[[1]]
      #       if( !is.integer(by) && abs(by - round(by)) > .Machine$double.eps ) {
      #         stop("Cannot subset data using non-integer index")
      #       }
      #       by <- as.integer(by)
      #       idx_call_args[[ ii ]] <- rpymat::py_slice(start, end, by)
      #
      #     } else {
      #       # check if stride is the same
      #       stride <- unique(idx[-1] - idx[-length(idx)])
      #       if(length(stride) == 1) {
      #         idx_call_args[[ ii ]] <- rpymat::py_slice(as.integer(idx[[1]] - 1L), as.integer(idx[[length(idx)]]), as.integer(stride))
      #       } else {
      #         idx_call_args[[ ii ]] <- do.call(rpymat::tuple, as.list(as.integer(idx) - 1L))
      #       }
      #     }
      #   } else {
      #
      #   }
      # }


    }, error = function(e) {
      if(identical(e$message, "argument is missing, with no default")) {
        idx_call_args[[ ii ]] <<- rpymat::py_slice(NULL)
      } else {
        stop(e)
      }
    })
  }
  key <- do.call(rpymat::tuple, idx_call_args)
  return(rpymat::py_get_item(x = x, key = key))
}


#' @export
`[.h5py._hl.dataset.Dataset` <- function(x, ..., drop = TRUE, convert = FALSE) {
  re <- easy_subset(x, ...)
  if( convert ) {
    re <- rpymat::py_to_r(re)
    if( drop ) {
      re <- drop(re)
    }
  }
  re
}

#' @export
`[.hdmf.container.Data` <- function(x, ..., drop = TRUE, convert = FALSE) {

  data_type <- NA

  try(silent = TRUE, {
    data_type <- rpymat::py_to_r(x$data_type)
  })

  if(identical(data_type, "VectorData")) {
    re <- x$data$dataset[..., drop = drop]
    if( convert ) {
      re <- rpymat::py_to_r(re)
      if( drop ) {
        re <- drop(re)
      }
    }
    return(re)
  }
  if(identical(data_type, 'ElementIdentifiers')) {
    re <- x$id$data[..., drop = drop]
    if( convert ) {
      re <- rpymat::py_to_r(re)
      if( drop ) {
        re <- drop(re)
      }
    }
    return(re)
  }
  NextMethod()
}


#' @export
`[.hdmf.common.table.DynamicTable` <- function(x, i, j, ..., drop = TRUE, convert = FALSE) {
  if(missing(i) || is.null(i)) {
    i <- rpymat::py_slice(NULL)
  } else {
    i <- as.integer(i) - 1L
    if(anyNA(i)) {
      stop("Cannot have NA in subset index")
    }
  }
  df <- rpymat::py_to_r(x$get(key = i, df = TRUE))
  if(!missing(j) && !is.null(j)) {
    df <- df[,j, drop = drop]
  }
  if( !convert ) {
    df <- rpymat::r_to_py(df)
  }
  return(df)
}
