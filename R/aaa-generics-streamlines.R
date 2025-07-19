#' @name imaging-streamlines
#' @title Read and write streamlines
#' @description
#' High-level functions to read and write streamlines, supporting \code{'TCK'},
#' \code{'TRK'}, \code{'TT'} (read-only), \code{'VTK'} poly-data (including
#' legacy \code{'.vtk'}, 'XML'-based \code{'.vtp'}, 'HDF5'-based \code{'.vtpb'})
#' @param file,con path to the streamline data
#' @param x R object that can be converted into an \code{ieegio} streamlines
#' instance
#' @param format format to write to file, the file extensions must match
#' with the format
#' @param vox2ras volume index to 'RAS' coordinate transform matrix;
#' default is identity matrix and used by \code{'TRK'} format
#' @param class additional class to be added to the instance
#' @param ... passed to low-level functions accordingly
#' @returns \code{read_streamlines} and \code{as_ieegio_streamlines} returns
#' a streamlines instance.
#'
#' @examples
#'
#'
#' # toy example
#' curve <- function(t) {
#'   x <- sin(4 * t + sample(300, 1) / 100) + t + sample(seq_along(t)) / length(t) / 10
#'   y <- cos(sin(t) + 5 * t) + sample(seq_along(t)) / length(t) / 10
#'   z <- t * 3
#'   cbind(x, y, z)
#' }
#'
#' # 10 lines, each line is represented by nx3 matrix
#' tracts <- lapply(seq(100, 109), function(n) {
#'   curve(seq_len(n) / 100)
#' })
#'
#' # convert to streamline
#' x <- as_ieegio_streamlines(tracts)
#'
#' # Display
#' print(x)
#' plot(x, col = 1:10)
#'
#' if(system.file(package = "r3js") != '') {
#'   plot(x, method = "r3js")
#' }
#'
#' # Subset the first line (transformed)
#' coords <- x[[1]]$coords
#' head(coords)
#'
#' # Save different formats
#' tdir <- tempfile()
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#'
#' write_streamlines(x, file.path(tdir, "sample.tck"))
#' write_streamlines(x, file.path(tdir, "sample.trk"))
#' write_streamlines(x, file.path(tdir, "sample.trk.gz"))
#'
#' \dontrun{
#'
#'   # Require Python
#'   write_streamlines(x, file.path(tdir, "sample.vtk"))
#'   write_streamlines(x, file.path(tdir, "sample.vtp"))
#'   write_streamlines(x, file.path(tdir, "sample.vtpb"))
#'
#' }
#'
#'
#' # Read formats
#' y <- read_streamlines(file.path(tdir, "sample.trk"))
#'
#' # Compare x and y
#' diffs <- mapply(
#'   x = as.vector(x),
#'   y = as.vector(y),
#'   function(x, y) {
#'     range(x$coords - y$coords)
#'   }
#' )
#' # Should be floating errors
#' max(abs(diffs))
#'
#' unlink(tdir, recursive = TRUE)
#'
#' @export
read_streamlines <- function(file, ...) {
  as_ieegio_streamlines.character(x = file, ...)
}

#' @rdname imaging-streamlines
#' @export
write_streamlines <- function(x, con, format = c("auto", "tck", "trk", "vtk", "vtp", "vtpb"), ...) {
  if(!inherits(x, "ieegio_streamlines")) {
    x <- as_ieegio_streamlines(x = x, ...)
  }
  supported_formats <- c("auto", "tck", "trk", "vtk", "vtp", "vtpb", "h5")
  format <- match.arg(format)
  if(format == "auto") {
    format <- strsplit(tolower(con), "\\.")[[1]]
    if(format[[length(format)]] == 'gz') {
      format <- format[[length(format) - 1]]
    } else {
      format <- format[[length(format)]]
    }

  } else {
    if(format %in% c("vtk", "vtp", "vtpb", "h5") && !endsWith(tolower(con), format)) {
      stop(sprintf("The file name must end with the correct extension `.%s` when writing with format '%s'",
                   format, format))
    }
  }
  if(!isTRUE(format %in% supported_formats)) {
    warning("Cannot infer format from the file name. Saving as TCK file.")
    format <- "tck"
  }
  switch (
    format,
    'tck' = {
      datatype <- c(list(...)$datatype, x$header$datatype, "Float32LE")[[1]]
      if(!isTRUE(datatype %in% c("Float32LE", "Float32BE", "Float64LE", "Float64BE"))) {
        datatype <- "Float32LE"
      }
      io_write_tck(x = x, con = con, datatype = datatype)
    },
    'trk' = {
      io_write_trk(x = x, con = con, ...)
    },
    'vtk' = {
      io_write_vtk_streamlines(x = x, con = con, ...)
    },
    'vtp' = {
      io_write_vtk_streamlines(x = x, con = con, ...)
    },
    'vtpb' = {
      io_write_vtk_streamlines(x = x, con = con, ...)
    },
    'h5' = {
      io_write_vtk_streamlines(x = x, con = con, ...)
    },
    {
      stop("Unsupported format: ", format)
    }
  )

  invisible(normalizePath(con, winslash = "/"))
}

#' @rdname imaging-streamlines
#' @export
as_ieegio_streamlines <- function(x, ...) {
  UseMethod("as_ieegio_streamlines")
}

#' @rdname imaging-streamlines
#' @export
as_ieegio_streamlines.default <- function(x, vox2ras = NULL, ..., class = NULL) {
  # x is a list of streamlines
  # > names(x[[1]])
  # [1] "scalars"    "properties" "coords"     "num_points"

  # Default properties
  scalar_names <- NULL
  n_scalars <- 0

  property_names <- NULL
  n_properties <- 0

  n_streamlines <- 0

  more_headers <- list()
  headers <- list(...)

  # Get headers from x
  if(length(x)) {
    if(!is.list(x)) {
      stop("`x` must be a list.")
    }
    item_headers <- c("scalars", "properties", "coords", "num_points")

    item <- x[[1]]

    if(any(item_headers %in% names(item))) {
      n_streamlines <- length(x)
    } else if( is.matrix(item) && ncol(item) %in% c(3, 4) ) {
      n_streamlines <- length(x)
    } else if(is.list(x$tracks)) {
      if(is.list(x$header)) {
        more_headers <- x$header
        scalar_names <- x$header$scalar_names
        property_names <- x$header$property_names
        if(!is.matrix(vox2ras)) {
          vox2ras <- x$header$vox2ras
        }
      }
      x <- x$tracks
      n_streamlines <- length(x)
    } else {
      stop(
        sprintf(
          "Default `as_ieegio_streamlines` function ingests `x` as a list, \n  either a list of streamlines, with each streamline containing the following names: \n    %s; \n  or `x` is a list containing `header` and `tracts`.",
          paste(sQuote(item_headers), collapse = ",")
        )
      )
    }

  } else {
    x <- list()
    item <- list()
  }

  if(n_streamlines > 0) {
    item <- x[[1]]
    if(is.matrix(item) || !all(names(item) %in% c("scalars", "properties", "coords", "num_points"))) {
      x <- lapply(x, function(item) {
        if(is.matrix(item)) {
          return(list(
            coords = item,
            num_points = nrow(item)
          ))
        }
        list(
          scalars = item$scalars,
          properties = item$properties,
          coords = item$coords,
          num_points = item$num_points
        )
      })
    }
    item <- x[[1]]
  } else {
    item <- list()
  }

  # correct the scalars and properties
  n_scalars <- length(item$scalars)
  n_properties <- length(item$properties)

  if("scalar_names" %in% names(headers)) {
    # explicit override
    if(length(headers$scalar_names) == n_scalars) {
      scalar_names <- headers$scalar_names
    }
  }
  if(n_scalars != length(scalar_names)) {
    scalar_names <- sprintf("Scalar%d", seq_len(n_scalars))
  } else {
    blank_names <- scalar_names == ''
    if(any(blank_names)) {
      scalar_names[blank_names] <- sprintf("Scalar%03d", which(blank_names))
    }
  }


  if("property_names" %in% names(headers)) {
    # explicit override
    if(length(headers$property_names) == n_properties) {
      property_names <- headers$property_names
    }
  }
  if(n_properties != length(property_names)) {
    property_names <- sprintf("Scalar%d", seq_len(n_properties))
  } else {
    blank_names <- property_names == ''
    if(any(blank_names)) {
      property_names[blank_names] <- sprintf("Property%03d", which(blank_names))
    }
  }

  headers$n_scalars <- n_scalars
  headers$scalar_names <- scalar_names

  headers$n_properties <- n_properties
  headers$property_names <- property_names

  if(!is.matrix(vox2ras)) {
    vox2ras <- diag(1, 4)
  }
  headers$vox2ras <- vox2ras

  nms <- names(more_headers)
  nms <- nms[!nms %in% names(headers)]
  if(length(nms)) {
    headers[nms] <- more_headers[nms]
  }

  structure(
    list(
      header = headers,
      data = x
    ),
    class = unique(c(class, "ieegio_streamlines"))
  )
}

#' @export
as_ieegio_streamlines.character <- function(x, ...) {
  file <- tolower(x)
  split_str <- strsplit(file[[1]], "\\.")[[1]]
  if(split_str[[length(split_str)]] == "gz") {
    ext <- split_str[[length(split_str) - 1]]
  } else {
    ext <- split_str[[length(split_str)]]
  }
  re <- switch (
    ext,
    "tck" = {
      io_read_tck(x)
    },
    "tt" = {
      io_read_tt(x)
    },
    "trk" = {
      args <- list(...)
      msg <- NULL
      half_voxel_offset <- args$half_voxel_offset
      endian <- args$endian
      if(length(half_voxel_offset) != 1) {
        message(strwrap("TCK file is detected. Please read the following text carefully. TCK file format has ambiguity by definition: if the file is generated from TrackVis, nibabel, then there is a half-voxel offset, which can be corrected by setting `half_voxel_offset=TRUE`. However, if the file is generated from DSI-Studio, then there is no such offset, please set the `half_voxel_offset=FALSE`."), "\n\n  `half_voxel_offset` is unspecified, setting to `TRUE` by default.\n")
        half_voxel_offset <- TRUE
      }
      io_read_trk(x, half_voxel_offset = half_voxel_offset)
    },
    "vtk" = {
      io_read_vtk_streamlines(x)
    },
    "vtp" = {
      io_read_vtk_streamlines(x)
    },
    {
      stop("Unsupported streamline format. Supported formats: trk, trk.gz, tck, tt, tt.gz, vtk, vtp.")
    }
  )
  re
}

#' @export
as_ieegio_streamlines.ieegio_streamlines <- function(x, ...) {
  x
}

#' @export
format.ieegio_streamlines <- function(x, ...) {

  mat <- x$header$vox2ras
  mat <- apply(mat, 2, function(x) {
    re <- sprintf("%.6f", x)
    re <- as.double(re)
    re <- sprintf("%.4g", re)
    stringr::str_pad(re, width = max(nchar(re)), side = "left")
  })
  re <- apply(mat, 1, function(x) {
    sprintf("      [%s]", paste(x, collapse = "  "))
  })
  transforms_str <- paste(c("  Transforms (vox2ras):", re), collapse = "\n")

  if(x$header$n_scalars > 0) {
    scalar_str <- c(
      sprintf("  Scalars (%d):\n    %s", x$header$n_scalars, paste(x$header$scalar_names, collapse = ", "))
    )
  } else {
    scalar_str <- "  Scalars (none)"
  }

  if(x$header$n_properties > 0) {
    property_str <- c(
      sprintf("  Properties (%d):\n    %s", x$header$n_properties, paste(x$header$property_names, collapse = ", "))
    )
  } else {
    property_str <- "  Properties (none)"
  }


  re <- c(
    "<DTI Streamlines>",
    sprintf("  Total streamlines: %d", length(x$data)),
    transforms_str,
    scalar_str,
    property_str
  )
  paste(re, collapse = "\n")
}

#' @export
print.ieegio_streamlines <- function(x, ...) {
  # x <- io_read_trk("~/Downloads/hcp1065_avg_tracts_trk/association/AF_L.trk.gz")
  cat(c(format(x, ...), ""), sep = "\n")
}

#' @export
`[[.ieegio_streamlines` <- function(x, i, ..., apply_transform = TRUE) {
  streamline <- x$data[[i]]
  vox2ras <- x$header$vox2ras
  if(apply_transform) {
    coords <- streamline$coords
    if(ncol(coords) == 3) {
      coords <- cbind(coords, 1)
    }
    coords <- coords %*% t(vox2ras)
    coords <- coords[, c(1,2,3), drop = FALSE]
    streamline$coords <- coords
    streamline$transformed <- TRUE
  } else {
    streamline$transformed <- FALSE
  }
  streamline
}

#' @export
`[.ieegio_streamlines` <- function(x, i, ..., apply_transform = TRUE) {
  if(missing(i)) {
    idx <- seq_len(length(x$data))
  } else {
    idx <- unlist(c(i, ...))
  }
  lapply(idx, function(i) {
    x[[i, apply_transform = apply_transform]]
  })
}

#' @export
`[<-.ieegio_streamlines` <- function(x, ..., value) {
  stop("Not implemented yet.")
}

#' @export
`[[<-.ieegio_streamlines` <- function(x, ..., value) {
  stop("Not implemented yet.")
}

#' @export
length.ieegio_streamlines <- function(x) {
  length(x$data)
}

#' @export
as.vector.ieegio_streamlines <- function(x, ...) {
  unname(lapply(seq_len(length(x$data)), function(ii) {
    x[[ii]]
  }))
}

#' @export
plot.ieegio_streamlines <- function(x, method = c("basic", "r3js"), col = 2, sample = TRUE, ...) {

  method <- match.arg(method)

  # basic
  n <- length(x$data)

  if(length(col) < n) {
    if( length(col) == 0 ) {
      col <- 2
    }
    col <- rep(col, ceiling(n / length(col)))
  }

  if(!isFALSE(sample)) {
    if(isTRUE(sample)) {
      sample <- min(n, 1000)
    }
    idx <- order(sample(n, sample))
    tracts <- x[idx]
    tracts_col <- col[idx]
  } else {
    tracts <- x[]
    tracts_col <- col
  }

  if(method == "r3js") {
    x <- as_ieegio_streamlines.default(x = tracts, vox2ras = diag(1, 4))
    return(helper_r3js_render_streamlines(streamlines = x, col = col[[1]], ...))
  }

  ranges <- sapply(x[], function(item) {
    coords <- item$coords
    apply(coords, 2, range, na.rm = TRUE)[, 1:3]
  })
  ranges <- list(
    xlim = c(min(ranges[1, ]), max(ranges[2, ])),
    ylim = c(min(ranges[3, ]), max(ranges[4, ])),
    zlim = c(min(ranges[5, ]), max(ranges[6, ]))
  )

  oldpar <- graphics::par(mfrow = c(1, 3))
  on.exit({ graphics::par(oldpar) })

  args <- list(...)

  # axial xy plane
  plot(x = ranges$xlim, y = ranges$ylim, type = "n", asp = 1, xlab = "Left - Right", ylab = "Posterior - Anterior", main = "Axial (R = R)")
  lapply(seq_along(tracts), function(ii) {
    item <- tracts[[ii]]
    coords <- item$coords
    do.call(graphics::lines, c(list(x = coords[, 1], y = coords[, 2], col = tracts_col[[ii]]), args))
    return(invisible())
  })

  # Coronal xz plane
  plot(x = ranges$xlim, y = ranges$zlim, type = "n", asp = 1, xlab = "Left - Right", ylab = "Inferior - Superior", main = "Coronal (R = R)")
  lapply(seq_along(tracts), function(ii) {
    item <- tracts[[ii]]
    coords <- item$coords
    do.call(graphics::lines, c(list(x = coords[, 1], y = coords[, 3], col = tracts_col[[ii]]), args))
    return(invisible())
  })

  # Sagittal yz plane
  plot(x = ranges$ylim, y = ranges$zlim, type = "n", asp = 1, xlab = "Posterior - Anterior", ylab = "Inferior - Superior", main = "Sagittal (R = A)")
  lapply(seq_along(tracts), function(ii) {
    item <- tracts[[ii]]
    coords <- item$coords
    do.call(graphics::lines, c(list(x = coords[, 2], y = coords[, 3], col = tracts_col[[ii]]), args))
    return(invisible())
  })

  return(invisible())
}

