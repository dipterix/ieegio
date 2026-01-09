# Compute display range for volume data using percentile-based windowing
# Following conventions from FSLeyes, AFNI, MRIcroGL, and SPM
# @param data volume data array
# @param percentiles numeric vector of length 2, default c(2, 98)
# @param exclude_zeros NA (auto-detect), TRUE, or FALSE; when NA, excludes
#        zeros by default but includes them if lower percentile < -500
#        (indicating CT with -1024 background)
# @param original_meta optional list with cal_min/cal_max from NIfTI header
# @returns numeric vector of length 2 (min, max display range)
compute_display_range <- function(data, percentiles = c(2, 98),
                                   exclude_zeros = NA,
                                   original_meta = NULL) {

  # For 4D+ data, use only the first 3D volume for efficiency
  data_shape <- dim(data)
  if (length(data_shape) >= 4 && data_shape[[4]] > 1) {
    data <- data[, , , 1, drop = TRUE]
  }

  # Flatten data and remove NA/NaN/Inf
  values <- as.vector(data)
  values <- values[is.finite(values)]

  if (length(values) == 0) {
    return(c(0, 1))
  }

  # Handle exclude_zeros = NA (auto-detect)
  if (is.na(exclude_zeros)) {
    # First pass: exclude zeros
    nonzero_values <- values[values != 0]
    if (length(nonzero_values) > 0) {
      vlim <- stats::quantile(nonzero_values,
                              probs = percentiles / 100,
                              na.rm = TRUE, names = FALSE)
      # If lower bound < -500, likely CT data - include zeros and recompute
      if (vlim[[1]] < -500) {
        vlim <- stats::quantile(values,
                                probs = percentiles / 100,
                                na.rm = TRUE, names = FALSE)
      }
    } else {
      # All zeros - use full range
      vlim <- c(0, 0)
    }
  } else if (isTRUE(exclude_zeros)) {
    values <- values[values != 0]
    if (length(values) == 0) {
      return(c(0, 1))
    }
    vlim <- stats::quantile(values,
                            probs = percentiles / 100,
                            na.rm = TRUE, names = FALSE)
  } else {
    vlim <- stats::quantile(values,
                            probs = percentiles / 100,
                            na.rm = TRUE, names = FALSE)
  }

  # Detect statistical overlay: symmetric around zero, typical z-score range
  # (min > -30, min < 0, max > 0, max < 30)
  if (vlim[[1]] > -30 && vlim[[1]] < 0 && vlim[[2]] > 0 && vlim[[2]] < 30) {
    # Use symmetric range for statistical maps
    vlim <- max(abs(vlim)) * c(-1, 1)
  }

  # Check if meta has valid cal_min and cal_max (NIfTI header hints)
  if (is.list(original_meta) &&
      all(c("cal_max", "cal_min") %in% names(original_meta))) {
    cal_min <- original_meta$cal_min
    cal_max <- original_meta$cal_max
    # Use header values if they are set and different
    if (any(c(cal_min, cal_max) != 0) && cal_max != cal_min) {
      vlim <- c(cal_min, cal_max)
    }
  }

  # Ensure valid range
  if (vlim[[1]] >= vlim[[2]]) {
    vlim[[2]] <- vlim[[1]] + 1
  }

  as.numeric(vlim)
}

get_vox2fsl <- function(shape, pixdim, vox2ras) {
  voxToScaledVoxMat <- diag(c(pixdim[1:3], 1))
  isneuro <- det(vox2ras) > 0

  if( isneuro ) {
    flip <- matrix(c(
      -1, 0, 0, (shape[1] - 1) * pixdim[1],
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    ), nrow = 4L, byrow = TRUE)
    voxToScaledVoxMat <- flip %*% voxToScaledVoxMat
  }

  voxToScaledVoxMat
}

get_vox2ras_tkr <- function(vox2ras, crs_c) {
  # Torig <- cbind(Norig[, 4], -Norig[, 4] %*% header$internal$Pcrs_c)
  vox2ras_tkr <- vox2ras[1:3, 1:3]
  vox2ras_tkr <- cbind(vox2ras_tkr, - vox2ras_tkr %*% crs_c[1:3])
  vox2ras_tkr <- rbind(vox2ras_tkr, c(0, 0, 0, 1))
  vox2ras_tkr
}


new_volume <- function(type, header, meta, transforms, data, shape) {
  use_expression <- FALSE
  if(is.null(data)) {
    class <- c(
      sprintf("ieegio_%s", type),
      "ieegio_header_only",
      "ieegio_volume"
    )
    force(shape)
    header_only <- TRUE
  } else {
    class <- c(sprintf("ieegio_%s", type), "ieegio_volume")
    header_only <- FALSE
    if(is.language(data)) {
      use_expression <- TRUE
      force(shape)
    } else {
      shape <- dim(data)
    }
  }

  structure(
    class = class,
    list(
      type = type,
      header = header,
      original_meta = meta,
      header_only = header_only,
      use_expression = use_expression,
      shape = shape,
      transforms = transforms,
      data = data
    )
  )
}


#' @export
print.ieegio_volume <- function(x, ...) {
  cat(c(format(x, ...), ""), sep = "\n")
}

#' @export
format.ieegio_volume <- function(x, ...) {

  if(x$header_only) {
    banner <- "<Image Volume, header-only>"
    footer <- "* This object is read-only."
  } else {
    banner <- "<Image Volume>"
    footer <- NULL
  }

  if(length(x$transforms)) {
    transforms_str <- vapply(names(x$transforms), function(nm) {
      mat <- x$transforms[[nm]]
      mat <- apply(mat, 2, function(x) {
        re <- sprintf("%.6f", x)
        re <- as.double(re)
        re <- sprintf("%.4g", re)
        stringr::str_pad(re, width = max(nchar(re)), side = "left")
      })
      re <- apply(mat, 1, function(x) {
        sprintf("      [%s]", paste(x, collapse = "  "))
      })
      paste(c(sprintf("    %s:", nm), re), collapse = "\n")
    }, FUN.VALUE = "")
    transforms_str <- c("  Transforms:", transforms_str)
  } else {
    transforms_str <- "  Transforms: none"
  }

  re <- c(
    banner,
    sprintf("  Type : %s", paste(x$type, collapse = "/")),
    sprintf("  Shape: %s", deparse1(x$shape)),
    transforms_str,
    footer
  )
  paste(re, collapse = "\n")
}

#' @export
names.ieegio_volume <- function(x) {
  c("type", "header", "shape", "transforms", "data")
}

#' @export
`$.ieegio_volume` <- function(x, name) {
  if(identical(name, "data")) {
    if(isTRUE(.subset2(x, "use_expression"))) {
      expr <- .subset2(x, "data")
      data <- eval(expr, envir = x, enclos = new.env(parent = globalenv()))
    } else {
      data <- .subset2(x, "data")
    }
    return(data)
  }
  .subset2(x, name)
}

#' @export
`[[.ieegio_volume` <- function(x, i, ...) {
  `$.ieegio_volume`(x, i)
}

#' @export
`[.ieegio_volume` <- function(x, ..., unpack_rgba = TRUE) {
  if(isTRUE(.subset2(x, "header_only"))) {
    return(NULL)
  }
  re <- `$.ieegio_volume`(x, "data")[...]
  if(unpack_rgba && inherits(x, "ieegio_rgba")) {
    # make sure re is integer (32bit)
    storage.mode(re) <- "integer"
    shape <- dim(re)
    # this is rgb(a) array and auto-expansion is on
    alpha <- bitwShiftR(re, 24)
    re <- re - bitwShiftL(alpha, 24)
    blue <- bitwShiftR(re, 16)
    re <- re - bitwShiftL(blue, 16)
    green <- bitwShiftR(re, 8)
    red <- as.vector(re - bitwShiftL(green, 8))

    if(all(alpha == 0)) {
      re <- grDevices::rgb(red = red, green = green, blue = blue, maxColorValue = 255)
    } else {
      re <- grDevices::rgb(red = red, green = green, blue = blue, alpha = alpha, maxColorValue = 255)
    }
    dim(re) <- shape
  }
  return(re)
}

#' @export
`[<-.ieegio_volume` <- function(x, ..., value) {

  if(isTRUE(.subset2(x, "header_only"))) {
    stop("Head-only image. Cannot assign data")
  }

  if(inherits(x, "ieegio_rgba")) {
    # this is rgb(a), and value is treated as rgba
    shape <- dim(value)
    value <- grDevices::col2rgb(value, alpha = TRUE)
    if(all(value[4, ] == 255)) {
      value[4, ] <- 0L
    }
    value <- bitwShiftL(bitwShiftL(bitwShiftL(value[4, ], 8) + value[3, ], 8) + value[2, ], 8) + value[1, ]
    dim(value) <- shape
  }

  if(isTRUE(.subset2(x, "use_expression"))) {
    x$header[...] <- value
  } else {
    x$data[...] <- value
  }

  x
}


#' @name imaging-volume
#' @title Read and write volume data
#' @description
#' Read and write volume data ('MRI', 'CT', etc.) in 'NIfTI' or 'MGH' formats.
#' Please use \code{read_volume} and \code{write_volume} for high-level
#' function. These functions
#' will call other low-level functions internally.
#' @param file file path to read volume data
#' @format format of the file; default is auto-detection, other choices are
#' \code{'nifti'} and \code{'mgh'};
#' @param header_only whether to read header data only;
#' default is \code{FALSE}
#' @param method method to read the file; choices are \code{'oro'} (using
#' \code{\link[oro.nifti]{readNIfTI}}), \code{'rnifti'} (using
#' \code{\link[RNifti]{readNifti}}), and \code{'ants'} (using
#' \code{\link[rpyANTs]{as_ANTsImage}}).
#' @param format format of the file to be written; choices are \code{'auto'},
#' \code{'nifti'} or \code{'mgh'}; default is to \code{'auto'}
#' detect the format based on file names, which will save as a 'MGH' file
#' when file extension is \code{'mgz'} or \code{'mgh'}, otherwise 'NIfTI'
#' format. We recommend explicitly setting this argument
#' @param x volume data (such as 'NIfTI' image, array, or 'MGH')
#' to be saved
#' @param con file path to store image
#' @param vox2ras a \code{4x4} transform matrix from voxel indexing (column,
#' row, slice) to scanner (often 'T1-weighted' image) 'RAS'
#' (right-anterior-superior) coordinate
#' @param gzipped for writing \code{'nii'} data: whether the file needs to be
#' compressed; default is inferred from the file name. When the file ends
#' with \code{'nii'}, then no compression is used; otherwise the file will
#' be compressed. If the file name does not end with \code{'nii'} nor
#' \code{'nii.gz'}, then the file extension will be added automatically.
#' @param datatype_code,xyzt_units,intent_code additional flags for
#' 'NIfTI' headers, for advanced users
#' @param ... passed to other methods
#' @returns Imaging readers return \code{ieegio_volume} objects. The writers
#' return the file path to where the file is saved to.
#'
#' @examples
#'
#'
#' library(ieegio)
#'
#' nifti_file <- "brain.demosubject.nii.gz"
#'
#' # Use `ieegio_sample_data(nifti_file)`
#' #   to download sample data
#'
#'
#' if( ieegio_sample_data(nifti_file, test = TRUE) ) {
#'
#' # ---- NIfTI examples ---------------------------------------------
#'
#' file <- ieegio_sample_data(nifti_file)
#'
#' # basic read
#' vol <- read_volume(file)
#'
#' # voxel to scanner RAS
#' vol$transforms$vox2ras
#'
#' # to freesurfer surface
#' vol$transforms$vox2ras_tkr
#'
#' # to FSL
#' vol$transforms$vox2fsl
#'
#' plot(vol, position = c(10, 0, 30))
#'
#' # ---- using other methods --------------------------------------
#' # default
#' vol <- read_volume(file, method = "rnifti", format = "nifti")
#' vol$header
#'
#' # lazy-load nifti
#' vol2 <- read_volume(file, method = "oro", format = "nifti")
#' vol2$header
#'
#' \dontrun{
#' # requires additional python environment
#'
#' # Using ANTsPyx
#' vol3 <- read_volume(file, method = "ants", format = "nifti")
#' vol3$header
#'
#' }
#'
#' # ---- write --------------------------------------------------------
#'
#' # write as NIfTI
#' f <- tempfile(fileext = ".nii.gz")
#'
#' write_volume(vol, f, format = "nifti")
#'
#' # alternative method
#' write_volume(vol$header, f, format = "nifti")
#'
#' # write to mgz/mgh
#' f2 <- tempfile(fileext = ".mgz")
#'
#' write_volume(vol, f, format = "mgh")
#'
#' # clean up
#' unlink(f)
#' unlink(f2)
#'
#' # ---- Special case in WebAsssembly --------------------------------
#' # oro.nifti backend is always used
#'
#' # Emulate WebAssemply when RNifti is unavailable, using oro.nifti instead
#' old_opt <- options("ieegio.debug.emscripten" = TRUE)
#' on.exit({ options(old_opt) }, add = TRUE)
#'
#'
#' # In WebAssemply, RNifti is not available, using oro.nifti instead
#' vol <- read_volume(file)
#'
#' stopifnot(vol$type[[1]] == "oro")
#'
#' # Cleanup: make sure the options are reset
#' options(old_opt)
#'
#' }
#'
#' @export
read_volume <- function(file, header_only = FALSE,
                        format = c("auto", "nifti", "mgh"), ...) {
  format <- match.arg(format)

  if( format == "auto" ) {
    fname <- tolower(basename(file))
    if( endsWith(fname, "mgh") || endsWith(fname, "mgz") ) {
      format <- "mgh"
    } else {
      format <- "nifti"
    }
  }

  if(format == "mgh") {
    # MGH
    return(io_read_mgz(file = file, header_only = header_only))
  }
  return(io_read_nii(file = file, header_only = header_only, ...))
}

#' @rdname imaging-volume
#' @export
write_volume <- function(x, con, format = c("auto", "nifti", "mgh"), ...) {

  format <- match.arg(format)

  if(format == "auto") {
    if(endsWith(tolower(con), "mgz") || endsWith(tolower(con), "mgh")) {
      format <- "mgh"
    }
  }
  if(format == "mgh") {
    re <- io_write_mgz(x = x, con = con, ...)
  } else {
    re <- io_write_nii(x = x, con = con, ...)
  }
  re
}

#' @title Plot '3D' volume in anatomical slices
#' @param x \code{'ieegio_volume'} object; see \code{\link{read_volume}}
#' @param position position in \code{'RAS'} (right-anterior-superior) coordinate
#' system on which cross-hair should focus
#' @param center_position whether to center canvas at \code{position},
#' default is \code{FALSE}
#' @param which which slice to plot; choices are \code{"coronal"},
#' \code{"axial"}, and \code{"sagittal"}
#' @param slice_index length of 1: if \code{x} has fourth dimension
#' (e.g. 'fMRI'), then which slice index to draw
#' @param transform which transform to apply, can be a 4-by-4 matrix,
#' an integer or name indicating the matrix in \code{x$transforms}; this needs
#' to be the transform matrix from voxel index to 'RAS'
#' (right-anterior-superior coordinate system), often called \code{'xform'},
#' \code{'sform'}, \code{'qform'} in 'NIfTI' terms, or \code{'Norig'} in
#' 'FreeSurfer'
#' @param zoom zoom-in level
#' @param pixel_width pixel size, ranging from \code{0.05} to \code{50};
#' default is the half of \code{zoom} or \code{1}, whichever is greater;
#' the unit of \code{pixel_width} divided by \code{zoom} is milliliter
#' @param col color palette for continuous \code{x} values
#' @param alpha opacity value if the image is to be displayed with transparency
#' @param crosshair_gap the cross-hair gap in milliliter
#' @param crosshair_lty the cross-hair line type
#' @param crosshair_col the cross-hair color; set to \code{NA} to hide
#' @param label_col the color of anatomical axis labels (i.e. \code{"R"} for
#' right, \code{"A"} for anterior, and \code{"S"} for superior); default is
#' the same as \code{crosshair_col}
#' @param continuous reserved
#' @param vlim the range limit of the data; default is computed from range of
#' \code{x$data}; data values exceeding the range will be trimmed
#' @param add whether to add the plot to existing underlay;
#' default is \code{FALSE}
#' @param axes whether to draw axes; default is \code{FALSE}
#' @param background,foreground background and foreground colors; default is
#' the first and last elements of \code{col}
#' @param main,... passed to \code{\link[graphics]{image}}
#' @param .xdata default is \code{x$data}, used to speed up the calculation
#' when multiple different angles are to be plotted
#' @examples
#'
#' library(ieegio)
#'
#' nifti_file <- "nifti/rnifti_example.nii.gz"
#' nifti_rgbfile <- "nifti/rnifti_example_rgb.nii.gz"
#'
#' # Use
#' #   `ieegio_sample_data(nifti_file)`
#' # and
#' #   `ieegio_sample_data(nifti_rgbfile)`
#' # to download sample data
#'
#'
#' if(
#'   ieegio_sample_data(nifti_file, test = TRUE) &&
#'   ieegio_sample_data(nifti_rgbfile, test = TRUE)
#' ) {
#'
#' # ---- NIfTI examples ---------------------------------------------
#'
#' underlay_path <- ieegio_sample_data(nifti_file)
#' overlay_path <- ieegio_sample_data(nifti_rgbfile)
#'
#' # basic read
#' underlay <- read_volume(underlay_path)
#' overlay <- read_volume(overlay_path)
#'
#' par(mfrow = c(1, 3), mar = c(0, 0, 3.1, 0))
#'
#' ras_position <- c(50, -10, 15)
#'
#' ras_str <- paste(sprintf("%.0f", ras_position), collapse = ",")
#'
#' for(which in c("coronal", "axial", "sagittal")) {
#'   plot(x = underlay, position = ras_position, crosshair_gap = 10,
#'        crosshair_lty = 2, zoom = 3, which = which,
#'        main = sprintf("%s T1RAS=[%s]", which, ras_str))
#'   plot(x = overlay, position = ras_position,
#'        crosshair_gap = 10, label_col = NA,
#'        add = TRUE, alpha = 0.9, zoom = 5, which = which)
#' }
#'
#'
#' }
#'
#' @export
plot.ieegio_volume <- function(
    x, position = c(0, 0, 0), center_position = FALSE,
    which = c("coronal", "axial", "sagittal"), slice_index = 1L,
    transform = "vox2ras", zoom = 1, pixel_width = max(zoom / 2, 1),
    col = c("black", "white"), alpha = NA,
    crosshair_gap = 4, crosshair_lty = 2,
    crosshair_col = "#00FF00A0", label_col = crosshair_col,
    continuous = TRUE, vlim = NULL,
    add = FALSE, main = "", axes = FALSE,
    background = col[[1]], foreground = col[[length(col)]],
    ..., .xdata = x$data) {

  which <- match.arg(which)

  # DIPSAUS DEBUG START
  # x <- read_volume(ieegio_sample_data("brain.demosubject.nii.gz"))
  # x <- ieegio::read_volume(system.file("extdata", "example_rgb.nii.gz", package="RNifti"))
  # x <- read_volume(ieegio_sample_data("nifti/rnifti_example.nii.gz"))
  # transform = "vox2ras"
  # which <- "coronal"
  # position <- c(10, 10, 10)
  # list2env(list(center_position = FALSE,
  #               transform = "vox2ras", zoom = 1, pixel_width = 0.5,
  #               vlim = NULL, col = c("black", "white"),
  #               add = FALSE, main = "", axes = TRUE), envir=.GlobalEnv)
  # background <- col[[1]]
  # foreground = col[[length(col)]]
  # slice_index <- 1
  # crosshair_col = "#00FF00A0"
  continuous <- TRUE

  if(is.matrix(transform)) {
    stopifnot(nrow(transform) == 4 && ncol(transform) == 4)
    mat <- transform
  } else {
    mat <- x$transforms[[transform]]
    if(is.null(mat) || !is.matrix(mat)) {
      stop("Cannot interpret transform", sQuote(transform))
    }
  }
  ras2vox <- solve(mat)

  x_shape <- x$shape

  slice_index <- as.integer(slice_index[[1]])
  if(slice_index <= 1L) {
    slice_index <- 1L
  }
  if(length(x_shape) >= 4) {
    if(slice_index > x_shape[[4]]) {
      stop("Cannot query the slice index. Maximum index is ", x_shape[[4]], ".")
    }
  } else {
    slice_index <- 1L
  }

  if(inherits(x, "ieegio_rgba")) {
    # this is RGB(A) image
    is_rgba <- TRUE
    x_data <- x[drop = FALSE]
  } else {
    is_rgba <- FALSE
    x_data <- .xdata
    if( continuous ) {
      if(length(vlim) == 1) {
        vlim <- c(-1, 1) * abs(vlim)
      } else if (length(vlim) >= 2) {
        vlim <- range(vlim, na.rm = TRUE)
      } else {
        # Use percentile-based windowing (similar to FSLeyes, AFNI, MRIcroGL)
        vlim <- compute_display_range(
          data = x_data,
          percentiles = c(2, 98),
          exclude_zeros = NA,
          original_meta = .subset2(x, "original_meta")
        )
      }
      if(length(col) < 256) {
        col <- grDevices::colorRampPalette(col)(256)
      }
    } else {
      # TODO: add atlas labels if given
      vlim <- range(x_data)
    }
  }

  if(length(pixel_width) < 1) {
    pixel_width <- 1
  }
  pixel_width <- abs(pixel_width)
  pixel_width[pixel_width > 50] <- 50
  pixel_width[pixel_width < 0.05] <- 0.05
  if(length(pixel_width) == 1) {
    p_x <- pixel_width
    p_y <- pixel_width
  } else {
    p_x <- pixel_width[[1]]
    p_y <- pixel_width[[2]]
  }

  # default (if pixel_width=1) is 255 x 255 pixels
  tmp <- seq.int(0, 127, by = p_x)
  x_axis <- c(-rev(tmp), tmp[-1]) / zoom
  tmp <- seq.int(0, 127, by = p_y)
  y_axis <- c(-rev(tmp), tmp[-1]) / zoom

  # Now get center of the image
  if( center_position ) {

    center_ras <- position

  } else {

    center_ras <- position * (-1 / zoom + 1)

  }
  switch(
    which,
    "coronal" = {
      x_axis <- x_axis + center_ras[[1]]
      center_ras[[2]] <- position[[2]]
      y_axis <- y_axis + center_ras[[3]]
    },
    "axial" = {
      x_axis <- x_axis + center_ras[[1]]
      y_axis <- y_axis + center_ras[[2]]
      center_ras[[3]] <- position[[3]]
    },
    "sagittal" = {
      center_ras[[1]] <- position[[1]]
      x_axis <- x_axis + center_ras[[2]]
      y_axis <- y_axis + center_ras[[3]]
    }
  )
  tmp <- as.matrix(expand.grid(x_axis, y_axis))

  vox_idx <- switch(
    which,
    "coronal" = {
      # x = left -> right
      # y = inf -> sup
      rbind(
        tmp[, 1],
        position[[2]],
        tmp[, 2],
        1
      )
    },
    "axial" = {
      # x = left -> right
      # y = post -> ant
      rbind(
        tmp[, 1],
        tmp[, 2],
        position[[3]],
        1
      )
    },
    "sagittal" = {
      # x = post -> ant
      # y = inf -> sup
      rbind(
        position[[1]],
        tmp[, 1],
        tmp[, 2],
        1
      )
    }
  )
  vox_idx <- ras2vox %*% vox_idx
  vox_idx <- round(vox_idx[1:3, , drop = FALSE])
  dim(vox_idx) <- c(3L, nrow(tmp))

  vox_idx[vox_idx < 0] <- NA_integer_
  vox_idx[vox_idx >= x_shape[1:3]] <- NA_integer_
  x_shape_cumprod <- cumprod(x_shape)
  multi <- c(1, x_shape_cumprod[1:2])
  vox_idx <- colSums(vox_idx * multi) + 1L +
    x_shape_cumprod[[3]] * (slice_index - 1)

  vox_data <- array(x_data[vox_idx], dim = c(length(x_axis), length(y_axis)))
  # Clamp vox_data to vlim so values outside range are rendered
  # as boundary colors instead of transparent/NA
  vox_data[vox_data < vlim[1]] <- vlim[1]
  vox_data[vox_data > vlim[2]] <- vlim[2]

  if(!add) {
    oldpar <- graphics::par(
      fg = foreground,
      col.axis = foreground,
      col.lab = foreground,
      col.main = foreground,
      col.sub = foreground,
      bg = background
    )
    on.exit({ graphics::par(oldpar) })
  }

  if( is_rgba ) {
    # RGBA raster
    xlim <- range(x_axis, na.rm = TRUE)
    ylim <- range(y_axis, na.rm = TRUE)
    if( !add ) {
      plot(
        x = xlim,
        y = ylim,
        type = "n",
        axes = axes,
        asp = 1,
        xlab = "",
        ylab = "",
        # ...,
        main = main
      )
    }
    vox_data <- t(vox_data)[rev(seq_len(ncol(vox_data))), , drop = FALSE]
    if(!is.na(alpha)) {
      alpha_255 <- floor(alpha * 255)
      alpha_255[alpha_255 < 0] <- 0
      alpha_255[alpha_255 > 255] <- 255
      vox_dm <- dim(vox_data)
      vox_data <- as.vector(vox_data)
      missing_color <- is.na(vox_data)
      vox_data <- grDevices::col2rgb(vox_data, alpha = TRUE)
      vox_data[4, ] <- alpha_255
      vox_data <- array(
        grDevices::rgb(vox_data[1, ], vox_data[2, ], vox_data[3, ], vox_data[4, ], maxColorValue = 255),
        dim = vox_dm
      )
      vox_data[missing_color] <- NA_character_
    }
    graphics::rasterImage(grDevices::as.raster(vox_data),
                           xlim[[1]],
                           ylim[[1]],
                           xlim[[2]],
                           ylim[[2]],
                           interpolate = FALSE)
  } else {
    if(!is.na(alpha)) {
      alpha_255 <- floor(alpha * 255)
      alpha_255[alpha_255 < 0] <- 0
      alpha_255[alpha_255 > 255] <- 255
      col <- grDevices::col2rgb(col, alpha = TRUE)
      col[4, ] <- alpha_255
      col <- grDevices::rgb(col[1, ], col[2, ], col[3, ], col[4, ], maxColorValue = 255)
    }

    graphics::image(
      z = vox_data,
      x = x_axis,
      y = y_axis,
      col = col,
      zlim = vlim,
      add = add,
      axes = axes,
      asp = 1,
      xlab = "",
      ylab = "",
      useRaster = TRUE,
      main = main,
      ...
    )
  }

  xrg <- range(x_axis)
  xrg <- c(xrg[[1]], mean(xrg), xrg[[2]])
  yrg <- range(y_axis)
  yrg <- c(yrg[[1]], mean(yrg), yrg[[2]])
  ax_label <- c("I", "L", "S", "R")

  c_xy <- switch(
    which,
    "coronal" = {
      # x = left -> right
      # y = inf -> sup
      c_xy <- position[c(1, 3)]
      ax_label <- c("I", "L", "S", "R")
      c_xy
    },
    "axial" = {
      # x = left -> right
      # y = post -> ant
      c_xy <- position[c(1, 2)]
      ax_label <- c("P", "L", "A", "R")
      c_xy
    },
    "sagittal" = {
      # x = post -> ant
      # y = inf -> sup
      ax_label <- c("I", "P", "S", "A")
      c_xy <- position[c(2, 3)]
      c_xy
    }
  )

  # bottom
  if(!is.na(label_col)) {
    graphics::text(
      x = xrg[[2]],
      y = yrg[[1]],
      labels = ax_label[[1]],
      adj = c(0.5, -1),
      col = label_col,
    )
    # left
    graphics::text(
      x = xrg[[1]],
      y = yrg[[2]],
      labels = ax_label[[2]],
      adj = c(-1, 0.5),
      col = label_col,
    )
    # top
    graphics::text(
      x = xrg[[2]],
      y = yrg[[3]],
      labels = ax_label[[3]],
      adj = c(0.5, 2),
      col = label_col,
    )
    # right
    graphics::text(
      x = xrg[[3]],
      y = yrg[[2]],
      labels = ax_label[[4]],
      adj = c(2, 0.5),
      col = label_col,
    )
  }
  if(!is.na(crosshair_col) && !is.na(crosshair_gap)) {
    crosshair_delta <- crosshair_gap / 2

    graphics::segments(
      x0 = c_xy[[1]] - crosshair_delta,
      y0 = c_xy[[2]],
      x1 = xrg[[1]],
      y1 = c_xy[[2]],
      col = crosshair_col,
      lty = crosshair_lty
    )
    graphics::segments(
      x0 = c_xy[[1]] + crosshair_delta,
      y0 = c_xy[[2]],
      x1 = xrg[[3]],
      y1 = c_xy[[2]],
      col = crosshair_col,
      lty = crosshair_lty
    )

    graphics::segments(
      x0 = c_xy[[1]],
      y0 = c_xy[[2]] - crosshair_delta,
      x1 = c_xy[[1]],
      y1 = yrg[[1]],
      col = crosshair_col,
      lty = crosshair_lty
    )
    graphics::segments(
      x0 = c_xy[[1]],
      y0 = c_xy[[2]] + crosshair_delta,
      x1 = c_xy[[1]],
      y1 = yrg[[3]],
      col = crosshair_col,
      lty = crosshair_lty
    )
  }

}


#' @export
dim.ieegio_volume <- function(x) {
  x$shape
}

#' @export
length.ieegio_volume <- function(x) {
  prod(x$shape)
}


#' Convert objects to \code{'ieegio'} image volumes
#' @description
#' Convert array, path, or 'NIfTI' images in other formats to \code{'ieegio'}
#' image volume instance
#'
#' @param x R object such as array, image path, or objects such as
#' \code{'RNifti'} or \code{'oro.nifti'} image instances
#' @param ... passed to other methods
#' @param vox2ras a \code{4x4} 'affine' matrix representing the transform
#' from 'voxel' index (column-row-slice) to 'RAS'
#' (right-anterior-superior) coordinate. This transform is often called
#' \code{'xform'}, \code{'sform'}, \code{'qform'} in 'NIfTI' terms, or
#' \code{'Norig'} in 'FreeSurfer'
#' @param as_color for converting arrays to volume, whether to treat \code{x}
#' as array of colors; default is true when \code{x} is a raster matrix (
#' matrix of color strings) and false when \code{x} is not a character array.
#'
#' @returns An \code{ieegio} volume object; see \code{\link{imaging-volume}}
#'
#' @examples
#'
#'
#' shape <- c(50, 50, 50)
#' vox2ras <- matrix(
#'   c(-1, 0, 0, 25,
#'     0, 0, 1, -25,
#'     0, -1, 0, 25,
#'     0, 0, 0, 1),
#'   nrow = 4, byrow = TRUE
#' )
#'
#' # continuous
#' x <- array(rnorm(125000), shape)
#'
#' volume <- as_ieegio_volume(x, vox2ras = vox2ras)
#' plot(volume, zoom = 3, pixel_width = 0.5)
#'
#' # color rgb(a)
#' x <- array(
#'   sample(c("red","blue", "green", "cyan", "yellow"),
#'          12500, replace = TRUE),
#'   shape
#' )
#' rgb <- as_ieegio_volume(x, vox2ras = vox2ras)
#' plot(rgb, zoom = 3, pixel_width = 0.5)
#'
#'
#'
#' # ---- When RNifti package is not available ---------------------------
#'
#' # Emulate WebAssemply when RNifti is unavailable, using oro.nifti instead
#' old_opt <- options("ieegio.debug.emscripten" = TRUE)
#' on.exit({ options(old_opt) }, add = TRUE)
#'
#' shape <- c(50, 50, 50)
#' vox2ras <- matrix(
#'   c(-1, 0, 0, 25,
#'     0, 0, 1, -25,
#'     0, -1, 0, 25,
#'     0, 0, 0, 1),
#'   nrow = 4, byrow = TRUE
#' )
#'
#' # continuous
#' x <- array(rnorm(125000), shape)
#'
#' # In WebAssemply, RNifti is not available, using oro.nifti instead
#' volume <- as_ieegio_volume(x, vox2ras = vox2ras)
#'
#' stopifnot(volume$type[[1]] == "oro")
#'
#' plot(volume, zoom = 3, pixel_width = 0.5)
#'
#' # Cleanup: make sure the options are reset
#' options(old_opt)
#'
#'
#' @export
as_ieegio_volume <- function(x, ...) {
  UseMethod("as_ieegio_volume")
}

#' @export
as_ieegio_volume.default <- function(x, ...) {
  stop("`as_ieegio_volume`: `x` to NIfTI or MGH conversion is not supported due to unrecognized format")
}

#' @rdname as_ieegio_volume
#' @export
as_ieegio_volume.character <- function(x, ...) {
  stopifnot(file.exists(x))
  read_volume(file = x, header_only = FALSE, ...)
}

#' @rdname as_ieegio_volume
#' @export
as_ieegio_volume.ieegio_volume <- function(x, ...) {
  return(x)
}

#' @rdname as_ieegio_volume
#' @export
as_ieegio_volume.array <- function(x, vox2ras = NULL, as_color = is.character(x), ...) {
  if(!is.matrix(vox2ras)) {
    warning("`io_write_nii.array`: `vox2ras` is missing, using identity matrix. Please specify voxel-to-RAS transform (4x4 matrix).")
    vox2ras <- diag(1, 4)
  }
  stopifnot(is.matrix(vox2ras) && nrow(vox2ras) == 4 && ncol(vox2ras) == 4)

  quaternion <- mat_to_quaternion(vox2ras)

  shape <- dim(x)
  nshapes <- length(shape)

  # In NIFTI-1 files, dimensions 1,2,3 are for space, dimension 4 is for time,
  # and dimension 5 is for storing multiple values at each spatiotemporal
  # voxel.
  stopifnot(nshapes %in% c(3, 4, 5))
  if(length(shape) > 3) {
    nframes <- shape[[4]]
  } else {
    nframes <- 1
  }

  m33 <- vox2ras[1:3, 1:3]
  pixdim <- sqrt(colSums(m33^2))
  pixdim <- c(sign(det(m33)), pixdim, nframes, 0, 0, 0)
  pixdim <- as.double(pixdim)

  # Drop frames if necessary
  if( nframes == 1 && length(shape) == 4 ) {
    pixdim[[5]] <- 0
    shape <- shape[1:3]
    x <- array(x[seq_len(prod(shape))], dim = shape)
  }

  nshapes <- length(shape)

  if(as_color) {
    # treat as rgbArray
    rgba <- grDevices::col2rgb(x, alpha = TRUE)
    storage.mode(rgba) <- "integer"
    rgba[, is.na(x)] <- 0L
    if(all(rgba[4, ] == 255)) {
      x <- RNifti::rgbArray(rgba[1, ], rgba[2, ], rgba[3, ], max = 255L, dim = shape)
      datatype_code <- as_nifti_type("NIFTI_TYPE_RGB24")
    } else {
      x <- RNifti::rgbArray(rgba[1, ], rgba[2, ], rgba[3, ], rgba[4, ], max = 255L, dim = shape)
      datatype_code <- as_nifti_type("NIFTI_TYPE_RGBA32")
    }
    type <- c("rnifti", "rgba", "nifti")
  } else {
    # check if we should use oro.nifti (when RNifti is not available)
    x[is.na(x)] <- 0
    rg <- range(x)

    if(all(x - round(x) == 0)) {
      if( rg[[1]] >= 0 && rg[[2]] <= 255 ) {
        # UINT8
        datatype_code <- 2L
        storage.mode(x) <- "integer"
      } else if ( rg[[1]] >= -32768 && rg[[2]] <= 32768 ) {
        # INT16
        datatype_code <- 4L
        storage.mode(x) <- "integer"
      } else if ( rg[[1]] >= -2147483648 && rg[[2]] <= 2147483648 ) {
        # INT32
        datatype_code <- 8L
        storage.mode(x) <- "integer"
      } else {
        # FLOAT32
        datatype_code <- 16L
      }
    } else {
      # FLOAT32
      datatype_code <- 16L
    }
    type <- c("rnifti", "nifti")
  }

  reference <- list(
    xyzt_units = 10L,
    datatype = datatype_code,
    bitpix = compute_nifti_bitpix(datatype_code),
    qform_code = 1L,
    sform_code = 1L,
    pixdim = pixdim,
    quatern_b = quaternion[[1]],
    quatern_c = quaternion[[2]],
    quatern_d = quaternion[[3]],
    qoffset_x = vox2ras[1, 4],
    qoffset_y = vox2ras[2, 4],
    qoffset_z = vox2ras[3, 4],
    srow_x = vox2ras[1, ],
    srow_y = vox2ras[2, ],
    srow_z = vox2ras[3, ],
    regular = "r",
    ...
  )

  use_oro <- FALSE
  if(!as_color && (get_os() == "emscripten" || getOption("ieegio.debug.emscripten", FALSE))) {
    # This is running for WASM (special as RNifti is not available in WASM)
    use_oro <- TRUE
    header <- oro.nifti::as.nifti(x)
    header@xyzt_units <- reference$xyzt_units
    header@datatype <- reference$datatype
    header@bitpix <- reference$bitpix
    header@qform_code <- reference$qform_code
    header@sform_code <- reference$sform_code
    header@pixdim <- reference$pixdim
    header@quatern_b <- reference$quatern_b
    header@quatern_c <- reference$quatern_c
    header@quatern_d <- reference$quatern_d
    header@qoffset_x <- reference$qoffset_x
    header@qoffset_y <- reference$qoffset_y
    header@qoffset_z <- reference$qoffset_z
    header@srow_x <- reference$srow_x
    header@srow_y <- reference$srow_y
    header@srow_z <- reference$srow_z
    header@regular <- reference$regular

    header@slice_start <- reference$slice_start %||% header@slice_start
    header@slice_end <- reference$slice_end %||% header@slice_end
    header@slice_code <- reference$slice_code %||% header@slice_code
    header@slice_duration <- reference$slice_duration %||% header@slice_duration

    header@scl_slope <- reference$scl_slope %||% header@scl_slope
    header@scl_inter <- reference$scl_inter %||% header@scl_inter

    header@cal_max <- reference$cal_max %||% header@cal_max
    header@cal_min <- reference$cal_min %||% header@cal_min

    header@toffset <- reference$toffset %||% header@toffset
    header@magic <- reference$magic %||% header@magic
    header@intent_name <- reference$intent_name %||% header@intent_name
    header@glmax <- reference$glmax %||% header@glmax
    header@glmin <- reference$glmin %||% header@glmin
    header@descrip <- reference$descrip %||% header@descrip
    header@aux_file <- reference$aux_file %||% header@aux_file

  } else {
    header <- RNifti::asNifti(x, reference = reference)
  }

  meta <- as_nifti_header(header)

  # vox2ras_tkr
  # vox2ras_tkr <- vox2ras
  # vox2ras_tkr[1:3, 4] <- - vox2ras[1:3, 1:3] %*% shape[1:3] / 2
  vox2ras_tkr <- get_vox2ras_tkr(vox2ras, shape / 2)

  # vox2fsl
  vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

  transforms <- list(
    vox2ras = vox2ras,
    vox2ras_tkr = vox2ras_tkr,
    vox2fsl = vox2fsl
  )

  if( use_oro ) {
    new_volume(
      type = c("oro", "nifti"),
      header = header,
      meta = meta,
      transforms = transforms,
      shape = shape,
      data = quote({ header@.Data })
    )
  } else {
    new_volume(
      type = type,
      header = header,
      meta = meta,
      transforms = transforms,
      shape = shape,
      data = quote({
        v <- header[drop = FALSE]
        class(v) <- "array"
        attr(v, ".nifti_image_ptr") <- NULL
        v
      })
    )
  }

}

#' @rdname as_ieegio_volume
#' @export
as_ieegio_volume.niftiImage <- function(x, ...) {
  header <- RNifti::asNifti(x)
  meta <- RNifti::niftiHeader(x)

  # vox2ras
  qfcode <- meta$qform_code
  sfcode <- meta$sform_code

  qform <- structure(
    RNifti::xform(meta, useQuaternionFirst = TRUE),
    code = qfcode
  )
  sform <- structure(
    RNifti::xform(meta, useQuaternionFirst = FALSE),
    code = sfcode
  )

  if( sfcode > 0 ) {
    vox2ras <- structure(sform, which_xform = "sform")
  } else {
    vox2ras <- structure(qform, which_xform = "qform")
  }
  if(nrow(vox2ras) == 3) {
    vox2ras <- rbind(vox2ras, c(0, 0, 0, 1))
  }

  pixdim <- RNifti::pixdim(meta)
  shape <- dim(header)

  # vox2ras_tkr
  # vox2ras_tkr <- vox2ras
  # vox2ras_tkr[1:3, 4] <- - vox2ras[1:3, 1:3] %*% shape[1:3] / 2
  vox2ras_tkr <- get_vox2ras_tkr(vox2ras, shape / 2)

  # vox2fsl
  vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

  transforms <- list(
    vox2ras = vox2ras,
    vox2ras_tkr = vox2ras_tkr,
    vox2fsl = vox2fsl
  )

  if(inherits(x, "rgbArray")) {
    type <- c("rnifti", "rgba", "nifti")
  } else {
    type <- c("rnifti", "nifti")
  }

  new_volume(
    type = type,
    header = header,
    meta = meta,
    transforms = transforms,
    shape = shape,
    data = quote({
      v <- header[drop = FALSE]
      class(v) <- "array"
      attr(v, ".nifti_image_ptr") <- NULL
      v
    })
  )
}

#' @rdname as_ieegio_volume
#' @export
as_ieegio_volume.nifti <- function(x, ...) {
  as_ieegio_volume.niftiImage(x, ...)
}

#' @rdname as_ieegio_volume
#' @export
as_ieegio_volume.ants.core.ants_image.ANTsImage <- function(x, ...) {

  f <- tempfile(fileext = ".nii.gz")
  on.exit({
    if(file.exists(f)) {
      unlink(f)
    }
  })
  path <- write_volume(x, f, format = "nifti")
  read_volume(path, format = "nifti", method = "rnifti")

}

#' @title Merge \code{'ieegio'} volumes
#' @description
#' Merge volume data into base image. The images must be static 3-dimensional
#' volume data. Currently time-series or 4-dimensional data is not supported.
#' @param x base image to be merged
#' @param y,... images to be merged into \code{x}
#' @param thresholds numerical threshold for \code{y,...}, can be length of one
#' or more, if images to overlay is more than one. The image values lower
#' than the threshold will be trimmed out
#' @param reshape output shape, default is the dimension of \code{x}; if
#' changed, then the underlay will be sampled into the new shape
#' @param na_fill how to handle missing values; default is \code{NA}; for
#' compatibility, you might want to set to 0
#' @returns Merged volume with dimension \code{reshape}.
#'
#' @examples
#'
#' base_array <- array(0, c(15, 15, 15))
#' base_array[4:6, 4:6, 4:6] <- runif(27) * 255
#'
#' # generate a 15x15x15 mask with 1mm spacing
#' vox2ras1 <- diag(1, 4)
#' vox2ras1[1:3, 4] <- -5
#' x <- as_ieegio_volume(base_array, vox2ras = vox2ras1)
#'
#'
#' # 15x15x15 mask with 0.5mmx1mmx1mm spacing but oblique to `x`
#' vox2ras2 <- matrix(
#'   nrow = 4, byrow = TRUE,
#'   c(
#'     2, 0.2, -0.1, -3,
#'     -0.2, 1, 0.4, -4,
#'     0.3, -0.1, 1, -1,
#'     0, 0, 0, 1
#'   )
#' )
#' # vox2ras2[1:3, 4] <- c(-3,-4, -1)
#' base_array[4:6, 4:6, 4:6] <- runif(27) * 255
#' y <- as_ieegio_volume(base_array, vox2ras = vox2ras2)
#'
#'
#' # merge y into x and up-sample mask to 64^3 volume
#' # set to higher number to get better interpolation quality
#' # Only voxels of y>0 will be merged to x
#' z <- merge(x, y, reshape = c(64, 64, 64), thresholds = 0)
#'
#' # Visualize
#'
#' oldpar <- par(mfrow = c(1, 3), mar = c(0, 0, 2, 0))
#'
#' zoom <- 10
#' crosshair_ras <- c(0, 0, 0)
#' pixel_width <- 2
#'
#' plot(x,
#'      zoom = zoom,
#'      position = crosshair_ras,
#'      pixel_width = pixel_width,
#'      main = "Original - underlay")
#' plot(y,
#'      zoom = zoom,
#'      position = crosshair_ras,
#'      pixel_width = pixel_width,
#'      main = "Original - overlay")
#' plot(
#'   z,
#'   zoom = zoom,
#'   position = crosshair_ras,
#'   pixel_width = pixel_width,
#'   main = "Merged & up-sampled")
#'
#' # reset graphical state
#' par(oldpar)
#'
#'
#' @export
merge.ieegio_volume <- function(x, y, ..., thresholds = 0, reshape = dim(x), na_fill = NA) {

  # DIPSAUS DEBUG START
  # nifti_file <- "brain.demosubject.nii.gz"
  # file <- ieegio_sample_data(nifti_file)
  # x <- as_ieegio_volume(file)
  # y <- as_ieegio_volume(file)
  # y$transforms$vox2ras[1:3,4] <- y$transforms$vox2ras[1:3,4] + 5
  # reshape = dim(x)
  # thresholds = 0
  # na_fill = NA
  # merge_list <- list(y)

  check_shape <- function(shape) {
    if(length(shape) >= 4 && all(shape[-c(1,2,3)] == 1)) {
      shape <- shape[1:3]
    }
    if(length(shape) != 3) {
      stop("Not yer implemented: cannot merge volumes with 4th dimension (e.g. time-series...).")
    }
    shape
  }
  dmx <- check_shape(dim(x))
  reshape <- check_shape(reshape)

  # resample if reshape is not dim(x)
  if(!all(dmx == reshape)) {
    x <- resample_volume(x, new_dim = reshape, na_fill = na_fill)
    dmx <- reshape
  }

  merge_list <- list(y, ...)
  if(!length(merge_list)) { return(x) }
  if(length(thresholds) < length(merge_list)) {
    thresholds <- rep(thresholds, ceiling(length(merge_list) / length(thresholds)))
  }

  env <- new.env(parent = emptyenv())
  env$xdata <- array(x[], reshape)

  # If we have ravetools installed
  ravetools <- check_ravetools_flag()
  if(!isFALSE(ravetools) && is.function(ravetools$resample_3d_volume)) {
    # use ravetools resample_3d_volume
    lapply(seq_along(merge_list), function(jj) {
      y <- merge_list[[jj]]
      thres <- thresholds[[jj]]
      if(!is.finite(thres)) {
        thres <- -Inf
      }
      dmy <- check_shape(dim(y))
      y_resamp <- ravetools$resample_3d_volume(
        x = array(y[], dmy),
        new_dim = reshape,
        vox2ras_old = y$transforms$vox2ras,
        vox2ras_new = x$transforms$vox2ras,
        na_fill = NA
      )
      sel <- !is.na(y_resamp) & y_resamp > thres

      env$xdata[sel] <- y_resamp[sel]
      return()
    })
  } else {
    dim(env$xdata) <- c(prod(reshape[1:2]), reshape[[3]])
    vox_idx <- t(cbind(arrayInd(seq_len(prod(reshape[1:2])), reshape[1:2]) - 1, 0, 1))
    lapply(seq_along(merge_list), function(jj) {
      y <- merge_list[[jj]]
      thres <- thresholds[[jj]]
      if(!is.finite(thres)) {
        thres <- -Inf
      }
      dmy <- check_shape(dim(y))
      cdmy <- c(1, cumprod(dmy))[1:3]
      vx2vy <- solve(y$transforms$vox2ras) %*% x$transforms$vox2ras
      dj <- as.vector(vx2vy %*% c(0, 0, 1, 0))[1:3]
      vox_y_base <- (vx2vy %*% vox_idx)[1:3, , drop = FALSE]

      lapply(seq_len(reshape[3]), function(ii) {
        vox_plane <- round(vox_y_base + (ii - 1) * dj)
        is_invalid <- colSums(is.na(vox_plane) | vox_plane < 0 | vox_plane >= dmy) > 0
        if(all(is_invalid)) { return() }
        vox_plane <- colSums(round(vox_plane) * cdmy) + 1
        vox_plane[is_invalid] <- NA
        plane_sample <- y[vox_plane]
        sel <- !is.na(plane_sample) & plane_sample > thres
        if(any(sel)) {
          env$xdata[sel , ii] <- plane_sample[sel]
        }
        return()
      })
      return()
    })
    dim(env$xdata) <- reshape
  }

  na_fill <- na_fill[[1]]
  if(!is.na(na_fill)) {
    env$xdata[is.na(env$xdata)] <- na_fill
  }

  re <- as_ieegio_volume.array(x = env$xdata, vox2ras = x$transforms$vox2ras)
  original_meta <- .subset2(x, "original_meta")
  if(length(original_meta)) {

    # No need to set pixdim as the 4-7th components are lost
    # pixdim <- original_meta$pixdim
    # pixdim[2:4] <- re$header$pixdim[2:4]
    # re$header$pixdim <- pixdim

    # scl_slope scl_inter ?
    re$header$intent_code <- original_meta$intent_code
    re$header$slice_start <- original_meta$slice_start
    re$header$slice_end <- original_meta$slice_end
    re$header$slice_code <- original_meta$slice_code
    re$header$xyzt_units <- original_meta$xyzt_units
    re$header$cal_max <- original_meta$cal_max
    re$header$cal_min <- original_meta$cal_min
    re$header$slice_duration <- original_meta$slice_duration
  }

  re$original_meta <- as_nifti_header(re$header)
  re

}
