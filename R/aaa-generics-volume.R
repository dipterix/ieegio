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
    x$data[...] <- array
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
#' @returns imaging readers return \code{ieegio_volume} objects.
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
#' vol <- read_volume(file, method = "oro", format = "nifti")
#' vol$header
#'
#' # lazy-load nifti
#' vol2 <- read_volume(file, method = "rnifti", format = "nifti")
#' vol2$header
#'
#' # Using ANTsPyx
#' vol3 <- read_volume(file, method = "ants", format = "nifti")
#' vol3$header
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
        vlim <- range(x_data, na.rm = TRUE)
        mode <- storage.mode(x_data)
        if( vlim[[1]] > 0 ) { vlim[[1]] <- 0 }
        if( mode == "integer" ) {
          if( vlim[[2]] >= 2 ) {
            if(vlim[[2]] < 255) {
              vlim[[2]] <- 255
              vlim[[1]] <- 0
            } else if (vlim[[2]] <- 32768) {
              vlim[[2]] <- 32768
              vlim[[1]] <- -32767
            } else if (vlim[[2]] <- 65535) {
              vlim[[2]] <- 65535
              vlim[[1]] <- 0
            }
          }
        } else {
          if( vlim[[1]] == 0 ) {
            if(vlim[[2]] < 1) {
              vlim[[2]] <- 1
            }
          } else if (vlim[[1]] < 0) {
            # sym map
            vlim <- max(abs(vlim)) * c(-1, 1)
          }
        }

        # check if meta has valid cal_min and cal_max
        original_meta <- .subset2(x, "original_meta")
        if(is.list(original_meta) && all(c("cal_max", "cal_min") %in% names(original_meta))) {
          cal_min <- original_meta$cal_min
          cal_max <- original_meta$cal_max
          if(any(c(cal_min, cal_max) != 0) && cal_max != cal_min) {
            # need to rescale to 0 - 1
            vlim <- c(0, 1)
            x_data <- (x_data - cal_min) / (cal_max - cal_min)
            x_data[x_data < 0] <- 0
            x_data[x_data > 1] <- 1
          }
        }

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
    vox_data <- t(vox_data)[ncol(vox_data):1L, , drop = FALSE]
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
