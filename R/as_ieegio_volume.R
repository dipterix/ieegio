
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
