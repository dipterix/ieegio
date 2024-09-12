#' @rdname imaging-volume
#' @export
io_read_nii <- function(file, method = c("oro", "rnifti", "ants"), header_only = FALSE, ...) {
  # DIPSAUS DEBUG START
  # file <- "~/rave_data/raw_dir/AnonSEEG0/rave-imaging/coregistration/CT_RAW.nii"
  method <- match.arg(method)

  if(header_only) {
    if(!identical(method, "oro")) {
      warning("`io_read_nii`: reading with header-only mode, method ", sQuote(method), " will be ignored.")
    }
    method <- "oro"
  }

  switch(
    method,
    "oro" = {
      args <- list(
        fname = file, read_data = !header_only,
        ...
      )
      if(is.null(args$reorient)) {
        args$reorient <- FALSE
      }
      if(is.null(args$rescale_data)) {
        args$rescale_data <- FALSE
      }
      volume <- do.call(oro.nifti::readNIfTI, args)
      shape <- dim(volume@.Data)

      if( header_only ) {
        volume@.Data <- array(NA, rep(1, length(shape)))
        data <- NULL
      } else {
        header <- volume
        data <- quote({ header@.Data })
      }

      pixdim <- volume@pixdim[c(2, 3, 4)]

      # vox2ras
      qfcode <- oro.nifti::qform_code(volume)
      sfcode <- oro.nifti::sform_code(volume)

      qform <- structure(
        oro.nifti::qform(volume),
        code = qfcode
      )
      sform <- structure(
        oro.nifti::sform(volume),
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

      # vox2ras_tkr
      vox2ras_tkr <- vox2ras
      vox2ras_tkr[1:3, 4] <- - shape[1:3] / 2

      # vox2fsl
      vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

      transforms <- list(
        vox2ras = vox2ras,
        vox2ras_tkr = vox2ras_tkr,
        vox2fsl = vox2fsl
      )

      return(new_volume(
        type = c("rnifti", "nifti"),
        header = volume,
        transforms = transforms,
        data = data,
        shape = shape
      ))
    },
    "rnifti" = {
      volume <- RNifti::readNifti(file, internal = TRUE)
      header <- RNifti::niftiHeader(volume)
      pixdim <- RNifti::pixdim(header)
      shape <- dim(volume)

      # vox2ras
      qfcode <- header$qform_code
      sfcode <- header$sform_code

      qform <- structure(
        RNifti::xform(header, useQuaternionFirst = TRUE),
        code = qfcode
      )
      sform <- structure(
        RNifti::xform(header, useQuaternionFirst = FALSE),
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

      # vox2ras_tkr
      vox2ras_tkr <- vox2ras
      vox2ras_tkr[1:3, 4] <- - shape[1:3] / 2

      # vox2fsl
      vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

      transforms <- list(
        vox2ras = vox2ras,
        vox2ras_tkr = vox2ras_tkr,
        vox2fsl = vox2fsl
      )

      return(new_volume(
        type = c("oro", "nifti"),
        header = volume,
        transforms = transforms,
        shape = shape,
        data = quote({
          v <- header[drop = FALSE]
          class(v) <- "array"
          attr(v, ".nifti_image_ptr") <- NULL
          v
        })
      ))

    },
    "ants" = {
      volume <- rpyANTs::as_ANTsImage(file, ...)
      header <- volume
      shape <- unlist(rpymat::py_to_r(volume$shape))
      pixdim <- unlist(rpymat::py_to_r(volume$spacing))

      vox2lps <- t(t(rpymat::py_to_r(volume$direction)) * as.double(rpymat::py_to_r(volume$spacing)))
      vox2lps <- rbind(cbind(vox2lps, as.double(rpymat::py_to_r(volume$origin))), c(0, 0, 0, 1))
      vox2ras <- diag(c(-1, -1, 1, 1)) %*% vox2lps

      attr(vox2ras, "which_xform") <- "qform"

      # vox2ras_tkr
      vox2ras_tkr <- vox2ras
      vox2ras_tkr[1:3, 4] <- - shape[1:3] / 2

      # vox2fsl
      vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

      transforms <- list(
        vox2ras = vox2ras,
        vox2ras_tkr = vox2ras_tkr,
        vox2fsl = vox2fsl
      )

      return(new_volume(
        type = c("antspy", "nifti"),
        header = volume,
        transforms = transforms,
        data = quote({ header[drop = FALSE] }),
        shape = shape
      ))
    }
  )

}

#' @rdname imaging-volume
#' @export
io_write_nii <- function(x, con, ...) {
  UseMethod("io_write_nii")
}

#' @export
io_write_nii.ieegio_nifti <- function(x, con, ...) {
  if(.subset2(x, "header_only")) {
    stop("The volume object is head-only.")
  }
  io_write_nii(x = x$header, con = con, ...)
}

#' @export
io_write_nii.ants.core.ants_image.ANTsImage <- function(x, con, ...) {
  con <- normalizePath(con, winslash = "/", mustWork = FALSE)
  x$to_file(con)
}

#' @export
io_write_nii.niftiImage <- function(x, con, ...) {
  RNifti::writeNifti(image = x, file = con, ...)
}

#' @export
io_write_nii.nifti <- function(x, con, ...) {
  if(grepl("\\.(nii|nii\\.gz)$", con, ignore.case = TRUE)) {
    con <- path_ext_remove(con)
  }
  oro.nifti::writeNIfTI(nim = x, filename = con, ...)
}

#' @export
io_write_nii.ieegio_mgh <- function(x, con, ...) {

  vox2ras <- x$transforms$vox2ras

  quaternion <- mat_to_quaternion(vox2ras)
  nframes <- x$header$internal$nframes

  m33 <- vox2ras[1:3, 1:3]
  pixdim <- sqrt(colSums(m33^2))
  pixdim <- c(sign(det(m33)), pixdim, nframes, 0, 0, 0)
  pixdim <- as.double(pixdim)

  data <- x$data
  data[is.na(data)] <- 0
  rg <- range(data)
  if(all(data - round(data) == 0)) {
    if( rg[[1]] >= 0 && rg[[2]] <= 255 ) {
      # UINT8
      datatype_code <- 2L
      bitpix <- 8L
      storage.mode(data) <- "integer"
    } else if ( rg[[1]] >= -32768 && rg[[2]] <= 32768 ) {
      # INT16
      datatype_code <- 4L
      bitpix <- 16L
      storage.mode(data) <- "integer"
    } else if ( rg[[1]] >= -2147483648 && rg[[2]] <= 2147483648 ) {
      # INT32
      datatype_code <- 8L
      bitpix <- 32L
      storage.mode(data) <- "integer"
    } else {
      # FLOAT32
      bitpix <- 32L
      datatype_code <- 16L
    }
  } else {
    # FLOAT32
    bitpix <- 32L
    datatype_code <- 16L
  }

  shape <- x$shape
  if(length(shape) == 3 || (length(shape) == 4 && shape[[4]] == 1)) {
    pixdim[[5]] <- 0
    shape <- shape[1:3]
    data <- array(data[seq_len(prod(shape))], dim = shape)
  }

  # functional
  nii <- oro.nifti::as.nifti(data)
  # sizeof_hdr = 348L,
  # dim_info = 0L,
  # dim = as.integer(c(length(x$shape), x$shape, rep(1, 7 - length(x$shape)))),
  # intent_p1 = 0, intent_p2 = 0, intent_p3 = 0, intent_code = 0L,
  nii@datatype <- datatype_code
  nii@data_type <- oro.nifti::convert.datatype(datatype_code)
  nii@bitpix <- bitpix
  # slice_start = 0L, slice_end = 0L, slice_code = 0L,
  nii@pixdim <- pixdim
  # vox_offset = 352,
  # scl_slope = 0, scl_inter = 0,
  # cal_max = 0, cal_min = 0,
  nii@xyzt_units <- 10L
  # slice_duration = 0, toffset = 0,
  # descrip = "Time=0.000", aux_file = "",
  nii@qform_code <- 1L
  nii@sform_code <- 1L
  nii@quatern_b <- quaternion[[1]]
  nii@quatern_c <- quaternion[[2]]
  nii@quatern_d <- quaternion[[3]]
  nii@qoffset_x <- vox2ras[1, 4]
  nii@qoffset_y <- vox2ras[2, 4]
  nii@qoffset_z <- vox2ras[3, 4]
  nii@srow_x <- vox2ras[1, ]
  nii@srow_y <- vox2ras[2, ]
  nii@srow_z <- vox2ras[3, ]
  # intent_name = "", magic = "n+1"
  nii@regular <- "r"

  io_write_nii.nifti(x = nii, con = con, ...)
}

#' @rdname imaging-volume
#' @export
io_write_nii.array <- function(x, con, vox2ras = NULL, ...) {
  if(!is.matrix(vox2ras)) {
    warning("`io_write_nii.array`: `vox2ras` is missing, using identity matrix. Please specify voxel-to-RAS transform (4x4 matrix).")
    vox2ras <- diag(1, 4)
  }
  stopifnot(is.matrix(vox2ras) && nrow(vox2ras) == 4 && ncol(vox2ras) == 4)

  quaternion <- mat_to_quaternion(vox2ras)

  shape <- dim(x)

  stopifnot(length(shape) %in% c(3, 4))
  if(length(shape) > 3) {
    nframes <- shape[[4]]
  } else {
    nframes <- 1
  }

  m33 <- vox2ras[1:3, 1:3]
  pixdim <- sqrt(colSums(m33^2))
  pixdim <- c(sign(det(m33)), pixdim, nframes, 0, 0, 0)
  pixdim <- as.double(pixdim)

  if( nframes == 1 && length(shape) != 3 ) {
    pixdim[[5]] <- 0
    shape <- shape[1:3]
    data <- array(data[seq_len(prod(shape))], dim = shape)
  }



  data[is.na(data)] <- 0
  rg <- range(data)
  if(all(data - round(data) == 0)) {
    if( rg[[1]] >= 0 && rg[[2]] <= 255 ) {
      # UINT8
      datatype_code <- 2L
      bitpix <- 8L
      storage.mode(data) <- "integer"
    } else if ( rg[[1]] >= -32768 && rg[[2]] <= 32768 ) {
      # INT16
      datatype_code <- 4L
      bitpix <- 16L
      storage.mode(data) <- "integer"
    } else if ( rg[[1]] >= -2147483648 && rg[[2]] <= 2147483648 ) {
      # INT32
      datatype_code <- 8L
      bitpix <- 32L
      storage.mode(data) <- "integer"
    } else {
      # FLOAT32
      bitpix <- 32L
      datatype_code <- 16L
    }
  } else {
    # FLOAT32
    bitpix <- 32L
    datatype_code <- 16L
  }

  # functional
  nii <- oro.nifti::as.nifti(data)
  # sizeof_hdr = 348L,
  # dim_info = 0L,
  # dim = as.integer(c(length(x$shape), x$shape, rep(1, 7 - length(x$shape)))),
  # intent_p1 = 0, intent_p2 = 0, intent_p3 = 0, intent_code = 0L,
  nii@datatype <- datatype_code
  nii@data_type <- oro.nifti::convert.datatype(datatype_code)
  nii@bitpix <- bitpix
  # slice_start = 0L, slice_end = 0L, slice_code = 0L,
  nii@pixdim <- pixdim
  # vox_offset = 352,
  # scl_slope = 0, scl_inter = 0,
  # cal_max = 0, cal_min = 0,
  nii@xyzt_units <- 10L
  # slice_duration = 0, toffset = 0,
  # descrip = "Time=0.000", aux_file = "",
  nii@qform_code <- 1L
  nii@sform_code <- 1L
  nii@quatern_b <- quaternion[[1]]
  nii@quatern_c <- quaternion[[2]]
  nii@quatern_d <- quaternion[[3]]
  nii@qoffset_x <- vox2ras[1, 4]
  nii@qoffset_y <- vox2ras[2, 4]
  nii@qoffset_z <- vox2ras[3, 4]
  nii@srow_x <- vox2ras[1, ]
  nii@srow_y <- vox2ras[2, ]
  nii@srow_z <- vox2ras[3, ]
  # intent_name = "", magic = "n+1"
  nii@regular <- "r"

  io_write_nii.nifti(x = nii, con = con, ...)
}
