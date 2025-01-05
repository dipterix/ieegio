impl_read_mgz_header <- function( filepath, is_gzipped = "AUTO" ) {
  if (!is.character(filepath)) {
    stop("Parameter 'filepath' msut be a character string.")
  }
  header <- list()
  filepath <- normalizePath(filepath, mustWork = TRUE)
  if (typeof(is_gzipped) == "logical") {
    is_gz <- is_gzipped
  } else if (typeof(is_gzipped) == "character") {
    if (toupper(is_gzipped) == "AUTO") {
      is_gz <- grepl( "mgz$", filepath, ignore.case = TRUE)
    } else {
      stop("Argument 'is_gzipped' must be 'AUTO' if it is a string.\n")
    }
  } else {
    stop(sprintf("ERROR: Argument is_gzipped must be logical (TRUE or FALSE) or 'AUTO'.\n"))
  }
  if (is_gz) {
    fh <- gzfile(filepath, "rb")
  } else {
    fh <- file(filepath, "rb")
  }
  on.exit({ close(fh) })
  v <- readBin(fh, integer(), n = 1, endian = "big")
  if (v != 1L) {
    stop("File not in MGH/MGZ format.")
  }
  ndim1 <- readBin(fh, integer(), n = 1, size = 4, endian = "big")
  ndim2 <- readBin(fh, integer(), n = 1, size = 4, endian = "big")
  ndim3 <- readBin(fh, integer(), n = 1, size = 4, endian = "big")
  nframes <- readBin(fh, integer(), n = 1, size = 4, endian = "big")
  dtype <- readBin(fh, integer(), n = 1, size = 4, endian = "big")
  dof <- readBin(fh, integer(), n = 1, size = 4, endian = "big")
  header$dtype <- dtype
  header$dof <- dof
  header$internal <- list()
  header$voldim_orig <- c(ndim1, ndim2, ndim3, nframes)
  unused_header_space_size_left <- 256L
  ras_flag_size <- 2L
  header$ras_good_flag <- readBin(fh, integer(), size = ras_flag_size, n = 1, endian = "big")
  unused_header_space_size_left <- unused_header_space_size_left - ras_flag_size
  if (header$ras_good_flag == 1L) {
    delta <- readBin(fh, numeric(), n = 3, size = 4, endian = "big")
    header$internal$xsize <- delta[1]
    header$internal$ysize <- delta[2]
    header$internal$zsize <- delta[3]
    Mdc <- readBin(fh, numeric(), n = 9, size = 4, endian = "big")
    header$internal$x_r <- Mdc[1]
    header$internal$x_a <- Mdc[2]
    header$internal$x_s <- Mdc[3]
    header$internal$y_r <- Mdc[4]
    header$internal$y_a <- Mdc[5]
    header$internal$y_s <- Mdc[6]
    header$internal$z_r <- Mdc[7]
    header$internal$z_a <- Mdc[8]
    header$internal$z_s <- Mdc[9]
    Mdc <- matrix(Mdc, nrow = 3, byrow = FALSE)
    Pxyz_c <- readBin(fh, numeric(), n = 3, size = 4, endian = "big")
    header$internal$c_r <- Pxyz_c[1]
    header$internal$c_a <- Pxyz_c[2]
    header$internal$c_s <- Pxyz_c[3]
    D <- diag(delta)
    Pcrs_c <- c(ndim1/2, ndim2/2, ndim3/2)
    Mdc_scaled <- Mdc %*% D
    Pxyz_0 <- Pxyz_c - (Mdc_scaled %*% Pcrs_c)
    M <- matrix(rep(0, 16), nrow = 4)
    M[1:3, 1:3] <- as.matrix(Mdc_scaled)
    M[4, 1:4] <- c(0, 0, 0, 1)
    M[1:3, 4] <- Pxyz_0
    header$internal$delta <- delta
    header$internal$Pxyz_c <- Pxyz_c
    header$internal$D <- D
    header$internal$Pcrs_c <- Pcrs_c
    header$internal$Pxyz_0 <- Pxyz_0
    header$internal$M <- M
    header$internal$Mdc <- Mdc
    header$internal$width <- ndim1
    header$internal$height <- ndim2
    header$internal$depth <- ndim3
    header$internal$nframes <- nframes
    x_half_length <- header$internal$width/2 * header$internal$xsize
    y_half_length <- header$internal$height/2 * header$internal$ysize
    z_half_length <- header$internal$depth/2 * header$internal$zsize
    header$internal$xstart <- -x_half_length
    header$internal$xend <- x_half_length
    header$internal$ystart <- -y_half_length
    header$internal$yend <- y_half_length
    header$internal$zstart <- -z_half_length
    header$internal$zend <- z_half_length
    xfov <- header$internal$xend - header$internal$xstart
    yfov <- header$internal$yend - header$internal$ystart
    zfov <- header$internal$zend - header$internal$zstart
    header$internal$fov <- ifelse(xfov > yfov, ifelse(xfov > zfov, xfov, zfov), ifelse(yfov > zfov, yfov, zfov))
    header$vox2ras_matrix <- as.matrix(M)
    RAS_space_size <- (3 * 4 + 4 * 3 * 4)
    unused_header_space_size_left <- unused_header_space_size_left - RAS_space_size
  } else {
    header$internal$slice_direction_name <- "unknown"
  }
  return( header )
}

#' @rdname imaging-volume
#' @export
io_read_mgz <- function(file, header_only = FALSE) {
  # DIPSAUS DEBUG START
  # file <- "~/rave_data/raw_dir/AnonSEEG0/rave-imaging/fs/mri/T1.mgz"
  if( header_only ) {
    header <- impl_read_mgz_header( file )
    data <- NULL
  } else {
    volume <- freesurferformats::read.fs.mgh(filepath = file, with_header = TRUE)
    data <- volume$data
    header <- volume$header
  }
  shape <- header$voldim_orig
  pixdim <- c(header$internal$xsize, header$internal$ysize, header$internal$zsize)

  # Norig: IJK to scanner-RAS
  Norig <- header$vox2ras_matrix

  # Torig: IJK to tkr-RAS
  # Torig <- Norig[1:4, 1:3]
  # Torig <- cbind(Torig, -Torig %*% header$internal$Pcrs_c)
  # Torig[4, 4] <- 1
  Torig <- get_vox2ras_tkr(Norig, header$internal$Pcrs_c)

  # IJK to fsl
  vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = Norig)

  transforms <- list(
    vox2ras = Norig,
    vox2ras_tkr = Torig,
    vox2fsl = vox2fsl
  )

  if(endsWith(tolower(file), "mgz")) {
    type <- c("mgz", "mgh")
  } else {
    type <- "mgh"
  }

  new_volume(type, header, NULL, transforms, data, shape = shape)

}

#' @rdname imaging-volume
#' @export
io_write_mgz <- function(x, con, ...) {
  UseMethod("io_write_mgz")
}

#' @rdname imaging-volume
#' @export
io_write_mgz.ieegio_volume <- function(x, con, ...) {
  if(isTRUE(x$header_only)) {
    stop("`io_write_mgz`: input `x` is header-only. No data is to be written")
  }
  freesurferformats::write.fs.mgh(filepath = con, data = x$data, vox2ras_matrix = x$transforms$vox2ras, ...)
  normalizePath(con)
}

#' @rdname imaging-volume
#' @export
io_write_mgz.ieegio_mgh <- function(x, con, ...) {
  if(isTRUE(x$header_only)) {
    stop("`io_write_mgz`: input `x` is header-only. No data is to be written")
  }
  freesurferformats::write.fs.mgh(filepath = con, data = x$data, vox2ras_matrix = x$transforms$vox2ras, mr_params = x$header$mr_params, ...)
  normalizePath(con)
}

#' @rdname imaging-volume
#' @export
io_write_mgz.nifti <- function(x, con, ...) {
  if( oro.nifti::sform_code(x) > 0 ) {
    xform <- oro.nifti::sform(x)
  } else {
    xform <- oro.nifti::qform(x)
  }
  if(nrow(xform) == 3) {
    xform <- rbind(xform, c(0, 0, 0, 1))
  }
  freesurferformats::write.fs.mgh(filepath = con, data = x[drop = FALSE], vox2ras_matrix = xform, ...)
  normalizePath(con)
}

#' @rdname imaging-volume
#' @export
io_write_mgz.niftiImage <- function(x, con, ...) {
  header <- RNifti::niftiHeader(x)
  if(header$sform_code > 0) {
    xform <- RNifti::xform(image = header, useQuaternionFirst = FALSE)
  } else {
    xform <- RNifti::xform(image = header, useQuaternionFirst = TRUE)
  }
  if(nrow(xform) == 3) {
    xform <- rbind(xform, c(0, 0, 0, 1))
  }
  freesurferformats::write.fs.mgh(filepath = con, data = x[drop = FALSE], vox2ras_matrix = xform, ...)
  normalizePath(con)
}

#' @rdname imaging-volume
#' @export
io_write_mgz.ants.core.ants_image.ANTsImage <- function(x, con, ...) {

  vox2lps <- t(t(rpymat::py_to_r(x$direction)) * as.double(rpymat::py_to_r(x$spacing)))
  vox2lps <- rbind(cbind(vox2lps, as.double(rpymat::py_to_r(x$origin))), c(0, 0, 0, 1))
  vox2ras <- diag(c(-1, -1, 1, 1)) %*% vox2lps

  attr(vox2ras, "which_xform") <- "qform"

  freesurferformats::write.fs.mgh(filepath = con, data = x[drop = FALSE], vox2ras_matrix = vox2ras, ...)
  normalizePath(con)
}

#' @rdname imaging-volume
#' @export
io_write_mgz.array <- function(x, con, vox2ras = NULL, ...) {
  if(!is.matrix(vox2ras)) {
    warning("`io_write_mgz.array`: `vox2ras` is missing, using identity matrix. Please specify voxel-to-RAS transform (4x4 matrix).")
    vox2ras <- diag(1, 4)
  }
  stopifnot(is.matrix(vox2ras) && nrow(vox2ras) == 4 && ncol(vox2ras) == 4)

  freesurferformats::write.fs.mgh(filepath = con, data = x, vox2ras_matrix = vox2ras, ...)
  normalizePath(con)
}
