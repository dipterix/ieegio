#' Down-sample or super-sample volume
#' @description
#' Using nearest-neighbor.
#' @param x image volume
#' @param new_dim new dimension
#' @param na_fill value to fill if missing
#' @returns A new volume with desired shape
#'
#' @examples
#'
#' nifti_file <- "brain.demosubject.nii.gz"
#'
#' if( ieegio_sample_data(nifti_file, test = TRUE) ) {
#'
#'   orig <- read_volume(ieegio_sample_data(nifti_file))
#'   dim(orig)
#'
#'   # resample
#'   downsampled <- resample_volume(orig, new_dim = c(30, 30, 30))
#'   dim(downsampled)
#'
#'   # up-sample on coronal
#'   upsampled <- resample_volume(orig, new_dim = c(300, 300, 64))
#'   dim(upsampled)
#'
#'   par(mfrow = c(2, 2), mar = c(0, 0, 2.1, 0.1))
#'   plot(orig, main = "Original")
#'   plot(downsampled, main = "Down-sampled")
#'   plot(upsampled, main = "Super-sampled")
#'   plot(
#'     orig,
#'     main = "Overlay super-sample",
#'     col = c("black", "white"),
#'     zoom = 2,
#'     vlim = c(0, 255)
#'   )
#'   plot(
#'     upsampled,
#'     add = TRUE,
#'     col = c("white", "black"),
#'     zoom = 2,
#'     alpha = 0.5,
#'     vlim = c(0, 255)
#'   )
#'
#'
#' }
#'
#'
#'
#' @export
resample_volume <- function(x, new_dim, na_fill = NA) {

  # DIPSAUS DEBUG START
  # nifti_file <- "brain.demosubject.nii.gz"
  # file <- ieegio_sample_data(nifti_file)
  # x <- as_ieegio_volume(file)

  x <- as_ieegio_volume(x)
  dim0 <- dim(x)[c(1, 2, 3)]

  crs0_half <- c(dim0 / 2, 1)
  vox2ras0 <- x$transforms$vox2ras
  # RAS of center of the volume
  ras0 <- vox2ras0 %*% crs0_half

  dim1 <- new_dim[c(1, 2, 3)]
  crs1_half <- c(dim1 / 2, 1)
  # ras0 <- vox2ras1 %*% crs1_half

  vox2ras1 <- vox2ras0 %*% diag(c(dim0 / dim1, 1))
  vox2ras1[1:3, 4] <- 0
  translation <- ras0 - vox2ras1 %*% crs1_half
  vox2ras1[1:3, 4] <- translation[1:3]

  # CRS/IJK in new image
  vox1 <- t(cbind(arrayInd(seq_len(prod(dim1)), dim1) - 1L, 1L))
  # CRS/IJK in old image
  vox0 <- round(solve(vox2ras0) %*% vox2ras1 %*% vox1)

  x_shape0 <- dim(x)
  x_data <- x[]

  nvox <- prod(x_shape0[1:3])
  dim(x_data) <- c(nvox, length(x_data) / nvox)

  vox0[1, vox0[1, ] >= x_shape0[[1]]] <- NA
  vox0[2, vox0[2, ] >= x_shape0[[2]]] <- NA
  vox0[3, vox0[3, ] >= x_shape0[[3]]] <- NA
  vox0[vox0 < 0] <- NA

  idx0 <- colSums(vox0[1:3, , drop = FALSE] * c(1, cumprod(dim0))[c(1, 2, 3)]) + 1

  x_shape1 <- x_shape0
  x_shape1[1:3] <- dim1
  x_data <- array(x_data[idx0, ], x_shape1)

  if(!is.na(na_fill)) {
    x_data[is.na(x_data)] <- na_fill
  }

  re <- as_ieegio_volume.array(x = x_data, vox2ras = vox2ras1)

  original_meta <- .subset2(x, "original_meta")
  if(length(original_meta)) {

    pixdim <- original_meta$pixdim
    pixdim[2:4] <- re$header$pixdim[2:4]
    re$header$pixdim <- pixdim

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

  re$original_meta <- RNifti::niftiHeader(re$header)
  re
}
