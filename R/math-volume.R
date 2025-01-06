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

resample_vox2ras <- function(vox2ras, old_dim, new_dim) {
  dim0 <- old_dim[c(1, 2, 3)]

  crs0_half <- c(dim0 / 2, 1)
  # RAS of center of the volume
  ras0 <- vox2ras %*% crs0_half

  dim1 <- new_dim[c(1, 2, 3)]
  crs1_half <- c(dim1 / 2, 1)
  # ras0 <- vox2ras1 %*% crs1_half

  vox2ras1 <- vox2ras %*% diag(c(dim0 / dim1, 1))
  vox2ras1[1:3, 4] <- 0
  translation <- ras0 - vox2ras1 %*% crs1_half
  vox2ras1[1:3, 4] <- translation[1:3]
  vox2ras1
}

#' Burn image at given positions
#' @description
#' Burn image at given positions with given color and radius.
#' @param image volume
#' @param ras_position image-defined right-anterior-posterior positions, an
#' \code{nx3} matrix, each row is an 'RAS' coordinate
#' @param col vector of integer or characters, color of each contact
#' @param radius vector of positive number indicating the burning radius
#' @param reshape whether to reshape the image at a different resolution;
#' default is false; can be \code{TRUE} (image resolution will be doubled),
#' a single number (size of isotropic volume along one side), or
#' a length of three defining the new shape.
#' @param ... passed to \code{\link{as_ieegio_volume}}, useful if \code{image}
#' is an array
#' @param preview indices (integer) of the position to visualize; default is
#' \code{NULL} (no preview)
#' @returns Color image that is burnt
#'
#' @examples
#'
#' if(interactive()) {
#'
#' image <- as_ieegio_volume(
#'   array(rnorm(125000), c(50, 50, 50)),
#'   vox2ras = rbind(cbind(diag(1, 3), c(-25, -25, -25)),
#'                   c(0, 0, 0, 1))
#' )
#'
#' ras_positions <- rbind(c(1, -0.5, 1.5), c(15, -15.7, 16.1))
#' burned <- burn_volume(
#'   image,
#'   ras_positions,
#'   col = c("red", "green"),
#'   radius = 2,
#'   reshape = c(150, 150, 150)
#' )
#'
#' plot(image, position = ras_positions[1, ], zoom = 5, pixel_width = 0.5, center_position = FALSE)
#' plot(burned, position = ras_positions[1, ], zoom = 5, pixel_width = 0.2,
#'      add = TRUE, center_position = FALSE, alpha = 0.5)
#'
#' }
#' @export
burn_volume <- function(image, ras_position, col = "red", radius = 1,
                        reshape = FALSE, ..., preview = NULL) {

  # DIPSAUS DEBUG START
  # image <- read_volume(ieegio_sample_data( "brain.demosubject.nii.gz"))
  # ras_position = array(rnorm(30), c(10, 3))
  # col <- sample(2:100, 10)
  # radius = 1
  # reshape <- FALSE
  # preview <- TRUE

  if(length(ras_position) == 3) {
    ras_position <- matrix(ras_position, nrow = 1)
  } else if(!is.matrix(ras_position)) {
    ras_position <- as.matrix(ras_position)
  }

  if(ncol(ras_position) != 3) {
    stop("`ras_position` must be a matrix of nx3")
  }

  n_contacts <- nrow(ras_position)
  if(length(radius) < n_contacts) {
    radius <- rep(radius, ceiling(nrow(ras_position) / length(radius)))
  }
  radius <- radius[seq_len(n_contacts)]

  if(length(col) < n_contacts) {
    col <- rep(col, ceiling(nrow(ras_position) / length(col)))
  }
  col <- col[seq_len(n_contacts)]

  image <- as_ieegio_volume(image, ...)
  shape <- dim(image)[c(1, 2, 3)]
  vox2ras <- image$transforms$vox2ras
  if(!isFALSE(reshape)) {
    stopifnot(isTRUE(reshape) || (
      is.numeric(reshape) &&
        length(reshape) %in% c(1, 3) &&
        all(reshape > 0)
    ))

    if(isTRUE(reshape)) {
      reshape <- shape * 2
    } else if(length(reshape) == 1) {
      reshape <- c(reshape, reshape, reshape)
    }
    vox2ras <- resample_vox2ras(vox2ras = vox2ras, old_dim = shape, new_dim = reshape)
    shape <- reshape
  }

  cum_shape <- c(1, cumprod(shape))

  vox2scan <- image$transforms$vox2ras
  scan2vox <- solve(vox2scan)
  voxel_sizes <- sqrt(colSums((vox2scan[1:3, 1:3])^2))

  col <- grDevices::adjustcolor(col)

  arr <- array(NA_integer_, dim = shape)

  nvox <- ceiling(max(radius, na.rm = TRUE) / voxel_sizes)
  search_table <- t(as.matrix(expand.grid(
    i = seq(-nvox[[1]], nvox[[1]]),
    j = seq(-nvox[[2]], nvox[[2]]),
    k = seq(-nvox[[3]], nvox[[3]])
  )))
  dimnames(search_table) <- NULL
  dist <- sqrt(colSums((vox2scan[1:3, 1:3] %*% search_table)^2))

  # burn sphere
  for(ii in seq_len(n_contacts)) {
    scan_pos <- ras_position[ii, ]
    radius_ii <- radius[[ii]]

    if( isTRUE(radius_ii > 0) && !anyNA(scan_pos) ) {

      val <- as.integer(ii)
      vox_pos <- round(scan2vox %*% c(scan_pos, 1))[1:3]

      # 0-based
      burn_index0 <- search_table + vox_pos

      # filter within radius
      burn_index0 <- burn_index0[, dist <= radius_ii, drop = FALSE]

      # get rid of invalid indices
      burn_index0[burn_index0 < 0 | burn_index0 >= shape] <- NA
      burn_index0 <- burn_index0[, !is.na(colSums(burn_index0)), drop = FALSE]

      burn_index <- colSums(burn_index0 * cum_shape[1:3]) + 1
      arr[burn_index] <- val
    }

  }

  current_palette <- grDevices::palette()
  grDevices::palette(col)
  on.exit({ grDevices::palette(current_palette) })

  burnt <- as_ieegio_volume(arr, vox2ras = image$transforms$vox2ras, as_color = TRUE)
  grDevices::palette(current_palette)

  preview <- preview[preview %in% seq_len(n_contacts)]
  if(length(preview)) {
    mfrow <- grDevices::n2mfrow(length(preview))
    oldpar <- graphics::par(mfrow = mfrow, mar = c(0, 0, 0, 0))
    on.exit({ graphics::par(oldpar) }, add = TRUE)

    lapply(preview, function(ii) {
      plot(
        image,
        position = ras_position[ii, ],
        center_position = TRUE,
        zoom = 5,
        crosshair_col = NA
      )
      plot(
        burnt,
        position = ras_position[ii, ],
        center_position = TRUE,
        zoom = 5,
        add = TRUE,
        alpha = 0.5
      )
    })
  }

  burnt
}
