# Calcualte v2ras matrix given new dimensions
resample_vox2ras <- function(vox2ras, old_dim, new_dim) {
  # DIPSAUS DEBUG START
  # vox2ras <- cbind(diag(1, nrow = 4, ncol = 3), c(rnorm(3), 1))
  # old_dim <- c(5,5,5)
  # new_dim <- c(16,16,14)

  dim0 <- old_dim[c(1, 2, 3)]
  dim1 <- new_dim[c(1, 2, 3)]

  # set pixdim
  vox2ras1 <- vox2ras %*% diag(c(dim0 / dim1, 1))
  vox2ras1[1:3, 4] <- 0

  # vox 0,0,0 -> 0,0,0
  # vox d0[1],d0[2],0 -> d1[1],d1[2],0
  # vox 0,d0[2],d0[3] -> 0,d1[2],d1[3]
  vox_corner0 <- cbind(
    c(0, 0, 0),
    c(dim0[[1]], dim0[[2]], 0),
    c(0, dim0[[2]], dim0[[3]])
  ) - 0.5
  vox_corner1 <- cbind(
    c(0, 0, 0),
    c(dim1[[1]], dim1[[2]], 0),
    c(0, dim1[[2]], dim1[[3]])
  ) - 0.5

  ras_corner0 <- vox2ras %*% rbind(vox_corner0, 1)

  translation <- ras_corner0 - vox2ras1 %*% rbind(vox_corner1, 1)
  vox2ras1[1:3, 4] <- rowMeans(translation)[1:3]
  vox2ras1
}

resample_volume_naive <- function(x, new_dim, na_fill = NA) {

  # DIPSAUS DEBUG START
  # nifti_file <- "brain.demosubject.nii.gz"
  # file <- ieegio_sample_data(nifti_file)
  # x <- as_ieegio_volume(file)

  x <- as_ieegio_volume(x)
  dim0 <- dim(x)[c(1, 2, 3)]
  dim1 <- new_dim[1:3]

  vox2ras0 <- x$transforms$vox2ras
  vox2ras1 <- resample_vox2ras(vox2ras = vox2ras0, old_dim = dim0, new_dim = dim1)

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

resample_volume_ravetools <- function(x, new_dim, na_fill = NA) {

  # DIPSAUS DEBUG START
  # nifti_file <- "brain.demosubject.nii.gz"
  # file <- ieegio_sample_data(nifti_file)
  # x <- as_ieegio_volume(file)
  # new_dim <- c(16, 16, 16)
  # na_fill <- NA
  ravetools <- check_ravetools_flag()

  x <- as_ieegio_volume(x)
  dim0 <- dim(x)[c(1, 2, 3)]
  dim1 <- new_dim[1:3]

  vox2ras0 <- x$transforms$vox2ras
  vox2ras1 <- resample_vox2ras(vox2ras = vox2ras0, old_dim = dim0, new_dim = dim1)
  x_shape0 <- dim(x)
  nvox <- prod(x_shape0[1:3])

  x_idx <- array(seq_len(nvox), x_shape0[1:3])
  x_idx <- ravetools$resample_3d_volume(x = x_idx, new_dim = dim1, vox2ras_old = vox2ras0, vox2ras_new = vox2ras1, na_fill = NA)
  x_idx <- as.vector(x_idx)

  x_data <- x[]
  dim(x_data) <- c(nvox, length(x_data) / nvox)
  x_data <- apply(x_data, 2L, function(slice) {
    re <- slice[x_idx]
    if(!is.na(na_fill)) {
      re[is.na(re)] <- na_fill
    }
    re
  })

  x_shape1 <- x_shape0
  x_shape1[1:3] <- dim1
  dim(x_data) <- x_shape1

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
#' # ---- Toy example ----------------------------
#'
#' dm <- c(6, 6, 6)
#' arr <- array(seq_len(prod(dm)) + 0.5, dm)
#' orig <- as_ieegio_volume(
#'   arr, vox2ras = cbind(diag(1, nrow = 4, ncol = 3), c(-dm / 2, 1)))
#'
#' # resample
#' downsampled <- resample_volume(orig, new_dim = c(3, 3, 3))
#' dim(downsampled)
#'
#' # up-sample on coronal
#' upsampled <- resample_volume(orig, new_dim = c(20, 20, 24))
#' dim(upsampled)
#'
#' par(mfrow = c(2, 2), mar = c(0, 0, 2.1, 0.1))
#' plot(orig, pixel_width = 0.5, zoom = 20, main = "Original")
#' plot(downsampled, pixel_width = 0.5, zoom = 20, main = "Down-sampled")
#' plot(upsampled, pixel_width = 0.5, zoom = 20, main = "Super-sampled")
#' plot(
#'   orig,
#'   main = "Overlay super-sample (diff)",
#'   col = c("black", "white"),
#'   pixel_width = 0.5, zoom = 20
#' )
#' plot(
#'   upsampled,
#'   add = TRUE,
#'   col = c("white", "black"),
#'   pixel_width = 0.5, zoom = 20,
#'   alpha = 0.5
#' )
#'
#' # ---- Real example ---------------------------
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
#' @export
resample_volume <- function(x, new_dim, na_fill = NA) {
  ravetools <- check_ravetools_flag()
  if(isFALSE(ravetools) || !is.function(ravetools$resample_3d_volume)) {
    re <- resample_volume_naive(x = x, new_dim = new_dim, na_fill = na_fill)
  } else {
    re <- resample_volume_ravetools(x = x, new_dim = new_dim, na_fill = na_fill)
  }
  re
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
#' @param alpha whether to include alpha (transparent) channel. Default is
#' false for compatibility concerns (legacy software might not support
#' reading alpha channel). In this case, the background will be black.
#' If \code{alpha=TRUE} is set, then the background will be fully transparent.
#' @param blank_underlay whether to use blank image or the input
#' \code{image} as underlay; default is \code{FALSE} (using \code{image}
#' as underlay); alternative is \code{TRUE}, and use black or transparent
#' background
#' @param ... passed to \code{\link{as_ieegio_volume}}, useful if \code{image}
#' is an array
#' @param preview indices (integer) of the position to visualize; default is
#' \code{NULL} (no preview)
#' @returns Color image that is burnt; see \code{\link{imaging-volume}}.
#'
#' @examples
#'
#' if(interactive()) {
#'
#' dim <- c(6, 6, 6)
#' image <- as_ieegio_volume(
#'   array(rnorm(prod(dim)), dim),
#'   vox2ras = rbind(cbind(diag(1, 3), -dim / 2),
#'                   c(0, 0, 0, 1))
#' )
#'
#' ras_positions <- rbind(c(1, -1, 1.5), c(-2.25, -1, -0.75))
#'
#'
#' burned <- burn_volume(
#'   image,
#'   ras_positions,
#'   col = c("red", "green"),
#'   radius = 0.5,
#'   reshape = c(24, 24, 24)
#' )
#'
#' plot(
#'   burned,
#'   position = ras_positions[1, ],
#'   zoom = 15,
#'   pixel_width = 0.25
#' )
#'
#' }
#' @export
burn_volume <- function(image, ras_position, col = "red", radius = 1,
                        reshape = FALSE, alpha = FALSE, blank_underlay = FALSE,
                        ..., preview = NULL) {

  # DIPSAUS DEBUG START
  # image <- read_volume(ieegio_sample_data( "brain.demosubject.nii.gz"))
  # ras_position = array(rnorm(30), c(10, 3))
  # col <- sample(2:100, 10)
  # radius = 1
  # reshape <- TRUE
  # preview <- TRUE
  #
  # image <- as_ieegio_volume(
  #   array(rnorm(125), c(5, 5, 5)),
  #   vox2ras = rbind(cbind(diag(1, 3), c(-2, -2, -2)),
  #                   c(0, 0, 0, 1))
  # )
  #
  # ras_position <- rbind(c(1, -1, 1.5), c(-2, -1, -1))
  # col = c("red", "green")
  # reshape = c(20, 20, 20)
  # radius = 0.5

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
    burn_vox2ras <- resample_vox2ras(vox2ras = vox2ras, old_dim = shape, new_dim = reshape)
    burn_shape <- reshape
  } else {
    burn_vox2ras <- vox2ras
    burn_shape <- shape
  }

  cum_shape <- c(1, cumprod(shape))
  burn_cum_shape <- c(1, cumprod(burn_shape))

  ras2vox <- solve(vox2ras)
  burn_ras2vox <- solve(burn_vox2ras)
  burn_voxel_sizes <- sqrt(colSums((burn_vox2ras[1:3, 1:3])^2))

  col <- grDevices::adjustcolor(col)
  background <- NA_character_

  if( !alpha ) {
    col <- grDevices::col2rgb(col, alpha = FALSE)
    col <- grDevices::rgb(col[1, ], col[2, ], col[3, ], maxColorValue = 255)
    background <- "#000000"
  }

  if( blank_underlay ) {
    arr <- array(background, dim = burn_shape)
    arr_dim <- burn_shape
  } else {
    if(!all(burn_shape == shape)) {
      # resample_volume shares the same vox2ras so should be safe to use
      arr <- resample_volume(image, new_dim = burn_shape)
      arr_dim <- dim(arr)
    } else {
      arr <- image
      arr_dim <- dim(image)
    }
    arr <- arr[drop = FALSE]
    if( !inherits(arr, "ieegio_rgba") ) {
      cal_min <- image$header$cal_min
      cal_max <- image$header$cal_max
      arr <- arr[drop = FALSE]
      if(length(cal_min) != 1 || length(cal_max) != 1 ||
         (cal_min == 0 && cal_max == 0)) {
        rg <- range(arr, na.rm = TRUE)
        cal_min <- rg[[1]]
        cal_max <- rg[[2]]
      }

      arr <- (arr - cal_min) * (255 / (cal_max - cal_min))
      arr <- round(arr)
      arr[is.na(arr) | arr < 0] <- 0
      arr[arr > 255] <- 255
      if( alpha ) {
        pal <- grDevices::gray.colors(256, start = 0, end = 1, alpha = 1)
      } else {
        pal <- grDevices::gray.colors(256, start = 0, end = 1)
      }
      arr <- pal[arr + 1]
    }
  }

  nr <- prod(arr_dim)
  dim(arr) <- c(nr, length(arr) / nr)



  nvox <- ceiling(max(radius, na.rm = TRUE) / burn_voxel_sizes)
  search_table <- t(as.matrix(expand.grid(
    i = seq(-nvox[[1]], nvox[[1]]),
    j = seq(-nvox[[2]], nvox[[2]]),
    k = seq(-nvox[[3]], nvox[[3]])
  )))
  # Speed up calculation
  dimnames(search_table) <- NULL
  # dist <- sqrt(colSums((vox2scan[1:3, 1:3] %*% search_table)^2))

  # burn sphere
  for(ii in seq_len(n_contacts)) {
    scan_pos <- ras_position[ii, ]
    radius_ii <- radius[[ii]]

    if( isTRUE(radius_ii > 0) && !anyNA(scan_pos) ) {

      # vox_pos is IJK in burning image
      vox_pos <- (burn_ras2vox %*% c(scan_pos, 1))[1:3]
      vox_pos0 <- round(vox_pos)[1:3]

      # burn_index0 is 0-based index IJK in burning image
      burn_index0 <- search_table + vox_pos0

      # dist is distance in RAS
      dist <- sqrt(colSums((burn_vox2ras[1:3, 1:3] %*% (burn_index0 - vox_pos))^2))

      # filter within radius
      burn_index0 <- burn_index0[, dist <= radius_ii, drop = FALSE]

      # get rid of invalid indices
      burn_index0[burn_index0 < 0 | burn_index0 >= burn_shape] <- NA
      burn_index0 <- burn_index0[, !is.na(colSums(burn_index0)), drop = FALSE]

      burn_index <- colSums(burn_index0 * burn_cum_shape[1:3]) + 1
      arr[burn_index, ] <- col[[ii]]
    }

  }

  dim(arr) <- arr_dim

  burnt <- as_ieegio_volume(arr, vox2ras = burn_vox2ras, as_color = TRUE)

  preview <- preview[preview %in% seq_len(n_contacts)]
  if(length(preview)) {
    mfrow <- grDevices::n2mfrow(length(preview))
    oldpar <- graphics::par(mfrow = mfrow, mar = c(0, 0, 0, 0))
    on.exit({ graphics::par(oldpar) })

    if( blank_underlay ) {
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
    } else {
      lapply(preview, function(ii) {
        plot(
          burnt,
          position = ras_position[ii, ],
          center_position = TRUE,
          zoom = 5,
          alpha = 0.5
        )
      })
    }

  }

  burnt
}
