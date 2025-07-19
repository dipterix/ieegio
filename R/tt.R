#' @name io-tt
#' @title Read \code{'TT'} streamline file
#' @description
#' Writer is not implemented yet. Please save as a \code{'TCK'} file.
#'
#' @returns An \code{\link{imaging-streamlines}} instance.
#' @param file path to the streamline file
#' @examples
#'
#'
#' # This example uses sample data, run
#' # `ieegio_sample_data("streamlines/CNVII_R.trk")` to download
#'
#' if( ieegio_sample_data("streamlines/CNVII_R.tt.gz", test = TRUE) ) {
#'
#'   path <- ieegio_sample_data("streamlines/CNVII_R.tt.gz")
#'
#'   # read
#'   x <- io_read_tt(path)
#'
#'   plot(x)
#'
#' }
#'
#' @export
io_read_tt <- function(file) {
  source_file <- file

  # source_file <- "~/Downloads/hcp1065_avg_tracts_tt/AF_L.tt.gz"

  if(endsWith(tolower(source_file), ".gz")) {
    file <- tempfile(fileext = ".mat")
    source_con <- gzfile(source_file, open = "rb")
    file_con <- file(file, "wb")

    on.exit({
      try({ close(file_con) }, silent = TRUE)
      try({ close(source_con) }, silent = TRUE)
    }, add = TRUE)

    while({
      raw <- readBin(source_con, raw(), n = 1e6)
      length(raw) > 0
    }) {
      writeBin(raw, file_con)
    }
    close(file_con)
    close(source_con)
  }


  tt <- io_read_mat(file, method = 'R.matlab')

  dimension <- as.vector(tt$dimension)
  voxel_size <- as.vector(tt$voxel.size)
  report <- as.integer(tt$report)

  if(length(tt$trans.to.mni) == 16) {
    vox2ras <- matrix(tt$trans.to.mni, c(4, 4), byrow = TRUE, dimnames = NULL)
  } else if(length(tt$trans.to.mni) == 12) {
    vox2ras <- matrix(tt$trans.to.mni, c(3, 4), byrow = TRUE, dimnames = NULL)
    vox2ras <- rbind(vox2ras, c(0, 0, 0, 1))
  } else {
    warning("Unable to obtain the transform information from `tt` file. Using identity transform.")
    vox2ras <- diag(1, 4)
  }

  track <- as.raw(tt$track)

  i <- 0L
  start_pos <- NULL
  while(i < length(track)) {
    start_pos <- c(start_pos, i)

    n_points <- readBin(track[i + seq_len(4)], what = "integer", size = 4, signed = TRUE, endian = "little")
    if (n_points < 0) { n_points <- as.double(n_points) + 2^32 }
    i <- i + n_points + 13
  }

  tracts <- lapply(start_pos, function(i) {
    n_points <- readBin(track[i + seq_len(4)], what = "integer", size = 4, signed = TRUE, endian = "little")
    if (n_points < 0) { n_points <- as.double(n_points) + 2^32 }

    # The first xyz are int32
    p0 <- readBin(
      con = track[i + 4 + seq_len(12)],
      what = "integer",
      size = 4,
      signed = TRUE,
      n = 3,
      endian = "little"
    )

    offsets <- c(
      0, 0, 0,
      readBin(
        track[seq.int(from = 17, length.out = n_points - 3)],
        what = "integer",
        size = 1L,
        endian = "little",
        signed = TRUE,
        n = n_points - 3
      )
    )
    dim(offsets) <- c(3, n_points / 3)
    list(
      coords = t(t(apply(offsets, 1L, cumsum)) + p0) / 32,
      num_points = n_points / 3
    )
  })

  re <- as_ieegio_streamlines.default(x = tracts, vox2ras = vox2ras, class = "ieegio_streamlines_tt")
  re
}


