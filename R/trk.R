read_char <- function(conn, n, to = "UTF-8") {
  txt <- readBin(conn, "raw", n)
  return(iconv(rawToChar(txt[txt != as.raw(0)]), to = to))
}

write_char <- function(conn, text, n = NA) {
  r <- charToRaw(text)
  if(!is.na(n)) {
    if(n > length(r)) {
      r <- r[seq_len(n)]
    } else if (n < length(r)) {
      r <- c(r, rep(as.raw(0), n - length(r)))
    }
  }
  writeBin(r, con = conn)
}

#' @name io-trk
#' @title Read or write \code{'TRK'} streamlines
#' @description
#' Low-level functions, supports compressed files; for high-level functions,
#' please use \code{\link{read_streamlines}} or
#' \code{\link{as_ieegio_streamlines}} instead.
#'
#' @param file,con file path to the streamline file
#' @param x \code{\link{imaging-streamlines}} instance
#' @param half_voxel_offset whether to add 0.5 millimeter shift on each side,
#' default is \code{TRUE}. See 'Details' for explanation.
#' @returns \code{io_read_trk} returns an \code{\link{imaging-streamlines}}
#' instance.
#' @details
#' \code{'TRK'} gains popularity due to its ability to store streamline attributes.
#' However, this file format suffer from ambiguous definition in the initial
#' \code{'TrackVis'} implementation. Typically in a medical image file, there
#' might exists a 4-by-4 matrix that maps the volume indices to the
#' corresponding anatomical right-anterior-superior \code{'RAS'} locations.
#' However, the original definition of \code{'TRK'} does not have this. Since
#' version 2, \code{'TRK'} introduced such matrix, but it was interpreted
#' differently. Instead of the volume index space, the source space is conformed
#' 1 millimeter space, with the origin at the first 'voxel' corner instead of
#' the center. Therefore there is a 0.5 mm shift at each direction, and
#' \code{half_voxel_offset} is designed to offset this shift.
#'
#' What has made this issue complicated was that some software, such as
#' \code{'DSI-studio'}, seemed to ignore that offset when converting from their
#' own format to the \code{'TRK'} format. If the file is generated in such
#' way, please set \code{half_voxel_offset=FALSE} to turn off the offset
#' correction. We always recommend that user store data in \code{'TCK'} format.
#'
#' @examples
#'
#'
#'
#' # This example uses sample data, run
#' # `ieegio_sample_data("streamlines/CNVII_R.trk")` to download
#'
#' if( ieegio_sample_data("streamlines/CNVII_R.trk", test = TRUE) ) {
#'
#'   path <- ieegio_sample_data("streamlines/CNVII_R.trk")
#'   tfile <- tempfile(fileext = ".trk")
#'
#'   # read
#'   x <- io_read_trk(path)
#'
#'   # write
#'   io_write_trk(x, tfile)
#'
#'   # compare two files
#'   file.size(path) == file.size(tfile)
#'
#'   src_raw <- readBin(path, "raw", n = file.size(path))
#'   dst_raw <- readBin(tfile, "raw", n = file.size(tfile))
#'
#'   equal_raw <- src_raw == dst_raw
#'
#'   # Some reserved information are removed
#'   all(equal_raw[-c(945:947)])
#'
#'   unlink(tfile)
#'
#' }
#'
#'
#'
#' @export
io_read_trk <- function(file, half_voxel_offset = TRUE) {
  # file <- "~/Downloads/hcp1065_avg_tracts_trk/association/AF_L.trk.gz"
  # half_voxel_offset <- TRUE
  # file <- "~/Downloads/junk.trk"
  half_voxel_offset <- !isFALSE(as.logical(half_voxel_offset))

  parse_header <- function(fh, endian) {
    trk <- list(header = list(endian = endian))
    trk$header$id_string <- read_char(fh, 6L)

    # Trick: using dim to determine if the endian should be flipped
    # This trick works when the dim < 32768 (2^15) as the integers are stored in signed short (int16)
    # However, it's impractical to have such large storage: imaging 32768 x 32768 x 32768 image
    # this would happen maybe in the future, but not in the near future
    dim_raw <- readBin(fh, "raw", n = 6, endian = endian)
    dim <- readBin(dim_raw, "integer", n = 3, size = 2, endian = endian, signed = TRUE)
    if(any(dim <= 0)) {
      endian <- "big"
      dim <- readBin(dim_raw, "integer", n = 3, size = 2, endian = endian, signed = TRUE)
    }
    trk$header$dim <- dim
    trk$header$voxel_size <- readBin(fh, numeric(), n = 3, size = 4, endian = endian)
    trk$header$origin <- readBin(fh, numeric(), n = 3, size = 4, endian = endian)
    trk$header$n_scalars <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
    trk$header$scalar_names <- read_char(fh, 200L)
    trk$header$n_properties <- readBin(fh, integer(), n = 1, size = 2, endian = endian)
    trk$header$property_names <- read_char(fh, 200L)
    # https://github.com/niivue/niivue/issues/926
    mm2ras <- matrix(readBin(fh, numeric(), n = 16, size = 4, endian = endian), ncol = 4, byrow = TRUE)
    if(mm2ras[[16]] == 0) { mm2ras <- diag(1, 4) }
    vox2mm <- diag(c(1 / trk$header$voxel_size, 1))
    if(half_voxel_offset) {
      vox2mm[1:3, 4] <- -0.5
    }
    trk$header$vox2ras <- mm2ras %*% vox2mm
    trk$header$reserved <- read_char(fh, 444L)
    trk$header$voxel_order <- read_char(fh, 4L)
    trk$header$pad2 <- read_char(fh, 4L)
    trk$header$image_orientation_patient <- readBin(fh, numeric(), n = 6, size = 4, endian = endian)
    trk$header$pad1 <- read_char(fh, 2L)
    trk$header$invert_x <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian)
    trk$header$invert_y <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian)
    trk$header$invert_z <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian)
    trk$header$swap_xy <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian)
    trk$header$swap_yz <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian)
    trk$header$swap_zx <- readBin(fh, integer(), n = 1, size = 1, signed = FALSE, endian = endian)
    trk$header$n_count <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
    trk$header$version <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
    trk$header$hdr_size <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
    trk
  }

  if(endsWith(tolower(file), ".gz")) {
    fh <- gzfile(file, "rb")
  } else {
    fh <- file(file, "rb")
  }
  on.exit({ try(silent = TRUE, { close(fh) }) }, add = FALSE)

  endian <- "little"
  trk <- parse_header(fh, endian)
  if(trk$header$version > 1000) {
    # inverse endianess
    close(fh)

    if(endsWith(tolower(file), ".gz")) {
      fh <- gzfile(file, "rb")
    } else {
      fh <- file(file, "rb")
    }
    on.exit({ try(silent = TRUE, { close(fh) }) }, add = FALSE)

    endian <- "big"
    trk <- parse_header(fh, endian)
  }

  if (trk$header$version != 2L) {
    warning(sprintf("TRK file `%s` is version %d, only version 2 is supported.\n", file, trk$header$version))
  }
  if (trk$header$hdr_size != 1000L) {
    warning(sprintf("TRK file `%s` header field hdr_size is '%d', must be 1000.\n", file, trk$header$hdr_size))
  }

  trk$header$half_voxel_offset <- half_voxel_offset

  n_scalars <- trk$header$n_scalars
  n_properties <- trk$header$n_properties

  tracks <- lapply(seq_len(trk$header$n_count), function(track_idx) {
    current_track <- list(scalars = NULL, properties = NULL, coords = NULL)
    num_points <- readBin(fh, integer(), n = 1, size = 4, endian = endian)
    current_track$num_points <- num_points

    if(num_points > 0) {
      buff <- readBin(fh, what = "numeric", n = (3 + n_scalars) * num_points, size = 4, endian = endian)
      dim(buff) <- c(3 + n_scalars, num_points)

      current_track$coords <- t(buff[c(1, 2, 3), , drop = FALSE])
      if (n_scalars > 0L) {
        current_track$scalars <- t(buff[-c(1, 2, 3), , drop = FALSE])
      }
    }

    # properties
    if (n_properties > 0L) {
      current_track$properties <- readBin(fh, what = "numeric", n = n_properties, size = 4, endian = endian)
    }
    current_track
  })

  trk$data <- tracks

  class(trk) <- c("ieegio_streamlines_trk", "ieegio_streamlines")

  return(trk)
}

#' @name io-trk
#' @export
io_write_trk <- function(x, con, half_voxel_offset = NA) {
  # DIPSAUS DEBUG START
  # file <- "/Users/dipterix/Downloads/hcp1065_avg_tracts_trk/CNVII_R.trk"
  # half_voxel_offset <- TRUE
  # x <- io_read_trk(file)
  # con <- "~/Downloads/junk.trk"
  # io_write_trk(x, con)
  # y <- io_read_trk(con)

  endian <- "little"

  # sample
  if(length(x$data) > 0) {
    item <- x$data[[1]]

    scalar_names <- as.character(names(item$scalars))
    if(length(scalar_names)) {
      scalar_names <- lapply(scalar_names, function(name) {
        if(is.na(name)) { return(NULL) }
        if(!is.numeric(item$scalars[[name]])) {
          warning("TRK scalars must be numeric, while ", sQuote(name),
                  " is not float/integer. Ignoring this scalar")
          return(NULL)
        }
        if(nchar(name, type = "bytes", keepNA = TRUE) > 20) {
          name <- substr(name, start = 1L, stop = 20)
        }
        return(name)
      })
      scalar_names <- unlist(scalar_names)
      n_scalars <- length(scalar_names)
      if(n_scalars > 10) {
        warning("TRK file can only store at most 10 scalars. Only the first 10 scalars will be saved")
        n_scalars <- 10
        scalar_names <- scalar_names[seq_len(n_scalars)]
      }
    } else {
      n_scalars <- 0
    }


    property_names <- as.character(names(item$properties))
    if(length(property_names)) {
      property_names <- lapply(property_names, function(name) {
        if(is.na(name)) { return(NULL) }
        if(!is.numeric(item$properties[[name]])) {
          warning("TRK properties must be numeric, while ", sQuote(name),
                  " is not float/integer. Ignoring this scalar")
          return(NULL)
        }
        if(nchar(name, type = "bytes", keepNA = TRUE) > 20) {
          name <- substr(name, start = 1L, stop = 20)
        }
        return(name)
      })
      property_names <- unlist(property_names)
      n_properties <- length(property_names)
      if(n_properties > 10) {
        warning("TRK file can only store at most 10 properties Only the first 10 scalars will be saved")
        n_properties <- 10
        property_names <- property_names[seq_len(n_properties)]
      }

    } else {
      n_properties <- 0
    }

  } else {
    n_scalars <- 0L
    scalar_names <- character()
    n_properties <- 0L
    property_names <- character()
  }


  if(!inherits(con, "connection")) {
    path <- con
    if(endsWith(tolower(path), ".gz")) {
      con <- gzfile(path, "wb")
    } else {
      con <- file(path, "wb")
    }
    on.exit({ close(con) }, add = FALSE)
  }

  write_char("TRACK", conn = con, n = 6)
  dim <- x$header$dim
  if(!length(dim)) { dim <- c(256, 256, 256) }
  dim <- dim[1:3]
  writeBin(as.integer(dim), con = con, size = 2, endian = endian)

  voxel_size <- x$header$voxel_size
  if(!length(voxel_size)) {
    voxel_size <- c(1, 1, 1)
  }
  voxel_size <- voxel_size[1:3]
  writeBin(as.double(voxel_size), con = con, size = 4, endian = endian)

  origin <- x$header$origin
  if(!length(origin)) { origin <- c(0, 0, 0) }
  origin <- origin[1:3]
  writeBin(as.double(origin), con = con, size = 4, endian = endian)


  # n_scalars & scalar_name[10][20]
  writeBin(as.integer(n_scalars), con = con, size = 2, endian = endian)
  scalar_names_raw <- unlist(lapply(scalar_names, function(name) {
    r <- charToRaw(name)
    c(r, rep(as.raw(0), 20 - length(r)))
  }))
  scalar_names_raw <- c(
    scalar_names_raw,
    rep(as.raw(0), 200 - length(scalar_names_raw))
  )
  writeBin(scalar_names_raw, con = con, endian = endian)

  # n_properties & property_name[10][20]
  writeBin(as.integer(n_properties), con = con, size = 2, endian = endian)
  property_names_raw <- unlist(lapply(property_names, function(name) {
    r <- charToRaw(name)
    c(r, rep(as.raw(0), 20 - length(r)))
  }))
  property_names_raw <- c(
    property_names_raw,
    rep(as.raw(0), 200 - length(property_names_raw))
  )
  writeBin(property_names_raw, con = con, endian = endian)

  # https://github.com/niivue/niivue/issues/926
  if(is.na(half_voxel_offset)) {
    half_voxel_offset <- !isFALSE(x$header$half_voxel_offset)
  }
  vox2mm <- diag(c(1 / voxel_size, 1))
  if(half_voxel_offset) {
    vox2mm[1:3, 4] <- -0.5
  }
  vox2ras <- x$header$vox2ras # mm2ras %*% vox2mm
  mm2ras <- vox2ras %*% solve(vox2mm)

  writeBin(as.double(t(mm2ras)), con = con, size = 4, endian = endian)

  # reserved 444 raw(0)
  writeBin(rep(as.raw(0), 444L), con = con, endian = endian)

  # voxel_order
  voxel_order <- x$header$voxel_order
  if(length(voxel_order) != 1 || nchar(voxel_order, "bytes") > 4L) {
    voxel_order <- "LPS"
  }
  write_char(conn = con, text = voxel_order, n = 4L)

  pad2 <- x$header$pad2
  if(length(pad2) != 1 || nchar(pad2, "bytes") > 4L) {
    pad2 <- "LPS"
  }
  write_char(conn = con, text = pad2, n = 4L)

  image_orientation_patient <- x$header$image_orientation_patient
  if(length(image_orientation_patient) != 6) {
    image_orientation_patient <- c(1, 0, 0, 0, 1, 0)
  }
  writeBin(as.double(image_orientation_patient), con = con, size = 4L,
           endian = endian)

  pad1 <- x$header$pad1
  if(!length(pad1) || nchar(pad1, "bytes") > 2L) {
    pad1 <- ""
  }
  write_char(conn = con, text = pad1, n = 2L)

  # invert_x, y, z, xy, yz, zx... Why this makes sense???
  writeBin(as.integer(c(x$header$invert_x, 0)[[1]]),
           con = con, size = 1, endian = endian)

  writeBin(as.integer(c(x$header$invert_y, 0)[[1]]),
           con = con, size = 1, endian = endian)

  writeBin(as.integer(c(x$header$invert_z, 0)[[1]]),
           con = con, size = 1, endian = endian)

  writeBin(as.integer(c(x$header$swap_xy, 0)[[1]]),
           con = con, size = 1, endian = endian)

  writeBin(as.integer(c(x$header$swap_yz, 0)[[1]]),
           con = con, size = 1, endian = endian)

  writeBin(as.integer(c(x$header$swap_zx, 0)[[1]]),
           con = con, size = 1, endian = endian)

  nl <- length(x$data)
  writeBin(object = as.integer(nl), con = con, size = 4, endian = endian)

  # version
  writeBin(object = 2L, con = con, size = 4, endian = endian)

  # hdr_size
  writeBin(object = 1000L, con = con, size = 4, endian = endian)

  lapply(seq_len(nl), function(ii) {
    item <- x$data[[ii]]

    coords <- item$coords[, 1:3, drop = FALSE]
    n_pts <- length(coords) / 3
    writeBin(object = as.integer(n_pts), con = con, size = 4, endian = endian)

    if(length(coords)) {
      coords <- as.vector(t(coords))
    }
    if(length(scalar_names)) {
      scalars <- do.call("cbind", lapply(scalar_names, function(name) {
        as.double(item$scalars[[name]])
      }))
      scalars <- as.vector(t(scalars))
    } else {
      scalars <- NULL
    }
    if(length(property_names)) {
      properties <- vapply(property_names, function(name) {
        as.double(item$properties[[name]])
      }, 0.0)
    } else {
      properties <- NULL
    }

    buff <- as.double(c(coords, scalars, properties))
    writeBin(object = buff, con = con, size = 4, endian = endian)
    return()
  })

  return(invisible(con))
}
