#' @name io-trk
#' @title Read or write \code{'TCK'} streamlines
#' @description
#' Low-level functions; for high-level functions,
#' please use \code{\link{read_streamlines}} or
#' \code{\link{as_ieegio_streamlines}} instead.
#'
#' @param file,con file path to the streamline file
#' @param x \code{\link{imaging-streamlines}} instance
#' @param datatype data storage type to write, default is \code{'Float32LE'},
#' 4-byte little 'endian' float; other choices are \code{'Float32BE'},
#' \code{'Float64LE'}, and \code{'Float64BE'}
#' @returns \code{io_read_tck} returns a \code{ieegio} streamline object,
#' \code{io_write_tck} returns the connection or file path.
#' @examples
#'
#'
#' # run `ieegio_sample_data("streamlines/CNVII_R.tck")` to
#' # download sample data
#'
#' if( ieegio_sample_data("streamlines/CNVII_R.tck", test = TRUE) ) {
#'
#'   path <- ieegio_sample_data("streamlines/CNVII_R.tck")
#'
#'   # Read
#'   streamlines <- io_read_tck(path)
#'
#'   plot(streamlines)
#'
#'   # write
#'   tfile <- tempfile(fileext = ".tck")
#'   io_write_tck(streamlines, tfile, datatype = streamlines$header$datatype)
#'
#'   # verify two files are identical
#'   digest::digest(file = tfile) == digest::digest(file = path)
#'
#'
#'   unlink(tfile)
#'
#' }
#'
#'
#' @export
io_read_tck <- function(file) {
  # file <- "~/Downloads/hcp1065_avg_tracts_trk/association/AF_L.tck"

  # freesurferformats::read.dti.tck(file)

  # source_file <- file
  #
  # if(endsWith(tolower(source_file), ".gz")) {
  #   file <- tempfile(fileext = ".mat")
  #   source_con <- gzfile(source_file, open = "rb")
  #   file_con <- file(file, "wb")
  #
  #   on.exit({
  #     try({ close(file_con) }, silent = TRUE)
  #     try({ close(source_con) }, silent = TRUE)
  #   }, add = TRUE)
  #
  #   while({
  #     raw <- readBin(source_con, raw(), n = 1e6)
  #     length(raw) > 0
  #   }) {
  #     writeBin(raw, file_con)
  #   }
  #   close(file_con)
  #   close(source_con)
  # }

  fsize <- as.double(file_size(file))
  if(fsize < 20) {
    stop("TCK file size too small")
  }

  conn <- file(file, open = "rb")
  on.exit({ try(silent = TRUE, { close(conn) }) })

  # read first lines
  header_text <- NULL
  while({
    line <- readLines(conn, n = 1)
    length(row) == 1 && !grepl("END", trimws(line), ignore.case = FALSE, fixed = TRUE)
  }) {
    header_text <- c(header_text, line)
  }
  header_text <- trimws(header_text)
  if(!any(grepl('mrtrix tracks', header_text, ignore.case = TRUE))) {
    stop('Not a valid TCK file: header text must contain "mrtrix tracks" keyword.')
  }

  offset_text <- header_text[grepl("file:", header_text, ignore.case = TRUE)]
  if(!length(offset_text)) {
    stop('Not a valid TCK file: missing header offset')
  }
  offset <- strsplit(offset_text[[1]], " ")[[1]]
  offset <- as.integer(offset[[length(offset)]])

  if(!isTRUE(offset >= 20)) {
    stop('Not a valid TCK file: invalid header offset')
  }


  all_lines <- NULL
  valid_datatypes <- c("Float32BE", "Float32LE", "Float64BE", "Float64LE")
  datatype_text <- header_text[grepl("datatype:", header_text, ignore.case = TRUE)]
  datatype <- trimws(gsub("datatype:", "", datatype_text, ignore.case = TRUE))

  if( !isTRUE(toupper(datatype) %in% toupper(valid_datatypes) )) {
    warning("Invalid datatype in TCK file header... Assuming little endian - Float32LE")
    datatype <- "Float32LE"
  } else {
    datatype <- valid_datatypes[toupper(valid_datatypes) %in% toupper(datatype)][[1]]
  }

  switch (
    datatype,
    "Float32BE" = {
      endian <- "big"
      byte_size <- 4L
    },
    "Float32LE" = {
      endian <- "little"
      byte_size <- 4L
    },
    "Float64BE" = {
      endian <- "big"
      byte_size <- 8L
    },
    "Float64LE" = {
      endian <- "little"
      byte_size <- 8L
    },
    {
      stop("Unsupported data format: ", datatype)
    }
  )

  seek(conn, where = offset, origin = "start")
  n <- (fsize - offset) / byte_size
  tracks_rawdata <- readBin(conn, what = "numeric", n = n, size = byte_size, endian = endian)
  close(conn)
  dim(tracks_rawdata) <- c(3, length(tracks_rawdata) / 3)

  cutoffs <- which(colSums(is.finite(tracks_rawdata)) < 3)
  last_cutoff <- ncol(tracks_rawdata) + 1L
  if(length(cutoffs) > 0) {
    if(cutoffs[[1]] != 0) {
      cutoffs <- c(0L, cutoffs)
    }
    if(cutoffs[[length(cutoffs)]] != last_cutoff) {
      cutoffs <- c(cutoffs, last_cutoff)
    }
  } else {
    cutoffs <- c(0L, ncol(tracks_rawdata) + 1)
  }
  tracts <- lapply(seq_len(length(cutoffs) - 1), function(ii) {
    start_idx <- cutoffs[[ii]] + 1
    end_idx <- cutoffs[[ii + 1]] - 1
    if(start_idx >= end_idx) { return(NULL) }
    idx <- seq.int(start_idx, end_idx)
    list(
      coords = t(tracks_rawdata[, idx, drop = FALSE]),
      num_points = length(idx)
    )
  })
  tracts <- tracts[!vapply(tracts, is.null, FALSE)]

  re <- as_ieegio_streamlines.default(tracts, vox2ras = diag(1, 4), datatype = datatype, class = "ieegio_streamlines_tck")
  re
}

#' @rdname io-trk
#' @export
io_write_tck <- function(x, con, datatype = c("Float32LE", "Float32BE", "Float64LE", "Float64BE")) {
  datatype <- match.arg(datatype)


  # DIPSAUS DEBUG START
  # file <- "~/Downloads/hcp1065_avg_tracts_trk/association/AF_L.tck"
  # x <- io_read_tck(file)
  # con <- "~/Downloads/junk.tck"
  # datatype <- "Float32LE"

  switch (
    datatype,
    "Float32BE" = {
      endian <- "big"
      byte_size <- 4L
    },
    "Float32LE" = {
      endian <- "little"
      byte_size <- 4L
    },
    "Float64BE" = {
      endian <- "big"
      byte_size <- 8L
    },
    "Float64LE" = {
      endian <- "little"
      byte_size <- 8L
    },
    {
      stop("Unsupported data format: ", datatype)
    }
  )

  n_tracts <- length(x$data)


  if(inherits(con, "connection")) {
    connection <- con
  } else {
    connection <- file(con, open = "wb")
    on.exit({ close(connection) })
  }

  # 67 characters with `\n`
  header_text <- c("mrtrix tracks", sprintf("count: %010d", n_tracts), sprintf("datatype: %s", datatype), "file: . 67", "END", "")
  header_text <- paste(header_text, collapse = "\n")


  writeBin(charToRaw(header_text), con = connection)

  # turn tract data to a whole matrix
  lapply(seq_len(n_tracts), function(ii) {
    coords_data <- t(x[[ii]]$coords[, c(1,2,3), drop = FALSE])
    dimnames(coords_data) <- NULL
    coords_data <- cbind(coords_data, NaN)
    # if(ii != n_tracts) {
    # }
    if(!is.double(coords_data)) {
      coords_data <- as.double(coords_data)
    }
    writeBin(object = as.vector(coords_data), con = connection, size = byte_size, endian = endian)
    return()
  })

  # ending the sequence in case n_tracts == 0
  writeBin(object = rep(Inf, 3L), con = connection, size = byte_size, endian = endian)

  invisible(con)
}
