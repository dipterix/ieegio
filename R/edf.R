parse_edf_annot <- function(x) {

  seps <- which(x == 0)
  nsegs <- length(seps)

  if(nsegs == length(x)) {
    # no annotations
    return(NULL)
  }

  parse_annot_seg <- function(x) {

    # x <- period[[12]]
    idx <- which(x == 20)

    if(!length(idx)) {
      return(NULL)
    }

    # TAL starts with onset + (optional) duration
    timestamp_with_duration <- x[seq_len(idx[[1]] - 1)]

    idx2 <- which(timestamp_with_duration == 21)
    if(length(idx2)) {
      idx2 <- idx2[[1]]
      timestamp <- as.numeric(intToUtf8(timestamp_with_duration[seq_len(idx2 - 1)]))
      duration <- as.numeric(intToUtf8(timestamp_with_duration[- seq_len(idx2)]))
    } else {
      timestamp <- as.numeric(intToUtf8(timestamp_with_duration))
      duration <- NA_real_
    }

    comments <- rawToChar(as.raw(x[-seq_len(idx[[1]])]))
    if(length(idx) > 1) {
      comments <- strsplit(comments, "\024")[[1]]
    }

    idx <- which(comments == 20)

    list(timestamp = timestamp, duration = duration, comments = paste(comments, collapse = " "))
  }

  seps <- c(0, seps)
  annotations <- data.table::rbindlist(lapply(seq_len(nsegs), function(ii) {
    idx1 <- seps[[ii]] + 1
    idx2 <- seps[[ii + 1]]
    if(idx1 == idx2) { return(NULL) }
    # print(x[seq(idx1, idx2)])
    parse_annot_seg(x[seq(idx1, idx2)])
  }))

  if(nrow(annotations) == 0) {
    return(NULL)
  }

  return(annotations)
}

internal_read_edf_header <- function(con) {
  # DIPSAUS DEBUG START
  # con <- "/Users/dipterix/Library/R/arm64/4.4/library/edfReader/extdata/bdfPlusC.bdf"
  # hdr <- edfReader::readEdfHeader(con)


  if(!inherits(con, "connection")) {
    file_assert(con)
    con <- file(con, "rb")
    on.exit({ close(con) })
  }

  # version
  version <- readBin(con, "integer", n = 1, size = 1, signed = FALSE, endian = "little")

  # file_type, sampling_bits
  # version must be either 48 or 255
  switch(
    as.character(version),
    "48" = {
      version <- "0"
      file_type <- "EDF"
      spaces <- readChar(con, 7, TRUE)
      sampling_bits <- 16
      if (spaces != "       ") {
        stop("File is not an EDF nor BDF file.")
      }
    },
    "255" = {
      file_type <- "BDF"
      bdf_version <- gsub("[[:space:]]*$", "", readChar(con, 7, TRUE))
      sampling_bits <- 24
      if( bdf_version == "BIOSEMI" ) {
        version <- '"255"BIOSEMI'
      } else {
        stop("File is not an EDF nor BDF file.")
      }
    },
    {
      stop("File is not an EDF nor BDF file.")
    }
  )

  # get patient
  patient <- trimws(readChar(con, 80, TRUE))

  # recording ID
  recording_id <- trimws(readChar(con, 80, TRUE))

  # Recording start time
  datetime_string <- readChar(con, 16, TRUE)
  ddmmyy <- substr(datetime_string, 1, 8)
  hhmmss <- substr(datetime_string, 9, 16)
  start_time <- strptime(sprintf("%s %s", ddmmyy, hhmmss), format = "%d.%m.%y %H.%M.%S", tz = "")

  # header-length for data
  header_length <- as.integer(readChar(con, 8, TRUE))

  # reserved
  header_reserved <- trimws(readChar(con, 44, TRUE))

  # # of recording
  n_records <- as.integer(readChar(con, 8, TRUE))

  # duration
  duration <- as.numeric(readChar(con, 8, TRUE))

  # # of signals
  n_signals <- as.integer(readChar(con, 4, TRUE))

  # read labels
  labels <- trimws( replicate(n_signals, { readChar(con, nchars = 16, useBytes = TRUE) }) )
  sel <- labels == ""
  if(any(sel)) {
    labels[sel] <- sprintf("unnamed channel %d", which(sel))
  }

  # transducer_type
  transducer_type <- trimws( replicate(n_signals, { readChar(con, nchars = 80, useBytes = TRUE) }) )

  # physical unit
  physical_unit <- trimws( replicate(n_signals, { readChar(con, nchars = 8, useBytes = TRUE) }) )

  # physical min and max
  physical_min <- as.numeric(replicate(n_signals, { readChar(con, nchars = 8, useBytes = TRUE) }))
  physical_max <- as.numeric(replicate(n_signals, { readChar(con, nchars = 8, useBytes = TRUE) }))

  # digital min and max
  digital_min <- as.numeric(replicate(n_signals, { readChar(con, nchars = 8, useBytes = TRUE) }))
  digital_max <- as.numeric(replicate(n_signals, { readChar(con, nchars = 8, useBytes = TRUE) }))

  # filters
  pre_filter <- trimws( replicate(n_signals, { readChar(con, nchars = 80, useBytes = TRUE) }) )

  # samples_per_record (length of each record)
  samples_per_record <- as.numeric(replicate(n_signals, { readChar(con, nchars = 8, useBytes = TRUE) }))

  reserved <- trimws( replicate(n_signals, { readChar(con, nchars = 32, useBytes = TRUE) }) )

  if( isTRUE(duration > 0) ) {
    sample_rate <- samples_per_record / duration
  } else {
    sample_rate <- rep(NA_real_, n_signals)
  }

  total_signal_size <- samples_per_record * n_records
  total_signal_duration <- duration * n_records

  is_plus <- FALSE
  continuous_recording <- TRUE
  is_annotation <- rep(FALSE, n_signals)
  if( startsWith(header_reserved, "EDF+") || startsWith(header_reserved, "BDF+") ) {
    is_plus <- TRUE

    # discrete recording
    if( endsWith(header_reserved, "+D") ) {
      continuous_recording <- FALSE
    }

    # check if is annotation channel
    is_annotation <- labels %in% c("EDF Annotations", "BDF Annotations")
  }

  sel <- !is_annotation & digital_min < digital_max & physical_min != physical_max

  # digital to analog conversion slope
  slope <- ifelse(
    sel,
    (physical_max - physical_min) / (digital_max - digital_min),
    1
  )

  intercept <- ifelse(sel, physical_min - slope * digital_min, 0)

  channel_info <- data.table::data.table(
    stringsAsFactors = FALSE,

    Channel = seq_along(labels),
    Label = labels,
    Annotation = is_annotation,
    TransducerType = transducer_type,
    Unit = physical_unit,
    Filter = pre_filter,
    SamplesPerRecord = samples_per_record,
    SampleRate = sample_rate,

    # conversion from digital to physical
    Slope = slope,
    Intercept = intercept,

    DigitalMin = digital_min,
    DigitalMax = digital_max,

    PhysicalMin = physical_min,
    PhysicalMax = physical_max,

    Comment = reserved
  )

  # get information for EDF+
  # start_time_fraction <- 0.0
  # if(any(is_annotation)) {
  #   expected_header_length <- 256 * (n_signals + 1)
  #   if( expected_header_length != header_length ) {
  #     seek(con, where = header_length, origin = "start")
  #   }
  #   # get the first data point
  #   sample_byte <- sampling_bits / 8
  #
  #   # # read raw
  #   # n <- samples_per_record
  #   # n[is_annotation] <- n[is_annotation] * sample_byte
  #   # size <- ifelse(is_annotation, 1L, sample_byte)
  #   #
  #   # period <- lapply(seq_len(n_signals), function(ii) {
  #   #   if( is_annotation[[ii]] ) {
  #   #     readBin(con, what = "integer", n = n[[ii]], size = size[[ii]], signed = TRUE, endian = "little")
  #   #     # readChar(con, n[[ii]], TRUE)
  #   #   } else {
  #   #     readBin(con, what = "integer", n = n[[ii]], size = size[[ii]], signed = TRUE, endian = "little")
  #   #   }
  #   # })
  #   #
  #   # annot <- parse_edf_annot(period[[which(is_annotation)[[1]]]])
  #   # start_time_fraction <- annot$timestamp
  #
  # }

  # basic headers

  header_basic <- structure(
    class = c("ieegio_edf_header_basic"),
    list(
      file_type = c(file_type, header_reserved),
      version = version,
      is_plus = is_plus,
      header_length = header_length,
      sampling_bits = sampling_bits,

      # recording meta
      recording_id = recording_id,
      patient = patient,

      continuous_recording = continuous_recording,
      n_records = n_records,
      record_duration = duration,
      start_time = start_time,
      # start_time_fraction = start_time_fraction,
      n_channels = n_signals
    )
  )


  header <- list(
    basic = header_basic,
    channel_table = channel_info
  )

  header
}

internal_write_edf_header <- function(header, con) {

  hdr <- header
  debug_pointer <- function() {}
  fout <- con

  # DIPSAUS DEBUG START
  # con <- "/Users/dipterix/Library/R/arm64/4.4/library/edfReader/extdata/edfPlusC.edf"
  # hdr <- internal_read_edf_header(con)
  # fout <- rawConnection(raw(), "w+")
  # debug_pointer <- function() {
  #   cat("File size: ", seek(fout), "\n")
  # }
  write_string <- function(str, len = nchar(str), padding = " ", collapse = TRUE) {
    if(collapse) {
      str <- paste(str, collapse = "")
    }
    str <- charToRaw(str)
    if(length(str) < len) {
      str <- c(str, rep(charToRaw(padding)[[1]], len))
    }
    str <- str[seq_len(len)]
    writeBin(str, fout, endian = "little", useBytes = TRUE)
    debug_pointer()
    invisible()
  }
  write_float <- function(x, len) {
    sci <- FALSE

    if(x > 0 && x < 0.1^len) {
      sci <- TRUE
    } else if (x < 0 && x > 0.1^(len - 1)) {
      sci <- TRUE
    } else {
      n1 <- nchar(sprintf("%.0f", x))
      if(n1 > len) {
        # must support sci
        warning("Number ", x, " is represented with scientific number")
        sci <- TRUE
      }
    }

    if( sci ) {
      dec_len <- max(7L - nchar(sprintf("%.0e", x)), 0)
      fmt <- sprintf("%%8.%de", dec_len)
    } else {
      dec_len <- max(7L - nchar(sprintf("%.0f", x)), 0)
      fmt <- sprintf("%%8.%df", dec_len)
    }
    str <- sprintf(fmt, x)
    stopifnot(nchar(str) == len)
    write_string(trimws(str), len = len)
    invisible()
  }
  write_int <- function(x, len) {
    str <- sprintf("%.0f", x)
    stopifnot(nchar(str) <= len)
    write_string(trimws(str), len = len)
    invisible()
  }

  # version little endian, unsigned 48, `0       `
  write_string("0", len = 8L)

  # patient
  write_string(hdr$basic$patient, len = 80L)

  # recording ID
  write_string(hdr$basic$recording_id, len = 80L)

  # Recording start time
  write_string(format(hdr$basic$start_time, "%d.%m.%y%H.%M.%S"), len = 16L)

  # header-length for data
  write_int((hdr$basic$n_channels + 1) * 256, len = 8L)

  # reserved (EDF+C or D)
  write_string(ifelse(hdr$basic$continuous_recording, "EDF+C", "EDF+D"), len = 44L)

  # # of recording
  write_int(hdr$basic$n_records, len = 8L)

  # duration
  write_float(hdr$basic$record_duration, len = 8L) # 245 - 252

  # # of signals
  n_channels <- hdr$basic$n_channels
  write_int(n_channels, len = 4L) # 253 - 256

  # channel labels 16 chars
  has_annotations <- length(hdr$channel_table$Annotation) > 0
  if(!length(hdr$channel_table$Label)) {
    warning("Channel table has no `Label` column, using default channel numbers")
    hdr$channel_table$Label <- sprintf("Ch%d", seq_len(n_channels))
  }
  if( has_annotations ) {
    lapply(seq_len(n_channels), function(ii) {
      if(hdr$channel_table$Annotation[[ii]]) {
        str <- "EDF Annotations"
      } else {
        str <- hdr$channel_table$Label[[ii]]
      }
      write_string(str, len = 16L)
    })
  } else {
    lapply(seq_len(n_channels), function(ii) {
      str <- hdr$channel_table$Label[[ii]]
      write_string(str, len = 16L)
    })
  }

  # transducer_type 80 chars
  if(!length(hdr$channel_table$TransducerType)) {
    warning("Channel table has no `TransducerType` column, leaving it blank")
    hdr$channel_table$TransducerType <- ""
  }
  lapply(seq_len(n_channels), function(ii) {
    write_string(hdr$channel_table$TransducerType[[ii]], len = 80L)
  })

  # physical unit 8 chars
  if(!length(hdr$channel_table$Unit)) {
    warning("Channel table has no `Unit` column (Physical Dimension), set to 'uV'")
    hdr$channel_table$Unit <- "uV"
  }
  lapply(seq_len(n_channels), function(ii) {
    write_string(hdr$channel_table$Unit[[ii]], len = 8L)
  })

  # physical min and max 8 + 8 chars
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$PhysicalMin[[ii]]
    write_float(str, len = 8L)
  })
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$PhysicalMax[[ii]]
    write_float(str, len = 8L)
  })

  # digital min and max 8 + 8 chars
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$DigitalMin[[ii]]
    write_int(str, len = 8L)
  })
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$DigitalMax[[ii]]
    write_int(str, len = 8L)
  })

  # filters 80 chars
  if(!length(hdr$channel_table$Filter)) {
    warning("Channel table has no `Filter` column (Prefiltering), set to blank")
    hdr$channel_table$Filter <- ""
  }
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$Filter[[ii]]
    write_string(str, len = 80L)
  })

  # samples_per_record (length of each record) 8 chars
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$SamplesPerRecord[[ii]]
    write_int(str, len = 8L)
  })

  # reserved, 32 chars
  if(!length(hdr$channel_table$Comment)) {
    hdr$channel_table$Comment <- ""
  }
  lapply(seq_len(n_channels), function(ii) {
    str <- hdr$channel_table$Comment[[ii]]
    write_string(str, len = 32L)
  })

  # actual <- rawConnectionValue(fout)
  #
  # expected <- readBin(con, "raw", n = length(actual), endian = "little")
  #
  # which(actual != expected)

  # rawToChar(actual[1595:1696])
  # rawToChar(expected[1595:1696])
  # which(actual != expected)

  invisible(hdr)
}

internal_read_edf_signal <- function(con, channels, begin = 0, end = Inf, convert = TRUE, header) {


  if(!isTRUE(begin < end)) {
    stop("Invalid time range: ", begin, " to ", end)
  }

  if(missing(header)) {
    if(inherits(con, "connection")) {
      if(isSeekable(con)) {
        seek(con = con, 0)
      }
    }
    header <- internal_read_edf_header(con)
  }

  n_channels <- header$basic$n_channels
  if(missing(channels) || is.null(channels)) {
    channels <- seq_len(n_channels)
  } else {
    channels <- parse_svec(channels, sort = TRUE)
    channels <- channels[channels %in% seq_len(n_channels)]
  }

  if(!length(channels)) {
    stop("No channel to read. Please specify a valid channel")
  }

  if(!inherits(con, "connection")) {
    con <- file(con, "rb")
    readBin(con, "raw", n = header$basic$header_length)
    on.exit({ close(con) })
  } else {
    if(isSeekable(con)) {
      seek(con = con, header$basic$header_length)
    }
  }

  # DIPSAUS DEBUG START
  # con <- "/Users/dipterix/Downloads/ST7011J0-PSG.edf"
  # con <- "/Users/dipterix/Library/R/arm64/4.4/library/edfReader/extdata/edfAnnonC.edf"
  # list2env(list(begin = 0, end = Inf, convert = TRUE), envir=.GlobalEnv)
  # header <- internal_read_edf_header(con)
  # n_channels <- header$basic$n_channels
  # channels <- seq_len(n_channels)
  # con <- file(con, "rb")
  # readBin(con, "raw", n = header$basic$header_length)

  sampling_bytes <- header$basic$sampling_bits / 8

  # read raw
  is_annotation <- header$channel_table$Annotation
  n <- header$channel_table$SamplesPerRecord
  # For signals, it's SamplesPerRecord x (2 bytes)
  # for annots, it's (SamplesPerRecord x 2) x (1 chars)
  n[is_annotation] <- n[is_annotation] * sampling_bytes
  size <- ifelse(is_annotation, 1L, sampling_bytes)

  if(any(is_annotation)) {
    annot_channel <- which(is_annotation)
    first_annot_channel <- min(annot_channel)
    if( !all(annot_channel %in% channels) ) {
      channels <- unique(sort(c(channels, annot_channel)))
    }
  } else {
    annot_channel <- integer()
    first_annot_channel <- NA
  }

  data <- fastmap::fastmap()
  for(chn in channels) {
    data$set(as.character(chn), fastmap::fastqueue())
  }
  read_next_record <- function() {
    lapply(seq_len(n_channels), function(ii) {
      if(!ii %in% channels) {
        # pass and ignore
        readBin(con, what = "raw", n = n[[ii]] * size[[ii]], size = 1L)
        return()
      }
      # map <- data$get(as.character(ii))
      if( !is_annotation[[ii]] ) {
        elem_size <- size[[ii]]
        n_elems <- n[[ii]]
        if( elem_size == 2 ) {
          slice <- readBin(con, what = "integer", n = n_elems, size = 2L, signed = TRUE, endian = "little")
        } else {
          # BDF format
          slice <- readBin(con, what = "integer", n = n_elems * elem_size, size = 1L, signed = TRUE, endian = "little")
          dim(slice) <- c(elem_size, n_elems)
          slice12 <- slice[c(1, 2), ]
          slice12[slice12 < 0] <- slice12[slice12 < 0] + 256
          slice[c(1, 2), ] <- slice12
          slice <- colSums(slice * c(1, 256, 65536))
        }

        # readChar(con, n[[ii]], TRUE)
        if( convert ) {
          slope <- header$channel_table$Slope[[ii]]
          interp <- header$channel_table$Intercept[[ii]]
          slice <- slice * slope + interp
        }
      } else {
        slice <- readBin(
          con,
          what = "integer",
          n = n[[ii]],
          size = size[[ii]],
          signed = FALSE,
          endian = "little"
        )
        slice <- parse_edf_annot(slice)
      }
      # map$add(slice)
      slice
    })
  }

  # seek(con = con, header$basic$header_length)
  env <- new.env(parent = emptyenv())
  env$previous_finish <- 0

  record_duration <- header$basic$record_duration

  continuous_recording <- header$basic$continuous_recording
  record_duration <- header$basic$record_duration

  annots <- fastmap::fastqueue()
  timestamps <- fastmap::fastqueue()

  drop_nulls(lapply(seq_len(header$basic$n_records), function(ii_rec) {
    # trying to obtain the timestamp
    # estimate earliest start
    earliest_start <- env$previous_finish
    if( earliest_start >= end ) { return() }

    record <- read_next_record()

    # https://www.edfplus.info/specs/edfplus.html
    # 2.2.4. Time keeping of data records
    # ... the **first annotation** of the **first 'EDF Annotations' signal** in
    # each data record is empty, but its timestamp specifies how many seconds
    # after the file start date/time that data record starts. So, if the
    # first TAL in a data record reads '+567\20\20', then that data record starts
    # 567s after the startdate/time of the file. If the data records contain
    # 'ordinary signals', then the starttime of each data record must be the
    # starttime of its signals. If there are no 'ordinary signals', then a
    # non-empty annotation immediately following the time-keeping annotation
    # (in the same TAL) must specify what event defines the starttime of this
    # data record. For example, '+3456.789\20\20R-wave\20' indicates that
    # this data record starts at the occurrence of an R-wave, which is 3456.789s
    # after file start.
    #       The startdate/time of a file is specified in the EDF+ header fields
    # 'startdate of recording' and 'starttime of recording'. These fields must
    # indicate the absolute second in which the start of the first data record
    # falls. So, the first TAL in the first data record always starts with
    # +0.X\20\20, indicating that the first data record starts a fraction, X,
    # of a second after the startdate/time that is specified in the EDF+ header.
    # If X=0, then the .X may be omitted.

    if( length(annot_channel) ) {
      timestamp <- NA_real_
      first_annot <- record[[annot_channel[[1]]]]
      if(!is.null(first_annot)) {
        timestamp <- first_annot$timestamp[[1]]
      }
      annot <- data.table::rbindlist(lapply(annot_channel, function(annot_channel_ii) {
        annot_item <- record[[annot_channel_ii]]
        if( is.null(annot_item) ) { return(NULL) }
        if(
          isTRUE(annot_channel_ii == first_annot_channel) &&
          isTRUE(annot_item$timestamp[[1]] == 0) &&
          isTRUE(is.na(annot_item$duration[[1]])) &&
          identical(annot_item$comments[[1]], "")
        ) {
          annot_item <- annot_item[-1, ]
        }
        if(nrow(annot_item) == 0) { return(NULL) }
        annot_item$channel <- annot_channel_ii
        return(annot_item)
      }))
      if(!nrow(annot)) {
        annot <- NULL
      }
    } else {
      # no annotation, assuming it's continuous
      timestamp <- record_duration * (ii_rec - 1)
      annot <- NULL
    }
    if(is.na(timestamp)) {
      # This normally unlikely to happen according to the spec (maybe...?)
      timestamp <- earliest_start
      # warning("EDF file annotations: the first annotation in the first 'EDF Annotations' signal must have timestamp.")
    }

    slice_start <- timestamp

    slice_finish <- slice_start + record_duration

    env$previous_finish <- slice_finish
    if( slice_finish < begin || slice_start >= end ) { return() }

    annots$add(annot)
    timestamps$add(timestamp)

    # annot$duration <- slice_duration

    lapply(seq_len(n_channels), function(ii) {
      slice <- record[[ii]]
      if(is.null(slice) || is.list(slice)) { return() }
      map <- data$get(as.character(ii))

      # if( is.list(slice) ) {
      #   # EDF annot
      #   map$add(slice$comment)
      # } else {

      # channel
      map$add(slice)

      # }

      return()
    })
    return()
  }))
  annots <- annots$as_list()
  timestamps <- timestamps$as_list()

  # print(unlist(annots))

  channels <- channels[!channels %in% annot_channel]

  result <- lapply(channels, function(chn) {
    map <- data$get(as.character(chn))
    n_segs <- map$size()

    srate <- header$channel_table$SampleRate[[chn]]
    samples_per_record <- header$channel_table$SamplesPerRecord[[chn]]

    time_0 <- (seq_len(samples_per_record) - 1) / srate

    signal <- lapply(seq_len(n_segs), function(ii) {
      seg <- map$remove()
      seg_start <- timestamps[[ii]]

      slen <- length(seg)
      if( slen > samples_per_record ) {
        seg <- seg[seq_len(samples_per_record)]
      } else if (slen < samples_per_record) {
        seg <- c(seg, rep(0.0, samples_per_record - slen))
      }
      time <- seg_start + time_0
      # cut time later
      # if( time[[1]] < begin || time[[length(time)]] >= end ) {
      #   sel <- time >= begin & time < end
      #   seg <- seg[sel]
      #   time <- time[sel]
      # }

      cbind(seg, time)
    })
    signal <- do.call("rbind", signal)

    info <- as.list(header$channel_table[chn, ])
    structure(
      class = "ieegio_edf_channel",
      list(
        info = info,
        value = signal[, 1],
        time = signal[, 2]
      )
    )
  })

  data$reset()

  if(length(annots)) {
    annots <- data.table::rbindlist(annots)
    if(nrow(annots) == 0) {
      annots <- NULL
    }
  } else {
    annots <- NULL
  }

  list(
    header = header,
    selection = list(
      channels = channels,
      begin = begin,
      end = end
    ),
    annotations = annots,
    results = result
  )

}

#' @title Read 'EDF' or 'BDF' data file
#' @param con file or connection to the data file
#' @inherit read_brainvis return params
#' @param begin,end begin and end of the data to read
#' @param convert whether to convert digital numbers to analog signals; default
#' is \code{TRUE}
#' @examples
#'
#' # ---- EDF/BDF(+) ---------------------------------------------------------
#'
#' # Run `ieegio_sample_data("edfPlusD.edf")` to download sample data
#'
#' # Tun example if the sample data exists
#' if(ieegio_sample_data("edfPlusD.edf", test = TRUE)) {
#'
#'   edf_path <- ieegio_sample_data("edfPlusD.edf")
#'
#'   data <- read_edf(edf_path)
#'
#'   data$get_header()
#'
#'   data$get_annotations()
#'
#'   data$get_channel_table()
#'
#'   channel <- data$get_channel(1)
#'
#'   plot(
#'     channel$time,
#'     channel$value,
#'     type = "l",
#'     main = channel$info$Label,
#'     xlab = "Time",
#'     ylab = channel$info$Unit
#'   )
#'
#' }
#'
#'
#' @export
read_edf <- function(
    con, extract_path = getOption("ieegio.extract_path", NULL),
    header_only = FALSE, cache_ok = TRUE,
    begin = 0, end = Inf, convert = TRUE, verbose = TRUE) {

  file_digest <- NULL
  con_path <- NULL
  if(!inherits(con, "connection")) {
    file_assert(con, dir_ok = FALSE)
    con_path <- con
    file_digest <- digest::digest(
      list(
        file = digest::digest(object = con, file = TRUE),
        time_range = as.double(c(begin, end))
      )
    )
    con <- file(con, "rb")
    on.exit({ close(con) })
  }
  header <- internal_read_edf_header(con)

  if( header_only ) {
    return(header)
  }

  temporary <- FALSE

  if( length(extract_path) != 1 || is.na(extract_path) ) {
    if(length(con_path)) {
      extract_dir <- sprintf("%s.cache", path_ext_remove(con_path))
    } else {
      extract_dir <- tempdir()
    }
  } else {
    extract_dir <- extract_path
  }

  if(length(file_digest)) {
    extract_path <- file_path(extract_dir, sprintf("ieegio_ebdf_digest_%s", file_digest))
  } else {
    extract_path <- tempfile(pattern = "ieegio_ebdf_", tmpdir = extract_dir)
    if(file_exists(extract_path)) {
      file_delete(extract_path, use_base_r = TRUE)
    }
    temporary <- TRUE
  }

  if(file_exists(extract_path)) {
    if( cache_ok ) {

      if( verbose ) {
        cat("Found existing cache. Trying to reuse the cache...\n")
      }

      tryCatch(
        {
          re <- EBDFCache$new(extract_path)
          if( re$valid ) {
            return(re)
          }
        },
        error = function(...) {}
      )

      if( verbose ) {
        cat("Existing cache is invalid. Re-generate cache\n")
      }

    }

    if( verbose ) {
      cat("Removing existing cache...\n")
    }

    file_delete(extract_path, use_base_r = TRUE)
  }

  extract_path <- dir_create(extract_path)


  data <- internal_read_edf_signal(
    con = con,
    begin = begin,
    end = end,
    convert = convert,
    header = header,
    channels = NULL
  )

  channel_data <- lapply(seq_along(data$selection$channels), function(ii) {
    chn <- data$selection$channels[[ii]]
    filebase <- file_path(extract_path, sprintf("Ch%d", chn))
    channel_data <- data$results[[ii]]

    sel <- channel_data$time >= begin & channel_data$time < end
    signal_length <- sum(sel)

    farr <- filearray::filearray_load_or_create(
      filebase,
      dimension = c(signal_length, 2L),
      type = "float",
      partition_size = 2L,
      mode = "readwrite",
      symlink_ok = FALSE
    )
    farr[] <- cbind(channel_data$value[sel], channel_data$time[sel])
    farr$set_header(key = "source_header",
                    value = data$header,
                    save = FALSE)
    farr$set_header(key = "channel_info",
                    value = channel_data$info,
                    save = FALSE)
    farr$set_header(key = "time_selection",
                    value = data$selection[c("begin", "end")],
                    save = FALSE)
    dimnames(farr) <- list(NULL, Name = c(channel_data$info$Label, "Time"))
    farr$.mode <- "readonly"
    farr
  })

  names(channel_data) <- sprintf("Ch%d", data$selection$channels)

  if(is.data.frame(data$annotations)) {
    fst_path <- file_path(extract_path, "annot.fst")
    io_write_fst(x = data$annotations, con = fst_path)
  }

  header_path <- file_path(extract_path, "header.rds")
  saveRDS(object = data[c("header", "selection")], file = header_path)
  re <- EBDFCache$new(extract_path)
  re$temporary <- temporary
  re
}

#' @noRd
#' @param item a list, names include 'Annotation', 'Label', 'PhysicalMin',
#' 'PhysicalMax', 'SampleRate', are required; optionally, 'Unit',
#' 'TransducerType', 'Filter', 'Comment' are recommended
#' @param chan_num channel order, integer
#' @param record_duration record_duration duration in seconds of each
#' recording chunk
as_edf_channel_descriptor <- function(item, chan_num, record_duration) {
  item <- as.list(item)
  re <- list(Channel = chan_num)
  is_annot <- isTRUE(item$Annotation)
  if( length(item$Label) && item$Label == "EDF Annotations" ) {
    is_annot <- TRUE
  }
  if(is_annot) {
    re$Annotation <- TRUE
    re$Label <- "EDF Annotations"
    re$Unit <- item$Unit %||% ""
  } else {
    re$Annotation <- FALSE
    if(length(item$Label)) {
      re$Label <- item$Label
    } else {
      re$Label <- sprintf("Ch%d", chan_num)
    }
    re$Unit <- item$Unit %||% "uV"
  }

  re$TransducerType <- item$TransducerType %||% ""
  re$Filter <- item$Filter %||% ""

  # 32 B... maybe I can put their coordinates?
  re$Comment <- item$Comment %||% ""

  re$DigitalMin <- -32768L
  re$DigitalMax <- 32767L

  re$PhysicalMin <- as.numeric(item$PhysicalMin)
  if(!length(re$PhysicalMin)) {
    if(is_annot) {
      re$PhysicalMin <- -1
    } else {
      re$PhysicalMin <- -3000
    }
  }
  re$PhysicalMax <- as.numeric(item$PhysicalMax)
  if(!length(re$PhysicalMax)) {
    if(is_annot) {
      re$PhysicalMax <- 1
    } else {
      re$PhysicalMax <- 3000
    }
  }

  sample_per_record <- ceiling(item$SampleRate * record_duration)
  if(!length(sample_per_record)) {
    sample_per_record <- 1L
    re$SampleRate <- 1 / record_duration
  } else {
    re$SampleRate <- item$SampleRate
  }
  re$SamplesPerRecord <- sample_per_record

  re$DurationPerRecord <- sample_per_record / re$SampleRate

  re
}

as_edf_header <- function(
    channels, record_duration = NA,
    patient_id = "anomymous",
    recording_id = NULL, start_time = Sys.time()) {

  # validate
  channel_num <- unlist(lapply(channels, function(channel) {
    if(!inherits(channel, c('ieegio_edf_channel', 'ieegio_edf_empty', 'ieegio_edf_annotation'))) {
      stop("Invalid channel data: please use `as_edf_channel` to construct your channels!")
    }
    channel$info$Channel[[1]]
  }))
  n_channels <- max(channel_num)
  if(n_channels != length(channel_num)) {
    skipped_channels <- seq_len(n_channels)
    skipped_channels <- skipped_channels[!skipped_channels %in% channel_num]
    message(sprintf("Channels `%s` are skipped. To avoid ambiguity, they will be inserted as empty annotation channels", deparse_svec(skipped_channels)))
  } else {
    skipped_channels <- NULL
  }

  # correct `channels`
  channels <- lapply(seq_len(n_channels), function(ii) {
    sel <- channel_num == ii
    if(any(sel)) {
      return(channels[sel][[1]])
    }
    as_edf_channel(NULL, channel_num = ii)
  })

  # headers
  channel_table <- data.table::rbindlist(lapply(channels, function(channel) {
    is_annot <- !inherits(channel, 'ieegio_edf_channel')

    list(
      Channel = channel$info$Channel,
      Annotation = is_annot,
      SampleRate = c(channel$info$SampleRate, NA)[[1]],
      Label = ifelse(is_annot, "EDF Annotation", channel$info$Label),
      Unit = ifelse(is_annot, "", channel$info$Unit),
      TransducerType = c(channel$info$TransducerType, "")[[1]],
      Filter = c(channel$info$Filter, "")[[1]],
      Comment = c(channel$info$Comment, "")[[1]],
      PhysicalMin = ifelse(is_annot, NA, channel$info$PhysicalMin),
      PhysicalMax = ifelse(is_annot, NA, channel$info$PhysicalMax),
      DigitalMin = -32768L,
      DigitalMax = 32767L,
      Skip = inherits(channel, "ieegio_edf_empty"),
      TimePoints = ifelse(is_annot, NA, length(channel$value))
    )
  }), use.names = TRUE)

  if(all(channel_table$Annotation)) {
    stop("Currently ieegio does not support when all channels are annotations. Please file a feature request.")
  }

  # Guess record_duration
  srates <- channel_table$SampleRate
  srates <- srates[!is.na(srates)]
  if(is.na(record_duration)) {
    for(record_duration in c(1, 2, 4, 5, 8, 10, 16, 25, 32, 50, 80, 100)) {
      tmp <- record_duration * srates
      if(all(tmp == round(tmp))) {
        break
      }
    }
    message(sprintf("Recording duration for each chunk is set to `%d sec`", record_duration))
  }

  samples_per_record <- vapply(channels, function(channel) {
    # 1 records (2 bytes)
    if( inherits(channel, "ieegio_edf_empty") ) { return(1L) }

    if(inherits(channel, 'ieegio_edf_channel')) {
      # signal
      re <- ceiling(channel$info$SampleRate * record_duration)
    } else {
      # annotation
      n_chunks <- ceiling(max(channel$time) / record_duration)
      annot_bytes <- vapply(seq_len(n_chunks), function(ii) {
        time_limit <- c(ii - 1, ii) * record_duration
        sel <- channel$time >= time_limit[[1]] & channel$time <= time_limit[[2]]
        str <- channel$tal_representation[sel]
        len <- length(unlist(str))
        as.integer(ceiling(len / 2))
      }, 0L)
      re <- max(annot_bytes)
    }
    as.integer(max(re, 1L))
  }, FUN.VALUE = 0L)

  channel_table$SamplesPerRecord <- samples_per_record

  channel_table$DurationPerRecord <- channel_table$SamplesPerRecord / channel_table$SampleRate

  channel_table$TotalDuration <- channel_table$TimePoints / channel_table$SampleRate
  channel_table$NRecords <- ceiling(channel_table$TotalDuration / record_duration)

  list(
    basic = list(
      # 1-8 is reserved
      # patient ID len=80
      patient = paste(patient_id, collapse = ""),

      # recording ID, len=80
      recording_id = paste(recording_id, collapse = ""),

      # Start time, len=16
      start_time = as.POSIXct(start_time),

      n_channels = n_channels,

      continuous_recording = TRUE,

      # duration for each packet/chunk
      record_duration = record_duration,

      # total number of chunks
      n_records = max(channel_table$NRecords, na.rm = TRUE)

    ),
    channel_table = channel_table,
    channels = channels
  )

}

#' @name write_edf
#' @title Write to 'EDF' format
#' @description
#' Currently supports continuous 'EDF+' format with annotations
#' @param con file path or binary connection
#' @param channels list of channel data, each element should be generated
#' from \code{as_edf_channel}
#' @param x channel signals or annotations; for signals, \code{x} is a
#' numeric vector; for annotations, \code{x} is a data frame with
#' \code{'timestamp'}, \code{'comments'}, and optionally \code{'duration'}
#' (case sensitive) columns
#' @param channel_num channel number, integer
#' @param sample_rate sampling frequency
#' @param label channel label, default is \code{'Ch'} followed by the channel
#' number for signal channels, or \code{"EDF Annotation"} for annotations
#' @param physical_min,physical_max range of the channel values when converting
#' from physical unit to digital; default is the range of \code{x}
#' @param unit physical unit or dimension; default is \code{'uV'}
#' @param is_annotation whether the channel is annotation
#' @param transducer_type transducer type
#' @param filter preliminary filters applied to the signals
#' @param comment additional comments (maximum 32 bytes)
#' @param patient_id patient identifier; default is \code{'anomymous'}
#' @param recording_id recording identifier
#' @param record_duration duration of each recording chunk: 'EDF' format slices
#' the data into equal-length chunks and writes the data (interleave channels)
#' to file; this is the duration for each chunk, not the entire recording length
#' @param start_time start time of the recording; see \code{\link{as.POSIXct}}
#' @returns \code{as_edf_channel} returns a channel wrapper (with metadata);
#' \code{write_edf} writes to the connection and returns nothing
#' @examples
#'
#'
#' signal <- sin(seq(0, 10, 0.01))
#'
#' channels <- list(
#'
#'   # signal
#'   as_edf_channel(channel_num = 1, signal,
#'                  sample_rate = 375.5),
#'
#'   as_edf_channel(channel_num = 2, signal,
#'                  sample_rate = 200),
#'
#'   # annotation
#'   as_edf_channel(channel_num = 3, data.frame(
#'     timestamp = c(0, 1, 2),
#'     comments = c("Start", "half to go", "Finish!")
#'   ))
#'
#' )
#'
#' # write to file
#' path <- tempfile(fileext = ".edf")
#' write_edf(con = path, channels = channels)
#'
#' edf <- read_edf(con = path, extract_path = tempdir())
#'
#' annot <- edf$get_annotations()
#' annot
#'
#'
#' ch1 <- edf$get_channel(1)
#'
#' # around 1e-5 due to digitization
#' range(ch1$value[seq_along(signal)] - signal)
#'
#' ch2 <- edf$get_channel(2)
#' range(ch2$value[seq_along(signal)] - signal)
#'
#'
#' plot(ch1$time, ch1$value, type = "l",
#'      main = "Toy-example")
#' lines(ch2$time, ch2$value, col = "red")
#' abline(v = annot$timestamp, col = "gray", lty = 2)
#'
#' edf$delete()
#' unlink(path)
#'
NULL

#' @rdname write_edf
#' @export
as_edf_channel <- function(
    x, channel_num, sample_rate, label = sprintf("Ch%d", channel_num),
    physical_min = NA, physical_max = NA, is_annotation = NA,
    transducer_type = "", unit = "uV", filter = "", comment = "") {
  channel_num <- as.integer(channel_num)[[1]]
  stopifnot(!is.na(channel_num) && channel_num > 0)
  label <- paste(label, collapse = "")

  is_annotation <- as.logical(is_annotation)[[1]]
  if(is.na(is_annotation)) {
    is_annotation <- FALSE
    if(!length(x) || all(c("timestamp", "comments") %in% names(x))) {
      is_annotation <- TRUE
    } else if (is.numeric(x)) {
      is_annotation <- FALSE
    }
  }


  if(is_annotation) {
    comment <- label
    # x needs to be a data.frame of timestamp, duration (optional), and comments
    if(!length(x)) {
      return(structure(
        list(
          type = "E/BDF(+)",
          info = list(
            Channel = channel_num,
            Label = "EDF Annotations",
            Annotation = is_annotation
          )
        ),
        class = 'ieegio_edf_empty'
      ))
    }
    x <- as.data.frame(x)

    stopifnot(all(c("timestamp", "comments") %in% names(x)))
    time <- as.double(x$timestamp)
    sign <- c("", "+")[(time >= 0) + 1]
    time_str <- sprintf("%s%.6f", sign, time)
    time_str <- gsub("[0]+$", "0", time_str)
    time_str <- iconv(time_str, to = "UTF-8")

    comments <- trimws(x$comments)
    comments <- iconv(comments, to = "UTF-8")

    if(!length(x$duration)) {
      x$duration <- NA
    }
    duration_str <- sprintf("%.6f", x$duration)
    duration_str <- gsub("[0]+$", "0", duration_str)
    duration_str[is.na(x$duration)] <- NA
    duration_str <- iconv(duration_str, to = "UTF-8")

    # "+%.6f %.6f %s\0", onset, duration (if not NA), comment
    event_str <- lapply(seq_len(nrow(x)), function(ii) {
      time_int <- charToRaw(time_str[[ii]])
      duration <- duration_str[[ii]]
      comment_int <- charToRaw(comments[[ii]])
      if(is.na(duration)) {
        duration_int <- as.raw(20L)
      } else {
        duration_int <- c(as.raw(21L), charToRaw(duration))
      }
      c(time_int, duration_int, as.raw(20L), comment_int, as.raw(20L), as.raw(0L))
    })

    structure(
      list(
        type = "E/BDF(+)",
        info = list(
          Channel = channel_num,
          Label = "EDF Annotations",
          Annotation = is_annotation,
          TransducerType = paste(transducer_type, collapse = "")
        ),
        time = time,
        duration = x$duration,
        value = x$comments,
        tal_representation = event_str
      ),
      class = "ieegio_edf_annotation"
    )

  } else {
    x <- as.double(x)
    sample_rate <- as.numeric(sample_rate)[[1]]
    stopifnot(!is.na(sample_rate))

    if(!nzchar(label)) {
      label <- sprintf("Ch%d", channel_num)
    }
    if(is.na(physical_min)) {
      rg <- range(x, na.rm = TRUE)
      physical_min <- rg[[1]]
      physical_max <- rg[[2]]
    }

    structure(
      list(
        type = "E/BDF(+)",
        continuous = TRUE,
        info = list(
          Channel = channel_num,
          Label = label,
          Annotation = is_annotation,
          TransducerType = paste(transducer_type, collapse = ""),
          Unit = unit,
          Filter = filter,
          # SamplesPerRecord,
          SampleRate = sample_rate,
          DigitalMin = -32768,
          DigitalMax = 32767,
          PhysicalMin = physical_min,
          PhysicalMax = physical_max,
          Comment = comment
        ),
        time = seq(0.0, length.out = length(x), by = 1 / sample_rate),
        value = x
      ),
      class = c("ieegio_edf_channel", "ieegio_get_channel")
    )
  }

}


#' @rdname write_edf
#' @export
write_edf <- function(
    channels, con,
    patient_id = "anomymous",
    recording_id = NULL,
    record_duration = NA,
    physical_min = NA, physical_max = NA,
    start_time = Sys.time()) {

  # DIPSAUS DEBUG START
  # f <- tempfile(fileext = ".edf")
  # con <- f
  # edf <- read_edf('/Users/dipterix/rave_data/bids_dir/ds005574/sub-02/ieeg/sub-02_task-podcast_ieeg.edf')
  # annot <- as_edf_channel(x = edf$annotations, channel_num = 114)
  # ii <- 1
  #
  # channels <- lapply(1:10, function(ii) {
  #   if(ii == 6) { return(as_edf_channel(NULL, ii)) }
  #   if(ii == 9) {
  #     annot <- as_edf_channel(x = edf$annotations, channel_num = ii)
  #     return(annot)
  #   }
  #   tmp <- edf$get_channel(ii)
  #   chan <- as_edf_channel(x = tmp$value, channel_num = ii, sample_rate = tmp$info$SampleRate, is_annotation = FALSE)
  #   chan
  # })
  # patient_id = "anomymous"
  # record_duration = NA
  # recording_id = NULL
  # start_time = Sys.time()
  # physical_min = NA; physical_max = NA

  recording_id <- paste(recording_id, collapse = "")
  if(is.na(recording_id) || !nzchar(recording_id)) {
    recording_id <- sprintf('{"generator":"ieegio","timestamp":"%s"}', Sys.time())
  }

  hdr <- as_edf_header(
    channels = drop_nulls(channels),
    record_duration = record_duration,
    patient_id = patient_id,
    recording_id = recording_id,
    start_time = start_time
  )
  channels <- hdr$channels
  hdr$channel_table$SampleRate[is.na(hdr$channel_table$SampleRate)] <- 1L
  if(!is.na(physical_min)) {
    hdr$channel_table$PhysicalMin <- physical_min
  }
  hdr$channel_table$PhysicalMin[hdr$channel_table$Annotation] <- -1
  if(!is.na(physical_max)) {
    hdr$channel_table$PhysicalMax <- physical_max
  }
  hdr$channel_table$PhysicalMax[hdr$channel_table$Annotation] <- 1

  n_records <- hdr$basic$n_records
  n_channels <- hdr$basic$n_channels
  record_duration <- hdr$basic$record_duration
  channel_table <- hdr$channel_table

  if(!inherits(con, "connection")) {
    con <- file(con, "wb")
    on.exit({ close(con) })
  }
  internal_write_edf_header(hdr, con)

  # --- Write data records (16-bit LE interleaved) ---
  lapply(seq_len(n_records), function(rec_i) {

    lapply(seq_len(n_channels), function(chn_i) {
      # chn_i=rec_i= 1
      row <- hdr$channel_table[chn_i, ]
      is_annot <- row$Annotation
      srate <- row$SampleRate

      channel <- channels[[chn_i]]

      if( is_annot ) {
        record_start <- record_duration * (rec_i - 1)
        record_end <- record_start + record_duration
        sel <- channel$time >= record_start & channel$time < record_end
        event_raw <- unlist(channel$tal_representation[sel])

        byte_len <- row$SamplesPerRecord * 2L

        slice_data <- c(event_raw, rep(as.raw(0L), byte_len - length(event_raw)))
        # print(c(rawToChar(slice_data), length(slice_data)))

        writeBin(object = slice_data, con = con, useBytes = TRUE)
        # writeBin(object = as.integer(slice_data), con = con, size = 2L, endian = "little")
      } else {
        # This might be more accurate for faction timestamp
        record_start <- row$DurationPerRecord * (rec_i - 1)
        pmin <- row$PhysicalMin
        pmax <- row$PhysicalMax
        dmin <- row$DigitalMin
        dmax <- row$DigitalMax
        slope <- (dmax - dmin) / (pmax - pmin)

        slice_data <- channel$value[round(record_start * srate) + seq_len(row$SamplesPerRecord)]
        slice_data <- round((slice_data - pmin) * slope + dmin)
        slice_data[slice_data < dmin] <- dmin
        slice_data[slice_data > dmax] <- dmax
        slice_data[is.na(slice_data)] <- 0L

        writeBin(object = as.integer(slice_data), con = con, size = 2L, endian = "little")
      }
      return()
    })
    return()
  })

  invisible()
}

# con <- "~/Downloads/NoisyWords_Run1_ded991f4-c126-45ad-8370-35132912c898.edf"
# DIPSAUS DEBUG START
# hdr <- edfReader::readEdfHeader(con)
# system.time({ data <- read_edf(con) }) |> print()
# system.time({ expect <- edfReader::readEdfSignals(hdr) }) |> print()

# data$temporary=T
# rm(data)
# profvis::profvis({ data <- read_edf(con) })

# par(mfrow = c(3,4), mar = c(0,0,4.1,0))
#
# for(ii in 1:11) {
#   x <- data$get_channel(ii)
#   label <- x$info$Label
#
#   plot(x$time, x$value, type = "l", main = label, lwd = 1.5)#, xlim = c(0, 2))
#
#   s <- expect[[label]]$signal
#   srate <- expect[[label]]$sRate
#   lines(seq_along(s) / srate, s, col = "red", lwd = 1)
#
#   print(range(x$value - s[!is.na(s)]))
# }
