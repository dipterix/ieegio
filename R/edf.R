parse_edf_annot <- function(x) {
  # x <- period[[12]]
  idx <- which(x == 20)

  # onset + duration
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

  comments <- intToUtf8(x[-seq_len(idx[[1]])])

  if(length(idx) > 1) {
    comments <- strsplit(comments, "\024")[[1]]
  }
  list(timestamp = timestamp, duration = duration, comments = paste(comments, collapse = "\n"))
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

  # function to parse a recording
  sampling_bytes <- header$basic$sampling_bits / 8

  # read raw
  is_annotation <- header$channel_table$Annotation
  n <- header$channel_table$SamplesPerRecord
  n[is_annotation] <- n[is_annotation] * sampling_bytes
  size <- ifelse(is_annotation, 1L, sampling_bytes)

  if(any(is_annotation)) {
    annot_channel <- max(which(is_annotation))
    if( !annot_channel %in% channels ) {
      channels <- sort(c(channels, annot_channel))
    }
  } else {
    annot_channel <- integer()
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
        slice <- readBin(con, what = "integer", n = n[[ii]], size = size[[ii]], signed = TRUE, endian = "little")
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
  drop_nulls(lapply(seq_len(header$basic$n_records), function(ii_rec) {
    # trying to obtain the timestamp
    # estimate earliest start
    earliest_start <- env$previous_finish
    if( earliest_start > end ) { return() }

    record <- read_next_record()

    if( length(annot_channel) ) {
      annot <- record[[annot_channel]]
    } else {
      # continuous
      annot <- list(
        timestamp = record_duration * (ii_rec - 1),
        duration = NA_real_,
        comments = ""
      )
    }
    annot$order <- ii_rec

    slice_start <- annot$timestamp
    if(is.na(annot$duration)) {
      slice_duration <- record_duration
    } else {
      slice_duration <- annot$duration
    }
    slice_finish <- slice_start + slice_duration

    env$previous_finish <- slice_finish
    if( slice_finish < begin || slice_start > end ) { return() }

    annot$duration <- slice_duration

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
    annots$add(annot)
    return()
  }))
  annots <- annots$as_list()

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
      annot <- annots[[ii]]
      seg_start <- annot$timestamp

      # FIXME: what if seg_duration > record_duration?
      seg_duration <- annot$duration

      slen <- length(seg)
      if( slen > samples_per_record ) {
        seg <- seg[seq_len(samples_per_record)]
      } else if (slen < samples_per_record) {
        seg <- c(seg, rep(0.0, samples_per_record - slen))
      }
      time <- seg_start + time_0

      # cut time later
      # if( time[[1]] < begin || time[[length(time)]] > end ) {
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

    signal_length <- length(channel_data$value)
    farr <- filearray::filearray_load_or_create(
      filebase,
      dimension = c(signal_length, 2L),
      type = "double",
      partition_size = 2L,
      mode = "readwrite",
      symlink_ok = FALSE
    )
    farr[] <- cbind(channel_data$value, channel_data$time)
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
