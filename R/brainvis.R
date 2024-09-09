# DIPSAUS DEBUG START
# file <- "/Users/dipterix/PennNeurosurgery Dropbox/Zhengjia Wang/RAVE/Samples/raw/Walker/Block001/Bursts.vhdr"
# hdr <- internal_read_brainvis_header(file)
# infer_names = FALSE

impl_read_brainvis_ini <- function (path, infer_names = FALSE,
                                    tidy_names = TRUE) {
  regexp_section <- "^\\s*\\[\\s*(.+?)\\s*]"
  regexp_keyval <- "^\\s*[^=]+=.+"
  regexp_comment <- "^\\s*[;#]"
  con <- file(path, open = "r")
  on.exit(close(con))

  re <- fastmap::fastmap()

  ensure_section <- function(name) {
    if(!re$has(name)) {
      re$set(name, list(
        data = fastmap::fastqueue(),
        comments = fastmap::fastqueue()
      ))
    }
    re$get(name)
  }

  add_comment <- function(x, section) {
    section$comments$add(x)
  }
  add_entry <- function(x, section) {
    section$data$add(x)
  }

  current_section <- ensure_section("Internal Comments")

  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (!length(line)) {
      break
    }
    if (grepl(regexp_comment, line)) {
      add_comment(line, current_section)
      next
    }
    if (grepl(regexp_section, line)) {
      matches <- regexec(regexp_section, line)
      section_name <- regmatches(line, matches)[[1]][2]
      current_section <- ensure_section(section_name)
    }
    if (grepl(regexp_keyval, line)) {
      s <- strsplit(line, "=")[[1]]
      key <- trimws(s[[1]], which = "both")
      value <- trimws(paste0(s[-1], collapse = "="), which = "both")
      add_entry(list(key = key, value = value), current_section)
    }
  }

  re <- re$as_list()

  nms <- names(re)
  if( tidy_names ) {
    nms <- stringr::str_replace_all(nms, " ", "")
  }

  re <- structure(
    names = nms,
    lapply(re, function(li) {
      comments <- unlist(li$comments$as_list())
      if(length(comments)) {
        comments <- trimws(gsub("^[ ]{0,};", "", comments))
      } else {
        comments <- NULL
      }

      size <- li$data$size()

      if(!size) { return(comments) }

      data <- data.table::rbindlist(li$data$as_list())
      if( infer_names ) {
        tmp <- paste(comments, collapse = " ")

        entry_info <- stringr::str_extract(tmp, "<([^=]+)>=(<[^<>]+>[,; ]+){0,}")[[1]]
        if(length(entry_info) && !is.na(entry_info)) {

          entry_info <- stringr::str_split(entry_info[[1]], ">[ ]{0,}=[ ]{0,}<")[[1]]
          if(length(entry_info) >= 2) {

            key_name <- stringr::str_remove_all(entry_info[[1]], "[<>,;'\"]+")
            key_name <- stringr::str_to_title(trimws(key_name))
            key_name <- stringr::str_replace_all(key_name, "[^a-zA-Z0-9]+", "")

            # unwrap
            value_names <- stringr::str_split(entry_info[[2]], "[<>,;]+")[[1]]
            value_names <- stringr::str_trim(value_names)

            # clean
            value_names <- value_names[!value_names %in% c("", "=")]

            # remove quotes
            value_names <- gsub("[\"']", "", value_names)

            # remove brackets
            value_names <- stringr::str_extract(value_names, "^[a-zA-Z0-9_ -]+")

            # collapse
            value_names <- stringr::str_to_title(value_names)
            value_names <- stringr::str_replace_all(value_names, "[^a-zA-Z0-9]+", "")

            # split value
            values <- stringr::str_split(data$value, ",", simplify = TRUE)

            nc <- ncol(values)

            if( nc < length(value_names) ) {
              value_names <- value_names[seq_len(nc)]
            } else if (nc > length(value_names)) {
              value_names2 <- sprintf("Unnamed%d", seq_len(nc))
              value_names2[seq_along(value_names)] <- value_names
              value_names <- value_names2
            }

            values <- cbind(data$key, values)
            colnames(values) <- c(key_name, value_names)

            data <- data.table::as.data.table(values)
          }
        }
      }

      if(length(comments)) {
        attr(data, "comments") <- comments
      }

      data
    })
  )

  drop_nulls(re)
}


impl_read_vmrk <- function (file) {
  items <- readLines(file)
  pattern <- stringr::regex("^Mk([0-9]*)?=(.*)$", dotall = TRUE)
  sel <- stringr::str_detect(items, pattern)
  # sel <- grepl("^Mk([0-9]*)?=(.*)$", items)
  comments <- items[!sel]
  items <- items[sel]
  if (!length(items)) { return() }
  tmp <- paste(comments, collapse = "")
  marker_info <- stringr::str_extract(tmp, "Mk<([^=]+)>=(<[^<>]+>[,; ]+){0,}")
  marker_info <- stringr::str_split(marker_info, "([<>,;]+)|(^Mk)")[[1]]
  marker_info <- stringr::str_trim(marker_info)
  marker_info <- marker_info[!marker_info %in% c("", "=")]
  markers <- stringr::str_match(items, pattern = pattern)
  markers <- cbind(markers[, 2], stringr::str_split(markers[, 3], ",", simplify = TRUE))
  nc <- ncol(markers)
  if(nc > length(marker_info)) {
    marker_info2 <- sprintf("Unnamed%d", seq_len(nc))
    marker_info2[seq_along(marker_info)] <- marker_info
    marker_info <- marker_info2
  } else if (nc < length(marker_info)) {
    marker_info <- marker_info[seq_len(nc)]
  }
  # turn marker_info into table title
  marker_cnames <- stringr::str_extract(marker_info, "^[a-zA-Z0-9_ -]+")
  marker_cnames <- stringr::str_to_title(marker_cnames)
  marker_cnames <- stringr::str_replace_all(marker_cnames, "[^a-zA-Z0-9]+", "")
  colnames(markers) <- marker_cnames
  markers <- data.table::data.table(markers)

  for(nm in marker_cnames) {
    if(
      all(grepl('^([0-9\\-][0-9]{1,}|[0-9]{1,})$', markers[[nm]]))
    ) {
      if( all(markers[[nm]] <= .Machine$integer.max) ) {
        markers[[nm]] <- as.integer(markers[[nm]])
      } else if (all(nchar(markers[[nm]]) < 16)) {
        markers[[nm]] <- as.numeric(markers[[nm]])
      }
    }
  }

  list(comments = comments, header = marker_info, content = markers)
}

internal_read_brainvis_header <- function (file)
{
  # needs to be .vhdr
  file <- path_abs(file)
  vhdr <- impl_read_brainvis_ini(file, infer_names = FALSE, tidy_names = TRUE)

  common <- structure(
    names = vhdr$CommonInfos$key,
    as.list(vhdr$CommonInfos$value)
  )

  common$NumberOfChannels <- as.integer(common$NumberOfChannels)
  common$SamplingInterval <- as.numeric(common$SamplingInterval)
  common$SampleRate <- 1e+06 / as.numeric(common$SamplingInterval)

  channel_comments <- attr(vhdr$ChannelInfos, "comments")
  channel_names <- vhdr$ChannelInfos$key
  channel_info <- t(simplify2array(strsplit(unlist(vhdr$ChannelInfos$value), ",", fixed = TRUE)))
  if (ncol(channel_info) == 3) {
    colnames(channel_info) <- c("Label", "Reference", "Resolution")
    channel_info <- data.table::data.table(channel_info)
    channel_info$Unit <- "uV"
  } else {
    colnames(channel_info) <- c("Label", "Reference", "Resolution", "Unit")
    channel_info <- data.table::data.table(channel_info)
  }
  sel <- channel_info$Reference == ""
  channel_info$Reference[sel] <- NA
  channel_info$Resolution <- as.numeric(channel_info$Resolution)
  channel_info$SampleRate <- common$SampleRate
  channel_info$ChannelString <- channel_names

  # remove "\1" and other none-ascii
  channel_info$Label <- gsub("[^a-zA-Z0-9]+", "-", trimws(channel_info$Label))

  vhdr$CommonInfos <- common
  vhdr$ChannelInfos <- channel_info
  vhdr$ChannelComments <- channel_comments
  # vhdr$Annotation <- markers

  vhdr$BinaryInfos <- structure(
    names = vhdr$BinaryInfos$key,
    as.list(vhdr$BinaryInfos$value)
  )

  vhdr
}

# DIPSAUS DEBUG START
# file <- "/Users/dipterix/PennNeurosurgery Dropbox/Zhengjia Wang/RAVE/Samples/raw/Walker/Block001/Bursts.vmrk"
internal_read_brainvis_annotation <- function(file) {
  markers <- NULL
  if(length(file) == 1 && !is.na(file) && file_exists(file)) {
    markers <- impl_read_brainvis_ini(file, infer_names = TRUE, tidy_names = TRUE)
    if(all(grepl("^Mk[0-9]+$", markers$MarkerInfos[[1]], ignore.case = TRUE))) {
      markers$MarkerInfos[[1]] <- as.integer(gsub("Mk", "", markers$MarkerInfos[[1]], ignore.case = TRUE))
    }
  }
  markers
}

internal_read_brainvis_data <- function(data_file, header) {
  # DIPSAUS DEBUG START
  # data_file <- "/Users/dipterix/PennNeurosurgery Dropbox/Zhengjia Wang/RAVE/Samples/bids/FragilityEEGDataset/sub-jh103/ses-presurgery/ieeg/sub-jh103_ses-presurgery_task-ictal_acq-ecog_run-01_ieeg.eeg"
  # header_file <- "/Users/dipterix/PennNeurosurgery Dropbox/Zhengjia Wang/RAVE/Samples/bids/FragilityEEGDataset/sub-jh103/ses-presurgery/ieeg/sub-jh103_ses-presurgery_task-ictal_acq-ecog_run-01_ieeg.vhdr"
  # header <- internal_read_brainvis_header(header_file)

  nchannels <- header$CommonInfos$NumberOfChannels

  resolution <- as.numeric(header$ChannelInfos$Resolution)
  if (length(resolution) != nchannels) {
    stop("Number of channels does not match with data read from the file.")
  }

  binary <- toupper(header$CommonInfos$DataFormat) == "BINARY"
  orientation <- toupper(header$CommonInfos$DataOrientation)
  multiplexed <- startsWith(orientation, "MULTI")
  if (binary) {

    binary_format <- toupper(header$BinaryInfos$BinaryFormat)
    # e.g.: IEEE FLOAT 32
    binary_format <- strsplit(binary_format, "_", fixed = TRUE)[[1]]
    binary_format <- binary_format[length(binary_format) - c(0, 1)]

    # datapoint in byte
    len <- as.integer(binary_format[[1]]) / 8
    binary_format <- binary_format[[2]]
    if (binary_format == "FLOAT") {
      type <- "double"
    } else if (binary_format == "INT") {
      type <- "integer"
    } else {
      stop("Found binary format ", sQuote(binary_format),
           " in header file. Not supported yet. ", "Only IEEE_FLOAT_32 (float) and INT_16 (int) are supported.")
    }
    filesize <- file_size(data_file)
    s <- readBin(data_file,
                 what = type,
                 size = len,
                 n = as.numeric(filesize) / len)
    s <- matrix(s, nrow = nchannels, byrow = !multiplexed)
  } else {

    text_info <- header$ASCIIInfos
    SkipLines <- as.integer(text_info$SkipLines)
    SkipColumns <- as.integer(text_info$SkipColumns)
    DecimalSymbol <- text_info$DecimalSymbol
    s <- data.table::fread(file = data_file, skip = SkipLines,
                           dec = DecimalSymbol, sep = " ", header = FALSE)
    if (length(SkipColumns) > 0 && any(SkipColumns > 0)) {
      s <- s[, -SkipColumns, with = FALSE]
    }
    s <- as.matrix(s)
    dimnames(s) <- NULL

    if (multiplexed) {
      s <- t(s)
    }
  }

  if (!all(resolution == 1)) {
    s <- s * resolution
  }
  return(s)
}


impl_read_brainvis <- function(
    file, extract_path = NULL, header_only = FALSE, cache_ok = TRUE, verbose = TRUE) {
  # DIPSAUS DEBUG START
  # file <- "/Users/dipterix/PennNeurosurgery Dropbox/Zhengjia Wang/RAVE/Samples/bids/FragilityEEGDataset/sub-jh103/ses-presurgery/ieeg/sub-jh103_ses-presurgery_task-ictal_acq-ecog_run-01_ieeg.eeg"
  # extract_path <- NULL; header_only = FALSE; cache_ok=FALSE

  if(length(file) < 3) {
    file <- c(file, NA, NA)
  }
  header_file <- file[[1]]

  if(is.na(header_file)) {
    stop("`read_brainvis`: input `file` is invalid. The first element of `file` must not be NA")
  }

  ext <- fs::path_ext(tolower(header_file))
  if(!endsWith(ext, "hdr")) {
    # this is not a header file
    header_file <- sprintf("%s.vhdr", c(header_file, path_ext_remove(header_file)))
    header_file <- header_file[file_exists(header_file)]
    if(!length(header_file)) {
      stop("`read_brainvis`: Cannot find `vhdr` file with given path: ", file[[1]])
    }
  }
  header_file <- header_file[[1]]
  header_file <- normalizePath(header_file, mustWork = TRUE, winslash = "/")

  prefix <- path_ext_remove(header_file)
  root_dir <- dirname(header_file)

  if(length(extract_path) != 1 || is.na(extract_path)) {
    extract_path <- sprintf("%s.cache", prefix)
  }

  # get header
  header <- internal_read_brainvis_header(header_file)

  if(header_only) {
    return(header)
  }

  # get marker
  marker_file <- file[[2]]
  if(is.na(marker_file)) {
    marker_file2 <- c(
      file_path(root_dir, header$CommonInfos$MarkerFile),
      header$CommonInfos$MarkerFile,
      sprintf("%s.vmrk", prefix)
    )
    if(length(marker_file2)) {
      marker_file2 <- marker_file2[dirname(marker_file2) != "."]
    }
    marker_file <- marker_file2[file_exists(marker_file2)]
  }


  # get signal data
  data_file <- file[[3]]
  if(is.na(data_file)) {
    # binary <- isTRUE(toupper(header$CommonInfos$DataFormat) == "BINARY")
    data_file <- c(
      file_path(root_dir, header$CommonInfos$DataFile),
      header$CommonInfos$DataFile,
      sprintf(c("%s.eeg", "%s.dat"), prefix)
    )
    if(length(data_file)) {
      data_file <- data_file[dirname(data_file) != "."]
    }
    data_file <- data_file[file_exists(data_file)]
  }
  if(!length(data_file) || !file_exists(data_file[[1]])) {
    stop("`read_brainvis`: Cannot find signal data file from header: file ", sQuote(header$CommonInfos$DataFile), " is missing.")
  }

  # calculate digest
  sig <- c(
    digest::digest(object = header_file, file = TRUE),
    digest::digest(object = data_file[[1]], file = TRUE)
  )
  if(length(marker_file)) {
    sig <- c(sig, digest::digest(object = marker_file[[1]], file = TRUE))
  }
  sig <- digest::digest(sig)

  cache_path <- file_path(extract_path, sprintf("ieegio_brainvis_digest_%s", sig))

  if( file_exists(cache_path) ) {

    if( cache_ok ) {

      if( verbose ) {
        cat("Found existing cache. Trying to reuse the cache...\n")
      }

      tryCatch({

        re <- filearray::filearray_load(cache_path, mode = "readonly")

        if(isTRUE(re$get_header("ready")) && identical(re$get_header("source_digests"), sig)) {
          return(re)
        }

      }, error = function(...) {})

      if( verbose ) {
        cat("Existing cache is invalid. Re-generate cache\n")
      }

    }

    if( verbose ) {
      cat("Removing existing cache...\n")
    }

    file_delete(cache_path, use_base_r = TRUE)
  }

  # read annotation
  annotations <- NULL
  if(length(marker_file)) {
    try({
      annotations <- internal_read_brainvis_annotation(marker_file[[1]])
      # annot_nms <- names(annotations)
      # header$AnnotationInfos <- annotations[!annot_nms %in% "MarkerInfos"]
      header$MarkerComments <- attr(annotations$MarkerInfos, "comments")
    }, silent = FALSE)
  }
  signals <- internal_read_brainvis_data(data_file = data_file[[1]], header = header)
  signals <- t(signals)

  dm <- dim(signals)
  ntimepoints <- dm[[1]]
  nchannels <- dm[[2]]

  # set dimensions
  srate <- header$CommonInfos$SampleRate
  dnames <- list(
    Time = seq(0, length.out = ntimepoints, by = 1 / srate),
    ChannelOrder = seq_len(nchannels)
  )

  # cache
  arr <- filearray::filearray_load_or_create(
    filebase = cache_path,
    dimension = dm,
    type = "double",
    mode = "readwrite",
    symlink_ok = FALSE,
    initialize = FALSE,
    partition_size = 1L,
    source_digests = sig
  )

  arr[] <- signals

  # merge annotation header into header
  channel_table <- header$ChannelInfos
  header$ChannelInfos <- NULL
  arr$set_header("source_header", header, save = FALSE)
  arr$set_header("channel_info", channel_table, save = FALSE)

  dimnames(arr) <- dnames

  # also write annotations
  if(!is.null(annotations)) {
    io_write_fst(x = annotations$MarkerInfos, con = file_path(cache_path, "annot.fst"))
  }

  arr$set_header("ready", TRUE, save = TRUE)
  arr$.mode <- "readonly"

  arr
}


#' @rdname read_brainvis
#'
#' @examples
#'
#' if( ieegio_sample_data("brainvis.dat", test = TRUE) ) {
#'   # ensure the header and marker files are downloaded as well
#'   ieegio_sample_data("brainvis.vhdr")
#'   ieegio_sample_data("brainvis.dat")
#'   file <- ieegio_sample_data("brainvis.vmrk")
#'
#'   x <- read_brainvis(file)
#'   print(x)
#'
#'   x$get_header()
#'
#'   x$get_channel_table()
#'
#'   x$get_annotations()
#'
#'   channel <- x$get_channel(10)
#'
#'   plot(
#'     channel$time,
#'     channel$value,
#'     type = "l",
#'     main = channel$info$Label,
#'     xlab = "Time",
#'     ylab = channel$info$Unit
#'   )
#' }
#'
#' @export
read_brainvis <- function(
    file, extract_path = getOption("ieegio.extract_path", NULL),
    header_only = FALSE, cache_ok = TRUE, verbose = TRUE) {
  re <- impl_read_brainvis(file = file, extract_path = extract_path,
                            header_only = header_only, cache_ok, verbose = verbose)
  if(!header_only) {
    re <- BrainVisionCache$new(re)
  }

  re
}
