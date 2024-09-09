
impl_read_bci2000_header <- function(file) {
  # DIPSAUS DEBUG START
  # file <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/ECoGAnalysisTraining/BCI2000Tools/mex/testdata.dat"
  # file <- "~/Dropbox (PennNeurosurgery)/RAVE/Samples/ECoGAnalysisTraining/data/bci2000_datafile.dat"
  # extract_path = NULL; header_only = FALSE; cache_ok = TRUE
  readNSx::read_bci2000_header(file)
}

impl_read_bci2000_data <- function(file, extract_path = NULL,
                                   cache_ok = TRUE, verbose = TRUE) {

  if(length(extract_path) != 1 || is.na(extract_path)) {
    prefix <- path_ext_remove(file)
    extract_path <- sprintf("%s.cache", prefix)
  }

  # calculate signature
  sig <- digest::digest(file, file = TRUE)
  cache_path <- file_path(extract_path, sprintf("ieegio_bci2000_digest_%s", sig))

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

  x <- readNSx::read_bci2000(file)

  # try to get sample rate
  sample_rate <- c(
    x$parameters$Source$SamplingRate$value,
    x$parameters$Source$`Signal Properties`$DataIOFilter$SamplingRate$value
  )

  if(!length(sample_rate)) {
    warning("Unable to find sample rate from Parameters > Source. Please contact the maintainer to report this issue. Using time-points instead")
    sample_rate <- 1
  } else {
    sample_rate <- sample_rate[[1]]
  }

  # header information
  source_header <- x
  source_header$states$data <- NULL
  source_header$signals <- NULL

  smry <- source_header$summary
  idx <- which(startsWith(smry, "[Enclosing Items]"))
  if(length(idx)) {
    idx <- idx[[length(idx)]]
    source_header$summary <- smry[seq_len(idx - 1)]
  }

  signals <- t(x$signals)

  nchannels <- ncol(signals)
  ntimepoints <- nrow(signals)

  arr <- filearray::filearray_load_or_create(
    filebase = cache_path,
    dimension = dim(signals),
    mode = "readwrite",
    symlink_ok = FALSE,
    type = "float",
    partition_size = 1L,
    initialize = FALSE
  )

  arr[] <- signals

  arr$set_header("source_digests", value = sig, save = FALSE)
  arr$set_header("source_header", value = source_header, save = FALSE)
  arr$set_header("sample_rate", value = sample_rate, save = FALSE)

  dnames <- list(
    Time = seq(0, length.out = ntimepoints, by = 1 / sample_rate),
    ChannelOrder = seq_len(nchannels)
  )
  dimnames(arr) <- dnames

  # save annotations
  annot <- data.table::data.table(t(x$states$data))
  io_write_fst(annot, file_path(cache_path, "annot.fst"))

  # add ready flag
  arr$set_header("ready", TRUE, save = TRUE)
  arr
}

#' @title Read 'BCI2000' data file
#' @param file file path to the data file
#'
#' @inherit read_brainvis return params
#' @examples
#'
#'
#' if( ieegio_sample_data("bci2k.dat", test = TRUE) ) {
#'   file <- ieegio_sample_data("bci2k.dat")
#'
#'   x <- read_bci2000(file)
#'   print(x)
#'
#'   channel <- x$get_channel(1)
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
#'
#' @export
read_bci2000 <- function(
    file, extract_path = getOption("ieegio.extract_path", NULL),
    header_only = FALSE, cache_ok = TRUE, verbose = TRUE) {
  if( header_only ) {
    return(impl_read_bci2000_header(file))
  }

  arr <- impl_read_bci2000_data(file, extract_path = extract_path, cache_ok = cache_ok, verbose = verbose)
  BCI2000Cache$new(arr)
}
