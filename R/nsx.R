# DIPSAUS DEBUG START
# file <- "/Users/dipterix/rave_data/raw_dir/PAV037/BLOCK021_mTurkWords_run2/PAV037_Datafile_021.nev"
# file <- "/Volumes/PennRAID/BeauchampServe/rave_data/raw/PAV045/BLOCK019_mTurkWords_run4/PAV045_Datafile_019.nev"
# extract_path <- NULL

internal_read_nsx <- function(file, extract_path = NULL, header_only = FALSE, cache_ok = TRUE, include_waveform = FALSE, verbose = TRUE) {
  if(length(extract_path) != 1 || is.na(extract_path)) {
    extract_path <- sprintf("%s.cache", path_ext_remove(file))
  }
  prefix <- file.path(extract_path, "data")

  if( include_waveform ) {
    exclude_events <- NULL
  } else {
    exclude_events <- "spike"
  }

  flag_path <- file_path(extract_path, "conversion_flags.txt")

  if(file_exists(flag_path) && cache_ok) {

    tryCatch({
      config <- as.list(io_read_yaml(flag_path))
      use_cache <- FALSE
      if( all(config$exclude_events %in% exclude_events) ) {
        if( isTRUE(config$header_converted) && isTRUE(config$data_converted) ) {
          use_cache <- TRUE
        } else if ( header_only && isTRUE(config$header_converted) ) {
          use_cache <- TRUE
        }
      }
      if( use_cache ) {
        if( verbose ) {
          cat("Found existing cache. Trying to reuse the cache...\n")
        }
        nsp <- readNSx::get_nsp(prefix)
        return(nsp)
      }
    }, error = function(e) {})
  }

  if( header_only ) {
    nsp <- readNSx::import_nsp(
      file,
      prefix = prefix,
      partition_prefix = "_part",
      exclude_nsx = 1:9,
      exclude_events = exclude_events,
      verbose = verbose
    )
    config <- list(
      header_converted = TRUE,
      data_converted = FALSE
    )
  } else {
    nsp <- readNSx::import_nsp(
      file,
      prefix = prefix,
      partition_prefix = "_part",
      exclude_nsx = NULL,
      exclude_events = exclude_events,
      verbose = verbose
    )
    config <- list(
      header_converted = TRUE,
      data_converted = TRUE
    )
  }

  config$exclude_events <- exclude_events
  config$comment <- "Please remove this file if you want to overwrite the cache."

  io_write_yaml(
    config, con = flag_path
  )

  nsp
}

# nsp <- internal_read_nsx(file)

#' @title Read ('BlackRock') 'NEV' 'NSx' data
#' @inherit read_brainvis return params
#' @param include_waveform whether to include 'waveform' data (usually for
#' online spike sorting); default is \code{FALSE}
#'
#' @export
read_nsx <- function(
    file, extract_path = getOption("ieegio.extract_path", NULL),
    header_only = FALSE, cache_ok = TRUE, include_waveform = FALSE, verbose = TRUE) {

  nsp <- internal_read_nsx(file = file, extract_path = extract_path, header_only = header_only,
                           cache_ok = cache_ok, include_waveform = include_waveform, verbose = verbose)

  NSXCache$new(nsp)
}
