
BrainVisionCache <- R6::R6Class(
  classname = "ieegio_BrainVisionCache",
  inherit = SignalDataCache,
  private = list(
    .filearray = NULL,
    assert_valid = function() {
      if(!self$valid) {
        stop("`BrainVisionCache`: the loaded cache was removed/changed. Please recache the data.")
      }
    },
    finalize = function() {
      if(self$temporary) {
        self$delete()
      }
    }
  ),
  active = list(
    valid = function() {
      arr <- private$.filearray
      if( dir_exists(arr$.filebase) && isTRUE(arr$get_header("ready")) ) {
        return(TRUE)
      }
      FALSE
    },
    header = function() {
      private$.filearray$get_header("source_header")
    }
  ),
  public = list(
    temporary = FALSE,
    channel_table = NULL,
    annotations = NULL,

    initialize = function(x) {
      if(!inherits(x, "FileArray")) {
        x <- filearray::filearray_load(x, mode = "readonly")
      }
      private$.filearray <- x
      annot_path <- file_path(x$.filebase, "annot.fst")
      if(file_exists(annot_path)) {
        self$annotations <- io_read_fst(annot_path, method = "proxy")
      }

      channel_table <- x$get_header("channel_info")

      # try to get channel number
      chn <- gsub("ch", "", channel_table$ChannelString, ignore.case = TRUE)
      if(all(grepl("^[0-9]+$", chn))) {
        chn <- as.integer(chn)
      } else {
        chn <- seq_along(chn)
      }
      channel_table$Number <- chn
      channel_table$Order <- seq_along(chn)
      self$channel_table <- channel_table

    },

    get_channel = function(x, begin = NA, end = NA) {
      private$assert_valid()

      if(is.character(x)) {
        chan <- which(self$channel_table$Label == x)
      } else {
        chan <- x[[1]]
      }
      chan <- unique(chan)

      if(length(chan) != 1) {
        stop("`BrainVisionCache`: Please load one channel at a time. ",
             "If two channels share the same label name, ",
             "please do not get channel by label. Instead, ",
             "specify the channel order explicitly (see column 'Order' in channel table).")
      }
      if(!isTRUE(chan %in% seq_len(nrow(self$channel_table)))) {
        stop("Channel order not found or not loaded: ", chan)
      }
      if(is.na(begin)) { begin <- 0 }
      if(is.na(end)) { end <- Inf }

      arr <- private$.filearray
      signals <- subset(arr,
                   Time ~ Time >= begin &
                     Time <= end,
                   ChannelOrder ~ ChannelOrder == x,
                   drop = FALSE)
      time <- as.numeric(rownames(signals))
      dimnames(signals) <- NULL
      signals <- drop(signals)

      info <- self$channel_table[x, ]
      info <- list(
        Channel = info$Number,
        Label = info$Label,
        Unit = c(info$Unit, "n/a")[[1]],
        SampleRate = info$SampleRate
      )

      structure(
        class = c("ieegio_brainvis_channel", "ieegio_get_channel"),
        list(
          type = "BrainVision",
          info = info,
          continuous = TRUE,
          time = time,
          value = signals
        )
      )
    },

    format = function(...) {
      # arr <- private$.filearray
      chn <- self$channel_table$Number
      all_chans <- deparse_svec(chn)
      header <- self$header


      s <- c(
        "<ieegio::BrainVisionCache>",
        sprintf("  Cached channels : %s", all_chans),
        sprintf("  Original format : %s (n=%s)",
                paste(header$CommonInfos$DataFormat, collapse = ""),
                length(chn)),
        sprintf("  Sample rate     : %.1f", header$CommonInfos$SampleRate)
      )

      if(length(header$ChannelComments)) {
        s <- c(
          s,
          "  Channel comments:",
          paste("    ", header$ChannelComments, sep = "")
        )
      }

      if(length(header$MarkerComments)) {
        s <- c(
          s,
          "  Annotation comments:",
          paste("    ", header$MarkerComments, sep = "")
        )
      }
      s
    },

    delete = function() {
      path <- arr$.filebase
      if(length(path) == 1 && file_exists(path)) {
        file_delete(path, use_base_r = TRUE)
      }
    }
  )
)

