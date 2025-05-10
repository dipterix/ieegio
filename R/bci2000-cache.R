
BCI2000Cache <- R6::R6Class(
  classname = "ieegio_BCI2000Cache",
  inherit = SignalDataCache,
  cloneable = FALSE,
  private = list(
    .filearray = NULL,
    assert_valid = function() {
      if(!self$valid) {
        stop("`BCI2000Cache`: the loaded cache was removed/changed. ",
             "Please recache the data.")
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
      nchannels <- dim(x)[[2]]

      channel_table <- data.frame(
        Channel = seq_len(nchannels),
        Label = sprintf("Ch%d", seq_len(nchannels)),
        SampleRate = x$get_header("sample_rate")
      )

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
        stop("`BCI2000Cache`: Please load one channel at a time. ",
             "If two channels share the same label name, ",
             "please do not get channel by label. Instead, ",
             "specify the channel order explicitly.")
      }
      if(!isTRUE(chan %in% seq_len(nrow(self$channel_table)))) {
        stop("Channel not found or not loaded: ", chan)
      }
      if(is.na(begin)) { begin <- 0 }
      if(is.na(end)) { end <- Inf }

      arr <- private$.filearray
      signals <- subset(arr,
                        Time ~ Time >= begin &
                          Time < end,
                        ChannelOrder ~ ChannelOrder == x,
                        drop = FALSE)
      time <- as.numeric(rownames(signals))
      dimnames(signals) <- NULL
      signals <- drop(signals)

      info <- self$channel_table[x, ]
      info <- list(
        Channel = info$Channel,
        Label = info$Label,
        Unit = c(info$Unit, "n/a")[[1]],
        SampleRate = info$SampleRate
      )

      structure(
        class = c("ieegio_bci2000_channel", "ieegio_get_channel"),
        list(
          type = "BCI2000",
          info = info,
          continuous = TRUE,
          time = time,
          value = signals
        )
      )
    },

    format = function(...) {
      smry <- strsplit(paste(collapse = "\n", self$header$summary), "\n")[[1]]
      smry[[1]] <- "<ieegio::BCI2000Cache>"
      smry
    },

    delete = function() {
      path <- arr$.filebase
      if(length(path) == 1 && file_exists(path)) {
        file_delete(path, use_base_r = TRUE)
      }
    }
  )
)

