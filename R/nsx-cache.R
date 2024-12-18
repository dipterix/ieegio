NSXCache <- R6::R6Class(
  classname = "ieegio_NSXCache",
  inherit = SignalDataCache,
  private = list(
    .nsp = NULL,
    assert_valid = function() {
      if(!self$valid) {
        stop("`NSXCache`: the loaded cache was removed/changed. Please recache the data.")
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
      nsp <- private$.nsp
      !is.null(nsp) && file_exists(dirname(nsp$nev$prefix))
    },
    header = function() {
      private$.nsp
    }
  ),
  public = list(
    temporary = FALSE,
    channel_table = NULL,

    initialize = function(nsp) {
      private$.nsp <- nsp
      rds <- sprintf("%s_channels.rds", nsp$nev$prefix)
      self$channel_table <- readRDS(rds)
    },

    get_channel = function(x, begin = NA, end = NA) {
      private$assert_valid()

      if(is.character(x)) {
        chan <- self$channel_table$original_channel[self$channel_table$name == x]
        chan <- unique(chan)
      } else {
        chan <- as.integer(x)
      }

      if(length(chan) != 1) {
        stop("`EBDFCache`: Please load one channel at a time. ",
             "If two channels share the same label name, ",
             "please do not get channel by label. Instead, ",
             "specify the channel index explicitly.")
      }
      if(!isTRUE(chan %in% self$channel_table$original_channel)) {
        stop("Channel not found or not loaded: ", chan)
      }
      if(is.na(begin)) { begin <- 0 }
      if(is.na(end)) { end <- Inf }

      nsp <- private$.nsp
      channel_data <- readNSx::get_channel(channel_id = chan, x = nsp)

      # list(Channel = 1L, Label = "LA1", Annotation = FALSE, TransducerType = "",
      #      Unit = "uV", Filter = "", SamplesPerRecord = 128, SampleRate = 1024,
      #      Slope = -0.265842679484245, Intercept = -0.132921339742097,
      #      Comment = "")

      sample_rate <- channel_data$channel_info$sample_rate_signal
      if(length(channel_data$channel_detail) == 1) {
        continuous <- TRUE
      } else {
        continuous <- FALSE
      }
      data <- lapply(channel_data$channel_detail, function(part) {
        # part <- channel_data$channel_detail[[1]]
        time_start <- part$meta$relative_time

        slen <- length(part$data)
        time <- seq(part$meta$relative_time, length.out = slen, by = 1 / sample_rate)
        sel <- time >= begin & time < end

        if(!any(sel)) { return() }

        # already converted
        signal <- part$data[sel]

        cbind(signal, time)
      })

      data <- do.call("rbind", data)

      structure(
        class = c("ieegio_nsx_channel", "ieegio_get_channel"),
        list(
          type = sprintf("Blackrock %s", paste(nsp$nev$header_basic$file_spec, collapse = ".")),
          info = list(
            Channel = chan,
            Label = channel_data$channel_info$electrode_label,
            Unit = channel_data$channel_info$units,
            SampleRate = channel_data$channel_info$sample_rate_signal
          ),
          continuous = continuous,
          time = data[, 2],
          value = data[, 1]
        )
      )
    },

    format = function(...) {
      nsp <- private$.nsp
      all_chans <- deparse_svec(self$channel_table$original_channel)

      s <- paste(format(nsp$nev), collapse = "\n")

      s <- sprintf("<ieegio::NSXCache> %s\nCached channels: %s",s, all_chans)
    },

    delete = function() {
      nsp <- private$.nsp
      path <- dirname(nsp$nev$prefix)
      if(length(path) == 1 && file_exists(path)) {
        file_delete(path, use_base_r = TRUE)
      }
    }
  )
)

