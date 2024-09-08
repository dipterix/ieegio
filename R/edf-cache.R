EBDFCache <- R6::R6Class(
  classname = "ieegio_EBDFCache",
  inherit = SignalDataCache,
  private = list(
    .root_path = character(0L),
    assert_valid = function() {
      if(!self$valid) {
        stop("`NSXCache`: the loaded cache was removed/changed. Please reload the E/BDF(+) data.")
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
      dir_exists(private$.root_path)
    }
  ),
  public = list(
    temporary = FALSE,
    header = list(),
    channel_table = NULL,
    selection = list(),
    annotations = NULL,

    initialize = function(path) {
      private$.root_path <- path
      # load header, selection, and annotations
      header_and_selection <- readRDS(file_path(path, "header.rds"))
      self$header <- header_and_selection$header$basic
      self$channel_table <- header_and_selection$header$channel_table
      self$selection <- header_and_selection$selection

      annot_path <- file_path(path, "annot.fst")
      if(file_exists(annot_path)) {
        self$annotations <- io_read_fst(annot_path, method = "proxy")
      }
    },

    get_channel = function(x, begin = NA, end = NA) {
      private$assert_valid()
      if(is.character(x)) {
        chan <- self$channel_table$Channel[self$channel_table$Label == x]
      } else {
        chan <- as.integer(x)
      }
      if(length(chan) != 1) {
        stop("`EBDFCache`: Please load one channel at a time. ",
             "If two channels share the same label name, ",
             "please do not get channel by label. Instead, ",
             "specify the channel index explicitly.")
      }
      if(!isTRUE(chan %in% self$selection$channels)) {
        stop("Channel not found or not loaded: ", chan)
      }
      if(is.na(begin)) { begin <- self$selection$begin }
      if(is.na(end)) { end <- self$selection$end }

      # load time
      annot_packet_start <- self$annotations$timestamp
      packet_id <- self$annotations$order[annot_packet_start >= begin & (annot_packet_start + self$annotations$duration) <= end]
      sample_rate <- self$channel_table$SampleRate[[chan]]
      samples_per_record <- self$channel_table$SamplesPerRecord[[chan]]

      # calculate index
      index <- as.vector(outer(
        seq_len(samples_per_record),
        (packet_id - 1) * samples_per_record,
        FUN = "+"
      ))

      arr <- filearray::filearray_load(file_path(private$.root_path, sprintf("Ch%d", chan)), mode = "readonly")
      re <- arr[index, , drop = FALSE, dimnames = FALSE]
      time <- re[, 2]
      sel <- time >= begin & time <= end

      structure(
        class = c("ieegio_edf_channel", "ieegio_get_channel"),
        list(
          type = "E/BDF(+)",
          info = arr$get_header("channel_info"),
          continuous = self$header$continuous_recording,
          time = time[sel],
          value = re[sel, 1, drop = TRUE]
        )
      )

    },

    format = function(...) {
      ftype <- self$header$file_type[[1]]
      if(self$header$is_plus) {
        ftype <- sprintf("%s+", ftype)
      }
      all_chans <- self$channel_table$Channel
      sel <- all_chans %in% self$selection$channels

      signal_channels <- deparse_svec(all_chans[sel & !self$channel_table$Annotation])
      annot_channels <- deparse_svec(all_chans[sel & self$channel_table$Annotation])

      c(
        "<ieegio::EBDFCache>",
        sprintf("  File type  : %s", ftype),
        sprintf("  Patient    : %s", self$header$patient),
        sprintf("  Recording  : %s", self$header$recording_id),
        sprintf("  Start time : %s", self$header$start_time),
        sprintf("  Continuous recording  : %s", self$header$continuous_recording),
        sprintf("  Loaded channels       : n=%s", sum(sel)),
        sprintf("    Signal channels     : %s", signal_channels),
        sprintf("    Annotation channels : %s", annot_channels)
      )

    },

    delete = function() {
      path <- private$.root_path
      if(length(path) == 1 && file_exists(path)) {
        file_delete(path, use_base_r = TRUE)
      }
    }
    # print = function(...) {
    #   self$format
    # }
  )
)


#' @export
format.ieegio_get_channel <- function(x, ...) {
  annot <- ifelse(isTRUE(x$info$Annotation), "annotation", "signal")
  continuous <- ifelse(isTRUE(x$continuous), "yes", "no")
  time_range <- range(x$time, na.rm = TRUE)
  sprintf("%s %s\n  Channel    : %s\n  label      : %s\n  Unit       : %s\n  Sample rate: %.1f\n  Continuous : %s\n  Number of timepoints: %d\n  Time range : %.1f to %.1f sec",
          x$type,
          annot,
          x$info$Channel,
          x$info$Label,
          x$info$Unit,
          x$info$SampleRate,
          continuous,
          length(x$value),
          time_range[[1]],
          time_range[[2]])
}

#' @export
print.ieegio_get_channel<- function(x, ...) {
  cat(format(x), "", sep = "\n")
}

