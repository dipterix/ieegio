#' Class definition for signal cache
#' @description
#' This class is an internal abstract class
SignalDataCache <- R6::R6Class(
  classname = "FileCache",
  portable = TRUE,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Get header information, often small list object
    #' @param ... passed to child methods
    get_header = function(...) {
      if("header" %in% names(self)) {
        return(self$header)
      }
      .NotYetImplemented()
    },

    #' @description
    #' Get annotation information, often a large table
    #' @param ... passed to child methods
    get_annotations = function(...) {
      if("annotations" %in% names(self)) {
        return(self$annotations)
      }
      .NotYetImplemented()
    },

    #' @description
    #' Get channel table
    #' @param ... passed to child methods
    get_channel_table = function(...) {
      if("channel_table" %in% names(self)) {
        return(self$channel_table)
      }
      .NotYetImplemented()
    },


    #' @description
    #' Get channel data
    #' @param x channel order or label
    #' @param ... passed to child methods
    #' @return Channel signal with time-stamps inheriting class
    #' \code{'ieegio_get_channel'}
    get_channel = function(x, ...) {
      .NotYetImplemented()
    },

    #' @description
    #' Delete file cache
    #' @param ... passed to child methods
    delete = function(...) {
      .NotYetImplemented()
    }
  )
)
