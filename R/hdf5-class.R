
h5FileValid <- function(filename){
  if(!length(filename)){ return(FALSE) }
  filename <- filename[[1]]
  if(!file.exists(filename)){ return(FALSE) }
  if(isTRUE(file.info(filename)[['isdir']])){ return(FALSE) }
  filename <- normalizePath(filename)
  return(tryCatch({
    hdf5r::is.h5file(filename)
  }, error = function(e){ FALSE }))
}

#' @title Lazy 'HDF5' file loader
#' @description provides hybrid data structure for 'HDF5' file
LazyH5 <- R6::R6Class(
  classname = 'LazyH5',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    file = NULL,
    name = NULL,
    read_only = TRUE,
    data_ptr = NULL,
    file_ptr = NULL,
    last_dim = NULL
  ),
  public = list(

    #' @field quiet whether to suppress messages
    quiet = FALSE,

    #' @description garbage collection method
    #' @return none
    finalize = function(){
      self$close(all = TRUE)
    },

    #' @description overrides print method
    #' @return self instance
    print = function(){
      if(!is.null(private$data_ptr)){
        if(private$data_ptr$is_valid){
          base::print(private$data_ptr)
        }else{
          base::cat('Pointer closed. Information since last open:\nDim: ',
                    paste(private$last_dim, collapse = 'x'), ' \tRank: ',
                    length(private$last_dim), "\n")
        }
      }
      invisible(self)
    },

    #' @description constructor
    #' @param file_path where data is stored in 'HDF5' format
    #' @param data_name the data stored in the file
    #' @param read_only whether to open the file in read-only mode. It's highly
    #' recommended to set this to be true, otherwise the file connection is
    #' exclusive.
    #' @param quiet whether to suppress messages, default is false
    #' @return self instance
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE){

      # First get absolute path, otherwise hdf5r may report file not found error
      if(read_only){
        private$file <- normalizePath(file_path)

        if( !hdf5r::is_hdf5(private$file) ) {
          stop("File is not an HDF5 file.")
        }
      }else{
        file_path <- normalizePath(file_path, mustWork = FALSE)
        private$file <- file_path
      }
      self$quiet <- isTRUE(quiet)
      private$name <- data_name
      private$read_only <- read_only
    },

    #' @description save data to a 'HDF5' file
    #' @param x vector, matrix, or array
    #' @param chunk chunk size, length should matches with data dimension
    #' @param level compress level, from 1 to 9
    #' @param replace if the data exists in the file, replace the file or not
    #' @param new_file remove the whole file if exists before writing?
    #' @param force if you open the file in read-only mode, then saving
    #' objects to the file will raise error. Use \code{force=TRUE} to force
    #' write data
    #' @param ctype data type, see \code{\link{mode}}, usually the data type
    #' of \code{x}. Try \code{mode(x)} or \code{storage.mode(x)} as hints.
    #' @param size deprecated, for compatibility issues
    #' @param ... passed to self \code{open()} method
    save = function(x, chunk = 'auto', level = 7, replace = TRUE,
                    new_file = FALSE, force = TRUE, ctype = NULL, size = NULL,
                    ...){
      # ctype and size is deprecated but kept in case of compatibility issues
      # ptr$create_dataset =
      # function (name, robj = NULL, dtype = NULL, space = NULL, dims = NULL,
      #           chunk_dims = "auto", gzip_level = 4, link_create_pl = h5const$H5P_DEFAULT,
      #           dataset_create_pl = h5const$H5P_DEFAULT, dataset_access_pl = h5const$H5P_DEFAULT)
      if(private$read_only){
        if(!force){
          stop('File is read-only. Use "force=TRUE"')
        }else{
          # Close current pointer
          self$close(all = TRUE)
          private$read_only <- FALSE

          on.exit({
            self$close(all = TRUE)
            private$read_only <- TRUE
          }, add = TRUE, after = FALSE)
        }
      }

      if(new_file && file.exists(private$file)){
        self$close(all = TRUE)
        file.remove(private$file)
      }

      self$open(new_dataset = replace, robj = x, chunk = chunk, gzip_level = level, ...)

      self$close(all = TRUE)

    },


    #' @description open connection
    #' @param new_dataset only used when the internal pointer is closed, or
    #' to write the data
    #' @param robj data array to save
    #' @param ... passed to \code{createDataSet} in \code{hdf5r} package
    open = function(new_dataset = FALSE, robj, ...){

      # check data pointer
      # if valid, no need to do anything, otherwise, enter if clause
      if(new_dataset || is.null(private$data_ptr) || !private$data_ptr$is_valid){

        # Check if file is valid,
        if(is.null(private$file_ptr) || !private$file_ptr$is_valid){
          # if no, create new link
          mode <- ifelse(private$read_only, 'r', 'a')
          tryCatch({
            private$file_ptr <- hdf5r::H5File$new(private$file, mode)
          }, error = function(e){
            # Open for writting, we should close all connections first
            # then the file can be opened, otherwise, Access type: H5F_ACC_RDONLY
            # will lock the file for writting
            f <- hdf5r::H5File$new(private$file, 'r')
            if(!self$quiet){
              cat('Closing all other connections to [{private$file}] - {f$get_obj_count() - 1}\n')
            }

            try({ f$close_all() }, silent = TRUE)
            private$file_ptr <- hdf5r::H5File$new(private$file, mode)
          })
        }

        has_data <- private$file_ptr$path_valid(private$name)

        if(!private$read_only && (new_dataset || ! has_data)){
          # need to create new dataset
          g <- stringr::str_split(private$name, '/', simplify = TRUE)
          g <- g[stringr::str_trim(g) != '']

          ptr <- private$file_ptr
          nm <- ''

          for(i in g[-length(g)]){
            nm <- sprintf('%s/%s', nm, i)
            if(!ptr$path_valid(path = nm)){
              ptr <- ptr$create_group(i)
              if(!self$quiet){
                cat('{private$file} => {nm} (Group Created)\n')
              }
            }else{
              ptr <- ptr[[i]]
            }
          }

          # create dataset
          nm <- g[length(g)]
          if(ptr$path_valid(path = nm)){
            # dataset exists, unlink first
            if(!self$quiet){
              cat('{private$file} => {private$name} (Dataset Removed)\n')
            }
            ptr$link_delete(nm)
          }
          # new create
          if(!self$quiet){
            cat('{private$file} => {private$name} (Dataset Created)\n')
          }
          if(missing(robj)){
            robj <- NA
          }
          ptr$create_dataset(nm, robj = robj, ...)
          if(ptr$is_valid && inherits(ptr, 'H5Group')){
            ptr$close()
          }
        }else if(!has_data){
          stop(sprintf(
            'File [%s] has no [%s] in it.',
            private$file, private$name
          ))
        }

        private$data_ptr <- private$file_ptr[[private$name]]

      }

      private$last_dim <- private$data_ptr$dims

    },


    #' @description close connection
    #' @param all whether to close all connections associated to the data file.
    #' If true, then all connections, including access from other programs,
    #' will be closed
    close = function(all = TRUE){
      try({
        # check if data link is valid
        if(!is.null(private$data_ptr) && private$data_ptr$is_valid){
          private$data_ptr$close()
        }

        # if file link is valid, get_obj_ids() should return a vector of 1
        if(all && !is.null(private$file_ptr) && private$file_ptr$is_valid){
          private$file_ptr$close_all()
        }
      }, silent = TRUE)
    },

    #' @description subset data
    #' @param i,j,... index along each dimension
    #' @param drop whether to apply \code{\link{drop}} the subset
    #' @param stream whether to read partial data at a time
    #' @param envir if \code{i,j,...} are expressions, where should the
    #' expression be evaluated
    #' @return subset of data
    subset = function(
    ...,
    drop = FALSE, stream = FALSE,
    envir = parent.frame()
    ) {
      self$open()
      on.exit({
        self$close(all = !private$read_only)
      })
      # dims <- self$get_dims()

      # step 1: eval indices
      dot_len <- ...length()
      args <- eval(substitute(alist(...)))
      if(dot_len == 0 || (dot_len == 1 && isTRUE(args[[1]] == ''))){
        return(private$data_ptr$read(drop = drop))
      }
      return(private$data_ptr$read(args = args, drop = drop, envir = envir))
      # args <- lapply(args, function(x){
      #   if(x == ''){
      #     return(x)
      #   }else{
      #     return(eval(x, envir = envir))
      #   }
      # })
      #
      # # step 2: get allocation size
      # alloc_dim <- sapply(seq_along(dims), function(ii){
      #   if(is.logical(args[[ii]])){
      #     return(sum(args[[ii]]))
      #   }else if(is.numeric(args[[ii]])){
      #     return(length(args[[ii]]))
      #   }else{
      #     # must be blank '', otherwise raise error
      #     return(dims[ii])
      #   }
      # })
      #
      # # step 3: get legit indices
      # legit_args <- lapply(seq_along(dims), function(ii){
      #   if(is.logical(args[[ii]])){
      #     return(args[[ii]])
      #   }else if(is.numeric(args[[ii]])){
      #     return(
      #       args[[ii]][args[[ii]] <= dims[ii] & args[[ii]] > 0]
      #     )
      #   }else{
      #     return(args[[ii]])
      #   }
      # })
      #
      # # step 4: get mapping
      # mapping <- lapply(seq_along(dims), function(ii){
      #   if(is.logical(args[[ii]])){
      #     return(
      #       rep(TRUE, sum(args[[ii]]))
      #     )
      #   }else if(is.numeric(args[[ii]])){
      #     return(args[[ii]] <= dims[ii] & args[[ii]] > 0)
      #   }else{
      #     return(args[[ii]])
      #   }
      # })
      #
      # # alloc space
      # re <- array(NA, dim = alloc_dim)
      #
      # if(stream){
      #   re <- do.call(`[<-`, c(list(re), mapping, list(
      #     value = private$data_ptr$read(
      #       args = legit_args,
      #       drop = FALSE,
      #       envir = environment()
      #     )
      #   )))
      # }else{
      #   re <- do.call(`[<-`, c(list(re), mapping, list(
      #     value = do.call('[', c(list(private$data_ptr$read()), legit_args, list(drop = FALSE)))
      #   )))
      # }
      #
      # self$close(all = !private$read_only)
      #
      #
      # if(drop){
      #   return(drop(re))
      # }else{
      #   return(re)
      # }
    },


    #' @description get data dimension
    #' @param stay_open whether to leave the connection opened
    #' @return dimension of the array
    get_dims = function(stay_open = TRUE){
      self$open()
      re <- private$data_ptr$dims
      if(!stay_open){
        self$close(all = !private$read_only)
      }
      re
    },

    #' @description get data type
    #' @param stay_open whether to leave the connection opened
    #' @return data type, currently only character, integer, raw,
    #' double, and complex are available, all other types will yield "unknown"
    get_type = function(stay_open = TRUE) {
      self$open()
      type <- private$data_ptr$get_type()
      if(!stay_open){
        self$close(all = !private$read_only)
      }
      if(inherits(type, "H5T_STRING")) { return("character") }
      if(inherits(type, "H5T_INTEGER")) { return("integer") }
      if(inherits(type, "H5T_BITFIELD")) { return("raw") }
      if(inherits(type, "H5T_FLOAT")) { return("double") }
      if(inherits(type, "H5T_COMPLEX")) { return("complex") }
      return("unknown")
    }
  )
)

#' @export
`[.LazyH5` <- function(obj, ...){
  on.exit({obj$close()}, add = TRUE)
  obj$subset(..., envir = parent.frame())
}

#' @export
`+.LazyH5` <- function(a, b){
  b + a$subset()
}

#' @export
`-.LazyH5` <- function(a, b){
  -(b - a$subset())
}

#' @export
`*.LazyH5` <- function(a, b){
  b * (a$subset())
}

#' @export
`/.LazyH5` <- function(a, b){
  if(inherits(b, 'LazyH5')){
    b <- b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyH5 <- function(x){
  dim_info <- x$get_dims(stay_open = FALSE)
  if(length(dim_info) == 1){
    dim_info <- NULL
  }
  dim_info
}


#' @export
length.LazyH5 <- function(x){
  dim_info <- x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyH5 <- function(x, ...){
  as.array(x$subset(), ...)
}

#' @export
Mod.LazyH5 <- function(z){
  base::Mod(z$subset())
}

#' @export
Arg.LazyH5 <- function(z){
  base::Arg(z$subset())
}


#' @export
exp.LazyH5 <- function(x){
  base::exp(x$subset())
}

