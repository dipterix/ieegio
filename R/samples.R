
# update_sample_registry <- function() {
#   # internal debug use
#   fnames <- list.files("inst/sample_data/", recursive = TRUE, include.dirs = FALSE, full.names = FALSE)
#   fnames <- fnames[!fnames %in% "registry.txt"]
#   writeLines(fnames, "inst/sample_data_registry.txt")
# }

#' @title Download sample files
#' @param file file to download; set to \code{NULL} to view all possible files
#' @param cache_ok whether to use cache
#' @param test test whether the sample file exists instead of downloading them;
#' default is \code{FALSE}
#' @returns When \code{test} is false, returns downloaded file path
#' (character); when \code{test} is true, returns whether the expected
#' sample exists (logical).
#' @examples
#'
#' # list available files
#' ieegio_sample_data()
#'
#' # check if file edfPlusD.edf exists
#' ieegio_sample_data("edfPlusD.edf", test = TRUE)
#'
#' \dontrun{
#'
#' ieegio_sample_data("edfPlusD.edf")
#'
#' }
#'
#' @export
ieegio_sample_data <- function(file, test = FALSE, cache_ok = TRUE) {
  dry_run <- enable_debugging(NA)

  # DIPSAUS DEBUG START
  # file <- "edfPlusD.edf"
  # cache_ok <- TRUE

  file_list <- readLines(system.file("sample_data_registry.txt", package = "ieegio"))

  if(missing(file) || is.null(file)) {
    cat("Available sample files:\n", paste0("  - ", file_list), "", sep = "\n")
    return(invisible(file_list))
  }

  if(!isTRUE(file %in% file_list)) {
    if( test ) {
      return(FALSE)
    }
    stop("Sample file not exists: ", sQuote(file), ". Available options are: ", paste(sQuote(file_list), collapse = ", "))
  }

  sample_dir <- file.path(tools::R_user_dir(package = "ieegio", which = "cache"), "sample_data")
  sample_file <- file.path(sample_dir, file)

  sample_exists <- file_exists(sample_file)
  if(test) {
    return(sample_exists)
  }

  if(dry_run && !sample_exists) {
    stop(sprintf("You are under debugging mode. The sample file will not be downloaded. If you see this error, that means you are running `ieegio_sample_data` in tests or examples that does not allow extra downloading files. Please wrap your code with condition `if(ieegio_sample_data('%s', test=TRUE)) ...` in package documentations and tests. If the error is in production mode, please file an issue/debug report.", file))
  }

  backup_file <- paste0(sample_file, ".backup")

  timeout <- getOption("timeout", 60)
  if( timeout < 3600 ) {
    options("timeout" = 3600)
    on.exit({
      options("timeout" = timeout)
    }, add = TRUE)
  }

  on.exit({
    if( file_exists(backup_file) ) {
      if(!file_exists(sample_file)) {
        # download failed
        file_move(backup_file, sample_file)
      } else {
        file_delete(backup_file)
      }
    }
  }, add = TRUE)

  if( file_exists(sample_file) ) {
    if( cache_ok ) {
      return(path_abs(sample_file))
    }
    file_move(sample_file, backup_file)
  }

  if(!file_exists(sample_file) || !cache_ok) {
    url <- sprintf("https://github.com/dipterix/ieegio/raw/main/inst/sample_data/%s", file)

    dir_create(dirname(sample_file))
    utils::download.file(url = url, destfile = sample_file, cacheOK = cache_ok)
  }

  path_abs(sample_file)

}
