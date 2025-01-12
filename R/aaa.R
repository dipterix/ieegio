#' @importFrom R6 R6Class
NULL

NIFTI_XFORM_CODE <- list(
  "NIFTI_XFORM_UNKNOWN" = "Unknown",
  "NIFTI_XFORM_SCANNER_ANAT" = "ScannerAnat",
  "NIFTI_XFORM_ALIGNED_ANAT" = "AlignedAnat",
  "NIFTI_XFORM_TALAIRACH" = "Talairach",
  "NIFTI_XFORM_MNI_152" = "MNI152"
)

parse_svec <- function(
    text, sep = ',', connect = '-:|', sort = FALSE, unique = TRUE){
  connect <- unique(unlist(strsplit(connect, '')))
  connect[connect %in% c('|', ':', '~')] <-
    paste0('\\', connect[connect %in% c('|', ':', '~')])
  if('-' %in% connect) {
    connect <- c(connect[connect != "-"], "-")
  }
  connect <- paste(connect, collapse = '')

  if(length(text) != 1) {
    text <- paste(text, collapse = sep)
  }


  if(length(text) == 0 || !nzchar(trimws(text))){
    return(NULL)
  }

  if(is.numeric(text)){
    if(unique) {
      text <- unique(text)
    }
    if(sort) {
      text <- sort(text)
    }
    return(text)
  }
  s <- unlist(strsplit(text, sep, perl = TRUE))
  s <- trimws(s)
  s <- s[s!='']

  s <- s[grepl(sprintf('^[0-9\\ %s]+$', connect), s)]
  # s <- s[str_detect(s, sprintf('^[0-9\\ %s]+$', connect))]

  re <- NULL
  for(ss in s){
    if(grepl(sprintf('[%s]', connect), ss)){
      ss <- unlist(strsplit(ss,  sprintf('[%s]', connect), perl = TRUE))
      # ss <- as.vector(stringr::str_split(ss, sprintf('[%s]', connect), simplify = TRUE))
      ss <- trimws(ss)
      ss <- ss[grepl('^[0-9]+$', ss)]
      ss <- as.numeric(ss)
      ss <- ss[!is.na(ss)]
      if(length(ss) >= 2){
        re <- c(re, (ss[1]:ss[2]))
      }
    }else{
      re <- c(re, as.numeric(ss))
    }
  }

  if(unique){
    re <- unique(re)
  }

  if(sort){
    re <- sort(re)
  }

  return(re)
}

deparse_svec <- function(
    nums, connect = '-', concatenate = TRUE, collapse = ',',
    max_lag = 1){
  nums <- nums[is.finite(nums)]
  if(length(nums) == 0){
    return('')
  }
  alag <- seq_len(max(1, max_lag))
  nums <- sort(unique(nums))
  lg <- c(NA, nums)[seq_len(length(nums))]
  ind <- nums - lg
  ind[1] <- 0
  ind2 <- c(ind[-1], -1)
  re <- apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1,function(x){
    if(x[1] == x[2]){
      as.character(x[1])
    }else{
      paste(x, collapse = connect)
    }
  })
  if(concatenate){
    re <- paste(re, collapse = collapse)
  }
  re
}

drop_nulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


ieegio_debug <- function(..., .on = NA) {
  if(isFALSE(.on)) {
    options(ieegio.debug = FALSE)
    return(invisible())
  }
  if(getOption("ieegio.debug", FALSE)) {
    message("[ieegio.debug] ", ...)
  }
  if(isTRUE(.on)) {
    options(ieegio.debug = TRUE)
  }
}


package_installed <- function(pkg) {
  return( system.file(package = pkg) != "" )
}

check_py_flag <- function() {
  if(nchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = "")) > 0) {
    stop("System environment 'IEEGIO_NO_PYTHON' is set, Python is disabled")
  }
}

ensure_r_package <- function(pkg, ...) {
  if(package_installed(pkg)) { return(invisible()) }
  if(package_installed("ravemanager")) {
    ravemanager <- asNamespace("ravemanager")
    ravemanager$add_r_package(pkg = pkg, ...)
    return(invisible())
  }
  if(package_installed("pak")) {
    pak <- asNamespace("pak")
    pak$pak(pkg = pkg, ...)
    return(invisible())
  }
  stop("R package `", pkg, "` is missing. Please manually install them in interactive mode.")
}

ensure_package <- function(pkg, ..., lang = c("r", "python"), need_interactive = TRUE) {
  lang <- match.arg(lang)
  if(need_interactive && !interactive()) {
    stop("Package `", pkg, "` [", lang, "] is missing. Please manually install them in interactive mode.")
  }
  if(lang == "r") {
    ensure_r_package(pkg, ...)
  } else {
    ensure_py_package(pkg, ...)
  }
  return(invisible())
}
