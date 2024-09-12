
# Correct path if prefix has incorrect cases (works on case-sensitive os)
correct_filepath <- function(path) {
  stopifnot(length(path) == 1)
  if(file_exists(path)) { return(path) }
  # path_prefix = "bidsr.rproj"
  dir <- dirname(path)
  prefix <- basename(path)
  files <- list.files(
    path = dir,
    full.names = FALSE,
    all.files = TRUE,
    recursive = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  sel <- toupper(files) == toupper(prefix)
  if(!any(sel)) { return(NA) }

  return(files[sel][[1]])
}

path_to_nearest_file <- function(filename, start, root = NA, ignore_cases = FALSE) {

  # filename <- "rave"
  # start = "."

  if( ignore_cases ) {
    filename <- tolower(filename)
    start <- tolower(start)
  }

  if(file_exists(start)) {
    if(fs::is_file(start)) {
      if( basename(start) == filename ) {
        # start is a file and is the filename
        return(start)
      }
      start <- fs::path_dir(start)
    }
    # now start is a folder
    tmp <- fs::path(start, filename)
    if( file_exists(tmp) ) {
      return(tmp)
    }
    # folder does not have this file
  }
  if(!fs::is_absolute_path(start)) {
    start <- fs::path_abs(start)
  }
  start_ <- fs::path_dir(start)

  if(!is.na(root)) {
    if(!fs::is_absolute_path(root)) {
      root <- fs::path_abs(root)
    }
    if(!fs::path_has_parent(start_, root)) {
      return(NA_character_)
    }
  } else {
    if(start_ == start) {
      return(NA_character_)
    }
  }
  Recall(filename = filename, start = start_, root = root)
}


# Do not use fs::dir_create as the `fs` file ops do not respect system umask
# and is default to 0755
dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(normalizePath(x))
}

path_abs <- function(path) {
  fs::path_abs(path)
}

path_expand <- function(path) {
  fs::path_norm(fs::path_expand(path))
}

path_norm <- function(path) {
  fs::path_norm(path)
}

path_rel <- function(path, start = ".") {
  fs::path_rel(path, start = start)
}

file_path <- function(..., ext = "") {
  fs::path(..., ext = ext)
}

file_exists <- function(path) {
  fs::file_exists(as.character(path))
}

file_assert <- function(path, dir_ok = FALSE, follow = TRUE) {
  if(length(path) != 1 || is.na(path)) {
    stop("File path must be length of 1 and cannot be N/A")
  }
  if( dir_ok ) {
    if(!fs::file_exists(path)) {
      stop("File or directory `", path, "` is missing.")
    }
  } else {
    if( !fs::is_file(path, follow = follow) ) {
      stop("File `", path, "` is missing.")
    }
  }
  invisible(TRUE)
}

dir_exists <- function(path) {
  fs::dir_exists(path)
}

path_has_parent <- function(path, parent) {
  fs::path_has_parent(path = path, parent = parent)
}

path_split <- function(path) {
  fs::path_split(path)
}

path_join <- function(parts) {
  fs::path_join(parts)
}

is_absolute_path <- function(path) {
  fs::is_absolute_path(path)
}

file_delete <- function(path, use_base_r = FALSE, ...) {
  if( use_base_r ) {
    if( dir_exists(path) ) {
      unlink(x = path, recursive = TRUE, ...)
    } else if (file_exists(path)) {
      unlink(path, recursive = FALSE, ...)
    }
  } else {
    fs::file_delete(path)
  }
}

file_size <- function(path, fail = TRUE) {
  fs::file_size(path, fail = fail)
}

ARCHIVE_EXTENSIONS <- c("zip", "rar", "7z", "tar", "tar.gz", "tar.bz2", "gz", "bz2", "xz", "iso")

path_ext_remove <- function(path, archive_ext = ARCHIVE_EXTENSIONS) {
  ext <- path_ext(path, archive_ext = archive_ext)
  fs::path_ext_remove(path)

  dir <- fs::path_dir(path)
  bname <- basename(path)
  na <- is.na(path)
  no_dir <- dir == "." | dir == ""

  ext <- path_ext(bname)
  has_ext <- !na & ext != ""

  bname2 <- bname[has_ext]
  bname[has_ext] <- substr(bname2, 1L, nchar(bname2) - nchar(ext[has_ext]) - 1)


  path[!na & no_dir] <- fs::path_tidy(bname[!na & no_dir])
  path[!na & !no_dir] <- fs::path(dir[!na & !no_dir], bname[!na & !no_dir])
  path
}

path_ext <- function(path, archive_ext = ARCHIVE_EXTENSIONS) {
  # DIPSAUS DEBUG START
  # path <- c("asda.dasd/aadda.nii.gz.tar.bz2", ".asda.dasd/aadda.nii.gz.tar.bz2",
  #   ".nii.gz.tar.bz2", "asda", ".", "NA", NA, "nii.gz.aaa.nii.GZ", "././..")
  if (length(path) == 0) {
    return(character())
  }

  vapply(strsplit(basename(path), ".", fixed = TRUE), function(x) {
    if(isTRUE(is.na(x))) { return(NA_character_) }
    if(length(x) == 1) { return("") }
    if(x[[1]] == "") {
      x <- x[-c(1, 2)]
    } else {
      x <- x[-1]
    }
    if(!length(x)) { return("") }
    sel <- which(!tolower(x) %in% tolower(archive_ext))
    if(!length(sel)) {
      return(paste(x, collapse = "."))
    }
    sel <- max(sel)
    if(sel == length(x)) {
      return(x[[sel]])
    }
    paste(x[seq.int(sel, length(x))], collapse = ".")
  }, FUN.VALUE = "")
}

file_move <- function(path, new_path) {
  fs::file_move(path = path, new_path = new_path)
}
