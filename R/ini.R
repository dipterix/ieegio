#' @rdname low-level-read-write
#' @export
io_read_ini <- function (con, ...) {
  regexp_section <- "^\\s*\\[\\s*(.+?)\\s*]"
  regexp_keyval <- "^\\s*[^=]+=.+"
  regexp_comment <- "^\\s*[;#]"

  if(!inherits(con, "connection")) {
    con <- file(con, open = "r", ...)
    on.exit(close(con))
  }

  re <- fastmap::fastmap()

  ensure_section <- function(name) {
    if(!re$has(name)) {
      re$set(name, list(
        data = fastmap::fastqueue(),
        comments = fastmap::fastqueue()
      ))
    }
    re$get(name)
  }

  add_comment <- function(x, section) {
    section$comments$add(x)
  }
  add_entry <- function(x, section) {
    section$data$add(x)
  }

  current_section <- ensure_section("Overall Descriptions")

  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (!length(line)) {
      break
    }
    if (grepl(regexp_comment, line)) {
      add_comment(line, current_section)
      next
    }
    if (grepl(regexp_section, line)) {
      matches <- regexec(regexp_section, line)
      section_name <- regmatches(line, matches)[[1]][2]
      print(section_name)
      current_section <- ensure_section(section_name)
    }
    if (grepl(regexp_keyval, line)) {
      s <- strsplit(line, "=")[[1]]
      key <- trimws(s[[1]], which = "both")
      value <- trimws(paste0(s[-1], collapse = "="), which = "both")
      add_entry(list(key = key, value = value), current_section)
    }
  }

  re <- re$as_list()

  re <- structure(
    names = names(re),
    lapply(re, function(li) {
      comments <- unlist(li$comments$as_list())
      comments <- trimws(gsub("^[ ]{0,};", "", comments))
      data <- data.table::rbindlist(li$data$as_list())
      list(
        data = data,
        comments = comments
      )
    })
  )

  re
}
