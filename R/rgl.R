check_rgl <- function (strict = NA) {
  rgl_unavailable <- function() {
    msg <- "Package `rgl` is not installed. Please install `rgl` to use this function."
    if (isTRUE(strict)) {
      stop(msg)
    }
    else if (is.na(strict)) {
      message(msg)
    }
    FALSE
  }
  if (identical(Sys.getenv("RAVETOOLS_RGL_DISABLED"), "1")) {
    return(rgl_unavailable())
  }
  if (getOption("ravetools.rgl.disabled", FALSE)) {
    return(rgl_unavailable())
  }
  if (!package_installed("rgl")) {
    return(rgl_unavailable())
  }
  TRUE
}

helper_rgl_call <- function (FUN, ...) {
  check_rgl()

  oldopt <- options(rgl.useNULL = TRUE)
  on.exit({ options(oldopt) })
  rgl <- asNamespace("rgl")
  f <- rgl[[FUN]]
  if (!is.function(f)) {
    stop("Function ", FUN, " is not a `rgl` function.")
  }
  rgl[[FUN]](...)
}

helper_rgl_view <- function (expr, quoted = FALSE, env = parent.frame()) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  dev <- helper_rgl_call("open3d")
  on.exit({
    helper_rgl_call("close3d", dev = dev)
  }, add = TRUE, after = TRUE)
  eval(expr, envir = env)
  helper_rgl_call("rglwidget")
}
