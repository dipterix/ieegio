get_vox2fsl <- function(shape, pixdim, vox2ras) {
  voxToScaledVoxMat <- diag(c(pixdim[1:3], 1))
  isneuro <- det(vox2ras) > 0

  if( isneuro ) {
    flip <- matrix(c(
      -1, 0, 0, (shape[1] - 1) * pixdim[1],
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    ), nrow = 4L, byrow = TRUE)
    voxToScaledVoxMat <- flip %*% voxToScaledVoxMat
  }

  voxToScaledVoxMat
}


new_volume <- function(type, header, transforms, data, shape) {
  use_expression <- FALSE
  if(is.null(data)) {
    class <- c(sprintf("ieegio_%s", type), "ieegio_header_only", "ieegio_volume")
    force(shape)
    header_only <- TRUE
  } else {
    class <- c(sprintf("ieegio_%s", type), "ieegio_volume")
    header_only <- FALSE
    if(is.language(data)) {
      use_expression <- TRUE
      force(shape)
    } else {
      shape <- dim(data)
    }
  }

  structure(
    class = class,
    list(
      type = type,
      header = header,
      header_only = header_only,
      use_expression = use_expression,
      shape = shape,
      transforms = transforms,
      data = data
    )
  )
}


#' @export
print.ieegio_volume <- function(x, ...) {
  cat(c(format(x, ...), ""), sep = "\n")
}

#' @export
format.ieegio_volume <- function(x, ...) {

  if(x$header_only) {
    banner <- "<Image Volume, header-only>"
  } else {
    banner <- "<Image Volume>"
  }

  if(length(x$transforms)) {
    transforms_str <- sapply(names(x$transforms), function(nm) {
      mat <- x$transforms[[nm]]
      mat <- apply(mat, 2, function(x) {
        re <- sprintf("%.6f", x)
        re <- as.double(re)
        re <- sprintf("%.4g", re)
        stringr::str_pad(re, width = max(nchar(re)), side = "left")
      })
      re <- apply(mat, 1, function(x) {
        sprintf("      [%s]", paste(x, collapse = "  "))
      })
      paste(c(sprintf("    %s:", nm), re), collapse = "\n")
    })
    transforms_str <- c("  Transforms:", transforms_str)
  } else {
    transforms_str <- "  Transforms: none"
  }

  c(
    banner,
    sprintf("  Type : %s", paste(x$type, collapse = "/")),
    sprintf("  Shape: %s", deparse1(x$shape)),
    transforms_str
  )
}

#' @export
names.ieegio_volume <- function(x) {
  c("type", "header", "shape", "transforms", "data")
}

#' @export
`$.ieegio_volume` <- function(x, name) {
  if(identical(name, "data")) {
    if(isTRUE(.subset2(x, "use_expression"))) {
      expr <- .subset2(x, "data")
      data <- eval(expr, envir = x, enclos = new.env(parent = globalenv()))
    } else {
      data <- .subset2(x, "data")
    }
    return(data)
  }
  .subset2(x, name)
}

#' @export
`[[.ieegio_volume` <- function(x, i, ...) {
  `$.ieegio_volume`(x, i)
}

#' @export
`[.ieegio_volume` <- function(x, ...) {
  if(isTRUE(.subset2(x, "header_only"))) {
    return(NULL)
  }
  return(`$.ieegio_volume`(x, "data")[...])
}

#' @export
`[<-.ieegio_volume` <- function(x, ..., value) {

  if(isTRUE(.subset2(x, "header_only"))) {
    stop("Head-only image. Cannot assign data")
  }

  if(isTRUE(.subset2(x, "use_expression"))) {
    x$header[...] <- value
  } else {
    x$data[...] <- array
  }

  x
}
