#' @name io-vtk-streamlines
#' @title Read or write streamline data in \code{'VTK'} format
#' @description
#' This reader uses 'Python' \code{'vtk'} package, supports \code{'.vtk'},
#' \code{'.vtp'}, \code{'.pvtp'}, \code{'.vtpb'} formats.
#' @param file,con file path to the \code{'VTK'} file, the format will be
#' inferred from the file extension (with default \code{'.vtk'})
#' @param x An \code{\link{imaging-streamlines}} object
#' @param binary for legacy \code{'.vtk'} file only, whether to store the
#' data as binary file or 'ASCII' plain text; default is true (binary).
#' @returns \code{io_read_vtk_streamlines} returns an
#' \code{\link{imaging-streamlines}} object, while
#' \code{io_write_vtk_streamlines} writes the data to file
#' @examples
#'
#' # This example shows how to convert tck to vtk
#'
#' # run `ieegio_sample_data("streamlines/CNVII_R.tck")` to
#' # download sample data
#'
#' if( ieegio_sample_data("streamlines/CNVII_R.tck", test = TRUE) ) {
#'
#'   path <- ieegio_sample_data("streamlines/CNVII_R.tck")
#'
#'   streamlines <- as_ieegio_streamlines(path)
#'
#'   # write to vtk
#'   tfile <- tempfile(fileext = ".vtk")
#'   io_write_vtk_streamlines(streamlines, con = tfile)
#'
#'   # read
#'   vtk_streamlines <- io_read_vtk_streamlines(tfile)
#'
#'   # compare
#'   plot(streamlines)
#'   plot(vtk_streamlines)
#'
#'   # 0 0
#'   range(streamlines[[1]]$coords - vtk_streamlines[[1]]$coords)
#'
#' }
#'
#'
#' @export
io_read_vtk_streamlines <- function(file) {
  # file <- "~/Dropbox (Personal)/Share_with_ZJ/Template Atlas/Peterson_vtk_files_v1.0/motorcortex/M1_cf_face.1k.vtk"
  # x <- io_read_vtk_streamlines(file)
  # y <- io_read_vtk_streamlines("~/Downloads/junk.vtk")
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)

  ext <- strsplit(tolower(file), "\\.")[[1]]
  ext <- ext[[length(ext)]]

  reader_names <- switch (
    ext,
    "pvtp" = c("vtkXMLPPolyDataReader", "vtkXMLPolyDataReader", "vtkPolyDataReader"),
    "vtp" = c("vtkXMLPolyDataReader", "vtkPolyDataReader"),
    "h5" = c("vtkHDFReader", "vtkPolyDataReader"),
    "vtpb" = c("vtkHDFReader", "vtkPolyDataReader"),
    {
      c("vtkPolyDataReader", "vtkXMLPolyDataReader", "vtkHDFReader")
    }
  )

  ensure_py_package("vtk")
  vtk <- rpymat::import('vtk')

  reader_valid <- FALSE
  coords <- NULL
  lines <- NULL
  lapply(reader_names, function(reader_name) {
    if(reader_valid) { return() }
    tryCatch({
      reader <- vtk[[reader_name]]()
      reader$SetFileName(file)
      reader$Update()
      polydata <- reader$GetOutput()
      pointdata <- polydata$GetPoints()$GetData()
      linedata <- polydata$GetLines()$GetData()
      coords <<- py_to_r(vtk$util$numpy_support$vtk_to_numpy(pointdata))
      lines <<- py_to_r(vtk$util$numpy_support$vtk_to_numpy(linedata))
      reader_valid <<- TRUE
    }, error = function(e) {
    })
    # free up
    reader <- NULL
    return()
  })

  if(!reader_valid) {
    stop("Unable to read the VTK file using the following readers: ",
         paste(sprintf("`vtk.%s`", reader_names), collapse = ", "))
  }

  # ensure_py_package("pyvista")
  # pyvista <- rpymat::import('pyvista')
  # vtp <- pyvista$read(file)
  #
  # coords <- py_to_r(vtp$points)
  #
  # lines <- py_to_r(vtp$lines)

  # build index
  nr <- nrow(coords)
  offsets <- 1
  offset_idx <- 1
  nl <- length(lines)
  while(offset_idx <= nl) {
    n_points <- lines[[offset_idx]]
    offset_idx <- offset_idx + 1 + n_points
    offsets <- c(offsets, offset_idx)
  }

  offsets <- c(offsets[offsets <= nl], nl + 1)
  tracts <- lapply(seq_len(length(offsets) - 1), function(ii) {
    start_idx <- offsets[[ii]] + 1
    end_idx <- offsets[[ii + 1]] - 1
    if(start_idx >= end_idx) { return(NULL) }
    idx <- lines[seq.int(from = start_idx, to = end_idx)] + 1L
    idx <- idx[idx <= nr]
    if(length(idx) < 2) { return(NULL) }
    list(
      coords = coords[idx, , drop = FALSE],
      num_points = length(idx)
    )
  })
  tracts <- tracts[!vapply(tracts, is.null, FALSE)]
  re <- as_ieegio_streamlines.default(tracts, diag(1, 4), class = "ieegio_streamlines_vtk")
  re
}

#' @rdname io-vtk-streamlines
#' @export
io_write_vtk_streamlines <- function(x, con, binary = TRUE) {
  con <- path_expand(con)
  binary <- as.logical(binary)[[1]]

  # con <- '~/Downloads/junk.h5'
  n <- length(x$data)

  tracts <- lapply(seq_len(n), function(i) {
    re <- x[[i]]$coords[, 1:3, drop = FALSE]
    dimnames(re) <- NULL
    re
  })

  points <- do.call(rbind, tracts)

  start_idx <- 0L
  lines <- lapply(seq_len(n), function(i) {
    num_points <- x$data[[i]]$num_points
    re <- c(num_points, seq.int(start_idx, length.out = num_points))
    start_idx <<- start_idx + num_points
    re
  })

  lines <- as.integer(unlist(lines))

  ensure_py_package("vtk")
  vtk <- rpymat::import('vtk')

  numpy_to_vtk <- vtk$util$numpy_support$numpy_to_vtk
  numpy_to_vtkIdTypeArray <- vtk$util$numpy_support$numpy_to_vtkIdTypeArray

  # Create vtkPoints
  vtk_points <- vtk$vtkPoints()
  vtk_points$SetData(numpy_to_vtk(points, deep = TRUE))

  # Create vtkCellArray for lines
  vtk_lines <- vtk$vtkCellArray()
  lines_py <- rpymat::r_to_py(matrix(lines, nrow = 1))
  lines_py <- lines_py$astype("int64")

  vtk_line_array <- numpy_to_vtkIdTypeArray(lines_py, deep=TRUE)
  vtk_lines$SetCells(n, vtk_line_array)

  # Create PolyData
  polydata <- vtk$vtkPolyData()
  polydata$SetPoints(vtk_points)
  polydata$SetLines(vtk_lines)

  if( endsWith(tolower(con), "vtp") ) {
    writer <- vtk$vtkXMLPolyDataWriter()
  } else if( endsWith(tolower(con), "h5") || endsWith(tolower(con), "vtpb") ) {
    writer <- vtk$vtkHDFWriter()
  } else {
    writer <- vtk$vtkPolyDataWriter()
    if( binary ) {
      writer$SetFileTypeToBinary()
    } else {
      writer$SetFileTypeToASCII()
    }
  }
  writer$SetFileName(con)
  writer$SetInputData(polydata)

  dname <- dirname(con)
  if(!dir_exists(dname)) {
    dir_create(dname)
  }
  writer$Write()

  # poly <- pyvista$PolyData()
  #
  # poly$points <- points
  # poly$lines  <- lines
  #
  # poly$save(con, binary = binary)
  invisible(con)
}


#
# # Step 4: Save to file (vtk or vtp)
# poly.save(filename)  # auto-detects .vtk or .vtp
#
# return poly
# pyvista$Line()
