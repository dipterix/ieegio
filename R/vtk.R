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
  # file <- normalizePath("~/Downloads/accumbens_L.vtk")
  # x <- io_read_vtk_streamlines(file)
  # y <- io_read_vtk_streamlines("~/Downloads/junk.vtk")
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)

  ext <- strsplit(tolower(file), "\\.")[[1]]
  ext <- ext[[length(ext)]]

  reader_names <- switch(
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
  vtk <- rpymat::import("vtk")

  reader_valid <- FALSE
  coords <- NULL
  lines <- NULL
  lapply(reader_names, function(reader_name) {
    if (reader_valid) { return() }
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

  if (!reader_valid) {
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
  while (offset_idx <= nl) {
    n_points <- lines[[offset_idx]]
    offset_idx <- offset_idx + 1 + n_points
    offsets <- c(offsets, offset_idx)
  }

  offsets <- c(offsets[offsets <= nl], nl + 1)
  tracts <- lapply(seq_len(length(offsets) - 1), function(ii) {
    start_idx <- offsets[[ii]] + 1
    end_idx <- offsets[[ii + 1]] - 1
    if (start_idx >= end_idx) { return(NULL) }
    idx <- lines[seq.int(from = start_idx, to = end_idx)] + 1L
    idx <- idx[idx <= nr]
    if (length(idx) < 2) { return(NULL) }
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
  vtk <- rpymat::import("vtk")

  numpy_to_vtk <- vtk$util$numpy_support$numpy_to_vtk
  numpy_to_vtkIdTypeArray <- vtk$util$numpy_support$numpy_to_vtkIdTypeArray

  # Create vtkPoints
  vtk_points <- vtk$vtkPoints()
  vtk_points$SetData(numpy_to_vtk(points, deep = TRUE))

  # Create vtkCellArray for lines
  vtk_lines <- vtk$vtkCellArray()
  lines_py <- rpymat::r_to_py(matrix(lines, nrow = 1))
  lines_py <- lines_py$astype("int64")

  vtk_line_array <- numpy_to_vtkIdTypeArray(lines_py, deep = TRUE)
  vtk_lines$SetCells(n, vtk_line_array)

  # Create PolyData
  polydata <- vtk$vtkPolyData()
  polydata$SetPoints(vtk_points)
  polydata$SetLines(vtk_lines)

  if (endsWith(tolower(con), "vtp")) {
    writer <- vtk$vtkXMLPolyDataWriter()
  } else if (endsWith(tolower(con), "h5") ||
             endsWith(tolower(con), "vtpb")) {
    writer <- vtk$vtkHDFWriter()
  } else {
    writer <- vtk$vtkPolyDataWriter()
    if (binary) {
      writer$SetFileTypeToBinary()
    } else {
      writer$SetFileTypeToASCII()
    }
  }
  writer$SetFileName(con)
  writer$SetInputData(polydata)

  dname <- dirname(con)
  if (!dir_exists(dname)) {
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


#' @name io-vtk-polys
#' @title Read or write surface mesh data in \code{'VTK'} format
#' @description
#' Reads and writes the polygon (\code{'Polys'}) part of a
#' \code{'vtkPolyData'} object. Like \code{\link{io_read_vtk_streamlines}},
#' this reader uses 'Python' \code{'vtk'} package, and supports \code{'.vtk'},
#' \code{'.vtp'}, \code{'.pvtp'}, \code{'.vtu'}, \code{'.vtpb'} formats.
#' @param file,con file path to the \code{'VTK'} file, the format will be
#' inferred from the file extension (with default \code{'.vtk'})
#' @param name name of the geometry; default is the file name
#' @param transform for \code{io_read_vtk_polys}, an optional 4 by 4 matrix
#' indicating the vertex position to scanner \code{'RAS'} transform; default is
#' \code{NULL} (identity matrix), i.e. the vertex positions are used as-is.
#' For \code{io_write_vtk_polys}, since \code{'VTK'} files cannot store any
#' coordinate system information, this is the transform applied to the vertex
#' positions before writing; the value is either a 4 by 4 matrix, or the index
#' or name of the transform stored in the surface object (default is \code{1},
#' the first transform); use \code{NULL} to write the vertex positions as-is
#' @param x an \code{\link{imaging-surface}} object, or anything that can be
#' converted via \code{\link{as_ieegio_surface}}
#' @param binary for legacy \code{'.vtk'} file only, whether to store the
#' data as binary file or 'ASCII' plain text; default is true (binary).
#' @returns \code{io_read_vtk_polys} returns an \code{\link{imaging-surface}}
#' object, while \code{io_write_vtk_polys} writes the data to file and returns
#' the file path.
#' @details
#' \code{'VTK'} allows polygons with arbitrary number of nodes as well as
#' triangle strips, while \code{'ieegio'} surface objects only support
#' triangular faces. Therefore the mesh is triangulated automatically
#' whenever needed.
#'
#' \code{'VTK'} files cannot store the vertex position to scanner \code{'RAS'}
#' transform, therefore \code{io_write_vtk_polys} applies the transform to the
#' vertex positions before writing, and \code{io_read_vtk_polys} reads the
#' vertex positions as-is (with an identity transform).
#'
#' Vertex-wise attributes stored as point data are imported as well:
#' arrays with one component become \code{measurements}, and unsigned
#' character arrays with three or four components (i.e. \code{'RGB'} or
#' \code{'RGBA'} colors) become the vertex \code{color}. Other arrays, such as
#' vertex normals, and cell (face-wise) data are ignored.
#'
#' @examples
#'
#' # This example shows how to convert a `GIfTI` surface to `VTK`
#'
#' # run `ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")`
#' # to download sample data
#'
#' geom_file <- "gifti/GzipBase64/sujet01_Lwhite.surf.gii"
#'
#' if( requireNamespace("rpymat", quietly = TRUE) &&
#'     dir.exists(rpymat::env_path()) &&
#'     ieegio_sample_data(geom_file, test = TRUE) ) {
#'
#'   surface <- read_surface(ieegio_sample_data(geom_file))
#'
#'   # write to vtk
#'   tfile <- tempfile(fileext = ".vtk")
#'   io_write_vtk_polys(surface, con = tfile)
#'
#'   # read
#'   vtk_surface <- io_read_vtk_polys(tfile)
#'
#'   print(vtk_surface)
#'
#'   # 0 0
#'   range(surface$geometry$faces - vtk_surface$geometry$faces)
#'
#'   # the vertex positions are written in the transformed space
#'   # 0 0
#'   range(
#'     surface$geometry$transforms[[1]] %*% surface$geometry$vertices -
#'       vtk_surface$geometry$vertices
#'   )
#'
#'   unlink(tfile)
#'
#' }
#'
#'
#' @export
io_read_vtk_polys <- function(file, name = basename(file), transform = NULL) {
  # DIPSAUS DEBUG START
  # file <- normalizePath("~/Downloads/lh.pial.vtk")
  # name <- basename(file)
  # transform <- NULL

  file <- normalizePath(file, winslash = "/", mustWork = TRUE)

  ext <- strsplit(tolower(file), "\\.")[[1]]
  ext <- ext[[length(ext)]]

  reader_names <- switch(
    ext,
    "pvtp" = c("vtkXMLPPolyDataReader", "vtkXMLPolyDataReader",
               "vtkPolyDataReader"),
    "vtp" = c("vtkXMLPolyDataReader", "vtkPolyDataReader"),
    "vtu" = c("vtkXMLUnstructuredGridReader", "vtkUnstructuredGridReader",
              "vtkDataSetReader"),
    "h5" = c("vtkHDFReader", "vtkPolyDataReader"),
    "vtkhdf" = c("vtkHDFReader", "vtkPolyDataReader"),
    "vtpb" = c("vtkHDFReader", "vtkPolyDataReader"),
    {
      # `vtkDataSetReader` handles all the legacy data set types, and it does
      # not print errors when the data set is not polygonal
      c("vtkDataSetReader", "vtkPolyDataReader", "vtkXMLPolyDataReader",
        "vtkHDFReader")
    }
  )

  ensure_py_package("vtk")
  vtk <- rpymat::import("vtk")

  reader_valid <- FALSE
  dataset <- NULL
  # `VTK` releases the output data once the reader is garbage-collected, hence
  # the reader must be kept alive until the data are extracted
  reader_used <- NULL
  lapply(reader_names, function(reader_name) {
    if (reader_valid) { return() }
    tryCatch({
      reader <- vtk[[reader_name]]()
      reader$SetFileName(file)
      reader$Update()
      output <- reader$GetOutput()
      # Readers do not always raise errors when the data set type mismatches;
      # instead they silently return empty data sets
      if (as.integer(py_to_r(output$GetNumberOfPoints())) == 0L) {
        stop("No vertex node is found in the data set")
      }
      dataset <<- output
      reader_used <<- reader
      reader_valid <<- TRUE
    }, error = function(e) {
    })
    # free up
    reader <- NULL
    return()
  })

  if (!reader_valid) {
    stop("Unable to read the VTK file using the following readers: ",
         paste(sprintf("`vtk.%s`", reader_names), collapse = ", "))
  }

  # Extract the surface when the data set is not polygonal (for example,
  # unstructured grids read from legacy `.vtk` or `.vtu` files); `IsA` returns
  # an integer instead of a logical
  if (isTRUE(as.logical(py_to_r(dataset$IsA("vtkPolyData"))))) {
    polydata <- dataset
  } else {
    geometry_filter <- vtk$vtkGeometryFilter()
    geometry_filter$SetInputData(dataset)
    geometry_filter$Update()
    polydata <- geometry_filter$GetOutput()

    if (as.integer(py_to_r(polydata$GetNumberOfPoints())) == 0L) {
      # The data set contains no cell (i.e. point cloud); the geometry filter
      # discards everything, so copy the vertex nodes over directly
      tryCatch({
        point_cloud <- vtk$vtkPolyData()
        point_cloud$SetPoints(dataset$GetPoints())
        polydata <- point_cloud
      }, error = function(e) {
      })
    }
  }

  # `ieegio` surfaces only support triangular faces, hence triangulate the
  # polygons and triangle strips when necessary
  n_polys <- as.integer(py_to_r(polydata$GetNumberOfPolys()))
  n_strips <- as.integer(py_to_r(polydata$GetNumberOfStrips()))
  # `IsHomogeneous` returns the common cell size, or -1 when the cells vary
  cell_size <- as.integer(py_to_r(polydata$GetPolys()$IsHomogeneous()))
  if (n_strips > 0L || (n_polys > 0L && !isTRUE(cell_size == 3L))) {
    triangle_filter <- vtk$vtkTriangleFilter()
    triangle_filter$SetInputData(polydata)
    triangle_filter$Update()
    polydata <- triangle_filter$GetOutput()
    n_polys <- as.integer(py_to_r(polydata$GetNumberOfPolys()))
  }

  vtk_to_numpy <- vtk$util$numpy_support$vtk_to_numpy

  # vertex node positions
  if (as.integer(py_to_r(polydata$GetNumberOfPoints())) == 0L) {
    stop("The `VTK` file does not contain any vertex node.")
  }
  vertices <- py_to_r(vtk_to_numpy(polydata$GetPoints()$GetData()))
  vertices <- vertices[, c(1, 2, 3), drop = FALSE]
  n_verts <- nrow(vertices)

  # face indices: legacy layout is `3 a b c 3 a b c ...` (zero-indexed)
  faces <- NULL
  if (n_polys > 0L) {
    poly_indices <- py_to_r(vtk_to_numpy(polydata$GetPolys()$GetData()))
    if (length(poly_indices) != n_polys * 4L) {
      stop("Unable to parse the `VTK` polygons: the mesh is expected to be ",
           "triangulated but the face index length is inconsistent.")
    }
    faces <- matrix(as.integer(poly_indices), nrow = 4L)
    if (!all(faces[1, ] == 3L)) {
      stop("Unable to parse the `VTK` polygons: the mesh is expected to be ",
           "triangulated but some faces have more or less than 3 nodes.")
    }
    faces <- t(faces[c(2, 3, 4), , drop = FALSE])
  }

  # vertex-wise attributes (point data)
  point_data <- polydata$GetPointData()
  n_arrays <- as.integer(py_to_r(point_data$GetNumberOfArrays()))

  parse_point_array <- function(index) {
    arr <- point_data$GetArray(as.integer(index))
    n_comp <- as.integer(py_to_r(arr$GetNumberOfComponents()))
    n_tuples <- as.integer(py_to_r(arr$GetNumberOfTuples()))
    if (n_tuples != n_verts) { return(NULL) }
    arr_name <- py_to_r(arr$GetName())
    if (!length(arr_name) || is.na(arr_name) || !nzchar(arr_name)) {
      arr_name <- sprintf("V%d", index + 1L)
    }
    list(
      name = arr_name,
      n_comp = n_comp,
      data_type = py_to_r(arr$GetDataTypeAsString()),
      value = py_to_r(vtk_to_numpy(arr))
    )
  }

  point_arrays <- lapply(seq_len(n_arrays) - 1L, function(index) {
    tryCatch({
      parse_point_array(index)
    }, error = function(e) { NULL })
  })
  point_arrays <- point_arrays[!vapply(point_arrays, is.null, FALSE)]

  # unsigned character arrays with 3 or 4 components are vertex colors
  color_arrays <- Filter(function(item) {
    isTRUE(item$n_comp %in% c(3L, 4L)) &&
      isTRUE(item$data_type == "unsigned char")
  }, point_arrays)
  vertex_colors <- NULL
  if (length(color_arrays)) {
    color_value <- color_arrays[[1]]$value
    vertex_colors <- grDevices::rgb(
      red = color_value[, 1],
      green = color_value[, 2],
      blue = color_value[, 3],
      maxColorValue = 255
    )
  }

  # single-component arrays are continuous measurements
  measurement_arrays <- Filter(function(item) {
    isTRUE(item$n_comp == 1L)
  }, point_arrays)
  measurements <- NULL
  if (length(measurement_arrays)) {
    measurement_names <- make.unique(vapply(
      measurement_arrays,
      function(item) { item$name },
      ""
    ))
    measurements <- data.frame(
      structure(
        names = measurement_names,
        lapply(measurement_arrays, function(item) { as.numeric(item$value) })
      ),
      check.names = FALSE
    )
  }

  # all data are extracted, free up the reader
  reader_used <- NULL

  surface <- as_ieegio_surface.default(
    vertices = vertices,
    faces = faces,
    face_start = 0L,
    transform = transform,
    vertex_colors = vertex_colors,
    measurements = measurements,
    name = name
  )

  # `VTK` files do not carry any coordinate system information; use an
  # identity transform with unknown spaces, consistent with `io_read_fs`
  if (!is.matrix(transform) && length(surface$geometry)) {
    surface$geometry$transforms <- list(
      Unknown = structure(
        diag(1, 4),
        source_space = "Unknown",
        target_space = "Unknown"
      )
    )
  }

  surface
}

#' @rdname io-vtk-polys
#' @export
io_write_vtk_polys <- function(x, con, binary = TRUE, transform = 1) {
  con <- path_expand(con)
  binary <- as.logical(binary)[[1]]

  if (!inherits(x, "ieegio_surface")) {
    x <- as_ieegio_surface(x = x)
  }
  if (!inherits(x, "ieegio_surface_contains_geometry")) {
    stop("`x` must be an `ieegio` surface object containing geometry")
  }

  # `VTK` has no notion of sparse node index; make sure the vertex nodes and
  # their attributes are dense before writing
  if (isTRUE(x$sparse)) {
    x <- sparse_to_dense_geometry(x)
  }

  vertices <- x$geometry$vertices
  if (nrow(vertices) == 3) {
    vertices <- rbind(vertices, 1)
  }

  # `VTK` files carry no coordinate system information, hence the vertex
  # positions must be written in the transformed (target) space
  if (is.matrix(transform)) {
    stopifnot(nrow(transform) == 4 && ncol(transform) == 4)
  } else if (is.null(transform) || !length(x$geometry$transforms)) {
    transform <- diag(1, 4)
  } else {
    transform <- x$geometry$transforms[[transform]]
    if (!is.matrix(transform)) {
      transform <- diag(1, 4)
    }
  }

  points <- t(transform %*% vertices)[, c(1, 2, 3), drop = FALSE]
  dimnames(points) <- NULL
  storage.mode(points) <- "double"
  n_verts <- nrow(points)

  # `VTK` face indices are zero-indexed
  faces <- x$geometry$faces
  n_faces <- 0L
  face_indices <- NULL
  if (length(faces)) {
    face_start <- x$geometry$face_start
    if (length(face_start) != 1 || is.na(face_start) ||
        !is.numeric(face_start)) {
      face_start <- min(faces, na.rm = TRUE)
    }
    faces <- faces[c(1, 2, 3), , drop = FALSE] - face_start
    n_faces <- ncol(faces)
    face_indices <- as.integer(rbind(3L, faces))
  }

  ensure_py_package("vtk")
  vtk <- rpymat::import("vtk")

  numpy_to_vtk <- vtk$util$numpy_support$numpy_to_vtk
  numpy_to_vtkIdTypeArray <- vtk$util$numpy_support$numpy_to_vtkIdTypeArray

  # Create vtkPoints
  vtk_points <- vtk$vtkPoints()
  vtk_points$SetData(numpy_to_vtk(points, deep = TRUE))

  # Create PolyData
  polydata <- vtk$vtkPolyData()
  polydata$SetPoints(vtk_points)

  # Create vtkCellArray for polygons
  if (n_faces > 0L) {
    vtk_polys <- vtk$vtkCellArray()
    faces_py <- rpymat::r_to_py(matrix(face_indices, nrow = 1))
    faces_py <- faces_py$astype("int64")

    vtk_face_array <- numpy_to_vtkIdTypeArray(faces_py, deep = TRUE)
    vtk_polys$SetCells(n_faces, vtk_face_array)
    polydata$SetPolys(vtk_polys)
  }

  point_data <- polydata$GetPointData()

  # vertex colors, stored as unsigned character `RGB` array
  if (length(x$color)) {
    color_value <- x$color[, c(1, 2, 3), drop = FALSE]
    storage.mode(color_value) <- "double"
    max_value <- max(color_value, na.rm = TRUE)
    if (is.finite(max_value) && max_value <= 1.1) {
      color_value <- color_value * 255
    }
    color_value[is.na(color_value) | color_value < 0] <- 0
    color_value[color_value > 255] <- 255
    dimnames(color_value) <- NULL

    color_py <- rpymat::r_to_py(round(color_value))
    color_py <- color_py$astype("uint8")
    color_array <- numpy_to_vtk(color_py, deep = TRUE,
                                array_type = vtk$VTK_UNSIGNED_CHAR)
    color_array$SetName("RGB")
    point_data$SetScalars(color_array)
  }

  # measurements, stored as single-component double arrays
  measurement_table <- x$measurements$data_table
  if (length(measurement_table)) {
    for (measurement_name in names(measurement_table)) {
      values <- as.double(measurement_table[[measurement_name]])
      if (length(values) == n_verts) {
        measurement_array <- numpy_to_vtk(values, deep = TRUE)
        measurement_array$SetName(measurement_name)
        point_data$AddArray(measurement_array)
      }
    }
  }

  if (endsWith(tolower(con), "vtp")) {
    writer <- vtk$vtkXMLPolyDataWriter()
  } else if (endsWith(tolower(con), "h5") ||
             endsWith(tolower(con), "vtpb")) {
    writer <- vtk$vtkHDFWriter()
  } else {
    writer <- vtk$vtkPolyDataWriter()
    if (binary) {
      writer$SetFileTypeToBinary()
    } else {
      writer$SetFileTypeToASCII()
    }
  }
  writer$SetFileName(con)
  writer$SetInputData(polydata)

  dname <- dirname(con)
  if (!dir_exists(dname)) {
    dir_create(dname)
  }
  writer$Write()

  invisible(con)
}


#
# # Step 4: Save to file (vtk or vtp)
# poly.save(filename)  # auto-detects .vtk or .vtp
#
# return poly
# pyvista$Line()
