# Experimental function to write STL format
write_binary_stl <- function(surf, file) {

  if(!inherits(surf, "ieegio_surface")) {
    surf <- as_ieegio_surface(x = surf)
  }
  if(!inherits(surf, "ieegio_surface_contains_geometry")) {
    stop("surface is not a valid `ieegio_surface` object")
  }

  # Extract vertices (first 3 rows of vb) and triangle indices
  verts <- surf$geometry$vertices[1:3, , drop = FALSE]            # 3 × Nverts
  tris  <- surf$geometry$faces + (1L - surf$geometry$face_start)  # 3 × Ntris (1-based indices into verts)
  n_tris <- ncol(tris)

  # Gather triangle corners
  v1 <- verts[, tris[1, ], drop = FALSE]
  v2 <- verts[, tris[2, ], drop = FALSE]
  v3 <- verts[, tris[3, ], drop = FALSE]

  # Compute facet normals via cross-product
  e1 <- v2 - v1
  e2 <- v3 - v1
  nx <- e1[2,]*e2[3,] - e1[3,]*e2[2,]
  ny <- e1[3,]*e2[1,] - e1[1,]*e2[3,]
  nz <- e1[1,]*e2[2,] - e1[2,]*e2[1,]
  lengths <- sqrt(nx^2 + ny^2 + nz^2)
  # normalize, avoid division by zero
  nonzero <- lengths > 0
  nx[nonzero] <- nx[nonzero] / lengths[nonzero]
  ny[nonzero] <- ny[nonzero] / lengths[nonzero]
  nz[nonzero] <- nz[nonzero] / lengths[nonzero]

  # Prepare binary header (80 bytes)
  header <- paste0("Created by ravetools::write_binary_stl_mesh3d on ", Sys.Date())
  hdr_raw <- charToRaw(substr(sprintf("%-80s", header), 1, 80))

  # Open file and write
  con <- file(file, "wb")
  con_is_closed <- FALSE
  on.exit({
    if(!con_is_closed) {
      close(con)
    }
  })

  # 1) 80-byte header
  writeBin(hdr_raw, con, useBytes = TRUE)
  # 2) number of triangles (uint32 little-endian)
  writeBin(as.integer(n_tris), con, size = 4, endian = "little")
  # 3) for each triangle: normal (3 × float32), v1/v2/v3 (9 × float32)
  all_floats <- rbind(nx, ny, nz, v1, v2, v3)

  apply(all_floats, 2, function(x) {
    writeBin(as.numeric(x), con, size = 4, endian = "little")
    writeBin(0L, con, size = 2, endian = "little")
  })

  close(con)
  con_is_closed <- TRUE
  invisible(con)
}

