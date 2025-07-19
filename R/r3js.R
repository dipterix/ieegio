helper_r3js_render_mesh <- function(mesh, col = "grey", r3plot = NULL) {
  ensure_r_package("r3js")
  xyzlim <- apply(mesh$vb, 1L, range, na.rm = TRUE)

  if(is.null(r3plot)) {
    r3plot <- r3js::plot3js(
      xlim = xyzlim[, 1],
      ylim = xyzlim[, 2],
      zlim = xyzlim[, 3],
      aspect = c(1, 1, 1),
      label_axes = FALSE,
      draw_box = FALSE,
      draw_grid = FALSE
    )
  }

  # add light
  r3plot <- r3js::light3js(r3plot, type = "ambient", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(1, 0, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(-1, 0, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 1, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, -1, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 0, 1), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 0, -1), type = "directional", intensity = 0.3)
  if(is.matrix(mesh$it)) {
    r3plot <- r3js::shape3js(
      r3plot,
      vertices = t(mesh$vb[1:3, , drop = FALSE]),
      faces = t(mesh$it[1:3, , drop = FALSE]),
      col = col,
      mat = "phong"
    )
  } else {
    r3plot <- r3js::points3js(
      r3plot,
      x = mesh$vb[1, ],
      y = mesh$vb[2, ],
      z = mesh$vb[3, ],
      col = col,
      mat = "phong"
    )
  }

  r3plot
}


helper_r3js_plot <- function(r3plot, rotation = c(0, 0, 0), camera = "orthographic", ...) {
  if("camera" %in% names(formals(r3js::r3js))) {
    r3js::r3js(r3plot, rotation = rotation, camera = camera, ...)
  } else {
    r3js::r3js(r3plot, rotation = rotation, ...)
  }
}


helper_r3js_render_streamlines <- function(streamlines, col = "grey", r3plot = NULL) {
  ensure_r_package("r3js")

  n <- length(streamlines$data)

  if(is.null(r3plot)) {
    ranges <- sapply(seq_len(n), function(i) {
      coords <- streamlines[[ i ]]$coords
      apply(coords, 2, range, na.rm = TRUE)[, 1:3]
    })


    r3plot <- r3js::plot3js(
      aspect = c(1, 1, 1),
      label_axes = FALSE,
      draw_box = FALSE,
      draw_grid = FALSE,
      xlim = c(min(ranges[1, ]), max(ranges[2, ])),
      ylim = c(min(ranges[3, ]), max(ranges[4, ])),
      zlim = c(min(ranges[5, ]), max(ranges[6, ]))
    )
  }

  # add light
  r3plot <- r3js::light3js(r3plot, type = "ambient", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(1, 0, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(-1, 0, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 1, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, -1, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 0, 1), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 0, -1), type = "directional", intensity = 0.3)


  for(i in seq_len(n)) {
    coords <- streamlines[[ i ]]$coords
    r3plot <- r3js::lines3js(r3plot, x = coords[, 1], y = coords[, 2], z = coords[, 3], col = col, geometry = FALSE, lwd = 1)
  }
  r3plot
}
