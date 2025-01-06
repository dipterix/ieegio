helper_r3js_render_mesh <- function(mesh, col = "grey") {
  ensure_r_package("r3js")
  xyzlim <- apply(mesh$vb, 1L, range, na.rm = TRUE)
  r3plot <- r3js::plot3js(
    xlim = xyzlim[, 1],
    ylim = xyzlim[, 2],
    zlim = xyzlim[, 3],
    aspect = c(1, 1, 1),
    label_axes = FALSE,
    draw_box = FALSE,
    draw_grid = FALSE
  )
  # add light
  r3plot <- r3js::light3js(r3plot, type = "ambient", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(1, 0, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(-1, 0, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 1, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, -1, 0), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 0, 1), type = "directional", intensity = 0.3)
  r3plot <- r3js::light3js(r3plot, position = c(0, 0, -1), type = "directional", intensity = 0.3)
  r3plot <- r3js::shape3js(
    r3plot,
    vertices = t(mesh$vb[1:3, , drop = FALSE]),
    faces = t(mesh$it[1:3, , drop = FALSE]),
    col = col,
    mat = "phong"
  )
  r3plot
}
