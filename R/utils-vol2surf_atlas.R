
volume_to_surface_atlas <- function(surface, volume, colormap = NULL, max_dist = 1, atlas_name = "Atlas") {
  # DIPSAUS DEBUG START
  # surface <- "/Users/dipterix/Library/Application Support/org.R-project.R/R/rpyANTs/templates/nmt_v2_0_acpc_asym/surf/lh.pial"
  # surface <- as_ieegio_surface(surface)
  # rpyANTs::as_ANTsTransform("/Users/dipterix/Library/Application Support/org.R-project.R/R/rpyANTs/templates/nmt_v2_0_acpc_asym/NMT_to_ACPC.mat")[] -> tmp
  # surface <- ieegio::as_ieegio_surface(
  #   diag(c(-1,-1,1,1)) %*% tmp %*% diag(c(-1,-1,1,1)) %*% diag(c(0.5, 0.5, 0.5, 1)) %*% surface$geometry$transforms[[1]] %*% surface$geometry$vertices |> t(),
  #   faces = surface$geometry$faces |> t(),
  #   face_start = surface$geometry$face_start
  # )
  # volume <- "~/Dropbox (Personal)/projects/macaque/NMT_v2.0_asym/NMT_v2.0_asym/supplemental_CHARM/CHARM_6_in_NMT_v2.0_asym.nii.gz"
  # atlas_name = "Atlas"
  # max_dist = 1
  # ktbl <- read.table("~/Dropbox (Personal)/projects/macaque/NMT_v2.0_asym/tables_CHARM/CHARM_key_all.txt", header = TRUE)
  # ctbl <- read.csv("~/Dropbox (Personal)/projects/macaque/NMT_v2.0_asym/tables_CHARM/hue_CHARM_cmap.pal", header = TRUE)
  # ctbl <- ctbl[[1]][seq_len(nrow(ktbl))]
  # tmp <- col2rgb(ctbl)
  # tbl <- data.frame(
  #   Key = ktbl$Index,
  #   Label = ktbl$Full_Name,
  #   R = tmp[1, ],
  #   G = tmp[2, ],
  #   B = tmp[3, ]
  # )
  # colormap <- as_ieegio_colormap(tbl)

  surface <- as_ieegio_surface(surface)
  volume <- as_ieegio_volume(volume)

  # Sanity check
  # plot(volume, position = c(0, 0, 20), which = "axial", zoom = 3)
  # ravetools::plot_mesh_polygon(surface, eye = c(0, 0, 100), lookat = c(0, 0, 20), col = "red",
  #                              up = c(0, 1, 0), add = TRUE, clipping_plane = c(0, 0, 1, 20, 0), alpha = 0.5)

  if (is.null(colormap)) {
    # Use FreeSurfer colormap
    fs_ctable <- fs_lut()
    ctable <- as_ieegio_colortable(fs_ctable)
    lut <- as_ieegio_lookup(data.frame(Key = fs_ctable$ColorID, Label = fs_ctable$Label))
    colormap <- as_ieegio_colormap(ctable, lookup = lut)
  }

  volume_array <- volume[]
  dm <- dim(volume)

  if (length(dm) > 3) {
    dm <- dm[seq_len(3)]
    volume_array <- array(volume_array[seq_len(prod(dm))], dim = dm)
  }

  idx <- which(volume_array > 0)

  ijk <- arrayInd(idx, dm) - 1L

  vol_ras <- volume$transforms$vox2ras %*% rbind(t(ijk), 1)
  vol_ras <- t(vol_ras[seq_len(3), , drop = FALSE])
  vol_key <- volume_array[idx]

  rm(volume_array)

  surf_transmat <- surface$geometry$transforms[[1]]
  if (length(surf_transmat) != 16) {
    surf_transmat <- diag(1, 4)
  }
  surf_ras <- surf_transmat %*% surface$geometry$vertices
  surf_ras <- t(surf_ras[seq_len(3), , drop = FALSE])

  kdtree <- ravetools::vcg_kdtree_nearest(
    target = vol_ras, query = surf_ras, k = 1L)

  surf_key <- vol_key[kdtree$index]
  surf_key[kdtree$distance > max_dist] <- 0L

  res <- as_ieegio_surface.default(
    x = t(surface$geometry$vertices),
    faces = t(surface$geometry$faces),
    annotation_labels = data.frame(
      Key = colormap$lookup$lookup_table$Key,
      Label = colormap$lookup$lookup_table$Label,
      Color = calculate_color(colormap$lookup$lookup_table$Key, colormap = colormap)
    ),
    annotation_values = as.data.frame(structure(list(surf_key), names = atlas_name))
  )

  return(res)

}
