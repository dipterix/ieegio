test_that("resample_volume", {

  ravetools <- ieegio:::check_ravetools_flag()
  testthat::skip_if(isFALSE(ravetools) || !is.function(ravetools$resample_3d_volume))

  dm <- c(6, 31, 23)

  arr <- array(seq_len(prod(dm)) + 0.5, dm)
  vox2ras_orig <- cbind(diag(1, nrow = 4, ncol = 3), c(-dm / 2, 1))

  dim_new <- c(3, 3, 3)

  vox2ras_new <- resample_vox2ras(vox2ras = vox2ras_orig, old_dim = dm, new_dim = dim_new)
  orig <- as_ieegio_volume(arr, vox2ras = vox2ras_orig)

  # resample
  downsampled_naive <- resample_volume_naive(orig, new_dim = dim_new)
  downsampled_ravetools <- resample_volume_ravetools(orig, new_dim = dim_new)

  idx <- c(0, 0, 0, 1)
  solve(vox2ras_orig) %*% vox2ras_new %*% idx

  expect_equal(downsampled_naive[], downsampled_ravetools[])

  # up-sample on coronal
  upsampled_naive <- resample_volume_naive(orig, new_dim = c(20, 20, 24))
  upsampled_ravetools <- resample_volume_ravetools(orig, new_dim = c(20, 20, 24))
  expect_equal(upsampled_naive[], upsampled_ravetools[])
})
