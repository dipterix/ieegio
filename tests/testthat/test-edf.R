test_that("read_edf", {

  testthat::skip_on_cran()
  testthat::skip_if_not(ieegio_sample_data('edfPlusD.edf', test = TRUE))
  testthat::skip_if_not(dir.exists(rpymat::env_path()))

  edf_path <- ieegio_sample_data("edfPlusD.edf")

  data <- read_edf(edf_path, extract_path = tempdir())

  mne <- rpymat::import("mne")
  dmne <- mne$io$read_raw_edf(edf_path)

  labels <- data$channel_table$Label[!data$channel_table$Annotation]
  testthat::expect_setequal(
    labels,
    rpymat::py_to_r(dmne$ch_names)
  )


  for(label in labels) {
    chan <- data$get_channel(label)
    cmne <- as.double(rpymat::py_to_r(dmne$get_data(label, units = "uV")))

    testthat::expect_equal(
      chan$value, cmne, tolerance = 1e-4
    )
  }

  dmne$close()
  data$delete()
})



test_that("write_edf", {


  signal <- sin(seq(0, 10, 0.01))

  channels <- list(

    # signal
    as_edf_channel(channel_num = 1, signal,
                   sample_rate = 375.5),

    as_edf_channel(channel_num = 2, -signal,
                   sample_rate = 375.5),

    # annotation
    as_edf_channel(channel_num = 3, data.frame(
      timestamp = c(0, 1, 2),
      comments = c("Start", "half to go", "Finish!")
    ))

  )

  # write to file
  path <- tempfile(fileext = ".edf")
  write_edf(con = path, channels = channels, record_duration = 1.5)

  edf <- read_edf(con = path, cache_ok = FALSE, extract_path = tempdir())

  on.exit({
    edf$delete()
    unlink(path)
  }, add = TRUE, after = FALSE)

  annot <- edf$get_annotations()

  testthat::expect_setequal(trimws(annot$comments), c("Start", "half to go", "Finish!"))

  # around 1e-5 due to digitization
  ch1 <- edf$get_channel(1)
  testthat::expect_equal(ch1$value[seq_along(signal)], signal, tolerance = 1e-4)

  ch2 <- edf$get_channel(2)
  testthat::expect_equal(ch2$value[seq_along(signal)], -signal, tolerance = 1e-4)

  testthat::skip_on_cran()
  testthat::skip_if_not(dir.exists(rpymat::env_path()))

  mne <- rpymat::import("mne")
  dmne <- mne$io$read_raw_edf(path)

  cmne <- as.double(rpymat::py_to_r(dmne$get_data("Ch1", units = "uV")))
  testthat::expect_equal(cmne[seq_along(signal)], signal, tolerance = 1e-4)

  cmne <- as.double(rpymat::py_to_r(dmne$get_data("Ch2", units = "uV")))
  testthat::expect_equal(cmne[seq_along(signal)], -signal, tolerance = 1e-4)

})
