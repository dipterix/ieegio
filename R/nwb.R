#' @title Read 'NWB' format
#' @description
#' Life cycle: experimental.
#' Read "Neurodata Without Borders" ('NWB' format) file. Unlike other readers
#' \code{read_nwb} returns low-level 'Python' class handler via \code{pynwb}
#' module.
#'
#' @param file path to 'NWB' file
#' @param mode file open mode; default is \code{'r'} (read-only)
#' @param ... passed to \code{\link{NWBHDF5IO}} initialize function
#' @returns A \code{\link{NWBHDF5IO}} instance
#' @examples
#'
#'
#' if(ieegio_sample_data("nwb_sample.nwb", test = TRUE)) {
#'   file <- ieegio_sample_data("nwb_sample.nwb")
#'
#'   # Create NWBIO container
#'   container <- read_nwb(file)
#'
#'   # Open connection
#'   container$open()
#'
#'   # read meta data
#'   data <- container$read()
#'   data
#'
#'   # get `test_timeseries` data
#'   ts_data <- data$get_acquisition("test_timeseries")
#'   ts_data
#'
#'   # read timeseries data into memory
#'   ts_arr <- ts_data$data[]
#'   ts_arr
#'
#'   # Convert Python array to R
#'   # using `rpymat::py_to_r(ts_arr)` or
#'   as.numeric(ts_arr)
#'
#'   # Make sure you close the connection
#'   container$close()
#'
#' }
#'
#'
#' # Requires setting up Python environment
#' # run `ieegio::install_pynwb()` to set up environment first
#'
#' \dontrun{
#'
#'
#' # Replicating tutorial
#' # https://pynwb.readthedocs.io/en/stable/tutorials/general/plot_file.html
#'
#' library(rpymat)
#'
#' # Load Python module
#' pynwb <- import("pynwb")
#' uuid <- import("uuid")
#' datetime <- import("datetime")
#' np <- import("numpy")
#' tz <- import("dateutil.tz")
#'
#'
#'
#' # 2018L is 2018 as integer
#' session_start_time <- datetime$datetime(
#'   2018L, 4L, 25L, 2L, 30L, 3L,
#'   tzinfo=tz$gettz("US/Pacific"))
#'
#' #  ---- Create NWB file object ------------------------
#' nwbfile <- pynwb$NWBFile(
#'   session_description="Mouse exploring a closed field",
#'   identifier=py_str(uuid$uuid4()),
#'   session_start_time=session_start_time,
#'   session_id="session_4321",
#'   experimenter=py_list(c("Baggins, Frodo")),
#'   lab="Bag End Laboratory",
#'   institution="University of Middle Earth at the Shire",
#'   experiment_description="Thank you Bilbo Baggins.",
#'   keywords=py_list(c("behavior", "exploration"))
#' )
#'
#' # ---- Add subject ------------------------------------
#' subject <- pynwb$file$Subject(
#'   subject_id="001",
#'   age="P90D",
#'   description="mouse 5",
#'   species="Mus musculus",
#'   sex="M"
#' )
#'
#' nwbfile$subject <- subject
#'
#' nwbfile
#'
#' # ---- Add TimeSeries ------------------------------------
#' data <- seq(100, 190, by = 10)
#' time_series_with_rate <- pynwb$TimeSeries(
#'   name="test_timeseries",
#'   description="an example time series",
#'   data=data,
#'   unit="m",
#'   starting_time=0.0,
#'   rate=1.0
#' )
#' time_series_with_rate
#'
#' nwbfile$add_acquisition(time_series_with_rate)
#'
#' # ---- New Spatial positions ------------------------------------
#' position_data <- cbind(
#'   seq(0, 10, length.out = 50),
#'   seq(0, 9, length.out = 50)
#' )
#' position_timestamps = seq(0, 49) / 200
#'
#' spatial_series_obj = pynwb$behavior$SpatialSeries(
#'   name="SpatialSeries",
#'   description="(x,y) position in open field",
#'   data=position_data,
#'   timestamps=position_timestamps,
#'   reference_frame="(0,0) is bottom left corner",
#' )
#' spatial_series_obj
#'
#' position_obj = pynwb$behavior$Position(
#'   spatial_series=spatial_series_obj)
#' position_obj
#'
#' # ---- Behavior Processing Module ------------------------------
#' behavior_module <- nwbfile$create_processing_module(
#'   name="behavior", description="processed behavioral data"
#' )
#' behavior_module$add(position_obj)
#'
#' nwbfile$processing$behavior
#'
#' # omit some process
#'
#' # ---- Write ---------------------------------------------------
#' f <- normalizePath(tempfile(fileext = ".nwb"),
#'                    winslash = "/",
#'                    mustWork = FALSE)
#' io <- pynwb$NWBHDF5IO(f, mode = "w")
#' io$write(nwbfile)
#' io$close()
#'
#'
#'
#' }
#'
#'
#' @export
read_nwb <- function(file, mode = c("r", "w", "r+", "a", "w-", "x"), ...) {
  mode <- match.arg(mode)
  NWBHDF5IO$new(path = file, mode = mode, ...)
}
