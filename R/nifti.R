NIFTI_TYPES <- list(
  NIFTI_TYPE_UINT8 =            2L,
  # /*! signed short. */
  NIFTI_TYPE_INT16 =            4L,
  # /*! signed int. */
  NIFTI_TYPE_INT32 =            8L,
  # /*! 32 bit float. */
  NIFTI_TYPE_FLOAT32 =         16L,
  # /*! 64 bit complex = 2 32 bit floats. */
  NIFTI_TYPE_COMPLEX64 =       32L,
  # /*! 64 bit float = double. */
  NIFTI_TYPE_FLOAT64 =         64L,
  # /*! 3 8 bit bytes. */
  NIFTI_TYPE_RGB24 =          128L,
  # /*! signed char. */
  NIFTI_TYPE_INT8 =           256L,
  # /*! unsigned short. */
  NIFTI_TYPE_UINT16 =         512L,
  # /*! unsigned int. */
  NIFTI_TYPE_UINT32 =         768L,
  # /*! signed long long. */
  NIFTI_TYPE_INT64 =         1024L,
  # /*! unsigned long long. */
  NIFTI_TYPE_UINT64 =        1280L,
  # /*! 128 bit float = long double. */
  NIFTI_TYPE_FLOAT128 =      1536L,
  # /*! 128 bit complex = 2 64 bit floats. */
  NIFTI_TYPE_COMPLEX128 =    1792L,
  # /*! 256 bit complex = 2 128 bit floats */
  NIFTI_TYPE_COMPLEX256 =    2048L,
  # /*! 4 8 bit bytes. */
  NIFTI_TYPE_RGBA32 =        2304L
)

as_nifti_type <- function(x) {
  stopifnot(length(x) == 1L)
  x_ <- x
  if(is.character(x)) {
    x <- NIFTI_TYPES[[x]]
  } else {
    x <- as.integer(x)
  }
  if(!isTRUE(x %in% NIFTI_TYPES)) {
    stop("Unknown NIfTI datatype: ", x_)
  }
  x
}

compute_nifti_bitpix <- function(datatype) {
  datatype <- as_nifti_type(datatype)

  switch(
    as.character(datatype),
    "2" = 8L,
    "4" = 16L,
    "8" = 32L,
    "16" = 32L,
    "32" = 64L,
    "64" = 64L,

    # NIFTI_TYPE_RGB24 =          128L,
    "128" = 24L,

    "256" = 8L,
    "512" = 16L,
    "768" = 32L,
    "1024" = 64L,
    "1280" = 64L,

    # /*! 128 bit float = long double. */
    "1536" = 128L,

    # /*! 128 bit complex = 2 64 bit floats. */
    "1792" = 128L,

    # /*! 256 bit complex = 2 128 bit floats */
    "2048" = 256L,

    # /*! 4 8 bit RGBA bytes. */
    "2304" = 32L
  )
}

NIFTI_UNITS <- list(
  NIFTI_UNITS_UNKNOWN = 0L,
  # /*! NIFTI code for meters. */
  NIFTI_UNITS_METER =   1L,
  # /*! NIFTI code for millimeters. */
  NIFTI_UNITS_MM =      2L,
  # /*! NIFTI code for micrometers. */
  NIFTI_UNITS_MICRON =  3L,

  # /** Time codes are multiples of 8. **/
  # /*! NIFTI code for seconds. */
  NIFTI_UNITS_SEC =     8L,
  # /*! NIFTI code for milliseconds. */
  NIFTI_UNITS_MSEC =   16L,
  # /*! NIFTI code for microseconds. */
  NIFTI_UNITS_USEC =   24L,

  # /*** These units are for spectral data: ***/
  # /*! NIFTI code for Hertz. */
  NIFTI_UNITS_HZ =     32L,
  # /*! NIFTI code for ppm. */
  NIFTI_UNITS_PPM =    40L,
  # /*! NIFTI code for radians per second. */
  NIFTI_UNITS_RADS =   48L
)

as_nifti_unit <- function(x, strict = FALSE) {
  stopifnot(length(x) == 1L)
  if(is.character(x)) {
    x <- NIFTI_UNITS[[x]]
    if(!length(x)) {
      x <- 0L
    }
  } else {
    x <- as.integer(x)
    if(is.na(x) || ( strict && !isTRUE(x %in% NIFTI_UNITS) )) {
      x <- 0L
    }
  }
  x
}


NIFTI_INTENTS <- list(
  NIFTI_INTENT_NONE =         0L,
  NIFTI_INTENT_CORREL =       2L,

  # /*! [C2, chap 28] Student t statistic (1 param): p1 = DOF. */
  NIFTI_INTENT_TTEST =        3L,

  # /*! [C2, chap 27] Fisher F statistic (2 params):
  #      p1 = numerator DOF, p2 = denominator DOF. */

  NIFTI_INTENT_FTEST =        4L,

  # /*! [C1, chap 13] Standard normal (0 params): Density = N(0,1). */
  NIFTI_INTENT_ZSCORE =       5L,

  # /*! [C1, chap 18] Chi-squared (1 param): p1 = DOF.
  #     Density(x) proportional to exp(-x/2) * x^(p1/2-1). */

  NIFTI_INTENT_CHISQ =        6L,

  # /*! [C2, chap 25] Beta distribution (2 params): p1=a, p2=b.
  #     Density(x) proportional to x^(a-1) * (1-x)^(b-1). */

  NIFTI_INTENT_BETA =         7L,

  # /*! [U, chap 3] Binomial distribution (2 params):
  #      p1 = number of trials, p2 = probability per trial.
  #     Prob(x) = (p1 choose x) * p2^x * (1-p2)^(p1-x), for x=0,1,...,p1. */

  NIFTI_INTENT_BINOM =        8L,

  # /*! [C1, chap 17] Gamma distribution (2 params):
  #      p1 = shape, p2 = scale.
  #     Density(x) proportional to x^(p1-1) * exp(-p2*x). */

  NIFTI_INTENT_GAMMA =        9L,

  # /*! [U, chap 4] Poisson distribution (1 param): p1 = mean.
  #     Prob(x) = exp(-p1) * p1^x / x! , for x=0,1,2,.... */

  NIFTI_INTENT_POISSON =     10L,

  # /*! [C1, chap 13] Normal distribution (2 params):
  #      p1 = mean, p2 = standard deviation. */

  NIFTI_INTENT_NORMAL =      11L,

  # /*! [C2, chap 30] Noncentral F statistic (3 params):
  #      p1 = numerator DOF, p2 = denominator DOF,
  #      p3 = numerator noncentrality parameter.  */

  NIFTI_INTENT_FTEST_NONC =  12L,

  # /*! [C2, chap 29] Noncentral chi-squared statistic (2 params):
  #      p1 = DOF, p2 = noncentrality parameter.     */

  NIFTI_INTENT_CHISQ_NONC =  13L,

  # /*! [C2, chap 23] Logistic distribution (2 params):
  #      p1 = location, p2 = scale.
  #     Density(x) proportional to sech^2((x-p1)/(2*p2)). */

  NIFTI_INTENT_LOGISTIC =    14L,

  # /*! [C2, chap 24] Laplace distribution (2 params):
  #      p1 = location, p2 = scale.
  #     Density(x) proportional to exp(-abs(x-p1)/p2). */

  NIFTI_INTENT_LAPLACE =     15L,

  # /*! [C2, chap 26] Uniform distribution: p1 = lower end, p2 = upper end. */

  NIFTI_INTENT_UNIFORM =     16L,

  # /*! [C2, chap 31] Noncentral t statistic (2 params):
  #      p1 = DOF, p2 = noncentrality parameter. */

  NIFTI_INTENT_TTEST_NONC =  17L,

  # /*! [C1, chap 21] Weibull distribution (3 params):
  #      p1 = location, p2 = scale, p3 = power.
  #     Density(x) proportional to
  #      ((x-p1)/p2)^(p3-1) * exp(-((x-p1)/p2)^p3) for x > p1. */

  NIFTI_INTENT_WEIBULL =     18L,

  # /*! [C1, chap 18] Chi distribution (1 param): p1 = DOF.
  #     Density(x) proportional to x^(p1-1) * exp(-x^2/2) for x > 0.
  #      p1 = 1 = 'half normal' distribution
  #      p1 = 2 = Rayleigh distribution
  #      p1 = 3 = Maxwell-Boltzmann distribution.                  */

  NIFTI_INTENT_CHI =         19L,

  # /*! [C1, chap 15] Inverse Gaussian (2 params):
  #      p1 = mu, p2 = lambda
  #     Density(x) proportional to
  #      exp(-p2*(x-p1)^2/(2*p1^2*x)) / x^3  for x > 0. */

  NIFTI_INTENT_INVGAUSS =    20L,

  # /*! [C2, chap 22] Extreme value type I (2 params):
  #      p1 = location, p2 = scale
  #     cdf(x) = exp(-exp(-(x-p1)/p2)). */

  NIFTI_INTENT_EXTVAL =      21L,

  # /*! Data is a 'p-value' (no params). */

  NIFTI_INTENT_PVAL =        22L,

  # /*! Data is ln(p-value) (no params).
  #     To be safe, a program should compute p = exp(-abs(this_value)).
  #     The nifti_stats.c library returns this_value
  #     as positive, so that this_value = -log(p). */


  NIFTI_INTENT_LOGPVAL =     23L,

  # /*! Data is log10(p-value) (no params).
  #     To be safe, a program should compute p = pow(10.,-abs(this_value)).
  #     The nifti_stats.c library returns this_value
  #     as positive, so that this_value = -log10(p). */

  NIFTI_INTENT_LOG10PVAL =   24L,

  # /*! Smallest intent_code that indicates a statistic. */

  NIFTI_FIRST_STATCODE =      2L,

  # /*! Largest intent_code that indicates a statistic. */

  NIFTI_LAST_STATCODE =      24L,

  # /*---------- these values for intent_code aren't for statistics ----------*/

  # /*! To signify that the value at each voxel is an estimate
  # of some parameter, set intent_code = NIFTI_INTENT_ESTIMATE.
  # The name of the parameter may be stored in intent_name.     */

  NIFTI_INTENT_ESTIMATE =   1001L,

  #   /*! To signify that the value at each voxel is an index into
  # some set of labels, set intent_code = NIFTI_INTENT_LABEL.
  # The filename with the labels may stored in aux_file.        */

  NIFTI_INTENT_LABEL =      1002L,

  #   /*! To signify that the value at each voxel is an index into the
  # NeuroNames labels set, set intent_code = NIFTI_INTENT_NEURONAME. */

  NIFTI_INTENT_NEURONAME =  1003L,

  #   /*! To store an M x N matrix at each voxel:
  #   - dataset must have a 5th dimension (dim[0]=5 and dim[5]>1)
  # - intent_code must be NIFTI_INTENT_GENMATRIX
  # - dim[5] must be M*N
  # - intent_p1 must be M (in float format)
  # - intent_p2 must be N (ditto)
  # - the matrix values A[i][[j] are stored in row-order:
  #                            - A[0][0] A[0][1] ... A[0][N-1]
  #                          - A[1][0] A[1][1] ... A[1][N-1]
  #                          - etc., until
  #                          - A[M-1][0] A[M-1][1] ... A[M-1][N-1]        */

  NIFTI_INTENT_GENMATRIX =  1004L,

  #   /*! To store an NxN symmetric matrix at each voxel:
  #   - dataset must have a 5th dimension
  # - intent_code must be NIFTI_INTENT_SYMMATRIX
  # - dim[5] must be N*(N+1)/2
  # - intent_p1 must be N (in float format)
  # - the matrix values A[i][[j] are stored in row-order:
  #                            - A[0][0]
  #                          - A[1][0] A[1][1]
  #                          - A[2][0] A[2][1] A[2][2]
  #                          - etc.: row-by-row                           */

  NIFTI_INTENT_SYMMATRIX =  1005L,

  #   /*! To signify that the vector value at each voxel is to be taken
  # as a displacement field or vector:
  #   - dataset must have a 5th dimension
  # - intent_code must be NIFTI_INTENT_DISPVECT
  # - dim[5] must be the dimensionality of the displacement
  # vector (e.g., 3 for spatial displacement, 2 for in-plane) */

  NIFTI_INTENT_DISPVECT =   1006L, #   /* specifically for displacements */
  NIFTI_INTENT_VECTOR =     1007L, #   /* for any other type of vector */

  #   /*! To signify that the vector value at each voxel is really a
  # spatial coordinate (e.g., the vertices or nodes of a surface mesh):
  #   - dataset must have a 5th dimension
  # - intent_code must be NIFTI_INTENT_POINTSET
  # - dim[0] = 5
  # - dim[1] = number of points
  # - dim[2] = dim[3] = dim[4] = 1
  # - dim[5] must be the dimensionality of space (e.g., 3 => 3D space).
  # - intent_name may describe the object these points come from
  # (e.g., "pial", "gray/white" , "EEG", "MEG").                   */

  NIFTI_INTENT_POINTSET =   1008L,

  #   /*! To signify that the vector value at each voxel is really a triple
  # of indexes (e.g., forming a triangle) from a pointset dataset:
  #   - dataset must have a 5th dimension
  # - intent_code must be NIFTI_INTENT_TRIANGLE
  # - dim[0] = 5
  # - dim[1] = number of triangles
  # - dim[2] = dim[3] = dim[4] = 1
  # - dim[5] = 3
  # - datatype should be an integer type (preferably DT_INT32)
  # - the data values are indexes (0,1,...) into a pointset dataset. */

  NIFTI_INTENT_TRIANGLE =   1009L,

  #   /*! To signify that the vector value at each voxel is a quaternion:
  #   - dataset must have a 5th dimension
  # - intent_code must be NIFTI_INTENT_QUATERNION
  # - dim[0] = 5
  # - dim[5] = 4
  # - datatype should be a floating point type     */

  NIFTI_INTENT_QUATERNION =  1010L,

  #   /*! Dimensionless value - no params - although, as in _ESTIMATE
  # the name of the parameter may be stored in intent_name.     */

  NIFTI_INTENT_DIMLESS =     1011L,

  # /*---------- these values apply to GIFTI datasets ----------*/
  #
  # /*! To signify that the value at each location is from a time series. */

  NIFTI_INTENT_TIME_SERIES =   2001L,

  #   /*! To signify that the value at each location is a node index, from
  # a complete surface dataset.                                       */

  NIFTI_INTENT_NODE_INDEX =    2002L,

  #   /*! To signify that the vector value at each location is an RGB triplet,
  # of whatever type.
  # - dataset must have a 5th dimension
  # - dim[0] = 5
  # - dim[1] = number of nodes
  # - dim[2] = dim[3] = dim[4] = 1
  # - dim[5] = 3
  # */

  NIFTI_INTENT_RGB_VECTOR =    2003L,

  #   /*! To signify that the vector value at each location is a 4 valued RGBA
  # vector, of whatever type.
  # - dataset must have a 5th dimension
  # - dim[0] = 5
  # - dim[1] = number of nodes
  # - dim[2] = dim[3] = dim[4] = 1
  # - dim[5] = 4
  # */

  NIFTI_INTENT_RGBA_VECTOR =   2004L,

  #   /*! To signify that the value at each location is a shape value, such
  # as the curvature.  */

  NIFTI_INTENT_SHAPE =         2005L,

  #   /*! The following intent codes have been used by FSL FNIRT for
  # displacement/coefficient files.
  #
  # These codes are included to prevent clashes in community-created
  # extensions to NIfTI. Encoding and decoding behavior for these
  # intents is not specified by the standard, and support is OPTIONAL
  # for conforming implementations.
  # */

  NIFTI_INTENT_FSL_FNIRT_DISPLACEMENT_FIELD =        2006L,
  NIFTI_INTENT_FSL_CUBIC_SPLINE_COEFFICIENTS =       2007L,
  NIFTI_INTENT_FSL_DCT_COEFFICIENTS =                2008L,
  NIFTI_INTENT_FSL_QUADRATIC_SPLINE_COEFFICIENTS =   2009L,

  #   /*! The following intent codes have been used by FSL TOPUP for
  # displacement/coefficient files.
  #
  # These codes are included to prevent clashes in community-created
  # extensions to NIfTI. Encoding and decoding behavior for these
  # intents is not specified by the standard, and support is OPTIONAL
  # for conforming implementations.
  # */

  NIFTI_INTENT_FSL_TOPUP_CUBIC_SPLINE_COEFFICIENTS =         2016L,
  NIFTI_INTENT_FSL_TOPUP_QUADRATIC_SPLINE_COEFFICIENTS =     2017L,
  NIFTI_INTENT_FSL_TOPUP_FIELD =                             2018L
)

as_nifti_intent <- function(x) {
  stopifnot(length(x) == 1L)
  if(is.character(x)) {
    x <- NIFTI_INTENTS[[x]]
    if(!length(x)) {
      x <- 0L
    }
  } else {
    x <- as.integer(x)
    if(is.na(x) || !isTRUE(x %in% NIFTI_INTENTS) ) {
      x <- 0L
    }
  }
  x
}

#' Internal method to extract header information from a 'NIfTI' file
#' @param x file path or an R object
#' @returns A list containing the file header information
#'
#' @export
as_nifti_header <- function(x) {
  UseMethod("as_nifti_header")
}

# From RNifti
#' @export
as_nifti_header.niftiHeader <- function(x) {
  x
}

# from oro.nifti
#' @export
as_nifti_header.nifti <- function(x) {
  if(get_os() == "emscripten" || getOption("ieegio.debug.emscripten", FALSE)) {
    hdr <- x
    structure(
      list(
        sizeof_hdr = as.integer(hdr@sizeof_hdr),
        dim_info = 0L,
        dim = as.integer(hdr@dim_),
        intent_p1 = hdr@intent_p1,
        intent_p2 = hdr@intent_p2,
        intent_p3 = hdr@intent_p3,
        intent_code = as.integer(hdr@intent_code),
        datatype = as.integer(hdr@datatype),
        bitpix = as.integer(hdr@bitpix),
        slice_start = as.integer(hdr@slice_start),
        pixdim = hdr@pixdim,
        vox_offset = hdr@vox_offset,
        scl_slope = hdr@scl_slope,
        scl_inter = hdr@scl_inter,
        slice_end = as.integer(hdr@slice_end),
        slice_code = as.integer(hdr@slice_code),
        xyzt_units = as.integer(hdr@xyzt_units),
        cal_max = hdr@cal_max,
        cal_min = hdr@cal_min,
        slice_duration = hdr@slice_duration,
        toffset = hdr@toffset,
        descrip = hdr@descrip,
        aux_file = hdr@aux_file,
        qform_code = as.integer(hdr@qform_code),
        sform_code = as.integer(hdr@sform_code),
        quatern_b = hdr@quatern_b,
        quatern_c = hdr@quatern_c,
        quatern_d = hdr@quatern_d,
        qoffset_x = hdr@qoffset_x,
        qoffset_y = hdr@qoffset_y,
        qoffset_z = hdr@qoffset_z,
        srow_x = hdr@srow_x,
        srow_y = hdr@srow_y,
        srow_z = hdr@srow_z,
        intent_name = hdr@intent_name,
        magic = hdr@magic
      ),
      class = "niftiHeader"
    )
  } else {
    RNifti::niftiHeader(x)
  }
}

# from NIfTI file
#' @export
as_nifti_header.character <- function(x) {
  # file
  if(get_os() == "emscripten" || getOption("ieegio.debug.emscripten", FALSE)) {
    hdr <- oro.nifti::nifti_header(x)
    meta <- as_nifti_header.nifti(hdr)
  } else {
    meta <- RNifti::niftiHeader(x)
  }
  meta
}

# RNifti image itself
#' @export
as_nifti_header.niftiImage <- function(x) {
  RNifti::niftiHeader(x)
}

#' @rdname imaging-volume
#' @export
io_read_nii <- function(file, method = c("rnifti", "oro", "ants"), header_only = FALSE, ...) {
  # DIPSAUS DEBUG START
  # file <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/coregistration/CT_RAW.nii.gz"
  if(get_os() == "emscripten" || getOption("ieegio.debug.emscripten", FALSE)) {
    # WASM: only oro is supported
    method <- "oro"
  } else {
    method <- match.arg(method)
  }

  if(header_only) {
    if(!identical(method, "oro")) {
      warning("`io_read_nii`: reading with header-only mode, method ", sQuote(method), " will be ignored.")
    }
    method <- "oro"
  }

  meta <- as_nifti_header(file)

  switch(
    method,
    "oro" = {
      args <- list(
        fname = file, read_data = !header_only,
        ...
      )
      if(is.null(args$reorient)) {
        args$reorient <- FALSE
      }
      if(is.null(args$rescale_data)) {
        args$rescale_data <- FALSE
      }
      volume <- do.call(oro.nifti::readNIfTI, args)
      shape <- dim(volume@.Data)

      if( header_only ) {
        volume@.Data <- array(NA, rep(1, length(shape)))
        data <- NULL
      } else {
        header <- volume
        data <- quote({ header@.Data })
      }

      pixdim <- volume@pixdim[c(2, 3, 4)]

      # vox2ras
      qfcode <- oro.nifti::qform_code(volume)
      sfcode <- oro.nifti::sform_code(volume)

      qform <- structure(
        oro.nifti::qform(volume),
        code = qfcode
      )
      sform <- structure(
        oro.nifti::sform(volume),
        code = sfcode
      )

      if( sfcode > 0 ) {
        vox2ras <- structure(sform, which_xform = "sform")
      } else {
        vox2ras <- structure(qform, which_xform = "qform")
      }
      if(nrow(vox2ras) == 3) {
        vox2ras <- rbind(vox2ras, c(0, 0, 0, 1))
      }

      # vox2ras_tkr
      # vox2ras_tkr <- vox2ras
      # vox2ras_tkr[1:3, 4] <- - vox2ras[1:3, 1:3] %*% shape[1:3] / 2
      vox2ras_tkr <- get_vox2ras_tkr(vox2ras, shape / 2)

      # vox2fsl
      vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

      transforms <- list(
        vox2ras = vox2ras,
        vox2ras_tkr = vox2ras_tkr,
        vox2fsl = vox2fsl
      )

      return(new_volume(
        type = c("oro", "nifti"),
        header = volume,
        meta = meta,
        transforms = transforms,
        data = data,
        shape = shape
      ))
    },
    "rnifti" = {
      volume <- RNifti::readNifti(file, internal = FALSE)
      is_color <- inherits(volume, "rgbArray")
      header <- meta
      pixdim <- RNifti::pixdim(header)
      shape <- dim(volume)

      # vox2ras
      qfcode <- header$qform_code
      sfcode <- header$sform_code

      qform <- structure(
        RNifti::xform(header, useQuaternionFirst = TRUE),
        code = qfcode
      )
      sform <- structure(
        RNifti::xform(header, useQuaternionFirst = FALSE),
        code = sfcode
      )

      if( sfcode > 0 ) {
        vox2ras <- structure(sform, which_xform = "sform")
      } else {
        vox2ras <- structure(qform, which_xform = "qform")
      }
      if(nrow(vox2ras) == 3) {
        vox2ras <- rbind(vox2ras, c(0, 0, 0, 1))
      }

      # vox2ras_tkr
      # vox2ras_tkr <- vox2ras
      # vox2ras_tkr[1:3, 4] <- - vox2ras[1:3, 1:3] %*% shape[1:3] / 2
      vox2ras_tkr <- get_vox2ras_tkr(vox2ras, shape / 2)

      # vox2fsl
      vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

      transforms <- list(
        vox2ras = vox2ras,
        vox2ras_tkr = vox2ras_tkr,
        vox2fsl = vox2fsl
      )

      if(is_color) {
        type <- c("rnifti", "rgba", "nifti")
      } else {
        type <- c("rnifti", "nifti")
      }

      return(new_volume(
        type = type,
        header = volume,
        meta = meta,
        transforms = transforms,
        shape = shape,
        data = quote({
          v <- header[drop = FALSE]
          class(v) <- "array"
          attr(v, ".nifti_image_ptr") <- NULL
          v
        })
      ))

    },
    "ants" = {
      check_py_flag()
      if(!rpyANTs::ants_available(module = "ants")) {
        if(dir.exists(rpymat::env_path())) {
          rpyANTs::install_ants(python_ver = "auto")
        } else {
          rpyANTs::install_ants()
        }
      }
      volume <- rpyANTs::as_ANTsImage(file, ...)
      header <- volume
      shape <- unlist(rpymat::py_to_r(volume$shape))
      pixdim <- unlist(rpymat::py_to_r(volume$spacing))

      vox2lps <- t(t(rpymat::py_to_r(volume$direction)) * as.double(rpymat::py_to_r(volume$spacing)))
      vox2lps <- rbind(cbind(vox2lps, as.double(rpymat::py_to_r(volume$origin))), c(0, 0, 0, 1))
      vox2ras <- diag(c(-1, -1, 1, 1)) %*% vox2lps

      attr(vox2ras, "which_xform") <- "qform"

      # vox2ras_tkr
      # vox2ras_tkr <- vox2ras
      # vox2ras_tkr[1:3, 4] <- - vox2ras[1:3, 1:3] %*% shape[1:3] / 2
      vox2ras_tkr <- get_vox2ras_tkr(vox2ras, shape / 2)

      # vox2fsl
      vox2fsl <- get_vox2fsl(shape = shape, pixdim = pixdim, vox2ras = vox2ras)

      transforms <- list(
        vox2ras = vox2ras,
        vox2ras_tkr = vox2ras_tkr,
        vox2fsl = vox2fsl
      )



      return(new_volume(
        type = c("antspy", "nifti"),
        header = volume,
        meta = meta,
        transforms = transforms,
        data = quote({ header[drop = FALSE] }),
        shape = shape
      ))
    }
  )

}

#' @rdname imaging-volume
#' @export
io_write_nii <- function(x, con, ...) {
  UseMethod("io_write_nii")
}

#' @rdname imaging-volume
#' @export
io_write_nii.ieegio_nifti <- function(x, con, ...) {
  if(.subset2(x, "header_only")) {
    stop("The volume object is head-only.")
  }
  io_write_nii(x = x$header, con = con, ...)
}

#' @rdname imaging-volume
#' @export
io_write_nii.ants.core.ants_image.ANTsImage <- function(x, con, ...) {
  con <- normalizePath(con, winslash = "/", mustWork = FALSE)
  x$to_file(con)
  normalizePath(con)
}

#' @rdname imaging-volume
#' @export
io_write_nii.niftiImage <- function(x, con, ...) {

  x <- RNifti::asNifti(x, reference = NULL, internal = TRUE)

  args <- list(...)
  datatype <- args$datatype
  version <- args$version
  compression <- args$compression

  if(!length(datatype)) {
    if(identical(as.double(x$datatype), 64.0)) {
      # using NIFTI_TYPE_FLOAT32 instead of NIFTI_TYPE_FLOAT64
      datatype <- "float"
    } else {
      datatype <- "auto"
    }
  }
  if(!length(version)) {
    if(any(dim(x) > 512)) {
      version <- 2
    } else {
      version <- 1
    }
  }
  if(!length(compression)) {
    compression <- 6
  }

  re <- RNifti::writeNifti(image = x, file = con, datatype = datatype, version = version, compression = compression)
  re <- unique(re)
  if(length(re) > 1) {
    re <- re[[2]]
  }
  re
}

#' @rdname imaging-volume
#' @export
io_write_nii.nifti <- function(x, con, gzipped = NA, ...) {
  if(is.na(gzipped)) {
    gzipped <- TRUE
  }
  if(grepl("\\.(nii|nii\\.gz)$", con, ignore.case = TRUE)) {
    if( grepl("\\.nii$", con, ignore.case = TRUE) ) {
      gzipped <- FALSE
    }
    con <- path_ext_remove(con)
  }
  oro.nifti::writeNIfTI(nim = x, filename = con, gzipped = gzipped, ...)
}

#' @rdname imaging-volume
#' @export
io_write_nii.ieegio_mgh <- function(x, con, ...) {

  vox2ras <- x$transforms$vox2ras

  quaternion <- mat_to_quaternion(vox2ras)
  nframes <- x$header$internal$nframes

  m33 <- vox2ras[1:3, 1:3]
  pixdim <- sqrt(colSums(m33^2))
  pixdim <- c(sign(det(m33)), pixdim, nframes, 0, 0, 0)
  pixdim <- as.double(pixdim)

  data <- x$data
  data[is.na(data)] <- 0
  rg <- range(data)
  if(all(data - round(data) == 0)) {
    if( rg[[1]] >= 0 && rg[[2]] <= 255 ) {
      # UINT8
      datatype_code <- 2L
      bitpix <- 8L
      storage.mode(data) <- "integer"
    } else if ( rg[[1]] >= -32768 && rg[[2]] <= 32768 ) {
      # INT16
      datatype_code <- 4L
      bitpix <- 16L
      storage.mode(data) <- "integer"
    } else if ( rg[[1]] >= -2147483648 && rg[[2]] <= 2147483648 ) {
      # INT32
      datatype_code <- 8L
      bitpix <- 32L
      storage.mode(data) <- "integer"
    } else {
      # FLOAT32
      bitpix <- 32L
      datatype_code <- 16L
    }
  } else {
    # FLOAT32
    bitpix <- 32L
    datatype_code <- 16L
  }

  shape <- x$shape
  if(length(shape) == 3 || (length(shape) == 4 && shape[[4]] == 1)) {
    pixdim[[5]] <- 0
    shape <- shape[1:3]
    data <- array(data[seq_len(prod(shape))], dim = shape)
  }

  # functional
  nii <- oro.nifti::as.nifti(data)
  # sizeof_hdr = 348L,
  # dim_info = 0L,
  # dim = as.integer(c(length(x$shape), x$shape, rep(1, 7 - length(x$shape)))),
  # intent_p1 = 0, intent_p2 = 0, intent_p3 = 0, intent_code = 0L,
  nii@datatype <- datatype_code
  nii@data_type <- oro.nifti::convert.datatype(datatype_code)
  nii@bitpix <- bitpix
  # slice_start = 0L, slice_end = 0L, slice_code = 0L,
  nii@pixdim <- pixdim
  # vox_offset = 352,
  # scl_slope = 0, scl_inter = 0,
  # cal_max = 0, cal_min = 0,
  nii@xyzt_units <- 10L
  # slice_duration = 0, toffset = 0,
  # descrip = "Time=0.000", aux_file = "",
  nii@qform_code <- 1L
  nii@sform_code <- 1L
  nii@quatern_b <- quaternion[[1]]
  nii@quatern_c <- quaternion[[2]]
  nii@quatern_d <- quaternion[[3]]
  nii@qoffset_x <- vox2ras[1, 4]
  nii@qoffset_y <- vox2ras[2, 4]
  nii@qoffset_z <- vox2ras[3, 4]
  nii@srow_x <- vox2ras[1, ]
  nii@srow_y <- vox2ras[2, ]
  nii@srow_z <- vox2ras[3, ]
  # intent_name = "", magic = "n+1"
  nii@regular <- "r"

  io_write_nii.nifti(x = nii, con = con, ...)
}

#' @rdname imaging-volume
#' @export
io_write_nii.array <- function(x, con, vox2ras = NULL,
                               datatype_code = NULL,
                               xyzt_units = c("NIFTI_UNITS_MM", "NIFTI_UNITS_SEC"),
                               intent_code = "NIFTI_INTENT_NONE", ...,
                               gzipped = NA) {
  # We are using RNifti, which automatically handles gzipped issue

  if(!is.na(gzipped)) {
    if(grepl("\\.(nii|nii\\.gz)$", con, ignore.case = TRUE)) {
      con <- path_ext_remove(con)
    }
    if( gzipped ) {
      con <- sprintf("%s.nii.gz", con)
    } else {
      con <- sprintf("%s.nii", con)
    }
  }

  x <- as_ieegio_volume(x, vox2ras = vox2ras, ...)
  io_write_nii(x = x$header, con = con)
}
