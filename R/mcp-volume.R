#' Read and Cache Volume File
#'
#' @description
#' Read a neuroimaging volume file (NIfTI or MGZ format) and cache it in memory
#' with a unique digest-based ID. Internally the tool calls function
#' `ieegio::as_ieegio_volume`. The volume is stored in session state for
#' efficient reuse by other MCP tools. All volumes are normalized to NIfTI
#' format internally. Returns volume metadata and the ID to reference this
#' volume in subsequent operations.
#'
#' Use the returned ID with other tools: \code{ieegio-mcp_tool_volume_write} to save
#' the volume to a file, \code{ieegio-mcp_tool_volume_info} to get detailed header
#' information, \code{ieegio-mcp_tool_volume_list} to see all cached volumes, or
#' \code{ieegio-mcp_tool_volume_clear} to remove volumes from cache.
#'
#' @param file_path Character string, absolute or relative path to the volume
#'   file. Supported formats: \code{.nii}, \code{.nii.gz}, \code{.mgz}.
#'   Example values: \code{"/path/to/brain.nii.gz"}, \code{"data/subject01.mgz"}
#' @param .state_env Fastmap, internal session state for caching volumes
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the volume was successfully read and cached}
#'   \item{id}{Character, unique digest-based identifier for this volume}
#'   \item{file_path}{Character, the original file path that was read}
#'   \item{shape}{Integer vector, volume dimensions (width, height, depth, ...)}
#'   \item{type}{Character vector, volume type information}
#'   \item{format}{Character, original file format (\code{"nifti"} or \code{"mgz"})}
#'   \item{vox2ras}{Numeric matrix, 4x4 voxel-to-RAS transformation matrix}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read a NIfTI volume file
#' # Note: .state_env is automatically provided when called via MCP.
#' # The example includes it only to make the code runnable for documentation.
#' sample_file <- ieegio_sample_data("nifti/rnifti_example.nii.gz")
#' ieegio:::mcp_tool_volume_read(sample_file)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_volume_read <- function(file_path, .state_env = fastmap::fastmap()) {
  tryCatch({
    # Validate file exists
    if (!file.exists(file_path)) {
      return(list(
        success = FALSE,
        error = sprintf("File does not exist: %s", file_path)
      ))
    }

    # Get absolute path
    file_path <- normalizePath(file_path, mustWork = TRUE)

    # Determine format based on extension
    ext <- tolower(tools::file_ext(file_path))
    if (grepl("\\.nii\\.gz$", file_path, ignore.case = TRUE)) {
      ext <- "nii.gz"
    }

    original_format <- if (ext %in% c("nii", "nii.gz"))
      "nifti"
    else if (ext == "mgz")
      "mgz"
    else
      NA

    if (is.na(original_format)) {
      return(list(
        success = FALSE,
        error = sprintf(
          "Unsupported file format. Expected .nii, .nii.gz, or .mgz, got: %s",
          basename(file_path)
        )
      ))
    }

    # Read volume
    if (original_format == "nifti") {
      # Read NIfTI directly with RNifti method
      volume <- as_ieegio_volume(file_path, method = "rnifti")
    } else {
      # MGZ: read, convert to temp NIfTI, read again as NIfTI, cleanup
      temp_nii <- tempfile(fileext = ".nii.gz")
      on.exit({
        if (file.exists(temp_nii)) {
          unlink(temp_nii)
        }
      }, add = TRUE)

      # Read MGZ first
      volume_mgz <- as_ieegio_volume(file_path)

      # Write to temporary NIfTI
      write_volume(volume_mgz, temp_nii, format = "nifti")

      # Read back as NIfTI
      volume <- as_ieegio_volume(temp_nii, method = "rnifti")
    }

    # Generate digest ID based on the volume object
    # Use a deterministic serialization
    id <- digest::digest(volume$header, algo = "md5")

    # Store in state environment
    if (!.state_env$has("volumes")) {
      .state_env$set("volumes", list())
    }

    volumes <- .state_env$get("volumes")

    # Check if this ID already exists and add path to references
    if (id %in% names(volumes)) {
      # Add path if not already present
      if (!file_path %in% volumes[[id]]$paths) {
        volumes[[id]]$paths <- c(volumes[[id]]$paths, file_path)
      }
    } else {
      # New volume entry
      volumes[[id]] <- list(object = volume,
                            paths = file_path,
                            format = original_format)
    }

    .state_env$set("volumes", volumes)

    # Return metadata
    list(
      success = TRUE,
      id = id,
      file_path = file_path,
      shape = volume$shape,
      type = volume$type,
      format = original_format,
      vox2ras = volume$transforms$vox2ras
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error reading volume: %s", conditionMessage(e))
    )
  })
}


#' Write Cached Volume to File
#'
#' @description
#' Write a previously cached volume (identified by its digest ID) to a temporary
#' file. The volume is retrieved from session state and written in the specified
#' format (NIfTI or MGZ). A temporary file is created using the volume ID as the
#' filename.
#'
#' @param id Character string, the digest-based identifier of the cached volume.
#'   This ID is returned by \code{ieegio-mcp_tool_volume_read}.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}
#' @param format Character string, output file format. Must be either \code{"nifti"}
#'   or \code{"mgz"}. Defaults to \code{"nifti"}.
#'   Example values: \code{"nifti"}, \code{"mgz"}
#' @param .state_env Fastmap, internal session state for cached volumes
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the volume was successfully written}
#'   \item{id}{Character, the volume identifier that was written}
#'   \item{output_path}{Character, absolute path to the temporary file}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read a volume, then write it to a temporary file
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' sample_file <- ieegio_sample_data("nifti/rnifti_example.nii.gz")
#' state_env <- fastmap::fastmap()
#' read_result <- ieegio:::mcp_tool_volume_read(sample_file, .state_env = state_env)
#'
#' # Write to temporary file
#' ieegio:::mcp_tool_volume_write(
#'   id = read_result$id,
#'   .state_env = state_env
#' )
#'
#' @keywords mcp-tool mcp-category-execution
#' @noRd
mcp_tool_volume_write <- function(id,
                                  format = c("nifti", "mgz"),
                                  .state_env = fastmap::fastmap()) {
  format <- match.arg(format)
  tryCatch({
    # Check if volumes exist in state
    if (!.state_env$has("volumes")) {
      return(list(
        success = FALSE,
        id = id,
        error = "No volumes in cache"
      ))
    }

    volumes <- .state_env$get("volumes")

    # Check if ID exists
    if (!id %in% names(volumes)) {
      return(list(
        success = FALSE,
        id = id,
        error = sprintf("Volume with ID '%s' not found in cache", id)
      ))
    }

    # Get volume from cache
    volume_entry <- volumes[[id]]
    volume <- volume_entry$object

    # Create temporary file path in tmpdir/ieegio/{id}.{ext}
    ieegio_tmpdir <- file.path(tempdir(), "ieegio")
    if (!dir.exists(ieegio_tmpdir)) {
      dir.create(ieegio_tmpdir,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }

    # Determine file extension based on format
    file_ext <- if (format == "nifti")
      ".nii.gz"
    else
      ".mgz"
    output_path <- file.path(ieegio_tmpdir, paste0(id, file_ext))

    # Write volume
    write_volume(volume, output_path, format = format)

    # Return absolute path
    output_path <- normalizePath(output_path, mustWork = TRUE)

    list(success = TRUE,
         id = id,
         output_path = output_path)

  }, error = function(e) {
    list(
      success = FALSE,
      id = id,
      error = sprintf("Error writing volume: %s", conditionMessage(e))
    )
  })
}


#' List Cached Volumes
#'
#' @description
#' List all volumes currently cached in session state. Returns basic metadata
#' for each cached volume including its ID, reference paths, original format,
#' and dimensions. This helps track which volumes are available for use by
#' other MCP tools.
#'
#' @param .state_env Fastmap, internal session state for cached volumes
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{count}{Integer, number of cached volumes}
#'   \item{volumes}{List of volume metadata, each containing:
#'     \describe{
#'       \item{id}{Character, volume identifier}
#'       \item{paths}{Character vector, file paths associated with this volume}
#'       \item{format}{Character, original format (\code{"nifti"} or \code{"mgz"})}
#'       \item{shape}{Integer vector, volume dimensions}
#'       \item{type}{Character vector, volume type information}
#'     }
#'   }
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read multiple volumes and list them
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' state_env <- fastmap::fastmap()
#' file1 <- ieegio_sample_data("nifti/rnifti_example.nii.gz")
#' ieegio:::mcp_tool_volume_read(file1, .state_env = state_env)
#' file2 <- ieegio_sample_data("brain.demosubject.mgz")
#' ieegio:::mcp_tool_volume_read(file2, .state_env = state_env)
#'
#' # List all cached volumes
#' ieegio:::mcp_tool_volume_list(.state_env = state_env)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_volume_list <- function(.state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if volumes exist in state
    if (!.state_env$has("volumes")) {
      return(list(
        success = TRUE,
        count = 0L,
        volumes = list()
      ))
    }

    volumes <- .state_env$get("volumes")

    if (length(volumes) == 0) {
      return(list(
        success = TRUE,
        count = 0L,
        volumes = list()
      ))
    }

    # Build volume list
    volume_list <- lapply(names(volumes), function(id) {
      entry <- volumes[[id]]
      list(
        id = id,
        paths = entry$paths,
        format = entry$format,
        shape = entry$object$shape,
        type = entry$object$type
      )
    })

    list(
      success = TRUE,
      count = length(volume_list),
      volumes = volume_list
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error listing volumes: %s", conditionMessage(e))
    )
  })
}


#' Get Volume Information
#'
#' @description
#' Retrieve detailed header information and metadata for a cached volume
#' without loading the full data array. Uses the \code{as_nifti_header}
#' function to extract NIfTI header fields. This is useful for inspecting
#' volume properties, transformations, and file provenance.
#'
#' @param id Character string, the digest-based identifier of the cached volume.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}
#' @param .state_env Fastmap, internal session state for cached volumes
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{id}{Character, the volume identifier}
#'   \item{paths}{Character vector, file paths associated with this volume}
#'   \item{format}{Character, original file format (\code{"nifti"} or \code{"mgz"})}
#'   \item{header}{List, NIfTI header information including dimensions, datatypes,
#'     transformations, and other metadata fields}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read a volume and get detailed header info
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' sample_file <- ieegio_sample_data("nifti/rnifti_example.nii.gz")
#' state_env <- fastmap::fastmap()
#' read_result <- ieegio:::mcp_tool_volume_read(sample_file, .state_env = state_env)
#'
#' # Get detailed header information
#' ieegio:::mcp_tool_volume_info(id = read_result$id, .state_env = state_env)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_volume_info <- function(id, .state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if volumes exist in state
    if (!.state_env$has("volumes")) {
      return(list(
        success = FALSE,
        id = id,
        error = "No volumes in cache"
      ))
    }

    volumes <- .state_env$get("volumes")

    # Check if ID exists
    if (!id %in% names(volumes)) {
      return(list(
        success = FALSE,
        id = id,
        error = sprintf("Volume with ID '%s' not found in cache", id)
      ))
    }

    # Get volume from cache
    entry <- volumes[[id]]
    volume <- entry$object

    # Extract header information
    header <- as_nifti_header(volume$header)

    list(
      success = TRUE,
      id = id,
      paths = entry$paths,
      format = entry$format,
      header = header
    )

  }, error = function(e) {
    list(
      success = FALSE,
      id = id,
      error = sprintf("Error retrieving volume info: %s", conditionMessage(e))
    )
  })
}


#' Clear Cached Volumes
#'
#' @description
#' Remove one or all volumes from the session cache. This frees up memory
#' occupied by cached volume objects. If an ID is provided, only that specific
#' volume is removed. If no ID is provided, all cached volumes are cleared.
#'
#' @param id Character string, the digest-based identifier of the volume to
#'   remove. Empty string (default) clears all volumes.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}
#' @param .state_env Fastmap, internal session state for cached volumes
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{message}{Character, human-readable status message}
#'   \item{cleared_ids}{Character vector, IDs of volumes that were removed}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Clear specific volume from cache
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' state_env <- fastmap::fastmap()
#' sample_file <- ieegio_sample_data("nifti/rnifti_example.nii.gz")
#' read_result <- ieegio:::mcp_tool_volume_read(sample_file, .state_env = state_env)
#'
#' # Clear the specific volume
#' ieegio:::mcp_tool_volume_clear(
#'   id = read_result$id,
#'   .state_env = state_env
#' )
#'
#' @keywords mcp-tool mcp-category-execution
#' @noRd
mcp_tool_volume_clear <- function(id = "",
                                  .state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if volumes exist in state
    if (!.state_env$has("volumes")) {
      return(
        list(
          success = TRUE,
          message = "No volumes in cache to clear",
          cleared_ids = character(0)
        )
      )
    }

    volumes <- .state_env$get("volumes")

    # Determine which volumes to clear
    if (is.null(id) || is.na(id) || id == "") {
      # Clear all volumes
      cleared_ids <- names(volumes)
      .state_env$set("volumes", list())

      message <- if (length(cleared_ids) == 0) {
        "No volumes in cache to clear"
      } else {
        sprintf("Cleared all %d volume%s from cache",
                length(cleared_ids),
                if (length(cleared_ids) == 1)
                  ""
                else
                  "s")
      }

    } else {
      # Clear specific volume
      if (!id %in% names(volumes)) {
        return(list(
          success = FALSE,
          error = sprintf("Volume with ID '%s' not found in cache", id)
        ))
      }

      volumes[[id]] <- NULL
      .state_env$set("volumes", volumes)
      cleared_ids <- id
      message <- "Cleared 1 volume from cache"
    }

    list(success = TRUE,
         message = message,
         cleared_ids = cleared_ids)

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error clearing volumes: %s", conditionMessage(e))
    )
  })
}


#' Discover Volume Files in Directory
#'
#' @description
#' Search a directory for neuroimaging volume files (NIfTI and MGZ formats).
#' Returns a list of found files with basic metadata including file paths,
#' sizes, and formats. Useful for discovering available volume files before
#' reading them with \code{ieegio-mcp_tool_volume_read}.
#'
#' @param path Character string, absolute or relative path to the directory
#'   to search. Example values: \code{"/data/subjects"}, \code{"~/imaging/scans"}
#' @param recursive Logical, whether to search subdirectories recursively.
#'   Defaults to \code{TRUE}. Example values: \code{TRUE}, \code{FALSE}
#' @param pattern Character string, optional filename pattern to filter results.
#'   Empty string (default) returns all volume files. Otherwise, only files whose
#'   basename matches the pattern are included.
#'   Example values: \code{"brain"}, \code{"T1"}, \code{"subject01"}
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the search completed successfully}
#'   \item{count}{Integer, number of volume files found}
#'   \item{files}{List of file relative-paths}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Discover all volume files in sample data directory
#' sample_dir <- dirname(ieegio_sample_data("nifti/rnifti_example.nii.gz"))
#' ieegio:::mcp_tool_volume_discover(sample_dir)
#'
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_volume_discover <- function(path,
                                     recursive = TRUE,
                                     pattern = "") {
  tryCatch({
    # Validate directory exists
    if (!dir.exists(path)) {
      return(list(
        success = FALSE,
        error = sprintf("Directory does not exist: %s", path)
      ))
    }

    # Get absolute path
    path <- normalizePath(path, mustWork = TRUE)

    # Sanitize pattern for list.files
    search_pattern <- if (is.null(pattern) || is.na(pattern) || !nzchar(pattern)) {
      NULL
    } else {
      pattern
    }

    # List all files recursively or non-recursively with pattern
    all_files <- list.files(
      path,
      pattern = search_pattern,
      full.names = FALSE,
      recursive = recursive,
      ignore.case = TRUE,
      all.files = FALSE,
      include.dirs = FALSE
    )

    # Filter by volume file extensions (.nii, .nii.gz, .mgz)
    volume_files <- all_files[tolower(path_ext(all_files)) %in% c("nii", "mgz", "nii.gz")]

    list(
      success = TRUE,
      count = length(volume_files),
      files = volume_files
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error discovering volumes: %s", conditionMessage(e))
    )
  })
}


#' Convert Cached Volume to Surface
#'
#' @description
#' Convert a previously cached volume (identified by its digest ID) to a surface
#' mesh using isosurface extraction. The volume is retrieved from the volume cache,
#' converted to a surface using \code{volume_to_surface}, and stored in the surface
#' cache. Returns the new surface ID and metadata.
#'
#' This tool bridges volume and surface operations, enabling workflows that start
#' with volumetric data and produce surface meshes for visualization or analysis.
#' Before using this tool, you must first read and cache the volume using
#' \code{ieegio-mcp_tool_volume_read}, which returns the volume ID needed for this
#' conversion. The resulting surface can be accessed with surface tools like
#' \code{ieegio-mcp_tool_surface_info}, and can be exported via \code{ieegio-mcp_tool_surface_write}.
#'
#' @param id Character string, the digest-based identifier of the cached volume.
#'   This ID is returned by \code{ieegio-mcp_tool_volume_read}.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}
#' @param lambda Numeric, smoothing parameter for surface mesh. Higher values
#'   produce smoother surfaces. Set to negative or \code{NA} to disable smoothing.
#'   Defaults to \code{0.2}. Example values: \code{0.2}, \code{0.5}, \code{-1}
#' @param degree Integer, degree of smoothing (passed to
#'   \code{vcg_smooth_implicit}). Defaults to \code{2}.
#'   Example values: \code{2}, \code{3}
#' @param threshold_lb Numeric, lower threshold for isosurface extraction. Voxels
#'   above this value are included in the surface. Defaults to \code{0.5} for binary or probablistic masks
#'   Example values: \code{0.5}, \code{100}, \code{0}
#' @param threshold_ub Numeric, upper threshold for isosurface extraction. Voxels
#'   below this value are included. Use \code{NA} for no upper limit (default).
#'   Example values: \code{NA}, \code{1000}, \code{255}
#' @param .state_env Fastmap, internal session state for cached volumes and surfaces
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the conversion completed successfully}
#'   \item{volume_id}{Character, the volume identifier that was converted}
#'   \item{surface_id}{Character, unique digest-based identifier for the new surface}
#'   \item{n_vertices}{Integer, number of vertices in the generated surface}
#'   \item{n_faces}{Integer, number of faces in the generated surface}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Convert a volume to surface
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' sample_file <- ieegio_sample_data("nifti/rnifti_example.nii.gz")
#' state_env <- fastmap::fastmap()
#' read_result <- ieegio:::mcp_tool_volume_read(sample_file, .state_env = state_env)
#'
#' # Convert to surface with default smoothing
#' ieegio:::mcp_tool_volume_to_surface(
#'   id = read_result$id,
#'   threshold_lb = 20,
#'   .state_env = state_env
#' )
#'
#' @keywords mcp-tool mcp-category-execution
#' @noRd
mcp_tool_volume_to_surface <- function(id,
                                       lambda = 0.2,
                                       degree = 2,
                                       threshold_lb = 0.5,
                                       threshold_ub = NA_real_,
                                       .state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if volumes exist in state
    if (!.state_env$has("volumes")) {
      return(list(
        success = FALSE,
        volume_id = id,
        error = "No volumes in cache"
      ))
    }

    volumes <- .state_env$get("volumes")

    # Check if ID exists
    if (!id %in% names(volumes)) {
      return(list(
        success = FALSE,
        volume_id = id,
        error = sprintf("Volume with ID '%s' not found in cache", id)
      ))
    }

    # Get volume from cache
    volume_entry <- volumes[[id]]
    volume <- volume_entry$object

    # Convert volume to surface
    surface <- volume_to_surface(
      volume = volume,
      lambda = lambda,
      degree = degree,
      threshold_lb = threshold_lb,
      threshold_ub = threshold_ub
    )

    # Generate digest ID for the new surface
    surface_id <- digest::digest(surface$header, algo = "md5")

    # Initialize surface cache if needed
    if (!.state_env$has("surfaces")) {
      .state_env$set("surfaces", list())
    }

    surfaces <- .state_env$get("surfaces")

    # Gather surface metadata
    n_vertices <- NA_integer_
    n_faces <- NA_integer_
    if (!is.null(surface$geometry)) {
      if (is.matrix(surface$geometry$vertices)) {
        n_vertices <- ncol(surface$geometry$vertices)
      }
      if (is.matrix(surface$geometry$faces)) {
        n_faces <- ncol(surface$geometry$faces)
      }
    }

    # Store surface in cache
    surfaces[[surface_id]] <- list(
      object = surface,
      paths = sprintf("converted_from_volume_%s", id),
      format = "converted"
    )

    .state_env$set("surfaces", surfaces)

    # Return metadata
    list(
      success = TRUE,
      volume_id = id,
      surface_id = surface_id,
      n_vertices = n_vertices,
      n_faces = n_faces
    )

  }, error = function(e) {
    list(
      success = FALSE,
      volume_id = id,
      error = sprintf("Error converting volume to surface: %s", conditionMessage(e))
    )
  })
}
