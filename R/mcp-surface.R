#' Read and Cache Surface File
#'
#' @description
#' Read a neuroimaging surface file (GIfTI, STL, or FreeSurfer format) and cache it in memory
#' with a unique digest-based ID. Internally the tool calls function
#' \code{ieegio::read_surface}. The surface is stored in session state for
#' efficient reuse by other MCP tools. Returns surface metadata and the ID to reference this
#' surface in subsequent operations.
#'
#' Use the returned ID with other tools: \code{ieegio-mcp_tool_surface_write} to save
#' the surface to a file, \code{ieegio-mcp_tool_surface_info} to get detailed information,
#' \code{ieegio-mcp_tool_surface_list} to see all cached surfaces, or
#' \code{ieegio-mcp_tool_surface_clear} to remove surfaces from cache.
#'
#' @param file_path Character string, absolute or relative path to the surface
#'   file. Supported formats: \code{.gii}, \code{.gii.gz} (GIfTI), \code{.stl} (STL),
#'   FreeSurfer geometry, annotations, measurements.
#'   Example values: \code{"/path/to/lh.pial.gii"}, \code{"data/lh.pial"}
#' @param .state_env Fastmap, internal session state for caching surfaces
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the surface was successfully read and cached}
#'   \item{id}{Character, unique digest-based identifier for this surface}
#'   \item{file_path}{Character, the original file path that was read}
#'   \item{format}{Character, file format (\code{"gifti"}, \code{"stl"}, or \code{"freesurfer"})}
#'   \item{contains}{Character vector, what data the surface contains (e.g., "geometry", "measurements")}
#'   \item{n_vertices}{Integer, number of vertices (if geometry present)}
#'   \item{n_faces}{Integer, number of faces (if geometry present)}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read a GIfTI surface file
#' # Note: .state_env is automatically provided when called via MCP.
#' # The example includes it only to make the code runnable for documentation.
#' sample_file <- ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")
#' ieegio:::mcp_tool_surface_read(sample_file)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_surface_read <- function(file_path, .state_env = fastmap::fastmap()) {
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
    fname <- basename(file_path)
    fname_lower <- tolower(fname)
    if (endsWith(fname_lower, ".gii") || endsWith(fname_lower, ".gii.gz")) {
      file_format <- "gifti"
    } else if (endsWith(fname_lower, ".stl")) {
      file_format <- "stl"
    } else {
      file_format <- "freesurfer"
    }

    # Read surface
    surface <- read_surface(file_path)

    # Generate digest ID based on the surface object
    # Use a deterministic serialization
    id <- digest::digest(surface$header, algo = "md5")

    # Store in state environment
    if (!.state_env$has("surfaces")) {
      .state_env$set("surfaces", list())
    }

    surfaces <- .state_env$get("surfaces")

    # Gather metadata
    contains <- character(0)
    if (!is.null(surface$geometry))
      contains <- c(contains, "geometry")
    if (!is.null(surface$color))
      contains <- c(contains, "color")
    if (!is.null(surface$annotations))
      contains <- c(contains, "annotations")
    if (!is.null(surface$measurements))
      contains <- c(contains, "measurements")
    if (!is.null(surface$time_series))
      contains <- c(contains, "time_series")

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

    # Check if this ID already exists and add path to references
    if (id %in% names(surfaces)) {
      # Add path if not already present
      if (!file_path %in% surfaces[[id]]$paths) {
        surfaces[[id]]$paths <- c(surfaces[[id]]$paths, file_path)
      }
    } else {
      # New surface entry
      surfaces[[id]] <- list(
        object = surface,
        paths = file_path,
        format = file_format
      )
    }

    .state_env$set("surfaces", surfaces)

    # Return metadata
    list(
      success = TRUE,
      id = id,
      file_path = file_path,
      format = file_format,
      contains = contains,
      n_vertices = n_vertices,
      n_faces = n_faces
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error reading surface: %s", conditionMessage(e))
    )
  })
}


#' Write Cached Surface to File
#'
#' @description
#' Write a previously cached surface (identified by its digest ID) to a temporary
#' file. The surface is retrieved from session state and written in the specified
#' format. A temporary file is created using format-specific naming: geometry data
#' uses \code{{id}.{ext}}, other data types use \code{{id}__{type}.{ext}}.
#'
#' Format-specific extensions:
#' \itemize{
#'   \item GIfTI (\code{.gii}): Supports all data types
#'   \item STL (\code{.stl}): Only geometry supported
#'   \item FreeSurfer: geometry (no extension), annotations (\code{.annot}), measurements (\code{.curv})
#' }
#'
#' @param id Character string, the digest-based identifier of the cached surface.
#'   This ID is returned by \code{ieegio-mcp_tool_surface_read}.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}
#' @param format Character string, output file format. Must be \code{"gifti"},
#'   \code{"stl"}, or \code{"freesurfer"}. Defaults to \code{"gifti"}.
#'   Example values: \code{"gifti"}, \code{"stl"}, \code{"freesurfer"}
#' @param type Character string, data type to write: \code{"geometry"},
#'   \code{"annotations"}, \code{"measurements"}, \code{"color"}, or \code{"time_series"}.
#'   Defaults to \code{"geometry"}. Each call writes one type; make separate calls for multiple types.
#'   Example values: \code{"geometry"}, \code{"annotations"}, \code{"measurements"}
#' @param .state_env Fastmap, internal session state for cached surfaces
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the surface was successfully written}
#'   \item{id}{Character, the surface identifier that was written}
#'   \item{output_path}{Character, absolute path to the temporary file}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read a surface, then write it to a temporary file
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' sample_file <- ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")
#' state_env <- fastmap::fastmap()
#' read_result <- ieegio:::mcp_tool_surface_read(sample_file, .state_env = state_env)
#'
#' # Write to temporary file
#' ieegio:::mcp_tool_surface_write(
#'   id = read_result$id,
#'   .state_env = state_env
#' )
#'
#' @keywords mcp-tool mcp-category-execution
#' @noRd
mcp_tool_surface_write <- function(id,
                                   format = c("gifti", "stl", "freesurfer"),
                                   type = c("geometry", "annotations", "measurements",
                                           "color", "time_series"),
                                   .state_env = fastmap::fastmap()) {
  format <- match.arg(format)
  type <- match.arg(type)

  tryCatch({
    # Check if surfaces exist in state
    if (!.state_env$has("surfaces")) {
      return(list(
        success = FALSE,
        id = id,
        error = "No surfaces in cache"
      ))
    }

    surfaces <- .state_env$get("surfaces")

    # Check if ID exists
    if (!id %in% names(surfaces)) {
      return(list(
        success = FALSE,
        id = id,
        error = sprintf("Surface with ID '%s' not found in cache", id)
      ))
    }

    # Get surface from cache
    surface_entry <- surfaces[[id]]
    surface <- surface_entry$object

    # Validate format-type compatibility
    if (format == "stl") {
      if (type != "geometry") {
        available_types <- character(0)
        if (!is.null(surface$geometry)) available_types <- c(available_types, "geometry")
        if (!is.null(surface$color)) available_types <- c(available_types, "color")
        if (!is.null(surface$annotations)) available_types <- c(available_types, "annotations")
        if (!is.null(surface$measurements)) available_types <- c(available_types, "measurements")
        if (!is.null(surface$time_series)) available_types <- c(available_types, "time_series")
        
        return(list(
          success = FALSE,
          id = id,
          error = sprintf(
            "STL format only supports geometry. Surface contains: %s. Use format='gifti' to write non-geometry data.",
            paste(available_types, collapse = ", ")
          )
        ))
      }
    }

    if (format == "freesurfer" && type %in% c("color", "time_series")) {
      return(list(
        success = FALSE,
        id = id,
        error = sprintf(
          "FreeSurfer format does not support '%s' data type. Supported types: geometry, annotations, measurements. Use format='gifti' for color/time_series.",
          type
        )
      ))
    }

    # Check if surface contains the requested data type
    if (!length(surface[[type]])) {
      available_types <- character(0)
      if (!is.null(surface$geometry)) available_types <- c(available_types, "geometry")
      if (!is.null(surface$color)) available_types <- c(available_types, "color")
      if (!is.null(surface$annotations)) available_types <- c(available_types, "annotations")
      if (!is.null(surface$measurements)) available_types <- c(available_types, "measurements")
      if (!is.null(surface$time_series)) available_types <- c(available_types, "time_series")
      
      return(list(
        success = FALSE,
        id = id,
        error = sprintf(
          "Surface does not contain '%s' data. Available types: %s",
          type,
          paste(available_types, collapse = ", ")
        )
      ))
    }

    # Create temporary file path in tmpdir/ieegio/
    ieegio_tmpdir <- file.path(tempdir(), "ieegio")
    if (!dir.exists(ieegio_tmpdir)) {
      dir.create(ieegio_tmpdir,
                recursive = TRUE,
                showWarnings = FALSE)
    }

    # Determine file name and extension based on format and type
    # Naming: geometry -> {id}.{ext}, others -> {id}__{type}.{ext}
    if (format == "gifti") {
      if (type == "geometry") {
        filename <- paste0(id, ".gii")
      } else {
        filename <- paste0(id, "__", type, ".gii")
      }
    } else if (format == "stl") {
      # STL only supports geometry
      filename <- paste0(id, ".stl")
    } else {
      # FreeSurfer format
      if (type == "geometry") {
        filename <- id  # No extension for geometry
      } else if (type == "annotations") {
        filename <- paste0(id, "__annotations.annot")
      } else if (type == "measurements") {
        filename <- paste0(id, "__measurements.curv")
      }
    }

    output_path <- file.path(ieegio_tmpdir, filename)

    # Write surface
    if (format == "gifti") {
      write_surface(surface, output_path, format = "gifti")
    } else if (format == "stl") {
      write_surface(surface, output_path, format = "freesurfer")  # write_surface detects .stl extension
    } else {
      # FreeSurfer
      write_surface(surface, output_path,
                   format = "freesurfer",
                   type = type)
    }

    # Return absolute path
    if (file.exists(output_path)) {
      output_path <- normalizePath(output_path, mustWork = TRUE)
    }

    list(
      success = TRUE,
      id = id,
      output_path = output_path
    )

  }, error = function(e) {
    list(
      success = FALSE,
      id = id,
      error = sprintf("Error writing surface: %s", conditionMessage(e))
    )
  })
}


#' List Cached Surfaces
#'
#' @description
#' List all surfaces currently cached in session state. Returns basic metadata
#' for each cached surface including its ID, reference paths, original format,
#' and contents. This helps track which surfaces are available for use by
#' other MCP tools.
#'
#' @param .state_env Fastmap, internal session state for cached surfaces
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{count}{Integer, number of cached surfaces}
#'   \item{surfaces}{List of surface metadata, each containing:
#'     \describe{
#'       \item{id}{Character, surface identifier}
#'       \item{paths}{Character vector, file paths associated with this surface}
#'       \item{format}{Character, original format (\code{"gifti"}, \code{"stl"}, or \code{"freesurfer"})}
#'       \item{contains}{Character vector, data types contained (e.g., "geometry", "measurements")}
#'     }
#'   }
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read multiple surfaces and list them
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' state_env <- fastmap::fastmap()
#' file1 <- ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")
#' ieegio:::mcp_tool_surface_read(file1, .state_env = state_env)
#' file2 <- ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.shape.gii")
#' ieegio:::mcp_tool_surface_read(file2, .state_env = state_env)
#'
#' # List all cached surfaces
#' ieegio:::mcp_tool_surface_list(.state_env = state_env)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_surface_list <- function(.state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if surfaces exist in state
    if (!.state_env$has("surfaces")) {
      return(list(
        success = TRUE,
        count = 0L,
        surfaces = list()
      ))
    }

    surfaces <- .state_env$get("surfaces")

    if (length(surfaces) == 0) {
      return(list(
        success = TRUE,
        count = 0L,
        surfaces = list()
      ))
    }

    # Build surface list
    surface_list <- lapply(names(surfaces), function(id) {
      entry <- surfaces[[id]]
      surf <- entry$object

      # Determine what the surface contains
      contains <- character(0)
      if (!is.null(surf$geometry))
        contains <- c(contains, "geometry")
      if (!is.null(surf$color))
        contains <- c(contains, "color")
      if (!is.null(surf$annotations))
        contains <- c(contains, "annotations")
      if (!is.null(surf$measurements))
        contains <- c(contains, "measurements")
      if (!is.null(surf$time_series))
        contains <- c(contains, "time_series")

      list(
        id = id,
        paths = entry$paths,
        format = entry$format,
        contains = contains
      )
    })

    list(
      success = TRUE,
      count = length(surface_list),
      surfaces = surface_list
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error listing surfaces: %s", conditionMessage(e))
    )
  })
}


#' Get Surface Information
#'
#' @description
#' Retrieve detailed information and metadata for a cached surface
#' without loading the full data. This is useful for inspecting
#' surface properties, vertex counts, and what data types are available.
#'
#' @param id Character string, the digest-based identifier of the cached surface.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}
#' @param .state_env Fastmap, internal session state for cached surfaces
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{id}{Character, the surface identifier}
#'   \item{paths}{Character vector, file paths associated with this surface}
#'   \item{format}{Character, original file format (\code{"gifti"}, \code{"stl"}, or \code{"freesurfer"})}
#'   \item{contains}{Character vector, data types contained}
#'   \item{n_vertices}{Integer, number of vertices (if geometry present)}
#'   \item{n_faces}{Integer, number of faces (if geometry present)}
#'   \item{sparse}{Logical, whether surface uses sparse indexing}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Read a surface and get detailed info
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' sample_file <- ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")
#' state_env <- fastmap::fastmap()
#' read_result <- ieegio:::mcp_tool_surface_read(sample_file, .state_env = state_env)
#'
#' # Get detailed information
#' ieegio:::mcp_tool_surface_info(id = read_result$id, .state_env = state_env)
#'
#' @keywords mcp-tool mcp-category-info
#' @noRd
mcp_tool_surface_info <- function(id, .state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if surfaces exist in state
    if (!.state_env$has("surfaces")) {
      return(list(
        success = FALSE,
        id = id,
        error = "No surfaces in cache"
      ))
    }

    surfaces <- .state_env$get("surfaces")

    # Check if ID exists
    if (!id %in% names(surfaces)) {
      return(list(
        success = FALSE,
        id = id,
        error = sprintf("Surface with ID '%s' not found in cache", id)
      ))
    }

    # Get surface from cache
    entry <- surfaces[[id]]
    surf <- entry$object

    # Gather information
    contains <- character(0)
    if (!is.null(surf$geometry))
      contains <- c(contains, "geometry")
    if (!is.null(surf$color))
      contains <- c(contains, "color")
    if (!is.null(surf$annotations))
      contains <- c(contains, "annotations")
    if (!is.null(surf$measurements))
      contains <- c(contains, "measurements")
    if (!is.null(surf$time_series))
      contains <- c(contains, "time_series")

    n_vertices <- NA_integer_
    n_faces <- NA_integer_
    if (!is.null(surf$geometry)) {
      if (is.matrix(surf$geometry$vertices)) {
        n_vertices <- ncol(surf$geometry$vertices)
      }
      if (is.matrix(surf$geometry$faces)) {
        n_faces <- ncol(surf$geometry$faces)
      }
    }

    list(
      success = TRUE,
      id = id,
      paths = entry$paths,
      format = entry$format,
      contains = contains,
      n_vertices = n_vertices,
      n_faces = n_faces,
      sparse = isTRUE(surf$sparse)
    )

  }, error = function(e) {
    list(
      success = FALSE,
      id = id,
      error = sprintf("Error retrieving surface info: %s", conditionMessage(e))
    )
  })
}


#' Clear Cached Surfaces
#'
#' @description
#' Remove one or all surfaces from the session cache. This frees up memory
#' occupied by cached surface objects. If an ID is provided, only that specific
#' surface is removed. If no ID is provided (empty string), all cached surfaces are cleared.
#'
#' @param id Character string, the digest-based identifier of the surface to
#'   remove. Empty string (default) clears all surfaces.
#'   Example values: \code{"a3f5e8d9c1b2"}, \code{"f7e4b2a1c6d8"}, \code{""}
#' @param .state_env Fastmap, internal session state for cached surfaces
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the operation completed successfully}
#'   \item{message}{Character, human-readable status message}
#'   \item{cleared_ids}{Character vector, IDs of surfaces that were removed}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Clear specific surface from cache
#' # Note: .state_env is automatically provided when called via MCP.
#' # AI does not need to specify this argument. The examples only use
#' # .state_env to make the code runnable for demonstration purposes.
#' state_env <- fastmap::fastmap()
#' sample_file <- ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")
#' read_result <- ieegio:::mcp_tool_surface_read(sample_file, .state_env = state_env)
#'
#' # Clear the specific surface
#' ieegio:::mcp_tool_surface_clear(
#'   id = read_result$id,
#'   .state_env = state_env
#' )
#'
#' @keywords mcp-tool mcp-category-execution
#' @noRd
mcp_tool_surface_clear <- function(id = "",
                                   .state_env = fastmap::fastmap()) {
  tryCatch({
    # Check if surfaces exist in state
    if (!.state_env$has("surfaces")) {
      return(list(
        success = TRUE,
        message = "No surfaces in cache to clear",
        cleared_ids = character(0)
      ))
    }

    surfaces <- .state_env$get("surfaces")

    # Determine which surfaces to clear
    if (is.null(id) || is.na(id) || id == "") {
      # Clear all surfaces
      cleared_ids <- names(surfaces)
      .state_env$set("surfaces", list())

      message <- if (length(cleared_ids) == 0) {
        "No surfaces in cache to clear"
      } else {
        sprintf(
          "Cleared all %d surface%s from cache",
          length(cleared_ids),
          if (length(cleared_ids) == 1) "" else "s"
        )
      }

    } else {
      # Clear specific surface
      if (!id %in% names(surfaces)) {
        return(list(
          success = FALSE,
          error = sprintf("Surface with ID '%s' not found in cache", id)
        ))
      }

      surfaces[[id]] <- NULL
      .state_env$set("surfaces", surfaces)
      cleared_ids <- id
      message <- "Cleared 1 surface from cache"
    }

    list(
      success = TRUE,
      message = message,
      cleared_ids = cleared_ids
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error clearing surfaces: %s", conditionMessage(e))
    )
  })
}


#' Discover Surface Files in Directory
#'
#' @description
#' Search a directory for neuroimaging surface files (GIfTI, STL, and FreeSurfer formats).
#' Returns a list of found files with basic metadata including file paths.
#' Useful for discovering available surface files before reading them with \code{ieegio-mcp_tool_surface_read}.
#'
#' @param path Character string, absolute or relative path to the directory
#'   to search. Example values: \code{"/data/subjects/fs/surf"}, \code{"~/imaging/surfaces"}
#' @param recursive Logical, whether to search subdirectories recursively.
#'   Defaults to \code{TRUE}. Example values: \code{TRUE}, \code{FALSE}
#' @param pattern Character string, optional filename pattern to filter results.
#'   Empty string (default) returns all surface files. Otherwise, only files whose
#'   basename matches the pattern are included.
#'   Example values: \code{"lh"}, \code{"pial"}, \code{"white"}, \code{""}
#'
#' @return A list containing:
#' \describe{
#'   \item{success}{Logical, whether the search completed successfully}
#'   \item{count}{Integer, number of surface files found}
#'   \item{files}{Character vector, relative file paths}
#'   \item{error}{Character, error message if success is FALSE}
#' }
#'
#' @examples
#'
#' # Discover all surface files in sample data directory
#' sample_dir <- dirname(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
#' ieegio:::mcp_tool_surface_discover(sample_dir)
#'
#' # Search with a filename pattern
#' ieegio:::mcp_tool_surface_discover(sample_dir, pattern = "shape")
#'
#' @keywords mcp-tool mcp-category-discovery
#' @noRd
mcp_tool_surface_discover <- function(path,
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

    # Filter by surface file extensions and common FreeSurfer surface names
    surface_files <- all_files[vapply(all_files, function(file) {
      fname_lower <- tolower(basename(file))
      
      # GIfTI files
      if (grepl("\\.gii(\\.gz)?$", fname_lower)) {
        return(TRUE)
      }
      
      # STL files
      if (grepl("\\.stl$", fname_lower)) {
        return(TRUE)
      }

      # FreeSurfer surface files (common patterns)
      # Geometry files: lh.pial, rh.white, etc.
      if (grepl("^[lr]h\\.(pial|white|inflated|sphere|orig|smoothwm)", fname_lower)) {
        return(TRUE)
      }
      # Annotation files
      if (grepl("\\.annot$", fname_lower)) {
        return(TRUE)
      }
      # Curvature/measurement files
      if (grepl("^[lr]h\\.(curv|sulc|thickness|area|jacobian_white)", fname_lower)) {
        return(TRUE)
      }

      FALSE
    }, FUN.VALUE = logical(1))]

    list(
      success = TRUE,
      count = length(surface_files),
      files = surface_files
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = sprintf("Error discovering surfaces: %s", conditionMessage(e))
    )
  })
}
