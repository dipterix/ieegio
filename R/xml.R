cdata <- function (string) {
  cdata_start_tag <- "<![CDATA["
  cdata_end_tag <- "]]>"
  if (grepl(string, cdata_start_tag, fixed = TRUE)) {
    stop(sprintf("Input string must not contain the cdata start tag '%s'.\n",
                 cdata_start_tag))
  }
  if (grepl(string, cdata_end_tag, fixed = TRUE)) {
    stop(sprintf("Input string must not contain the cdata end tag '%s'.\n",
                 cdata_end_tag))
  }
  return(paste(cdata_start_tag, string, cdata_end_tag, sep = ""))
}

check_data_and_settings_consistency <- function (index, data, datatype, intent, force = FALSE) {
  msg <- NULL
  if (is.integer(data) & startsWith(datatype, "NIFTI_TYPE_FLOAT")) {
    msg <- sprintf("Dataset # %d in file will be corrupted: integer R data passed, and written as '%s'.\n",
                  index, datatype)
  }
  if (gifti::convert_intent(intent) == "unknown") {
    msg <- sprintf("Dataset # %d: Invalid NIFTI intent '%s'.\n",
                  index, intent)
  }
  if (!is.null(msg)) {
    if (force) {
      warning(msg)
    }
    else {
      stop(msg)
    }
  }
}

xml_node_gifti_coordtransform <- function (transform_matrix, data_space = "NIFTI_XFORM_UNKNOWN",
          transformed_space = "NIFTI_XFORM_UNKNOWN", as_cdata = TRUE)
{
  if (!is.character(data_space)) {
    stop("Parameter 'data_space' must be a character string.")
  }
  if (!is.character(transformed_space)) {
    stop("Parameter 'transformed_space' must be a character string.")
  }
  if (!is.matrix(transform_matrix)) {
    if(is.null(transform_matrix)) {
      # special case where there is no transform, use identity matrix
      transform_matrix <- diag(1, 4)
    } else {
      stop("Parameter 'transform_matrix' must be a numerical matrix.")
    }
  }
  if (!(ncol(transform_matrix) == 4L & nrow(transform_matrix) ==
        4L)) {
    stop("Parameter 'transform_matrix' must be a numerical 4x4 matrix.")
  }
  if (as_cdata) {
    data_space <- cdata(data_space)
    transformed_space <- cdata(transformed_space)
  }
  r1 <- paste(sprintf("%f", transform_matrix[1, ]), collapse = " ")
  r2 <- paste(sprintf("%f", transform_matrix[2, ]), collapse = " ")
  r3 <- paste(sprintf("%f", transform_matrix[3, ]), collapse = " ")
  r4 <- paste(sprintf("%f", transform_matrix[4, ]), collapse = " ")
  matrix_data_formatted <- paste(r1, r2, r3, r4, sep = "\n")
  matrix_data_formatted <- sprintf("\n%s\n", matrix_data_formatted)
  matrix_node <- xml2::read_xml("<CoordinateSystemTransformMatrix/>")
  data_space_node <- xml2::read_xml(paste("<DataSpace>", data_space,
                                         "</DataSpace>", sep = ""))
  transformed_space_node <- xml2::read_xml(paste("<TransformedSpace>",
                                                transformed_space, "</TransformedSpace>", sep = ""))
  matrix_data_node <- xml2::read_xml(paste("<MatrixData>", matrix_data_formatted,
                                          "</MatrixData>", sep = ""))
  xml2::xml_add_child(matrix_node, data_space_node)
  xml2::xml_add_child(matrix_node, transformed_space_node)
  xml2::xml_add_child(matrix_node, matrix_data_node)
  return(matrix_node)
}

create_gii_xml <- function (data_array,
                            intent = "NIFTI_INTENT_SHAPE",
                            datatype = "NIFTI_TYPE_FLOAT32",
                            encoding = "GZipBase64Binary",
                            endian = "LittleEndian",
                            transform_matrix = NULL,
                            label_table = NULL,
                            force = FALSE)
{
  if (!is.list(data_array)) {
    stop("Parameter 'data_array' must be a list.")
  }
  supported_encodings <- c("ASCII", "Base64Binary", "GZipBase64Binary")
  num_data_arrays <- length(data_array)
  dataarray_contains_matrices <- any(lapply((lapply(data_array, dim)), length) > 0L)
  num_transform_matrices <- 0L
  if (!is.null(transform_matrix)) {
    if (!is.list(transform_matrix)) {
      stop("Parameter 'transform_matrix' must be NULL or a list.")
    }
    if (length(names(transform_matrix)) != 0L) {
      stop(
        "Parameter 'transform_matrix' must not be a named list. Hint: When passing a single matrix, you need to enclose it in an outer list."
      )
    }
    num_transform_matrices <- length(transform_matrix)
    if (num_transform_matrices != num_data_arrays) {
      stop(
        sprintf(
          "Found %d data arrays, but %d transform matrices: mismatch. Pass NA if you have no matrix for a data array.\n",
          num_data_arrays,
          num_transform_matrices
        )
      )
    }
  }
  if (num_data_arrays > 1L) {
    if (length(intent) == 1L) {
      intent <- rep(intent, num_data_arrays)
    }
    if (length(datatype) == 1L) {
      datatype <- rep(datatype, num_data_arrays)
    }
    if (length(encoding) == 1L) {
      encoding <- rep(encoding, num_data_arrays)
    }
    if (length(endian) == 1L) {
      endian <- rep(endian, num_data_arrays)
    }
  }
  dim0 <- rep(1L, num_data_arrays)
  dim1 <- rep(1L, num_data_arrays)
  dimensionality <- rep(1L, num_data_arrays)
  array_indexing_order <- rep("RowMajorOrder", num_data_arrays)
  root <- xml2::xml_new_root("GIFTI",
                            Version = "1.0",
                            NumberOfDataArrays = num_data_arrays)
  metadata <- xml2::xml_add_child(root, xml2::read_xml("<MetaData></MetaData>"))
  xml2::xml_add_child(metadata,
                      xml2::read_xml("<MD><Name>Generator</Name><Value>ieegio</Value></MD>"))
  da_index <- 1L
  data_is_matrix <- FALSE
  for (da in data_array) {
    da_meta <- as.list(attr(da, "meta"))
    if (is.vector(da)) {
      dim0[da_index] <- length(da)
      dim1[da_index] <- 1L
      dimensionality[da_index] <- 1L
    } else if (is.matrix(da)) {
      data_is_matrix <- TRUE
      dimensionality[da_index] <- 2L
      dim0[da_index] <- dim(da)[1]
      dim1[da_index] <- dim(da)[2]
      if (array_indexing_order[da_index] == "RowMajorOrder") {
        da <- as.vector(t(da))
      } else if (array_indexing_order[da_index] == "ColumnMajorOrder") {
        da <- as.vector((da))
      } else {
        stop(
          sprintf(
            "Dataarray # %d: invalid array_indexing_order, must be 'RowMajorOrder' or 'ColumnMajorOrder'.\n",
            da_index
          )
        )
      }
    } else {
      stop("The data_arrays must be of type vector or matrix.")
    }
    if (!encoding[da_index] %in% supported_encodings) {
      stop(sprintf(
        "Dataarray # %d: invalid encoding '%s'.\n",
        da_index,
        encoding[da_index]
      ))
    }
    if (!endian[da_index] %in% c("LittleEndian", "BigEndian")) {
      stop(sprintf("Dataarray # %d: invalid endian '%s'.\n",
                   da_index,
                   endian[da_index]))
    }
    check_data_and_settings_consistency(da_index, da,
                                        datatype[da_index], intent[da_index], force = force)
    data_array_node <- xml2::read_xml("<DataArray/>")
    xml2::xml_set_attr(data_array_node, "Dimensionality",
                       dimensionality[da_index])
    xml2::xml_set_attr(data_array_node, "Dim0", dim0[da_index])
    if (dataarray_contains_matrices) {
      xml2::xml_set_attr(data_array_node, "Dim1",
                         dim1[da_index])
    }
    xml2::xml_set_attr(data_array_node, "Encoding",
                       encoding[da_index])
    xml2::xml_set_attr(data_array_node, "DataType",
                       datatype[da_index])
    xml2::xml_set_attr(data_array_node, "Intent", intent[da_index])
    xml2::xml_set_attr(data_array_node, "Endian", endian[da_index])
    xml2::xml_set_attr(data_array_node, "ExternalFileName",
                       "")
    xml2::xml_set_attr(data_array_node, "ExternalFileOffset",
                       "")
    xml2::xml_set_attr(data_array_node,
                       "ArrayIndexingOrder",
                       array_indexing_order[da_index])
    data_array_node_added <- xml2::xml_add_child(root,
                                                data_array_node)

    data_array_meta_content <- xml2::read_xml("<MetaData></MetaData>")
    if(length(da_meta)) {
      lapply(names(da_meta), function(key) {
        if(!nzchar(key)) { return() }
        md_content <- xml2::read_xml("<MD></MD>")
        val <- paste(format(da_meta[[key]]), collapse = "")
        item <- xml2::read_xml("<Name></Name>")
        xml2::xml_add_child(item, xml2::xml_cdata(key))
        xml2::xml_add_child(md_content, item)

        item <- xml2::read_xml("<Value></Value>")
        xml2::xml_add_child(item, xml2::xml_cdata(val))
        xml2::xml_add_child(md_content, item)
        xml2::xml_add_child(data_array_meta_content, md_content)
        return()
      })
    }
    data_array_metadata <- xml2::xml_add_child(data_array_node_added, data_array_meta_content)
    encoded_data <- gifti::data_encoder(da,
                                       encoding = encoding[da_index],
                                       datatype = datatype[da_index],
                                       endian = endian[da_index])
    data_node <- xml2::read_xml(sprintf("<Data>%s</Data>",
                                       encoded_data))
    if (num_transform_matrices > 0L) {
      tf <- transform_matrix[[da_index]]
      if (is.list(tf)) {
        tf_node <- xml_node_gifti_coordtransform(
          tf$transform_matrix,
          data_space = tf$data_space,
          transformed_space = tf$transformed_space
        )
        xml2::xml_add_child(data_array_node_added,
                            tf_node)
      }
      else if (is.matrix(tf)) {
        tf_node <- xml_node_gifti_coordtransform(tf)
        xml2::xml_add_child(data_array_node_added,
                            tf_node)
      }
      else {
        if (!is.na(tf)) {
          stop(
            sprintf(
              "Invalid transformation matrix at index %d: neither a named list, nor NA.\n",
              da_index
            )
          )
        }
      }
    }
    xml2::xml_add_child(data_array_node_added, data_node)
    da_index <- da_index + 1L
  }

  # construct look-up table
  if(is.data.frame(label_table) && nrow(label_table)) {
    if(!length(label_table$Alpha)) { label_table$Alpha <- 1.0 }
    xml_label_table <- sprintf(
      '<Label Key="%d" Red="%f" Green="%f" Blue="%f" Alpha="%f"><![CDATA[%s]]></Label>',
      label_table$Key, label_table$Red, label_table$Green, label_table$Blue,
      label_table$Alpha, label_table$Label
    )
    xml_label_table <- c("<LabelTable>", xml_label_table, "</LabelTable>")
    label_table_node <- xml2::read_xml(paste(xml_label_table, collapse = "\n"))
    xml2::xml_add_child(root, label_table_node)
  }

  return(root)
}
