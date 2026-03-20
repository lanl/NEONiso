# hdf5_utils.R
# Internal HDF5 abstraction layer.
# Supports rhdf5 (Bioconductor, preferred if installed) and h5lite (CRAN, fallback) backends.
# The fallback backend is identified as "h5lite" internally so that existing tests
# (which check backend %in% c("h5lite", "rhdf5")) continue to pass.

# Package-level cache for the detected HDF5 backend.
# Avoids repeated requireNamespace() calls on every HDF5 operation.
.hdf5_cache <- new.env(parent = emptyenv())

#' Detect available HDF5 backend (cached)
#' @return Character string: "h5lite" or "rhdf5"
#' @noRd
.hdf5_backend <- function() {
  if (!is.null(.hdf5_cache$backend)) {
    return(.hdf5_cache$backend)
  }
  if (requireNamespace("rhdf5", quietly = TRUE)) {
    .hdf5_cache$backend <- "rhdf5"
  } else if (requireNamespace("h5lite", quietly = TRUE)) {
    # "h5lite" is used as the identifier for the CRAN backend (now h5lite)
    .hdf5_cache$backend <- "h5lite"
  } else {
    stop("An HDF5 package is required. Install one with:\n",
         "  install.packages('h5lite')       # recommended (CRAN)\n",
         "  BiocManager::install('rhdf5')    # alternative (Bioconductor)")
  }
  .hdf5_cache$backend
}

# ---------------------------------------------------------------------------
# Pseudo-handle for h5lite (path-based API)
# h5lite takes file paths rather than open file handles.  To preserve the
# handle-based public API of hdf5_utils.R, we wrap the file path and current
# group path in a lightweight S3 object that callers treat like an HDF5 handle.
# ---------------------------------------------------------------------------

.h5lite_handle <- function(file_path, group_path = "") {
  structure(list(file_path = file_path, group_path = group_path),
            class = "h5lite_handle")
}

.h5lite_full_path <- function(handle, name) {
  if (handle$group_path == "") name
  else paste0(handle$group_path, "/", name)
}

#' Create a new HDF5 file
#' @param path File path.
#' @return File handle.
#' @noRd
h5_create_file <- function(path) {
  # Remove existing file so we always start fresh
  if (file.exists(path)) {
    file.remove(path)
  }
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    h5lite::h5_create_file(path)
    .h5lite_handle(path)
  } else {
    rhdf5::h5createFile(path)
    rhdf5::H5Fopen(path)
  }
}

#' Open an existing HDF5 file
#' @param path File path.
#' @return File handle.
#' @noRd
h5_open <- function(path) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    .h5lite_handle(path)
  } else {
    rhdf5::H5Fopen(path)
  }
}

#' Close an HDF5 file and all associated handles
#' @param handle File handle.
#' @noRd
h5_close <- function(handle) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    invisible(NULL)
  } else {
    rhdf5::h5closeAll()
  }
}

#' Create a group within an HDF5 file or group
#' @param parent File or group handle.
#' @param name Group path (relative to parent).
#' @return Group handle (invisibly).
#' @noRd
h5_create_group <- function(parent, name) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    full_path <- .h5lite_full_path(parent, name)
    h5lite::h5_create_group(parent$file_path, full_path)
    invisible(.h5lite_handle(parent$file_path, full_path))
  } else {
    rhdf5::H5Gcreate(parent, name)
  }
}

#' Open an existing group
#' @param parent File or group handle.
#' @param name Group path (relative to parent).
#' @return Group handle.
#' @noRd
h5_open_group <- function(parent, name) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    .h5lite_handle(parent$file_path, .h5lite_full_path(parent, name))
  } else {
    rhdf5::H5Gopen(parent, name)
  }
}

#' Close a group handle
#' @param handle Group handle.
#' @noRd
h5_close_group <- function(handle) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    invisible(NULL)
  } else {
    rhdf5::H5Gclose(handle)
  }
}

#' Read all attributes from a group in an HDF5 file
#' @param file_path Path to HDF5 file.
#' @param group_path Group path (without leading /).
#' @return Named list of attributes.
#' @noRd
h5_read_attrs <- function(file_path, group_path) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    attr_names <- h5lite::h5_attr_names(file_path, group_path)
    attrs <- lapply(attr_names, function(nm) h5lite::h5_read(file_path, group_path, attr = nm))
    names(attrs) <- attr_names
    attrs
  } else {
    rhdf5::h5readAttributes(file_path, paste0("/", group_path))
  }
}

#' Write an attribute to an HDF5 object
#' @param handle File or group handle.
#' @param name Attribute name.
#' @param value Attribute value.
#' @noRd
h5_write_attr <- function(handle, name, value) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    h5lite::h5_write(I(value), handle$file_path, handle$group_path, attr = name)
  } else {
    rhdf5::h5writeAttribute(h5obj = handle, attr = value, name = name)
  }
}

#' Write a dataset to an HDF5 group
#' @param parent Group handle.
#' @param name Dataset name.
#' @param data Data to write (data.frame, vector, matrix, etc.).
#' @noRd
h5_write_dataset <- function(parent, name, data) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    full_path <- .h5lite_full_path(parent, name)
    h5lite::h5_write(data, parent$file_path, full_path)
  } else {
    rhdf5::h5writeDataset(obj = data, h5loc = parent, name = name,
                          DataFrameAsCompound = TRUE)
  }
}

#' List top-level groups in an HDF5 file
#' @param file_path Path to HDF5 file.
#' @return Data frame with at least a \code{name} column.
#' @noRd
h5_ls <- function(file_path) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    names_vec <- h5lite::h5_ls(file_path, name = "/", recursive = FALSE)
    data.frame(name = names_vec, stringsAsFactors = FALSE)
  } else {
    tmp <- rhdf5::h5ls(file_path, recursive = 1)
    tmp[tmp$group == "/", "name", drop = FALSE]
  }
}

#' List children of a specific group in an HDF5 file
#' @param file_path Path to HDF5 file.
#' @param group_path Group path (without leading /).
#' @return Character vector of child names.
#' @noRd
h5_ls_group <- function(file_path, group_path) {
  backend <- .hdf5_backend()
  if (backend == "h5lite") {
    h5lite::h5_ls(file_path, name = group_path, recursive = FALSE)
  } else {
    tmp <- rhdf5::h5ls(file_path, recursive = TRUE)
    tmp[tmp$group == paste0("/", group_path), "name"]
  }
}
