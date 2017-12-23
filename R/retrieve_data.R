#' Download file from URL
#'
#' Uses [utils::download.file()] to download file from an IRIDL URL, and 
#' returns the file name.
#'
#' @param url_string Properly formatted IRIDL URL. See [expert2url()].
#' @inheritParams utils::download.file
#' @param ... Additional arguments to [utils::download.file()]
#' @export
retrieve_data <- function(url_string, destfile = tempfile(), ...) {
    download.file(URLencode(url_string), destfile = destfile, ...)
    out_file
}

#' Read all variables from NetCDF file to list
#'
#' @param filename Name of file to read
#' @param dims Vector or list of NetCDF dimensions to read
#' @export
read_nc2list <- function(filename, dims) {
    ncfile <- ncdf4::nc_open(filename)
    on.exit(ncdf4::nc_close(ncfile))
    dims_list <- lapply(dims, function(x) read_ncvar(ncfile, x, TRUE))
    names(dims_list) <- dims

    var_names <- names(ncfile$var)
    vars_list <- lapply(var_names, function(x) read_ncvar(ncfile, x, TRUE))
    names(vars_list) <- var_names

    c(dims_list, vars_list)
}

#' Read single variable from NetCDF file
#'
#' @inheritParams ncdf4::ncvar_get
#' @param get_attributes Add attributes from NetCDF variable to returned value
read_ncvar <- function(nc, varid, get_attributes = TRUE, ...) {
    value <- ncdf4::ncvar_get(nc, varid, ...)
    if (get_attributes) {
        attributes(value) <- ncdf4::ncatt_get(ncfile, varname)
    }
    value
}
