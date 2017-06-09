#' @export
retrieve_data <- function(url_string, out_file = tempfile()) {
    download.file(URLencode(url_string), out_file)
    return(out_file)
}

#' @export
read_nc2list <- function(filename, dims) {
    ncfile <- ncdf4::nc_open(filename)
    on.exit(ncdf4::nc_close(ncfile))
    dims_list <- lapply(dims, function(x) read_ncvar(ncfile, x, TRUE))
    names(dims_list) <- dims

    var_names <- names(ncfile$var)
    vars_list <- lapply(var_names, function(x) read_ncvar(ncfile, x, TRUE))
    names(vars_list) <- var_names

    out_list <- c(dims_list, vars_list)
    return(out_list)
}

#' @export
read_ncvar <- function(ncfile, varname, get_attributes = TRUE) {
    value <- ncdf4::ncvar_get(ncfile, varname)
    if (get_attributes) {
        attributes(value) <- ncdf4::ncatt_get(ncfile, varname)
    }
    return(value)
}
