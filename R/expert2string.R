#' Convert IRIDL "expert mode" code to URL string
#'
#' Replaces spaces with slashes, and trims leading and trailing whitespace.
#'
#' @param code String containing IRIDL "expert mode" code
#' @export
expert2string <- function(code) {
    tws <- trimws(code)
    gsub("[[:space:]](?![[:alnum:][:space:]]+[)])+", "/", tws, perl = TRUE)
}

#' Convert IRIDL "expert mode" code directly to URL
#'
#' Convert an IRIDL code string directly to a URL by replacing spaces with 
#' dashes, prepending the IRIDL base URL, and appending a suffix for the 
#' appropriate type of output (see "Details").
#'
#' The following types of output are supported:
#' - `table` -- ASCII table; see [returntype_table()]
#' - `ncdf` -- NetCDF file with filename `data.nc`.
#' 
#' @inheritParams expert2string
#' @param return_type Options for returning the result. See "Details".
#' @export
expert2url <- function(code, return_type = "table", ...) {
    code_string <- expert2string(code)
    comp_url <- paste(iridl_base_url, code_string, sep = "/")
    if (return_type == "table") {
        typestring <- returntype_table(...)
        return_url <- paste(comp_url, typestring, sep = "/")
    } else if (return_type == "ncdf") {
        typestring <- "data.nc"
    } else {
        stop("Unknown return type ", return_type)
    }
    URLencode(paste(comp_url, typestring, sep = "/"))
}

