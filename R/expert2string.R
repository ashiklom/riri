#' @export
expert2string <- function(code) {
    tws <- trimws(code)
    string <- gsub('[[:space:]](?![[:alnum:][:space:]]+[)])+', '/', tws, perl = TRUE)
    return(string)
}

#' @export
expert2url <- function(code, return_type = 'table', ...) {
    base_url <- 'http://iridl.ldeo.columbia.edu'
    code_string <- expert2string(code)
    comp_url <- paste(base_url, code_string, sep = '/')
    if (return_type == 'table') {
        typestring <- returntype_table(...)
        return_url <- paste(comp_url, typestring, sep = '/')
    } else if (return_type == 'ncdf') {
        typestring <- 'data.nc'
    } else {
        stop('Unknown return type ', return_type)
    }
    return_url <- URLencode(paste(comp_url, typestring, sep = '/'))
    return(return_url)
}

