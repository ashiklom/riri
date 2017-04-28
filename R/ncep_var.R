#' @export
ncep_var <- function(url_string, varcode) {
    ncep_base <- c('SOURCES', '.NOAA', '.NCEP', '.CPC')
    accepted_varcodes <- c('air_temperature', 'precipitation', 'soil_moisture')
    if (!varcode %in% accepted_varcodes) {
        stop('Invalid varcode. Currently working varcodes are: ', 
             paste(accepted_varcodes, collase = ', '))
    }
    varlist <- switch(varcode, 
                      air_temperature = c('.GHCN_CAMS', '.gridded', '.deg0p5', '.temp'),
                      precipitation = c('.PRECL', '.v1p0'),
                      soil_moisture = c('.GMSM', '.w'),
                      stop())
    output <- c(url_string, ncep_base, varlist)
    return(output)
}
