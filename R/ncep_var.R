#' Shortcut for adding NCEP variable
#'
#' Quickly select a variable from NCEP (`SOURCES .NOAA .NCEP .CPC`).
#'
#' Currently accepted variable names are:
#' - `"air_temperature"` (`.GHCN_CAMS .gridded .deg0p5 .temp`) -- 0.5 degree 
#' gridded surface temperature
#' - `"precipitation"` (`.PRECL .v1p0 .deg0p5 .rain`) -- 0.5 degree gridded 
#' precipitation
#' -`"soil_moisture"` (`.GMSM .w`)
#'
#' @param varcode Type of variable.
#' @export
ncep_var <- function(varcode) {
    ncep_base <- c("SOURCES", ".NOAA", ".NCEP", ".CPC")
    accepted_varcodes <- c("air_temperature", "precipitation", "soil_moisture")
    if (!varcode %in% accepted_varcodes) {
        stop("Invalid varcode. Currently working varcodes are: ",
             paste(accepted_varcodes, collase = ", "))
    }
    varlist <- switch(varcode,
                      air_temperature = c(".GHCN_CAMS", ".gridded", ".deg0p5", ".temp"),
                      precipitation = c(".PRECL", ".v1p0", ".deg0p5", ".rain"),
                      soil_moisture = c(".GMSM", ".w"),
                      stop())
    c(ncep_base, varlist)
}
